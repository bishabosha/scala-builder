package builder.targets

import builder.reporter
import builder.errors.*

import builder.{Module, ModuleKind}
import builder.{settings, Settings}

import builder.ScalaCommand, ScalaCommand.SubCommand

import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global
import builder.PlatformKind

/** Tasks operate on the target graph, i.e. they do not produce cacheable results. */
object Tasks:

  def clean(modules: Set[Module])(using Settings): Result[Unit, String] =
    if settings.sequential then
      Result:
        for module <- modules do
          Shared.doCleanModule(module, dependency = false).?
    else
      val futures = for module <- modules.toList yield
        Future:
          blocking:
            Shared.doCleanModule(module, dependency = false)
      val results = Await.result(Future.sequence(futures), Duration.Inf)
      Result:
        results.foreach(_.?)
  end clean

  def run(module: Module, info: ModuleKind.Application, project: Targets)(using Settings): Result[Unit, String] =
    Result:
      val target = project.application(module.name)
      val appCommand = target.outCommand

      val mainMessage = info.mainClass match
        case None => failure("no main class specified, TODO: interactive mode")
        case Some(value) => s"with specified main class $value"

      reporter.debug(s"running command: ${appCommand.mkString(" ")}")
      val result = ScalaCommand.spawn(List(appCommand)).?

      if !result.join() then
        failure(s"failure with exit code ${result.exitCode()}")

  def repl(module: Module, project: Targets)(using Settings): Result[Unit, String] =
    def resourceDir = os.pwd / ".scala-builder" / module.name / "managed_resources"

    Result:
      val target = project.library(module.name, PlatformKind.jvm)
      val classpath = target.depsClasspath
      val dependencies = target.depsDependencies
      val resourceArgs =
        if module.resourceGenerators.sizeIs > 0 then
          "--resource-dir" :: resourceDir.toString :: Nil
        else
          Nil
      val args = ScalaCommand.makeArgs(module, SubCommand.Repl, classpath, dependencies, PlatformKind.jvm, resourceArgs)
      reporter.debug(s"running command: ${args.map(_.value.mkString(" ")).mkString(" ")}")
      val result = ScalaCommand.spawn(args).?

      if !result.join() then
        failure(s"failure with exit code ${result.exitCode()}")

  def test(modules: Set[Module], project: Targets, initial: Targets)(using Settings): Result[Unit, String] =
    def testOne(module: Module): Result[Unit, String] =
      def resourceDir = os.pwd / ".scala-builder" / module.name / "managed_resources"
      Result:
        val initialDeps = module.dependsOn.flatMap(initial.optLibrary(_, PlatformKind.jvm)) // might not exist yet
        val targetDeps = module.dependsOn.map(project.library(_, PlatformKind.jvm)) // must exist
        val shouldClean = initialDeps.map(_.token) != targetDeps.map(_.token)
        if shouldClean then
          // need to clean
          Shared.doCleanModule(module, dependency = true).?

        val classpath = targetDeps.flatMap(_.outClasspath).distinct.sorted
        val dependencies = targetDeps.flatMap(_.outDependencies).distinct.sorted

        val resourceArgs =
          if module.resourceGenerators.sizeIs > 0 then
            "--resource-dir" :: resourceDir.toString :: Nil
          else
            Nil

        val args = ScalaCommand.makeArgs(module, SubCommand.Test, classpath, dependencies, PlatformKind.jvm, resourceArgs)
        reporter.debug(s"running command: ${args.map(_.value.mkString(" ")).mkString(" ")}")
        val result = ScalaCommand.spawn(args).?

        if !result.join() then
          failure(s"failure with exit code ${result.exitCode()}")
    Result:
      if settings.sequential then
        for module <- modules do
          testOne(module).?
      else
        val futures = for module <- modules.toList yield
          Future:
            blocking:
              testOne(module)
        val results = Await.result(Future.sequence(futures), Duration.Inf)
        results.foreach(_.?)
