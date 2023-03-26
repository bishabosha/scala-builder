package builder

import ScalaCommand.SubCommand
import SharedPlan.*

sealed trait RunPlan:
  def exec(): Unit

sealed trait TestPlan:
  def test(): Unit

sealed trait ReplPlan:
  def repl(): Unit

sealed trait CleanPlan:
  def clean(): Unit

sealed trait Target[T]:
  /** retrieves the target, and signals if it changed */
  def fetch: (Boolean, T)

object Target:
  type Result[T] = (Boolean, T)

private object SharedPlan:

  trait ClasspathTarget(module: Module) extends Target[List[String]]

  private def compileLibrary(module: Module)(using Settings): ClasspathTarget =
    val deps = compileDeps(module)

    new ClasspathTarget(module):

      def fetch: Target.Result[List[String]] =
        val (clean, dclasspath) = depsClasspath(deps)
        reporter.info(s"maybe compiling library module ${module.name}...")
        if clean then doCleanModule(module)

        val args = ScalaCommand.makeArgs(module, SubCommand.Compile, dclasspath, "--print-class-path")
        reporter.debug(s"exporting classpath of module ${module.name} with args: ${args.mkString(" ")}")
        val res = os.proc(args)
          .call(stdout = os.Pipe, stderr = os.Pipe)

        val downstreamClean = clean || res.err.lines().nonEmpty
        val mclasspath = res.out.lines().head.split(":").toList.distinct.sorted

        (downstreamClean, mclasspath)
      end fetch
    end new
  end compileLibrary

  private def compileResource(module: Module)(using Settings): ClasspathTarget =
    new ClasspathTarget(module):
      def fetch: Target.Result[List[String]] = (false, Nil)

  private def compileDep(module: Module)(using Settings): ClasspathTarget =
    module.kind match
      case ModuleKind.Library => compileLibrary(module)
      case ModuleKind.Resource => compileResource(module)
      case ModuleKind.Application(_) => assert(false, "application modules should not be dependencies")

  def doCleanModule(module: Module)(using Settings): Unit =
    reporter.info(s"dependency of ${module.name} updated, cleaning module ${module.name}...")
    os.proc(ScalaCommand.makeArgs(module, SubCommand.Clean, Nil))
      .spawn(stdin = os.Inherit, stdout = os.Inherit, stderr = os.Inherit).join()

  def depsClasspath(deps: List[ClasspathTarget]): (Boolean, List[String]) =
    val (cleans, classpaths) = deps.map(_.fetch).unzip
    (cleans.exists(identity), classpaths.flatten.distinct.sorted)

  def compileDeps(module: Module)(using Settings): List[ClasspathTarget] =
    module.dependsOn.flatMap(settings.config.modules.get).map(compileDep)

end SharedPlan

object CleanPlan:
  def compile(module: Module)(using Settings): CleanPlan =
    new:
      def clean(): Unit =
        reporter.info(s"cleaning module ${module.name}")
        os.proc("scala", "clean", os.pwd / module.root).call()

object TestPlan:

  def compile(module: Module)(using Settings): TestPlan = compileLeaf(module)

  private def compileLeaf(module: Module)(using Settings): TestPlan =
    val deps = compileDeps(module)

    new:
      def test(): Unit =
        val (clean, classpath) = depsClasspath(deps)

        if clean then doCleanModule(module)

        reporter.info(s"testing module ${module.name}:")
        val result =
          os.proc(ScalaCommand.makeArgs(module, SubCommand.Test, classpath, Nil))
            .spawn(stdin = os.Inherit, stdout = os.Inherit, stderr = os.Inherit)

        if !result.join() then
          println(s"failure with exit code ${result.exitCode()}")

object RunPlan:

  def compile(module: Module)(using Settings): Either[String, RunPlan] =
    module.kind match
      case app @ ModuleKind.Application(_) => Right(compileApplication(module, app))
      case _ => Left(s"module ${module.name} is not an application module")

  private def compileApplication(module: Module, app: ModuleKind.Application)(using Settings): RunPlan =
    val deps = compileDeps(module)

    new:
      def exec(): Unit =
        val (clean, classpath) = depsClasspath(deps)

        if clean then doCleanModule(module)

        val (mainMessage, mainArgs) = app.mainClass match
          case None => ("with dynamic main class", "--interactive" :: Nil)
          case Some(value) => (s"with specified main class $value", "--main-class" :: value :: Nil)

        reporter.info(s"running module ${module.name} $mainMessage:")
        val result =
          val args = ScalaCommand.makeArgs(module, SubCommand.Run, classpath, mainArgs)
          reporter.debug(s"running: ${args.mkString(" ")}")
          os.proc(args)
            .spawn(stdin = os.Inherit, stdout = os.Inherit, stderr = os.Inherit)

        if !result.join() then
          println(s"failure with exit code ${result.exitCode()}")

object ReplPlan:

  def compile(module: Module)(using Settings): ReplPlan = compileLeaf(module)

  private def compileLeaf(module: Module)(using Settings): ReplPlan =
    val deps = compileDeps(module)

    new:
      def repl(): Unit =
        val (clean, classpath) = depsClasspath(deps)

        if clean then doCleanModule(module)

        reporter.info(s"running module ${module.name}:")
        val result =
          os.proc(ScalaCommand.makeArgs(module, SubCommand.Repl, classpath))
            .spawn(stdin = os.Inherit, stdout = os.Inherit, stderr = os.Inherit)

        if !result.join() then
          println(s"failure with exit code ${result.exitCode()}")