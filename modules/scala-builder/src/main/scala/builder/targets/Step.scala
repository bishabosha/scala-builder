package builder.targets

import builder.{Module, ModuleKind}
import builder.{settings, Settings}
import builder.errors.*
import builder.ScalaCommand
import builder.ScalaCommand.SubCommand
import builder.reporter

/** A Step possibly recomputes a Target, and can be parallelized with
  * other steps at the same level
  */
sealed trait Step:
  def module: Module
  /** executes the step, and signals if the project changed */
  def exec(project: Targets, initial: Targets)(using Settings): Result[Option[TargetUpdate], String]


object CompileScalaStep:

  def of(module: Module)(using Settings): Result[CompileScalaStep, String] = Result:
    for dep <- module.dependsOn do
      if settings.config.modules(dep).kind.isInstanceOf[ModuleKind.Application] then
        failure(s"module ${module.name} depends on application module ${dep}")
    CompileScalaStep(module)

final case class CompileScalaStep private (module: Module) extends Step:
  def exec(project: Targets, initial: Targets)(using Settings): Result[Option[TargetUpdate], String] = Result:
    val initialDeps = module.dependsOn.flatMap(initial.optLibrary) // might not exist yet
    val currentDeps = module.dependsOn.map(project.library) // must exist

    val dclasspath = currentDeps.collect {
      case Target.Library(_, _, cp) => cp
    }.flatten.toList.distinct.sorted

    val oldTarget = project.graph.get(module.name)

    val structure = Shared.readStructure(module, dclasspath).?
    val inputHash = Shared.hash(module, structure, Set("main")).?

    def structureIsSame = oldTarget.exists(_.project == structure)
    def inputHashIsSame = oldTarget.flatMap(_.optLibrary).exists(_.inputHash == inputHash)
    def depsChanged = initialDeps.map(_.token) != currentDeps.map(_.token)

    def computeClasspath() =
      Result:
        val args = ScalaCommand.makeArgs(module, SubCommand.Compile, dclasspath, "--print-class-path")
        reporter.debug(s"exporting classpath of module ${module.name} with args: ${args.map(_.value.mkString(" ")).mkString(" ")}")
        val res = ScalaCommand.call(args).?
        if res.exitCode != 0 then
          failure(s"failed to compile module ${module.name}: ${res.err.lines().mkString("\n")}")
        res.out.lines().head.split(":").toList.distinct.sorted

    if !structureIsSame then
      if depsChanged then
        Shared.doCleanModule(module, dependency = true).?
      val mclasspath = computeClasspath().?
      Some(TargetUpdate(structure, Target.Library(inputHash, dclasspath, mclasspath)))
    else
      val previous = oldTarget.get.optLibrary
      if depsChanged then
        Shared.doCleanModule(module, dependency = true).?
        val mclasspath = computeClasspath().? // execute compile for side-effects
        for old <- previous do
          assert(old.outClasspath == mclasspath) // output classpath should be the same
        Some(TargetUpdate(structure, Target.Library(inputHash, dclasspath, mclasspath)))
      else if !inputHashIsSame then
        val mclasspath = computeClasspath().? // execute compile for side-effects
        for old <- previous do
          assert(old.outClasspath == mclasspath) // output classpath should be the same
        Some(TargetUpdate(structure, Target.Library(inputHash, dclasspath, mclasspath)))
      else
        None
  end exec
end CompileScalaStep

object RunScalaStep:

  def of(module: Module, info: ModuleKind.Application)(using Settings): Result[RunScalaStep, String] = Result:
    for dep <- module.dependsOn do
      if settings.config.modules(dep).kind.isInstanceOf[ModuleKind.Application] then
        failure(s"module ${module.name} depends on application module ${dep}")
    RunScalaStep(module, info)

final case class RunScalaStep private (module: Module, info: ModuleKind.Application) extends Step:
  def exec(project: Targets, initial: Targets)(using Settings): Result[Option[TargetUpdate], String] = Result:
    val initialDeps = module.dependsOn.flatMap(initial.optLibrary) // might not exist yet
    val currentDeps = module.dependsOn.map(project.library) // must exist

    val dclasspath = currentDeps.collect {
      case Target.Library(_, _, cp) => cp
    }.flatten.toList.distinct.sorted

    val oldTarget = project.graph.get(module.name)

    val structure = Shared.readStructure(module, dclasspath).?
    val inputHash = Shared.hash(module, structure, Set("main")).?

    def structureIsSame = oldTarget.exists(_.project == structure)
    def inputHashIsSame = oldTarget.flatMap(_.optApplication).exists(_.inputHash == inputHash)
    def depsChanged = initialDeps.map(_.token) != currentDeps.map(_.token)

    def computeCommand() = Result:
      val mainArgs = info.mainClass match
        case None => failure("no main class specified, TODO: interactive mode")
        case Some(value) => "--main-class" :: value :: "--command" :: Nil
      val args = ScalaCommand.makeArgs(module, SubCommand.Run, dclasspath, mainArgs)
      reporter.debug(s"compiling application ${module.name} with args: ${args.map(_.value.mkString(" ")).mkString(" ")}")
      val res = ScalaCommand.call(args).?
      if res.exitCode != 0 then
        failure(s"failed to compile application module ${module.name}: ${res.err.lines().mkString("\n")}")
      res.out.lines().toList

    if !structureIsSame then
      // reporter.info(s"project structure changed for ${module.name}")
      if depsChanged then
        // reporter.info(s"cleaning module ${module.name} because dependencies changed")
        Shared.doCleanModule(module, dependency = true).?
      val outCommand = computeCommand().?
      Some(TargetUpdate(structure, Target.Application(inputHash, outCommand)))
    else
      val previous = oldTarget.get.optApplication
      if depsChanged then
        Shared.doCleanModule(module, dependency = true).?
        val outCommand = computeCommand().? // execute compile for side-effects
        for old <- previous do
          assert(old.outCommand == outCommand) // output classpath should be the same
        Some(TargetUpdate(structure, Target.Application(inputHash, outCommand)))
      else if !inputHashIsSame then
        val outCommand = computeCommand().? // execute compile for side-effects
        for old <- previous do
          assert(old.outCommand == outCommand) // output classpath should be the same
        Some(TargetUpdate(structure, Target.Application(inputHash, outCommand)))
      else
        None
  end exec
end RunScalaStep
