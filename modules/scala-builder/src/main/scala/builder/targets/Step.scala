package builder.targets

import builder.{Module, ModuleKind}
import builder.{settings, Settings}
import builder.errors.*
import builder.ScalaCommand
import builder.ScalaCommand.SubCommand
import builder.reporter
import builder.ResourceGenerator
import builder.ScalaCommand.InternalCommand
import builder.PlatformKind
import java.io.IOException
import builder.targets.Step.CompileInputs

/** A Step possibly recomputes a Target, and can be parallelized with
  * other steps at the same level
  */
sealed trait Step:
  def module: Module
  def target: Target
  /** executes the step, and signals if the project changed */
  def exec(project: Targets, initial: Targets)(using Settings): Result[Option[TargetState], String]

object Step:

  case class CompileInputs(module: Module, platform: PlatformKind, extra: Shared.Dependencies, ph: ProjectHash)

  case class CachedCompilation[S, T <: TargetState](
    target: Target,
    module: Module,
    platform: PlatformKind,
    lookupState: TargetContext => Option[T],
    projectHash: T => String,
    sourcesHash: T => String,
    computeResult: CompileInputs => Result[S, String],
    test: (T, S) => Unit,
    computeState: (CompileInputs, S) => T,
  ) extends Step:

    def exec(project: Targets, initial: Targets)(using Settings): Result[Option[TargetState], String] = Result:
      val ph = Step.projectHash(module, platform, Set("main")).?
      val deps = Shared.dependencies(module, platform, project, initial)
      val currentState = project.graph.get(module.name).flatMap(lookupState)
      val inputs = CompileInputs(module, platform, deps, ph)
      if !currentState.exists(s => projectHash(s) == ph.projectHash) then
        if deps.changedState then
          Shared.cleanBeforeCompile(module).?
        val newState = computeResult(inputs).?
        Some(computeState(inputs, newState))
      else
        if deps.changedState then
          Shared.cleanBeforeCompile(module).?
          val newState = computeResult(inputs).? // execute compile for side-effects
          for current <- currentState do
            test(current, newState)
          Some(computeState(inputs, newState))
        else if !currentState.exists(s => sourcesHash(s) == ph.sourcesHash) then
          val newState = computeResult(inputs).? // execute compile for side-effects
          for current <- currentState do
            test(current, newState)
          Some(computeState(inputs, newState))
        else
          None
  end CachedCompilation

  case class ProjectHash(structure: ujson.Value, projectHash: String, sourcesHash: String)

  def projectHash(module: Module, platform: PlatformKind, scopes: Set[String])(using Settings): Result[ProjectHash, String] = Result:
    val (projectHash, structure) = Shared.readStructure(module, platform).?
    val sourcesHash = Shared.hash(module, structure, scopes).?
    ProjectHash(structure, projectHash, sourcesHash)

  def mainArgs(info: ModuleKind.Application): Result[List[String], String] =
    Result:
      info.mainClass match
        case None => failure("no main class specified, TODO: interactive mode")
        case Some(value) => "--main-class" :: value :: Nil

end Step

object CompileScalaStep:

  case class LibraryOuts(dependencies: List[String], classpath: List[String])

  def computeClasspath(inputs: CompileInputs)(using Settings) = Result:
    import inputs.*
    val buildDir = os.pwd / os.RelPath(module.root) / ".scala-build"
    val buildDirStr = buildDir.toString
    val resourceArgs = Shared.resourceArgs(module)
    val args = ScalaCommand.makeArgs(module, InternalCommand.Compile, extra.classpath, extra.libraries, platform, "--print-class-path", resourceArgs)
    reporter.debug(s"exporting classpath of module ${module.name} with args: ${args.map(_.value.mkString(" ")).mkString(" ")}")
    val res = ScalaCommand.call(args).?
    if res.exitCode != 0 then
      failure(s"failed to compile module ${module.name}: ${res.err.lines().mkString("\n")}")
    val rawClasspath = res.out.lines().head.split(":").toList.distinct.sorted
    val classpath = rawClasspath.filter(_.startsWith(buildDirStr))
    reporter.debug(s"classpath of module ${module.name}:main:${platform} is ${classpath.mkString(":")}")
    val dependencies = Shared.libraryDeps(ph.structure, "main").?
    LibraryOuts(dependencies, classpath)
  end computeClasspath

  def nextLibrary(inputs: CompileInputs, outs: LibraryOuts): TargetState.Library =
    TargetState.Library(
      inputs.ph.projectHash,
      inputs.ph.sourcesHash,
      inputs.platform,
      inputs.extra.libraries,
      inputs.extra.classpath,
      outs.dependencies,
      outs.classpath
    )

  def compareClasspath(old: TargetState.Library, outs: LibraryOuts) =
    assert(old.dependencies == outs.dependencies)
    assert(old.classpath == outs.classpath)

  def apply(module: Module, target: Target, platform: PlatformKind)(using Settings) =
    Step.CachedCompilation(
      target = target,
      module = module,
      platform = platform,
      lookupState = _.optLibrary(platform),
      projectHash = _.projectHash,
      sourcesHash = _.sourcesHash,
      computeResult = computeClasspath,
      test = compareClasspath,
      computeState = nextLibrary,
    )
end CompileScalaStep

object PackageScalaStep:

  def computePackage(inputs: CompileInputs)(using Settings): Result[String, String] = Result:
    import inputs.*
    val info = module.kind.asInstanceOf[ModuleKind.Application]
    val mainArgs = Step.mainArgs(info).?
    val resourceArgs = Shared.resourceArgs(module)
    val artifact = module.platforms.head match
      case PlatformKind.jvm => os.rel / "main.jar"
      case PlatformKind.`scala-js` => os.rel / "main.js"
      case PlatformKind.`scala-native` => os.rel / "main"
    val outputPath = os.pwd / ".scala-builder" / module.name / "packaged"
    Shared.makeDir(outputPath).?
    val artifactPath = outputPath / artifact
    val args = ScalaCommand.makeArgs(module, InternalCommand.Package(artifactPath), extra.classpath, extra.libraries, platform, mainArgs, resourceArgs)
    reporter.debug(s"packaging application ${module.name} with args: ${args.map(_.value.mkString(" ")).mkString(" ")}")
    val res = ScalaCommand.call(args).?
    if res.exitCode != 0 then
      failure(s"failed to package application module ${module.name}: ${res.err.lines().mkString("\n")}")
    reporter.debug(s"package of module ${module.name}:package is $artifactPath")
    artifactPath.toString
  end computePackage

  def nextPackage(inputs: CompileInputs, outPath: String): TargetState.Package =
    TargetState.Package(inputs.ph.projectHash, inputs.ph.sourcesHash, outPath)

  def testPackage(old: TargetState.Package, outPath: String) =
    assert(old.outPath == outPath)

  def of(module: Module, target: Target)(using Settings) =
    Result:
      val possiblePlatforms = module.platforms
      if possiblePlatforms.sizeIs == 1 then
        Step.CachedCompilation(
          target = target,
          module = module,
          platform = possiblePlatforms.head,
          lookupState = _.optPackage,
          projectHash = _.projectHash,
          sourcesHash = _.sourcesHash,
          computeResult = computePackage,
          test = testPackage,
          computeState = nextPackage,
        )
      else
        failure(s"cannot create plan for target ${target.show}, module ${module.name} has multiple platforms: ${possiblePlatforms.mkString(", ")}")

end PackageScalaStep

final case class CopyResourceStep(module: Module, target: Target, fromTarget: Target) extends Step:
  def exec(project: Targets, initial: Targets)(using Settings): Result[Option[TargetState], String] = Result:
    assert(fromTarget.kind == TargetKind.Package)
    val initialDep = initial.optPackage(fromTarget.module)
    val currentDep = project.getPackage(fromTarget.module) // must exist

    def depIsSame = initialDep.exists(_.token == currentDep.token)

    if depIsSame then
      None
    else
      val sourceResourceDest = module.resourceGenerators.collectFirst({
        case ResourceGenerator.Copy(`fromTarget`, dest) => dest
      }).get

      val copyTo = os.RelPath(sourceResourceDest)

      val destDir = Shared.resourceDir(module) / copyTo.segments.init
      Shared.makeDir(destDir).?
      val dest = destDir / copyTo.last
      os.copy(os.Path(currentDep.outPath), dest, replaceExisting = true)
      reporter.debug(s"copied resource from ${currentDep.outPath} to $dest")
      Some(TargetState.Copy(fromTarget))
  end exec
end CopyResourceStep

object RunScalaStep:

  def computeCommand(inputs: CompileInputs)(using Settings) = Result:
    import inputs.*
    val info = module.kind.asInstanceOf[ModuleKind.Application]
    val mainArgs = Step.mainArgs(info).?
    val resourceArgs = Shared.resourceArgs(module)
    val args = ScalaCommand.makeArgs(module, SubCommand.Run, extra.classpath, extra.libraries, PlatformKind.jvm, "--command", mainArgs, resourceArgs)
    reporter.debug(s"compiling application ${module.name} with args: ${args.map(_.value.mkString(" ")).mkString(" ")}")
    val res = ScalaCommand.call(args).?
    if res.exitCode != 0 then
      failure(s"failed to compile application module ${module.name}: ${res.err.lines().mkString("\n")}")
    val command = res.out.lines().toList
    reporter.debug(s"command of application module ${module.name}:runner is ${command.mkString(" ")}")
    command

  def nextApplication(inputs: CompileInputs, outCommand: List[String]): TargetState.Application =
    TargetState.Application(inputs.ph.projectHash, inputs.ph.sourcesHash, outCommand)

  def testApplication(old: TargetState.Application, outCommand: List[String]) =
    assert(old.outCommand == outCommand)

  def apply(module: Module, target: Target)(using Settings) =
    Step.CachedCompilation(
      target = target,
      module = module,
      platform = PlatformKind.jvm,
      lookupState = _.optApplication,
      projectHash = _.projectHash,
      sourcesHash = _.sourcesHash,
      computeResult = computeCommand,
      test = testApplication,
      computeState = nextApplication,
    )
end RunScalaStep
