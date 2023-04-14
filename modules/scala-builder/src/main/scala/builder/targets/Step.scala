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

/** A Step possibly recomputes a Target, and can be parallelized with
  * other steps at the same level
  */
sealed trait Step:
  def module: Module
  /** executes the step, and signals if the project changed */
  def exec(project: Targets, initial: Targets)(using Settings): Result[Option[TargetUpdate], String]

final case class CompileScalaStep(module: Module, platform: PlatformKind) extends Step:
  def exec(project: Targets, initial: Targets)(using Settings): Result[Option[TargetUpdate], String] = Result:
    val initialDeps = module.dependsOn.flatMap(initial.optLibrary(_, platform)) // might not exist yet
    val currentDeps = module.dependsOn.map(project.library(_, platform)) // must exist

    val dclasspath = currentDeps.map(_.outClasspath).flatten.toList.distinct.sorted

    val oldTarget = project.graph.get(module.name)

    val structure = Shared.readStructure(module, platform, dclasspath).?
    val inputHash = Shared.hash(module, structure, Set("main")).?

    def structureIsSame = oldTarget.flatMap(_.optProject(platform)).exists(_ == structure)
    def inputHashIsSame = oldTarget.flatMap(_.optLibrary(platform)).exists(_.inputHash == inputHash)
    def depsChanged = initialDeps.map(_.token) != currentDeps.map(_.token)

    def resourceDir = os.pwd / ".scala-builder" / module.name / "managed_resources"

    def computeClasspath() =
      Result:
        val resourceArgs =
          if module.resourceGenerators.sizeIs > 0 then
            "--resource-dir" :: resourceDir.toString :: Nil
          else
            Nil
        val args = ScalaCommand.makeArgs(module, InternalCommand.Compile, dclasspath, platform, "--print-class-path", resourceArgs)
        reporter.debug(s"exporting classpath of module ${module.name} with args: ${args.map(_.value.mkString(" ")).mkString(" ")}")
        val res = ScalaCommand.call(args).?
        if res.exitCode != 0 then
          failure(s"failed to compile module ${module.name}: ${res.err.lines().mkString("\n")}")
        val classpath = res.out.lines().head.split(":").toList.distinct.sorted
        reporter.debug(s"classpath of module ${module.name}:main:${platform} is ${classpath.mkString(":")}")
        classpath

    if !structureIsSame then
      if depsChanged then
        Shared.doCleanModule(module, dependency = true).?
      val mclasspath = computeClasspath().?
      Some(TargetUpdate(Some(platform -> structure), TargetState.Library(inputHash, platform, dclasspath, mclasspath)))
    else
      val previous = oldTarget.get.optLibrary(platform)
      if depsChanged then
        Shared.doCleanModule(module, dependency = true).?
        val mclasspath = computeClasspath().? // execute compile for side-effects
        for old <- previous do
          assert(old.outClasspath == mclasspath) // output classpath should be the same
        Some(TargetUpdate(Some(platform -> structure), TargetState.Library(inputHash, platform, dclasspath, mclasspath)))
      else if !inputHashIsSame then
        val mclasspath = computeClasspath().? // execute compile for side-effects
        for old <- previous do
          assert(old.outClasspath == mclasspath) // output classpath should be the same
        Some(TargetUpdate(Some(platform -> structure), TargetState.Library(inputHash, platform, dclasspath, mclasspath)))
      else
        None
  end exec
end CompileScalaStep

final case class PackageScalaStep(module: Module, info: ModuleKind.Application, platform: PlatformKind) extends Step:

  def exec(project: Targets, initial: Targets)(using Settings): Result[Option[TargetUpdate], String] = Result:
    val initialDeps = module.dependsOn.flatMap(initial.optLibrary(_, platform)) // might not exist yet
    val currentDeps = module.dependsOn.map(project.library(_, platform)) // must exist

    val dclasspath = currentDeps.map(_.outClasspath).flatten.toList.distinct.sorted

    val oldTarget = project.graph.get(module.name)

    val structure = Shared.readStructure(module, platform, dclasspath).?
    val inputHash = Shared.hash(module, structure, Set("main")).?

    def structureIsSame = oldTarget.flatMap(_.optProject(platform)).exists(_ == structure)
    def inputHashIsSame = oldTarget.flatMap(_.optPackage).exists(_.inputHash == inputHash)
    def depsChanged = initialDeps.map(_.token) != currentDeps.map(_.token)

    def resourceDir = os.pwd / ".scala-builder" / module.name / "managed_resources"

    def computePackage(): Result[String, String] = Result:
      val mainArgs = info.mainClass match
        case None => failure("no main class specified, TODO: interactive mode")
        case Some(value) => "--main-class" :: value :: Nil
      val resourceArgs =
        if module.resourceGenerators.sizeIs > 0 then
          "--resource-dir" :: resourceDir.toString :: Nil
        else
          Nil
      val artifact = module.platforms.head match
        case PlatformKind.jvm => os.rel / "main.jar"
        case PlatformKind.`scala-js` => os.rel / "main.js"
        case PlatformKind.`scala-native` => os.rel / "main"
      val outputPath = os.pwd / ".scala-builder" / module.name / "packaged"
      Shared.makeDir(outputPath).?
      val artifactPath = outputPath / artifact
      val args = ScalaCommand.makeArgs(module, InternalCommand.Package(artifactPath), dclasspath, platform, mainArgs, resourceArgs)
      reporter.debug(s"packaging application ${module.name} with args: ${args.map(_.value.mkString(" ")).mkString(" ")}")
      val res = ScalaCommand.call(args).?
      if res.exitCode != 0 then
        failure(s"failed to package application module ${module.name}: ${res.err.lines().mkString("\n")}")
      reporter.debug(s"package of module ${module.name}:package is $artifactPath")
      artifactPath.toString

    if !structureIsSame then
      // reporter.info(s"project structure changed for ${module.name}")
      if depsChanged then
        // reporter.info(s"cleaning module ${module.name} because dependencies changed")
        Shared.doCleanModule(module, dependency = true).?
      val outPath = computePackage().?
      Some(TargetUpdate(Some(platform -> structure), TargetState.Package(inputHash, outPath)))
    else
      val previous = oldTarget.get.optPackage
      if depsChanged then
        Shared.doCleanModule(module, dependency = true).?
        val outPath = computePackage().? // execute compile for side-effects
        for old <- previous do
          assert(old.outPath == outPath) // output classpath should be the same
        Some(TargetUpdate(Some(platform -> structure), TargetState.Package(inputHash, outPath)))
      else if !inputHashIsSame then
        val outPath = computePackage().? // execute compile for side-effects
        for old <- previous do
          assert(old.outPath == outPath) // output classpath should be the same
        Some(TargetUpdate(Some(platform -> structure), TargetState.Package(inputHash, outPath)))
      else
        None
  end exec

end PackageScalaStep

object PackageScalaStep:

  def of(target: Target, module: Module)(using Settings): Result[PackageScalaStep, String] =
    Result:
      val possiblePlatforms = module.platforms
      if possiblePlatforms.sizeIs == 1 then
        PackageScalaStep(module, module.kind.asInstanceOf[ModuleKind.Application], possiblePlatforms.head)
      else
        failure(s"cannot create plan for target ${target.show}, module ${module.name} has multiple platforms: ${possiblePlatforms.mkString(", ")}")


end PackageScalaStep

final case class CopyResourceStep(module: Module, fromTarget: Target) extends Step:
  def exec(project: Targets, initial: Targets)(using Settings): Result[Option[TargetUpdate], String] = Result:
    assert(fromTarget.kind == TargetKind.Package)
    val currentDep = project.getPackage(fromTarget.module) // must exist
    val sourceResourceDest = module.resourceGenerators.collectFirst({
      case ResourceGenerator.Copy(m, dest) => dest
    }).get

    val copyTo = os.RelPath(sourceResourceDest)

    val destDir = os.pwd / ".scala-builder" / module.name / "managed_resources" / copyTo.segments.init
    Shared.makeDir(destDir).?
    val dest = destDir / copyTo.last
    os.copy(os.Path(currentDep.outPath), dest, replaceExisting = true)
    reporter.debug(s"copied resource from ${currentDep.outPath} to $dest")
    None // No caching for now
  end exec
end CopyResourceStep

final case class RunScalaStep(module: Module, info: ModuleKind.Application) extends Step:
  def exec(project: Targets, initial: Targets)(using Settings): Result[Option[TargetUpdate], String] = Result:
    val initialDeps = module.dependsOn.flatMap(initial.optLibrary(_, PlatformKind.jvm)) // might not exist yet
    val currentDeps = module.dependsOn.map(project.library(_, PlatformKind.jvm)) // must exist

    val dclasspath = currentDeps.map(_.outClasspath).flatten.toList.distinct.sorted

    val oldTarget = project.graph.get(module.name)

    val structure = Shared.readStructure(module, PlatformKind.jvm, dclasspath).?
    val inputHash = Shared.hash(module, structure, Set("main")).?

    def structureIsSame = oldTarget.flatMap(_.optProject(PlatformKind.jvm)).exists(_ == structure)
    def inputHashIsSame = oldTarget.flatMap(_.optApplication).exists(_.inputHash == inputHash)
    def depsChanged = initialDeps.map(_.token) != currentDeps.map(_.token)

    def resourceDir = os.pwd / ".scala-builder" / module.name / "managed_resources"

    def computeCommand() = Result:
      val mainArgs = info.mainClass match
        case None => failure("no main class specified, TODO: interactive mode")
        case Some(value) => "--main-class" :: value :: "--command" :: Nil
      val resourceArgs =
        if module.resourceGenerators.sizeIs > 0 then
          "--resource-dir" :: resourceDir.toString :: Nil
        else
          Nil
      val args = ScalaCommand.makeArgs(module, SubCommand.Run, dclasspath, PlatformKind.jvm, mainArgs, resourceArgs)
      reporter.debug(s"compiling application ${module.name} with args: ${args.map(_.value.mkString(" ")).mkString(" ")}")
      val res = ScalaCommand.call(args).?
      if res.exitCode != 0 then
        failure(s"failed to compile application module ${module.name}: ${res.err.lines().mkString("\n")}")
      val command = res.out.lines().toList
      reporter.debug(s"command of application module ${module.name}:runner is ${command.mkString(" ")}")
      command

    if !structureIsSame then
      // reporter.info(s"project structure changed for ${module.name}")
      if depsChanged then
        // reporter.info(s"cleaning module ${module.name} because dependencies changed")
        Shared.doCleanModule(module, dependency = true).?
      val outCommand = computeCommand().?
      Some(TargetUpdate(Some(PlatformKind.jvm -> structure), TargetState.Application(inputHash, outCommand)))
    else
      val previous = oldTarget.get.optApplication
      if depsChanged then
        Shared.doCleanModule(module, dependency = true).?
        val outCommand = computeCommand().? // execute compile for side-effects
        for old <- previous do
          assert(old.outCommand == outCommand) // output classpath should be the same
        Some(TargetUpdate(Some(PlatformKind.jvm -> structure), TargetState.Application(inputHash, outCommand)))
      else if !inputHashIsSame then
        val outCommand = computeCommand().? // execute compile for side-effects
        for old <- previous do
          assert(old.outCommand == outCommand) // output classpath should be the same
        Some(TargetUpdate(Some(PlatformKind.jvm -> structure), TargetState.Application(inputHash, outCommand)))
      else
        None
  end exec
end RunScalaStep
