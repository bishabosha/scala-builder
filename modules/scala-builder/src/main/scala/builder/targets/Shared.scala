package builder.targets

import builder.Module
import builder.Settings
import builder.errors.*
import builder.ScalaCommand
import builder.ScalaCommand.SubCommand
import builder.ScalaCommand.InternalCommand
import builder.reporter
import java.security.MessageDigest
import java.math.BigInteger
import java.io.IOException
import builder.PlatformKind

private[builder] object Shared:


  case class Dependencies(initialState: List[TargetState.Library], currentState: List[TargetState.Library]):
    val libraries = currentState.flatMap(s => s.dependencies ::: s.extraDependencies).distinct.sorted
    val classpath = currentState.flatMap(s => s.classpath ::: s.extraClasspath).distinct.sorted
    def changedState = initialState.map(_.token) != currentState.map(_.token)


  def dependencies(module: Module, platform: PlatformKind, project: Targets, initial: Targets)(using Settings): Dependencies =
    val initialDeps = module.dependsOn.flatMap(initial.optLibrary(_, platform)) // might not exist yet
    val currentDeps = module.dependsOn.map(project.library(_, platform)) // must exist
    Dependencies(initialDeps, currentDeps)

  def resourceDir(module: Module) = os.pwd / ".scala-builder" / module.name / "managed_resources"

  def resourceArgs(module: Module): List[String] =
    if module.resourceGenerators.sizeIs > 0 then
      "--resource-dir" :: resourceDir(module).toString :: Nil
    else
      Nil

  def makeDir(path: os.Path): Result[Unit, String] =
    Result.attempt:
      os.makeDir.all(path)
    .resolve:
      case err: IOException => s"failed to create directory $path: ${err.getMessage}"

  def clearAndRemoveDir(path: os.Path): Result[Unit, String] =
    Result.attempt:
      os.remove.all(path)
    .resolve:
      case err: IOException => s"failed to create directory $path: ${err.getMessage}"

  def readStructure(module: Module, platform: PlatformKind)(using Settings): Result[(String, ujson.Value), String] =
    Result:
      val args = ScalaCommand.makeArgs(module, InternalCommand.ExportJson, classpath = Nil, dependencies = Nil, platform)
      val result = ScalaCommand.call(args).?
      if result.exitCode != 0 then
        failure(s"failed to read structure of module ${module.name}: ${result.err.lines().mkString("\n")}")
      else
        val json = result.out.text()
        val project = ujson.read(json)
        val hasher = Hasher()
        hasher.mix(json.getBytes("UTF-8"))
        (hasher.result, project)

  def libraryDeps(project: ujson.Value, scope: String): Result[List[String], String] =
    def dependency(value: ujson.Value): Option[String] = optional:
      val groupId = value.obj.get("groupId").?.str
      val fullName =
        val artifactId = value.obj.get("artifactId").?
        artifactId.obj.get("fullName").?.str
      val version = value.obj.get("version").?.str
      s"$groupId:$fullName:$version"

    val dependencies =
      optional:
        val scopesObj = project.obj.get("scopes").?
        val scopeObj = scopesObj.obj.get(scope).?
        val dependencies = scopeObj.obj.get("dependencies").map(_.arr.toList).getOrElse(Nil)
        dependencies.map(dependency.?)

    dependencies.asSuccess("failed to read dependencies")
  end libraryDeps

  def hash(module: Module, project: ujson.Value, scopes: Set[String]): Result[String, String] =
    def readFile(path: String): Result[Array[Byte], String] =
      Result.attempt:
        os.read.bytes(os.pwd / os.RelPath(module.root) / os.RelPath(path))
      .resolve:
        case err: IOException => s"failed to hash file ${path}: ${err.getMessage}"

    def parseScopedSources = optional:
      val scopesObj = project.obj.get("scopes").?
      val scopeObjs = scopes.toList.map(scopesObj.obj.get.?)
      val sourceArrs = scopeObjs.map(_.obj.get("sources").?.arr)
      sourceArrs

    Result:
      val scopedSources = parseScopedSources.asSuccess("failed to read sources").?
      val hasher = Hasher()
      for
        scope <- scopedSources
        source <- scope
      do
        val bytes = readFile(source.str).?
        hasher.mix(bytes)

      hasher.result
  end hash

  class Hasher:
    val md = MessageDigest.getInstance("SHA-1")

    def mix(bytes: Array[Byte]): Unit =
      md.update(bytes)

    def result: String =
      val digest        = md.digest()
      val calculatedSum = BigInteger(1, digest)
      val hash          = String.format(s"%040x", calculatedSum).take(10)
      hash
    end result
  end Hasher

  def runStep(
    step: Step, curr: Targets, initial: Targets
  )(using Settings): Result[Option[(String, TargetState)], String] =
    Result:
      reporter.debug(s"running step for target ${step.target.show}...")
      val update = step.exec(project = curr, initial = initial).?
      update.map(step.module.name -> _)

  def cleanBeforeCompile(module: Module)(using Settings) =
    reporter.debug(s"dependency of ${module.name} updated, cleaning before compilation...")
    Shared.doCleanModule(module)

  def doCleanModule(module: Module)(using Settings): Result[Unit, String] =
    Result:
      val args = ScalaCommand.makeArgs(module, SubCommand.Clean, Nil, Nil, PlatformKind.jvm)
      val result = ScalaCommand.call(args).?
      if result.exitCode != 0 then
        failure(s"failed to clean module ${module.name}: ${result.err.lines().mkString("\n")}")

end Shared
