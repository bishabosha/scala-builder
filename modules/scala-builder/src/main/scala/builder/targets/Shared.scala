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

private[targets] object Shared:

  def makeDir(path: os.Path): Result[Unit, String] =
    Result.attempt:
      os.makeDir.all(path)
    .resolve:
      case err: IOException => s"failed to create directory $path: ${err.getMessage}"

  def readStructure(module: Module, platform: PlatformKind)(using Settings): Result[ujson.Value, String] =
    Result:
      val args = ScalaCommand.makeArgs(module, InternalCommand.ExportJson, classpath = Nil, dependencies = Nil, platform)
      val result = ScalaCommand.call(args).?
      if result.exitCode != 0 then
        failure(s"failed to read structure of module ${module.name}: ${result.err.lines().mkString("\n")}")
      else
        ujson.read(result.out.text())

  def dependencies(project: ujson.Value, scope: String): Result[List[String], String] =
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
        val dependencies = scopeObj.obj.get("dependencies").?
        dependencies.arr.toList.map(dependency.?)

    dependencies.asSuccess("failed to read dependencies")
  end dependencies

  def hash(module: Module, project: ujson.Value, scopes: Set[String]): Result[String, String] =
    val md = MessageDigest.getInstance("SHA-1")

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
      for
        scope <- scopedSources
        source <- scope
      do
        val bytes = readFile(source.str).?
        md.update(bytes)

      val digest        = md.digest()
      val calculatedSum = BigInteger(1, digest)
      val hash          = String.format(s"%040x", calculatedSum).take(10)
      hash
  end hash

  def runStep(
    step: Step, curr: Targets, initial: Targets
  )(using Settings): Result[Option[(String, TargetUpdate)], String] =
    Result:
      reporter.debug(s"running step for target ${step.target.show}...")
      val update = step.exec(project = curr, initial = initial).?
      update.map(step.module.name -> _)

  def doCleanModule(module: Module, dependency: Boolean)(using Settings): Result[Unit, String] =
    Result:
      if dependency then
        reporter.debug(s"dependency of ${module.name} updated, cleaning module ${module.name}...")
      else
        reporter.info(s"cleaning module ${module.name}...")
      val args = ScalaCommand.makeArgs(module, SubCommand.Clean, Nil, Nil, PlatformKind.jvm)
      val result = ScalaCommand.call(args).?
      if result.exitCode != 0 then
        failure(s"failed to clean module ${module.name}: ${result.err.lines().mkString("\n")}")

end Shared
