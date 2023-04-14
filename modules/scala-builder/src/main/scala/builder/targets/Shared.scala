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

  def readStructure(module: Module, platform: PlatformKind, classpath: List[String])(using Settings): Result[ujson.Value, String] =
    Result:
      val args = ScalaCommand.makeArgs(module, InternalCommand.ExportJson, classpath, platform)
      val result = ScalaCommand.call(args).?
      if result.exitCode != 0 then
        failure(s"failed to read structure of module ${module.name}: ${result.err.lines().mkString("\n")}")
      else
        ujson.read(result.out.text())

  def hash(module: Module, project: ujson.Value, scopes: Set[String]): Result[String, String] =
    val md = MessageDigest.getInstance("SHA-1")

    def readFile(path: String): Result[Array[Byte], String] =
      Result.attempt:
        os.read.bytes(os.pwd / os.RelPath(module.root) / os.RelPath(path))
      .resolve:
        case err: IOException => s"failed to hash file ${path}: ${err.getMessage}"

    Result:
      for
        scopesObj <- project.obj.get("scopes")
        scope <- scopes.toList.flatMap(scopesObj.obj.get)
        source <- scope("sources").arr
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
      reporter.debug(s"running step for module ${step.module.name}...")
      val update = step.exec(project = curr, initial = initial).?
      update.map(step.module.name -> _)

  def doCleanModule(module: Module, dependency: Boolean)(using Settings): Result[Unit, String] =
    Result:
      if dependency then
        reporter.debug(s"dependency of ${module.name} updated, cleaning module ${module.name}...")
      else
        reporter.info(s"cleaning module ${module.name}...")
      val args = ScalaCommand.makeArgs(module, SubCommand.Clean, Nil, PlatformKind.jvm)
      val result = ScalaCommand.call(args).?
      if result.exitCode != 0 then
        failure(s"failed to clean module ${module.name}: ${result.err.lines().mkString("\n")}")

end Shared
