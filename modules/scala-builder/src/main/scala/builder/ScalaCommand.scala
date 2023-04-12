package builder

import os.Shellable
import builder.errors.*
import java.io.IOException

object ScalaCommand:

  private def globalScalaVersionArgs(using Settings): List[String] =
    settings.config.scalaVersion.map(s => "-S" :: s :: Nil).getOrElse(Nil)

  sealed trait CommandReader:
    def commandString(module: Module)(rest: Settings ?=> List[Shellable])(using Settings): List[Shellable]

  enum InternalCommand extends CommandReader:
    case ExportJson

    def commandString(module: Module)(rest: Settings ?=> List[Shellable])(using Settings): List[Shellable] = this match
      case ExportJson => List("--power", "export", "--json", rest)



  enum SubCommand extends CommandReader:
    case Clean, Compile, Test, Run, Repl

    def commandString(module: Module)(rest: Settings ?=> List[Shellable])(using Settings): List[Shellable] = this match
      case Run => "run" :: rest
      case Compile => "compile" :: rest
      case Test => "test" :: rest
      case Repl => "repl" :: rest
      case Clean => "clean" :: Nil

  def makeArgs(module: Module, subcommand: CommandReader, classpath: List[String], extraArgs: Shellable*)(using Settings): List[os.Shellable] =
    val workspace = os.pwd / os.RelPath(module.root)
    List(
      "scala",
      subcommand.commandString(module)(
        List(
          (if classpath.nonEmpty then "--classpath" :: classpath.mkString(":") :: Nil else Nil),
          "--workspace", workspace,
          extraArgs,
        )
      ),
      workspace,
    )

  def call(args: List[os.Shellable]): Result[os.CommandResult, String] =
    Result.attempt:
      os.proc(args).call(stdin = os.Pipe, stdout = os.Pipe, stderr = os.Pipe, check = false)
    .resolve:
      case err: IOException => s"failed to run scala command: ${err.getMessage}"

  def spawn(args: List[os.Shellable]): Result[os.SubProcess, String] =
    Result.attempt:
      os.proc(args).spawn(stdin = os.Inherit, stdout = os.Inherit, stderr = os.Inherit)
    .resolve:
      case err: IOException => s"failed to run scala command: ${err.getMessage}"
