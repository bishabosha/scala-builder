package builder

import os.Shellable

object ScalaCommand:

  private def globalScalaVersionArgs(using Settings): List[String] =
    settings.config.scalaVersion.map(s => "-S" :: s :: Nil).getOrElse(Nil)

  enum SubCommand:
    case Clean, Compile, Test, Run, Repl

    def commandString(module: Module)(rest: Settings ?=> List[Shellable])(using Settings): List[Shellable] = this match
      case Run => "run" :: rest
      case Compile => "compile" :: rest
      case Test => "test" :: rest
      case Repl => "repl" :: rest
      case Clean => "clean" :: Nil

  def makeArgs(module: Module, subcommand: SubCommand, classpath: List[String], extraArgs: Shellable*)(using Settings): List[os.Shellable] =
    val workspace = os.pwd / module.root
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
