package builder

import os.Shellable

object ScalaCommand:

  private def globalScalaVersionArgs(using Settings): List[String] =
    settings.config.scalaVersion.map(s => "-S" :: s :: Nil).getOrElse(Nil)

  enum SubCommand:
    case Clean, Compile, Test, Run, Repl

    def commandString(module: Module)(using Settings): Shellable = this match
      case Run => "run" :: globalScalaVersionArgs
      case Compile => "compile" :: globalScalaVersionArgs
      case Test => "test" :: globalScalaVersionArgs
      case Repl => "repl" :: globalScalaVersionArgs
      case Clean => "clean"

  def makeArgs(module: Module, subcommand: SubCommand, classpath: List[String], extraArgs: Shellable*)(using Settings): List[os.Shellable] =
    List(
      "scala",
      subcommand.commandString(module),
      (if classpath.nonEmpty then "--classpath" :: classpath.mkString(":") :: Nil else Nil),
      extraArgs,
      os.pwd / module.root,
    )