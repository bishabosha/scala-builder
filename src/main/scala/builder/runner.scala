package builder

import ConsoleSubCommand.*
import upickle.default.*

import builder.targets.*
import builder.errors.*
import java.io.IOException
import builder.ScalaCommand.SubCommand

@main def runner(args: String*): Unit =
  Result:
    execCommand(ConsoleCommand.parse(args.toList).?).?
  .match
    case Result.Failure(err) => reporter.error(err)
    case Result.Success(()) => ()

enum ConsoleSubCommand:
  case Run, Clean, ShowConfig
  case Repl(project: String)
  case Test(projects: List[String])
  case Validate

case class ConsoleCommand(sub: ConsoleSubCommand, debug: Boolean, sequential: Boolean)

def settings(using Settings): Settings = summon[Settings]

case class Settings(debug: Boolean, sequential: Boolean, config: Config)

private lazy val cachePath = os.pwd / ".scala-builder" / "cache.json"

object ConsoleCommand:

  private def bool(args: List[String], flag: String): Boolean =
    args.contains(flag)

  def parse(args: List[String]): Result[ConsoleCommand, String] =
    Result:
      args match
        case "run" :: args => ConsoleCommand(Run, debug = bool(args, "--debug"), sequential = bool(args, "--sequential"))
        case "clean" :: args => ConsoleCommand(Clean, debug = args.contains("--debug"), sequential = bool(args, "--sequential"))
        case "test" :: args =>
          val (projects, args1) = args match
            case arg :: rest if !arg.startsWith("-") =>
              (arg.split(":").toList, rest)
            case _ =>
              (Nil, args)
          ConsoleCommand(Test(projects), debug = args1.contains("--debug"), sequential = bool(args, "--sequential"))
        case "repl" :: Nil => failure("missing project name for `repl` command")
        case "repl" :: project :: args => ConsoleCommand(Repl(project), debug = args.contains("--debug"), sequential = bool(args, "--sequential"))
        case "show-config" :: args => ConsoleCommand(ShowConfig, debug = args.contains("--debug"), sequential = bool(args, "--sequential"))
        case "validate" :: args => ConsoleCommand(Validate, debug = args.contains("--debug"), sequential = bool(args, "--sequential"))
        case _ => failure("Invalid command. Try `run [args]`")
  end parse

end ConsoleCommand

def run()(using Settings): Result[Unit, String] = Result:
  settings.config.modules.values.filter(_.kind.isInstanceOf[ModuleKind.Application]).toList match
    case Nil => failure("No application modules found")
    case app :: Nil =>
      val plan = Plan.compile(Set(app), SubCommand.Run).?
      val initialState = parseCache.?
      val finalResult = plan.exec(initialState).?
      writeCache(finalResult).?
      Tasks.run(app, app.kind.asInstanceOf[ModuleKind.Application], finalResult).?

    case _ => failure("Multiple application modules found (TODO: ask which one to run)")
end run

def clean()(using Settings): Result[Unit, String] =
  Result:
    Tasks.clean(settings.config.modules.values.toSet).?
    writeCache(targets.Targets(Map.empty)).?

def test(opts: Test)(using Settings): Result[Unit, String] =
  Result:
    val mods = settings.config.modules.values.toSet
    val filtered =
      if opts.projects.isEmpty then mods
      else mods.filter(m => opts.projects.contains(m.name))
    val modsMsg = if filtered.sizeIs == 1 then "module" else "modules"
    val plan = Plan.compile(filtered, SubCommand.Test).?
    val initialState = parseCache.?
    val finalResult = plan.exec(initialState).?
    writeCache(finalResult).?
    Tasks.test(filtered, finalResult, initial = initialState).?

def repl(opts: Repl)(using Settings): Result[Unit, String] =
  Result:
    settings.config.modules.values.find(_.name == opts.project) match
      case Some(module) =>
        val plan = Plan.compile(Set(module), SubCommand.Repl).?
        val initialState = parseCache.?
        val finalResult = plan.exec(initialState).?
        writeCache(finalResult).?
        Tasks.repl(module, finalResult).?
      case None => failure(s"Module ${opts.project} not found")

def showConfig()(using Settings): Unit =
  println(write(settings.config, indent = 2))

def validate(): Unit =
  println("config is valid")

def parseConfig: Result[Config, String] =
  val location = os.pwd / "builder.toml"

  def readConfig() =
    Result.attempt:
      os.read(location)
    .resolve:
      case err: IOException => s"error while reading config file: $err"

  Result:
    if os.exists(location) then
      Config.parse(readConfig().?).?
    else
      failure("No builder.toml file found in current directory")

end parseConfig

def parseCache(using Settings): Result[targets.Targets, String] =
  Result.attempt:
    if os.exists(cachePath) then
      reporter.debug(s"found cache at $cachePath")
      read[targets.Targets](os.read(cachePath))
      // TODO: validate against config
    else
      targets.Targets(Map.empty)
  .resolve:
    case err: IOException => s"error while parsing cache: $err"

def writeCache(project: targets.Targets)(using Settings): Result[Unit, String] =
  reporter.debug(s"writing cache to $cachePath")
  Result.attempt:
    os.write.over(cachePath, write(project), createFolders = true)
  .resolve:
    case err: IllegalArgumentException => s"error while writing cache: $err"

def execCommand(command: ConsoleCommand): Result[Unit, String] =
  Result:
    val settings = Settings(command.debug, command.sequential, parseConfig.?)
    given Settings = settings
    command.sub match
      case Run => run().?
      case Clean => clean().?
      case opts @ Repl(_) => repl(opts).?
      case opts @ Test(_) => test(opts).?
      case ShowConfig => showConfig()
      case Validate => validate()
