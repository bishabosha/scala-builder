package builder

import ConsoleSubCommand.*
import upickle.default.*

import builder.targets.*
// import builder.errors.*
import steps.result.Result, Result.eval.{raise, ok}
import builder.ScalaCommand.SubCommand

import java.io.IOException

@main def runner(args: String*): Unit =
  Result[Unit, AppError]:
    execCommand(ConsoleCommand.parse(args.toList).ok).ok
  .match
    case Result.Err(err) => reporter.error(err.message)
    case Result.Ok(()) => ()

enum ConsoleSubCommand:
  case Clean, ShowConfig
  case Run(project: Option[String])
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
        case "run" :: args =>
          val (project, args1) = args match
            case arg :: rest if !arg.startsWith("-") =>
              (Some(arg), rest)
            case _ =>
              (None, args)
          ConsoleCommand(Run(project), debug = bool(args1, "--debug"), sequential = bool(args1, "--sequential"))
        case "clean" :: args => ConsoleCommand(Clean, debug = args.contains("--debug"), sequential = bool(args, "--sequential"))
        case "test" :: args =>
          val (projects, args1) = args match
            case arg :: rest if !arg.startsWith("-") =>
              (arg.split(":").toList, rest)
            case _ =>
              (Nil, args)
          ConsoleCommand(Test(projects), debug = args1.contains("--debug"), sequential = bool(args, "--sequential"))
        case "repl" :: Nil => raise("missing project name for `repl` command")
        case "repl" :: project :: args => ConsoleCommand(Repl(project), debug = args.contains("--debug"), sequential = bool(args, "--sequential"))
        case "show-config" :: args => ConsoleCommand(ShowConfig, debug = args.contains("--debug"), sequential = bool(args, "--sequential"))
        case "validate" :: args => ConsoleCommand(Validate, debug = args.contains("--debug"), sequential = bool(args, "--sequential"))
        case _ => raise("Invalid command. Try `run [args]`")
  end parse

end ConsoleCommand

def run(project: Option[String])(using Settings): Result[Unit, String] =
  def doRun(app: Module) = Result:
    val plan = Plan.compile(Set(app), SubCommand.Run).ok
    val initialState = parseCache.ok
    val finalResult = plan.exec(initialState).ok
    writeCacheDiff(finalResult, initialState).ok
    Tasks.run(app, app.kind.asApplication, finalResult).ok

  Result:
    settings.config.modules.values.filter(_.kind.isInstanceOf[ModuleKind.Application]).toList match
      case Nil => raise("No application modules found")
      case apps =>
        project match
          case Some(name) =>
            apps.find(_.name == name) match
              case Some(app) => doRun(app).ok
              case None => raise(s"No application module found for name $name")
          case None =>
            apps match
              case app :: Nil => doRun(app).ok
              case _ =>
                raise(s"Multiple application modules found (${apps.map(_.name).mkString(", ")}), please specify one with `run <name>`")
end run

def clean()(using Settings): Result[Unit, String] =
  Result:
    Tasks.clean(settings.config.modules.values.toSet).ok
    Shared.clearAndRemoveDir(os.pwd / ".scala-builder").ok

def test(opts: Test)(using Settings): Result[Unit, String] =
  Result:
    val mods = settings.config.modules.values.toSet
    val filtered =
      if opts.projects.isEmpty then mods
      else mods.filter(m => opts.projects.contains(m.name))
    val modsMsg = if filtered.sizeIs == 1 then "module" else "modules"
    val plan = Plan.compile(filtered, SubCommand.Test).ok
    val initialState = parseCache.ok
    val finalResult = plan.exec(initialState).ok
    writeCacheDiff(finalResult, initialState).ok
    Tasks.test(filtered, finalResult, initial = initialState).ok

def repl(opts: Repl)(using Settings): Result[Unit, String] =
  Result:
    settings.config.modules.values.find(_.name == opts.project) match
      case Some(module) =>
        val plan = Plan.compile(Set(module), SubCommand.Repl).ok
        val initialState = parseCache.ok
        val finalResult = plan.exec(initialState).ok
        writeCacheDiff(finalResult, initialState).ok
        Tasks.repl(module, finalResult).ok
      case None => raise(s"Module ${opts.project} not found")

def showConfig()(using Settings): Unit =
  println(write(settings.config, indent = 2))

def validate(): Unit =
  println("config is valid")

def parseConfig: Result[Config, AppError] = {
  val location = os.pwd / "builder.toml"

  def readConfig() =
    Result
      .catchException:
        case err: IOException => AppError(s"error while reading config file: $err")
      .apply:
        os.read(location)

  Result:
    if os.exists(location) then
      Config.parse(readConfig().ok).ok
    else
      raise(AppError("No builder.toml file found in current directory"))
}

given Conversion[String, AppError] = AppError(_)
case class AppError(message: String)

def parseCache(using Settings): Result[targets.Targets, String] =
  Result
    .catchException:
      case err: IOException => s"error while parsing cache: $err"
    .apply:
      if os.exists(cachePath) then
        reporter.debug(s"found cache at $cachePath")
        read[targets.Targets](os.read(cachePath))
        // TODO: validate against config
      else
        targets.Targets(Map.empty)


def writeCacheDiff(project: targets.Targets, initial: targets.Targets)(using Settings): Result[Unit, String] =
  if initial eq project then
    reporter.debug(s"targets had no diff, will not write any updates to the cache.")
    Result.Ok(())
  else
    reporter.debug(s"writing cache to $cachePath")
    Result
      .catchException:
        case err: IllegalArgumentException => s"error while writing cache: $err"
      .apply:
        os.write.over(cachePath, write(project), createFolders = true)

def execCommand(command: ConsoleCommand): Result[Unit, AppError] =
  Result:
    val settings = Settings(command.debug, command.sequential, parseConfig.ok)
    given Settings = settings
    command.sub match
      case Run(project) => run(project).ok
      case Clean => clean().ok
      case opts @ Repl(_) => repl(opts).ok
      case opts @ Test(_) => test(opts).ok
      case ShowConfig => showConfig()
      case Validate => validate()
