package builder

import ConsoleSubCommand.*
import upickle.default.*

import builder.errors.*

// TODO: export the build of each module to json with scala-cli.

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

case class ConsoleCommand(sub: ConsoleSubCommand, debug: Boolean)

def settings(using Settings): Settings = summon[Settings]

case class Settings(debug: Boolean, config: Config)

object ConsoleCommand:

  def parse(args: List[String]): Result[ConsoleCommand, String] =
    Result:
      args match
        case "run" :: args => ConsoleCommand(Run, debug = args.contains("--debug"))
        case "clean" :: args => ConsoleCommand(Clean, debug = args.contains("--debug"))
        case "test" :: args =>
          val (projects, args1) = args match
            case arg :: rest if !arg.startsWith("-") =>
              (arg.split(":").toList, rest)
            case _ =>
              (Nil, args)
          ConsoleCommand(Test(projects), debug = args1.contains("--debug"))
        case "repl" :: Nil => failure("missing project name for `repl` command")
        case "repl" :: project :: args => ConsoleCommand(Repl(project), debug = args.contains("--debug"))
        case "show-config" :: args => ConsoleCommand(ShowConfig, debug = args.contains("--debug"))
        case "validate" :: args => ConsoleCommand(Validate, debug = args.contains("--debug"))
        case _ => failure("Invalid command. Try `run [args]`")


def run()(using Settings): Result[Unit, String] =
  Result:
    settings.config.modules.values.filter(_.kind.isInstanceOf[ModuleKind.Application]).toList match
      case Nil => failure("No application modules found")
      case app :: Nil => RunPlan.compile(app).?.exec()
      case _ => failure("Multiple application modules found (TODO: ask which one to run)")

def clean()(using Settings): Unit =
  for plan <- settings.config.modules.values.map(CleanPlan.compile) do
    plan.clean()

def test(opts: Test)(using Settings): Unit =
  val mods = settings.config.modules.values
  val filtered = if opts.projects.isEmpty then mods else mods.filter(m => opts.projects.contains(m.name))
  for plan <- filtered.map(TestPlan.compile) do
    plan.test()

def repl(opts: Repl)(using Settings): Unit =
  for
    proj <- settings.config.modules.values.find(_.name == opts.project)
  do
    ReplPlan.compile(proj).repl()

def showConfig()(using Settings): Unit =
  println(write(settings.config, indent = 2))

def validate()(using Settings): Unit =
  println("config is valid")

def parseConfig: Result[Config, String] =
  Result:
    if os.exists(os.pwd / "builder.toml") then
      Config.parse(os.read(os.pwd / "builder.toml")).?
    else
      failure("No builder.toml file found in current directory")

def execCommand(command: ConsoleCommand): Result[Unit, String] =
  Result:
    val settings = Settings(command.debug, parseConfig.?)
    given Settings = settings
    command.sub match
      case Run => run().?
      case Clean => clean()
      case opts @ Test(_) => test(opts)
      case opts @ Repl(_) => repl(opts)
      case ShowConfig => showConfig()
      case Validate => validate()
