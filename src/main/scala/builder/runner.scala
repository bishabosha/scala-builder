package builder

import ConsoleSubCommand.*
import upickle.default.*

// TODO: export the build of each module to json with scala-cli.

@main def runner(args: String*): Unit =
  ConsoleCommand.parse(args.toList) match
    case Left(err) => Console.err.println(s"[error] $err")
    case Right(command) => execCommand(command)

enum ConsoleSubCommand:
  case Run, Clean, ShowConfig
  case Repl(project: String)
  case Test(projects: List[String])
  case Validate

case class ConsoleCommand(sub: ConsoleSubCommand, debug: Boolean)

def settings(using Settings): Settings = summon[Settings]

case class Settings(debug: Boolean, config: Config)

object ConsoleCommand:

  def parse(args: List[String]): Either[String, ConsoleCommand] = args match
    case "run" :: args => Right(ConsoleCommand(Run, debug = args.contains("--debug")))
    case "clean" :: args => Right(ConsoleCommand(Clean, debug = args.contains("--debug")))
    case "test" :: args =>
      val (projects, args1) = args match
        case arg :: rest if !arg.startsWith("-") =>
          (arg.split(":").toList, rest)
        case _ =>
          (Nil, args)
      Right(ConsoleCommand(Test(projects), debug = args1.contains("--debug")))
    case "repl" :: Nil => Left("missing project name for `repl` command")
    case "repl" :: project :: args => Right(ConsoleCommand(Repl(project), debug = args.contains("--debug")))
    case "show-config" :: args => Right(ConsoleCommand(ShowConfig, debug = args.contains("--debug")))
    case "validate" :: args => Right(ConsoleCommand(Validate, debug = args.contains("--debug")))
    case _ => Left("Invalid command. Try `run [args]`")


def run()(using Settings): Unit =
  settings.config.modules.values.filter(_.kind.isInstanceOf[ModuleKind.Application]).toList match
    case Nil => Console.err.println("[error] No application modules found")
    case app :: Nil =>
      RunPlan.compile(app) match
        case Left(err) => Console.err.println(s"[error] $err")
        case Right(plan) => plan.exec()
    case _ => Console.err.println("[error] Multiple application modules found (TODO: ask which one to run)")

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
  println(s"[info] config:\n${write(settings.config, indent = 2)}")

def validate()(using Settings): Unit =
  println("[info] config is valid")

def withSettings(command: ConsoleCommand)(body: Settings ?=> Unit): Unit =
  val buildConfig = Either.cond(
    os.exists(os.pwd / "builder.toml"),
    Config.parse(os.read(os.pwd / "builder.toml")),
    "No builder.toml file found in current directory"
  ).flatten

  buildConfig match
    case Left(error) => Console.err.println(s"[error] $error")
    case Right(config) =>
      body(using Settings(command.debug, config))

def execCommand(command: ConsoleCommand): Unit = withSettings(command) {
  command.sub match
    case Run => run()
    case Clean => clean()
    case opts @ Test(_) => test(opts)
    case opts @ Repl(_) => repl(opts)
    case ShowConfig => showConfig()
    case Validate => validate()
}
