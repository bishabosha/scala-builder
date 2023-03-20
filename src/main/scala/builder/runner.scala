package builder

import ConsoleCommand.*

// TODO: export the build of each module to json with scala-cli.

@main def runner(args: String*) =
  ConsoleCommand.parse(args.toList) match
    case Left(err) => Console.err.println(s"[error] $err")
    case Right(command) => execCommand(command)

enum ConsoleCommand:
  case Run, Clean, Test
  case Repl(project: String)

object ConsoleCommand:
  def parse(args: List[String]): Either[String, ConsoleCommand] = args match
    case "run" :: args => Right(Run)
    case "clean" :: args => Right(Clean)
    case "test" :: args => Right(Test)
    case "repl" :: Nil => Left("missing project name for `repl` command")
    case "repl" :: project :: args => Right(Repl(project))
    case _ => Left("Invalid command. Try `run [args]`")


def run()(using Config): Unit =
  config.modules.filter(_.kind.isInstanceOf[ModuleKind.Application]) match
    case Nil => Console.err.println("[error] No application modules found")
    case app :: Nil =>
      RunPlan.compile(app) match
        case Left(err) => Console.err.println(s"[error] $err")
        case Right(plan) => plan.exec()
    case _ => Console.err.println("[error] Multiple application modules found (TODO: ask which one to run)")

def clean()(using Config): Unit =
  for plan <- config.modules.map(CleanPlan.compile) do
    plan.clean()

def test()(using Config): Unit =
  for plan <- config.modules.map(TestPlan.compile) do
    plan.test()

def repl(opts: Repl)(using Config): Unit =
  for
    proj <- config.modules.find(_.name == opts.project)
  do
    ReplPlan.compile(proj).repl()

def withConfig(body: Config ?=> Unit): Unit =
  val buildConfig = Either.cond(
    os.exists(os.pwd / "builder.toml"),
    Config.parse(os.read(os.pwd / "builder.toml")),
    "No builder.toml file found in current directory"
  ).flatten

  buildConfig match
    case Left(error) => Console.err.println(s"[error] $error")
    case Right(config) =>
      body(using config)

def execCommand(command: ConsoleCommand): Unit = withConfig {
  command match
    case Run => run()
    case Clean => clean()
    case Test => test()
    case opts @ Repl(_) => repl(opts)
}



    // if os.exists(os.pwd / "builder.toml") then
    //   val config = Config.parse(os.read(os.pwd / "builder.toml")) match
    //     case Left(err) => println(s"[error] $err")
    //     case Right(value) => value
    //   val builder = Builder(config)
    //   builder.build()
    // else
    //   println("[error] No builder.toml file found in current directory")

    // val config = Config.parse(args) match
    //   case Left(err) => println(s"[error] $err")
    //   case Right(value) => value
    // val builder = Builder(config)
    // builder.build()
