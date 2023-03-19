package builder

import ConsoleCommand.*

@main def runner(args: String*) =
  ConsoleCommand.parse(args.toList) match
    case None => println("[error] Invalid command. Try `run [args]`")
    case Some(command) => execCommand(command)

enum ConsoleCommand:
  case Run, Clean

object ConsoleCommand:
  def parse(args: List[String]): Option[ConsoleCommand] = args match
    case "run" :: args => Some(Run)
    case "clean" :: args => Some(Clean)
    case _ => None


def run()(using Config): Unit =
  config.modules.filter(_.kind.isInstanceOf[ModuleKind.Application]) match
    case Nil => println("[error] No application modules found")
    case app :: Nil =>
      RunPlan.compile(app) match
        case Left(err) => Console.err.println(s"[error] $err")
        case Right(plan) => plan.exec()
    case _ => println("[error] Multiple application modules found (TODO: ask which one to run)")

def clean()(using Config): Unit =
  for plan <- config.modules.map(CleanPlan.compile) do
    plan.clean()

def withConfig(body: Config ?=> Unit): Unit =
  val buildConfig = Either.cond(
    os.exists(os.pwd / "builder.toml"),
    Config.parse(os.read(os.pwd / "builder.toml")),
    "No builder.toml file found in current directory"
  ).flatten

  buildConfig match
    case Left(error) => println(s"[error] $error")
    case Right(config) =>
      body(using config)

def execCommand(command: ConsoleCommand): Unit = withConfig {
  command match
    case Run => run()
    case Clean => clean()
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
