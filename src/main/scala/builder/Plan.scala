package builder

import ScalaCommand.SubCommand
import SharedPlan.*

object reporter:
  inline def debug(msg: String)(using Settings): Unit =
    if settings.debug then println(s"[debug] $msg")

private object SharedPlan:
  def compileLibrary(module: Module)(using Settings): CompilePlan =
    val deps = compileDeps(module)

    new:
      def classpath =
        val (clean, dclasspath) = depsClasspath(deps)
        println(s"[info] maybe compiling library module ${module.name}...")
        if clean then doCleanModule(module)

        val args = ScalaCommand.makeArgs(module, SubCommand.Compile, dclasspath, "--print-class-path")
        reporter.debug(s"exporting classpath of module ${module.name} with args: ${args.mkString(" ")}")
        val res = os.proc(args)
          .call(stdout = os.Pipe, stderr = os.Pipe)

        val downstreamClean = clean || res.err.lines().nonEmpty
        val mclasspath = res.out.lines().head.split(":").toList.distinct.sorted

        (downstreamClean, mclasspath)
      end classpath
    end new
  end compileLibrary

  def compileResource(module: Module)(using Settings): CompilePlan =
    new:
      def classpath = (false, Nil)

  def compileDep(module: Module)(using Settings): CompilePlan =
    module.kind match
      case ModuleKind.Library => compileLibrary(module)
      case ModuleKind.Resource => compileResource(module)
      case ModuleKind.Application(_) => assert(false, "application modules should not be dependencies")

  def doCleanModule(module: Module)(using Settings): Unit =
    println(s"[info] dependency of ${module.name} updated, cleaning module ${module.name}...")
    os.proc(ScalaCommand.makeArgs(module, SubCommand.Clean, Nil))
      .spawn(stdin = os.Inherit, stdout = os.Inherit, stderr = os.Inherit).join()

  def depsClasspath(deps: List[CompilePlan]): (Boolean, List[String]) =
    val (cleans, classpaths) = deps.map(_.classpath).unzip
    (cleans.exists(identity), classpaths.flatten.distinct.sorted)

  def compileDeps(module: Module)(using Settings): List[CompilePlan] =
    module.dependsOn.flatMap(settings.config.modules.get).map(compileDep)

end SharedPlan

trait RunPlan:
  def exec(): Unit

trait ReplPlan:
  def repl(): Unit

trait CompilePlan:
  def classpath: (Boolean, List[String])

trait CleanPlan:
  def clean(): Unit

object CleanPlan:
  def compile(module: Module)(using Settings): CleanPlan =
    new:
      def clean(): Unit =
        println(s"[info] cleaning module ${module.name}")
        os.proc("scala", "clean", os.pwd / module.root).call()

trait TestPlan:
  def test(): Unit

object TestPlan:

  def compile(module: Module)(using Settings): TestPlan = compileLeaf(module)

  private def compileLeaf(module: Module)(using Settings): TestPlan =
    val deps = compileDeps(module)

    new:
      def test() =
        val (clean, classpath) = depsClasspath(deps)

        if clean then doCleanModule(module)

        println(s"[info] testing module ${module.name}:")
        val result =
          os.proc(ScalaCommand.makeArgs(module, SubCommand.Test, classpath, Nil))
            .spawn(stdin = os.Inherit, stdout = os.Inherit, stderr = os.Inherit)

        if !result.join() then
          println(s"failure with exit code ${result.exitCode()}")

object RunPlan:

  def compile(module: Module)(using Settings): Either[String, RunPlan] =
    module.kind match
      case app @ ModuleKind.Application(_) => Right(compileApplication(module, app))
      case _ => Left(s"module ${module.name} is not an application module")

  private def compileApplication(module: Module, app: ModuleKind.Application)(using Settings): RunPlan =
    val deps = compileDeps(module)

    new:
      def exec() =
        val (clean, classpath) = depsClasspath(deps)

        if clean then doCleanModule(module)

        val (mainMessage, mainArgs) = app.mainClass match
          case None => ("with dynamic main class", "--interactive" :: Nil)
          case Some(value) => (s"with specified main class $value", "--main-class" :: value :: Nil)

        println(s"[info] running module ${module.name} $mainMessage:")
        val result =
          val args = ScalaCommand.makeArgs(module, SubCommand.Run, classpath, mainArgs)
          reporter.debug(s"running: ${args.mkString(" ")}")
          os.proc(args)
            .spawn(stdin = os.Inherit, stdout = os.Inherit, stderr = os.Inherit)

        if !result.join() then
          println(s"failure with exit code ${result.exitCode()}")

object ReplPlan:

  def compile(module: Module)(using Settings): ReplPlan = compileLeaf(module)

  private def compileLeaf(module: Module)(using Settings): ReplPlan =
    val deps = compileDeps(module)

    new:
      def repl() =
        val (clean, classpath) = depsClasspath(deps)

        if clean then doCleanModule(module)

        println(s"[info] running module ${module.name}:")
        val result =
          os.proc(ScalaCommand.makeArgs(module, SubCommand.Repl, classpath))
            .spawn(stdin = os.Inherit, stdout = os.Inherit, stderr = os.Inherit)

        if !result.join() then
          println(s"failure with exit code ${result.exitCode()}")