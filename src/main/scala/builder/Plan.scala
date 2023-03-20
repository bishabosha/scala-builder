package builder

trait RunPlan:
  def exec(): Unit

trait CompilePlan:
  def classpath: (Boolean, List[String])

trait CleanPlan:
  def clean(): Unit

object CleanPlan:
  def compile(module: Module)(using Config): CleanPlan =
    new:
      def clean(): Unit =
        println(s"[info] cleaning module ${module.name}")
        os.proc("scala", "clean", os.pwd / module.root).call()

trait TestPlan:
  def test(): Unit

object TestPlan:
  private def globalScalaVersionArgs(using Config): List[String] =
    config.scalaVersion.map("-S" :: _ :: Nil).getOrElse(Nil)

  private def moduleClasspath(clean: Boolean, module: Module)(using Config): (Boolean, List[String]) =
    if clean then
      println(s"[info] dependency of ${module.name} updated, cleaning module ${module.name}...")
      os.proc("scala", "clean", os.pwd / module.root).call()

    val res = os.proc("scala", "compile", globalScalaVersionArgs, "--print-class-path", os.pwd / module.root)
      .call(stdout = os.Pipe, stderr = os.Pipe)

    (clean || res.err.lines().nonEmpty, res.out.lines().head.split(":").toList)

  def compile(module: Module)(using Config): TestPlan =
    module.kind match
      case ModuleKind.Library => compileLeaf(module)
      case ModuleKind.Resource => compileLeaf(module)
      case ModuleKind.Application(_) => compileLeaf(module)

  def compileDep(module: Module)(using Config): CompilePlan =
    module.kind match
      case ModuleKind.Library => compileLibrary(module)
      case ModuleKind.Resource => compileResource(module)
      case ModuleKind.Application(_) => assert(false, "application modules should not be dependencies")

  private def compileLibrary(module: Module)(using Config): CompilePlan =
    val depsLookup = config.modules.map(m => m.name -> m).toMap

    val deps = module.dependsOn.flatMap(depsLookup.get).map(compileDep)

    def depsClasspath = deps.map(_.classpath)

    new:
      def classpath =
        println(s"[info] maybe compiling library module ${module.name}...")

        val (cleans, classpaths) = depsClasspath.unzip
        val clean0 = cleans.exists(identity)
        val (clean, mclasspath) = moduleClasspath(clean0, module)

        (clean, (mclasspath ::: classpaths.flatten).distinct)

  private def compileResource(module: Module)(using Config): CompilePlan =
    new:
      def classpath = (false, Nil)

  private def compileLeaf(module: Module)(using Config): TestPlan =
    val depsLookup = config.modules.map(m => m.name -> m).toMap

    val deps = module.dependsOn.flatMap(depsLookup.get).map(compileDep)

    def depsClasspath = deps.map(_.classpath)

    def listMains(classpath: String) =
      os.proc("scala", "run", globalScalaVersionArgs, "--list-main-classes", "-classpath", classpath, os.pwd / module.root).call().out
        .lines()
        .last
        .split(" ")
        .toList

    new:
      def test() =
        val (clean, classpath) =
          val (cleans, classpaths) = depsClasspath.unzip
          (cleans.exists(identity), classpaths.flatten.mkString(":"))

        if clean then
          println(s"[info] dependency of ${module.name} updated, cleaning module ${module.name}...")
          os.proc("scala", "clean", os.pwd / module.root).spawn(stdin = os.Inherit, stdout = os.Inherit, stderr = os.Inherit).join()

        println(s"[info] testing module ${module.name}:")
        val result =
          os.proc("scala", "test", globalScalaVersionArgs, "-classpath", classpath, os.pwd / module.root)
            .spawn(stdin = os.Inherit, stdout = os.Inherit, stderr = os.Inherit)

        if !result.join() then
          println(s"failure with exit code ${result.exitCode()}")

object RunPlan:

  private def globalScalaVersionArgs(using Config): List[String] =
    config.scalaVersion.map("-S" :: _ :: Nil).getOrElse(Nil)

  private def moduleClasspath(clean: Boolean, module: Module)(using Config): (Boolean, List[String]) =
    if clean then
      println(s"[info] dependency of ${module.name} updated, cleaning module ${module.name}...")
      os.proc("scala", "clean", os.pwd / module.root).call()

    val res = os.proc("scala", "compile", globalScalaVersionArgs, "--print-class-path", os.pwd / module.root)
      .call(stdout = os.Pipe, stderr = os.Pipe)

    (clean || res.err.lines().nonEmpty, res.out.lines().head.split(":").toList)

  def compile(module: Module)(using Config): Either[String, RunPlan] =
    module.kind match
      case app @ ModuleKind.Application(_) => Right(compileApplication(module, app))
      case _ => Left(s"module ${module.name} is not an application module")

  private def compileDep(module: Module)(using Config): CompilePlan =
    module.kind match
      case ModuleKind.Library => compileLibrary(module)
      case ModuleKind.Resource => compileResource(module)
      case ModuleKind.Application(_) => assert(false, "application modules should not be dependencies")

  private def compileLibrary(module: Module)(using Config): CompilePlan =
    val depsLookup = config.modules.map(m => m.name -> m).toMap

    val deps = module.dependsOn.flatMap(depsLookup.get).map(compileDep)

    def depsClasspath = deps.map(_.classpath)

    new:
      def classpath =
        println(s"[info] maybe compiling library module ${module.name}...")

        val (cleans, classpaths) = depsClasspath.unzip
        val clean0 = cleans.exists(identity)
        val (clean, mclasspath) = moduleClasspath(clean0, module)

        (clean, (mclasspath ::: classpaths.flatten).distinct)

  private def compileResource(module: Module)(using Config): CompilePlan =
    new:
      def classpath = (false, Nil)

  private def compileApplication(module: Module, app: ModuleKind.Application)(using Config): RunPlan =
    val depsLookup = config.modules.map(m => m.name -> m).toMap

    val deps = module.dependsOn.flatMap(depsLookup.get).map(compileDep)

    def depsClasspath = deps.map(_.classpath)

    def listMains(classpath: String) =
      os.proc("scala", "run", globalScalaVersionArgs, "--list-main-classes", "-classpath", classpath, os.pwd / module.root).call().out
        .lines()
        .last
        .split(" ")
        .toList

    new:
      def exec() =
        val (clean, classpath) =
          val (cleans, classpaths) = depsClasspath.unzip
          (cleans.exists(identity), classpaths.flatten.mkString(":"))

        val (mainMessage, mainArgs) = app.mainClass match
          case None => ("with dynamic main class", "--interactive" :: Nil)
          case Some(value) => (s"with specified main class $value", "--main-class" :: value :: Nil)


        if clean then
          println(s"[info] dependency of ${module.name} updated, cleaning module ${module.name}...")
          os.proc("scala", "clean", os.pwd / module.root).spawn(stdin = os.Inherit, stdout = os.Inherit, stderr = os.Inherit).join()

        println(s"[info] running module ${module.name} $mainMessage:")
        val result =
          os.proc("scala", "run", globalScalaVersionArgs, "-classpath", classpath, os.pwd / module.root, mainArgs)
            .spawn(stdin = os.Inherit, stdout = os.Inherit, stderr = os.Inherit)

        if !result.join() then
          println(s"failure with exit code ${result.exitCode()}")