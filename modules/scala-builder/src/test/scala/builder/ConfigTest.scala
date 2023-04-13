package builder

import builder.errors.Result
import builder.targets.TargetGraph
import builder.ScalaCommand.SubCommand

class ConfigTest extends munit.FunSuite {

  val exampleFullStackAppConf = """
  scalaVersion = "3.2.2"

  [modules.webserver]
  kind = "application" # cannot be depended on by other modules
  mainClass = "example.WebServer"
  dependsOn = ["core"]

  [modules.webpage]
  kind = "application"
  mainClass = "example.webpage"
  dependsOn = ["core"]

  [modules.core]
  kind = "library" # dependents depend on the classpath output
  """

  extension [T](result: Result[T, String])
    def orFail: T = result match
      case Result.Success(value) => value
      case Result.Failure(err) => fail(s"failed result: $err")

  test("parse config from valid toml") {

    val config = Config.parse(exampleFullStackAppConf).orFail

    assertEquals(config, Config(
      scalaVersion = Some("3.2.2"),
      modules = Map(
        "webserver" -> Module(
          name = "webserver",
          root = "webserver",
          kind = ModuleKind.Application(mainClass = Some("example.WebServer")),
          dependsOn = List("core")
        ),
        "webpage" -> Module(
          name = "webpage",
          root = "webpage",
          kind = ModuleKind.Application(mainClass = Some("example.webpage")),
          dependsOn = List("core")
        ),
        "core" -> Module(
          name = "core",
          root = "core",
          kind = ModuleKind.Library,
          dependsOn = Nil
        )
      )
    ))
  }

  def stageTest(name: munit.TestOptions)(rawConfig: String, command: SubCommand, targets: Seq[String], expected: List[List[String]])(using munit.Location) = {
    test(name) {
      val config = Config.parse(rawConfig).orFail

      val targetGraph =
        TargetGraph.compile(
          graph = config.modules,
          targetModules =
            if targets.isEmpty then config.modules.values.toList
            else targets.map(config.modules).toList,
          command
        ).orFail

      val targetNames = targetGraph.stages.map(_.map(_.show))

      assertEquals(targetNames, expected)
    }
  }

  stageTest("sort module deps into stages [full-stack app, run]")(
    rawConfig = exampleFullStackAppConf,
    command = SubCommand.Run,
    targets = Seq("webserver"),
    expected = List(
      List("core:main"),
      List("webserver:runner")
    )
  )

  stageTest("sort module deps into stages [full-stack app, repl]")(
    rawConfig = exampleFullStackAppConf,
    command = SubCommand.Repl,
    targets = Seq("webserver"),
    expected = List(
      List("core:main"),
      List("webserver:main")
    )
  )

  val diamondAppConf = """
  [modules.bottom]

  [modules.left]
  dependsOn = ["bottom"]

  [modules.right]
  dependsOn = ["bottom"]

  [modules.top]
  dependsOn = ["left", "right"]
  """

  stageTest("sort module deps into stages [diamond]")(
    rawConfig = diamondAppConf,
    command = SubCommand.Repl,
    targets = Seq(),
    expected = List(
      List("bottom:main"),
      List("left:main", "right:main"),
      List("top:main")
    )
  )

  stageTest("sort module deps into stages [diamond, filtered]")(
    rawConfig = diamondAppConf,
    command = SubCommand.Repl,
    targets = Seq("right"),
    expected = List(
      List("bottom:main"),
      List("right:main"),
    )
  )

  val chainAppConf = """
  [modules.D]

  [modules.C]
  dependsOn = ["D"]

  [modules.B]
  dependsOn = ["C"]

  [modules.A]
  dependsOn = ["B"]
  """

  stageTest("sort module deps into stages [chain]")(
    rawConfig = chainAppConf,
    command = SubCommand.Repl,
    targets = Seq(),
    expected = List(
      List("D:main"),
      List("C:main"),
      List("B:main"),
      List("A:main"),
    )
  )

  stageTest("sort module deps into stages [chain, filtered-D]")(
    rawConfig = chainAppConf,
    command = SubCommand.Repl,
    targets = Seq("D"),
    expected = List(
      List("D:main"),
    )
  )

  stageTest("sort module deps into stages [chain, filtered-C]")(
    rawConfig = chainAppConf,
    command = SubCommand.Repl,
    targets = Seq("C"),
    expected = List(
      List("D:main"),
      List("C:main"),
    )
  )

  stageTest("sort module deps into stages [chain, filtered-B]")(
    rawConfig = chainAppConf,
    command = SubCommand.Repl,
    targets = Seq("B"),
    expected = List(
      List("D:main"),
      List("C:main"),
      List("B:main"),
    )
  )

  stageTest("sort module deps into stages [chain, filtered-A]")(
    rawConfig = chainAppConf,
    command = SubCommand.Repl,
    targets = Seq("A"),
    expected = List(
      List("D:main"),
      List("C:main"),
      List("B:main"),
      List("A:main"),
    )
  )

  val forkedAppConf = """
  [modules.common]

  [modules.libA]
  dependsOn = ["common"]

  [modules.libB]
  dependsOn = ["common"]

  [modules.topA]
  dependsOn = ["libA"]

  [modules.topB]
  dependsOn = ["libB"]
  """


  stageTest("sort module deps into stages [forked]")(
    rawConfig = forkedAppConf,
    command = SubCommand.Repl,
    targets = Seq(),
    expected = List(
      List("common:main"),
      List("libA:main", "libB:main"),
      List("topA:main", "topB:main"),
    )
  )

  stageTest("sort module deps into stages [forked, filtered-topA]")(
    rawConfig = forkedAppConf,
    command = SubCommand.Repl,
    targets = Seq("topA"),
    expected = List(
      List("common:main"),
      List("libA:main"),
      List("topA:main"),
    )
  )

  stageTest("sort module deps into stages [forked, filtered-topA+topB]")(
    rawConfig = forkedAppConf,
    command = SubCommand.Repl,
    targets = Seq("topA", "topB"),
    expected = List(
      List("common:main"),
      List("libA:main", "libB:main"),
      List("topA:main", "topB:main"),
    )
  )

  stageTest("sort module deps into stages [forked, filtered-libA+libB]")(
    rawConfig = forkedAppConf,
    command = SubCommand.Repl,
    targets = Seq("libA", "libB"),
    expected = List(
      List("common:main"),
      List("libA:main", "libB:main"),
    )
  )

}
