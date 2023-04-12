package builder

import builder.errors.Result

class ConfigTest extends munit.FunSuite {

  val exampleFullStackAppConf = """
  scalaVersion = "3.2.2"

  [modules.webserver]
  kind = "application" # cannot be depended on by other modules
  mainClass = "com.example.Main"
  dependsOn = ["core", "webpage"]

  [modules.webpage]
  kind = "resource" # dependends depend on the packaged output
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
          kind = ModuleKind.Application(mainClass = Some("com.example.Main")),
          dependsOn = List("core", "webpage")
        ),
        "webpage" -> Module(
          name = "webpage",
          root = "webpage",
          kind = ModuleKind.Resource,
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

  def stageTest(name: munit.TestOptions)(rawConfig: String, targets: Set[String], expected: List[List[String]])(using munit.Location) = {
    test(name) {
      val config = Config.parse(rawConfig).orFail

      val stages = ModuleGraph.stages(
        if targets.isEmpty then config.modules
        else
          ModuleGraph.reachable(
            graph = config.modules,
            targets = targets.map(config.modules),
            excludeTarget = false
          )
      )

      val stageNames = stages.map(_.map(_.name))

      assertEquals(stageNames, expected)
    }
  }

  stageTest("sort module deps into stages [full-stack app]")(
    rawConfig = exampleFullStackAppConf,
    targets = Set(),
    expected = List(
      List("core"),
      List("webpage"),
      List("webserver")
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
    targets = Set(),
    expected = List(
      List("bottom"),
      List("left", "right"),
      List("top")
    )
  )

  stageTest("sort module deps into stages [diamond, filtered]")(
    rawConfig = diamondAppConf,
    targets = Set("right"),
    expected = List(
      List("bottom"),
      List("right"),
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
    targets = Set(),
    expected = List(
      List("D"),
      List("C"),
      List("B"),
      List("A"),
    )
  )

  stageTest("sort module deps into stages [chain, filtered-D]")(
    rawConfig = chainAppConf,
    targets = Set("D"),
    expected = List(
      List("D"),
    )
  )

  stageTest("sort module deps into stages [chain, filtered-C]")(
    rawConfig = chainAppConf,
    targets = Set("C"),
    expected = List(
      List("D"),
      List("C"),
    )
  )

  stageTest("sort module deps into stages [chain, filtered-B]")(
    rawConfig = chainAppConf,
    targets = Set("B"),
    expected = List(
      List("D"),
      List("C"),
      List("B"),
    )
  )

  stageTest("sort module deps into stages [chain, filtered-A]")(
    rawConfig = chainAppConf,
    targets = Set("A"),
    expected = List(
      List("D"),
      List("C"),
      List("B"),
      List("A"),
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
    targets = Set(),
    expected = List(
      List("common"),
      List("libA", "libB"),
      List("topA", "topB"),
    )
  )

  stageTest("sort module deps into stages [forked, filtered-topA]")(
    rawConfig = forkedAppConf,
    targets = Set("topA"),
    expected = List(
      List("common"),
      List("libA"),
      List("topA"),
    )
  )

  stageTest("sort module deps into stages [forked, filtered-topA+topB]")(
    rawConfig = forkedAppConf,
    targets = Set("topA", "topB"),
    expected = List(
      List("common"),
      List("libA", "libB"),
      List("topA", "topB"),
    )
  )

  stageTest("sort module deps into stages [forked, filtered-libA+libB]")(
    rawConfig = forkedAppConf,
    targets = Set("libA", "libB"),
    expected = List(
      List("common"),
      List("libA", "libB"),
    )
  )

}
