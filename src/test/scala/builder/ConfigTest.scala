package builder

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

  test("parse config from valid toml") {

    val config = Config.parse(exampleFullStackAppConf) match
      case Left(err) => fail(err)
      case Right(value) => value

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

  def stageTest(name: munit.TestOptions)(rawConfig: String, target: Option[String], expected: List[List[String]])(using munit.Location) = {
    test(name) {
      val config = Config.parse(rawConfig) match
        case Left(err) => fail(err)
        case Right(value) => value

      val stages = target match
        case None => ModuleGraph.stages(config.modules)
        case Some(value) => ModuleGraph.stages(ModuleGraph.reachable(config.modules, value))

      val stageNames = stages.map(_.map(_.name))

      assertEquals(stageNames, expected)
    }
  }

  stageTest("sort module deps into stages [full-stack app]")(
    rawConfig = exampleFullStackAppConf,
    target = None,
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
    target = None,
    expected = List(
      List("bottom"),
      List("left", "right"),
      List("top")
    )
  )

  stageTest("sort module deps into stages [diamond, filtered]")(
    rawConfig = diamondAppConf,
    target = Some("right"),
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
    target = None,
    expected = List(
      List("D"),
      List("C"),
      List("B"),
      List("A"),
    )
  )

  stageTest("sort module deps into stages [chain, filtered-D]")(
    rawConfig = chainAppConf,
    target = Some("D"),
    expected = List(
      List("D"),
    )
  )

  stageTest("sort module deps into stages [chain, filtered-C]")(
    rawConfig = chainAppConf,
    target = Some("C"),
    expected = List(
      List("D"),
      List("C"),
    )
  )

  stageTest("sort module deps into stages [chain, filtered-B]")(
    rawConfig = chainAppConf,
    target = Some("B"),
    expected = List(
      List("D"),
      List("C"),
      List("B"),
    )
  )

  stageTest("sort module deps into stages [chain, filtered-A]")(
    rawConfig = chainAppConf,
    target = Some("A"),
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
    target = None,
    expected = List(
      List("common"),
      List("libA", "libB"),
      List("topA", "topB"),
    )
  )

  stageTest("sort module deps into stages [forked, filtered-topA]")(
    rawConfig = forkedAppConf,
    target = Some("topA"),
    expected = List(
      List("common"),
      List("libA"),
      List("topA"),
    )
  )

}
