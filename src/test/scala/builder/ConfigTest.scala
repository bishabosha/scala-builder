package builder

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

val diamondAppConf = """
[modules.bottom]

[modules.left]
dependsOn = ["bottom"]

[modules.right]
dependsOn = ["bottom"]

[modules.top]
dependsOn = ["left", "right"]
"""

class ConfigTest extends munit.FunSuite {

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

  test("sort module deps into stages [full-stack app]") {
    val config = Config.parse(exampleFullStackAppConf) match
      case Left(err) => fail(err)
      case Right(value) => value

    val stages = ModuleGraph.stages(config.modules)

    val stageNames = stages.map(_.map(_.name))

    assertEquals(stageNames, List(
      List("core"),
      List("webpage"),
      List("webserver")
    ))
  }

  test("sort module deps into stages [diamond]") {
    val config = Config.parse(diamondAppConf) match
      case Left(err) => fail(err)
      case Right(value) => value

    val stages = ModuleGraph.stages(config.modules)

    val stageNames = stages.map(_.map(_.name))

    assertEquals(stageNames, List(
      List("bottom"),
      List("left", "right"),
      List("top")
    ))
  }

}
