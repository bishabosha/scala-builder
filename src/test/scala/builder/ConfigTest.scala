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

class ConfigTest extends munit.FunSuite {

  test("parse config from valid toml") {

    val config = Config.parse(exampleFullStackAppConf) match
      case Left(err) => fail(err)
      case Right(value) => value

    assertEquals(config, Config(
      scalaVersion = Some("3.2.2"),
      modules = List(
        Module(
          name = "webserver",
          root = "webserver",
          kind = ModuleKind.Application(mainClass = Some("com.example.Main")),
          dependsOn = List("core", "webpage")
        ),
        Module(
          name = "webpage",
          root = "webpage",
          kind = ModuleKind.Resource,
          dependsOn = List("core")
        ),
        Module(
          name = "core",
          root = "core",
          kind = ModuleKind.Library,
          dependsOn = Nil
        )
      )
    ))
  }

}
