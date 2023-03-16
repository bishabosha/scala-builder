package builder

import toml.Codecs.{*, given}

val exampleGlobalConf = """
# name = "example"

[[modules]]
name = "client"

[[modules]]
name = "server"
"""

class ExampleTest extends munit.FunSuite {

  test("example") {
    import toml.Value.*

    val Right(config) = toml.Toml.parse(exampleGlobalConf): @unchecked

    assertEquals(config, Tbl(Map(
      // "name" -> Str("example"),
      "modules" -> Arr(List(
        Tbl(Map("name" -> Str("client"))),
        Tbl(Map("name" -> Str("server")))
      ))
    )))
  }

}
