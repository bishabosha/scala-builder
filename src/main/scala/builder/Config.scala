package builder

import toml.Value.*
import upickle.default.Writer
import builder.errors.*

object Config:

  def parse(text: String): Result[Config, String] =
    toml.Toml.parse(text) match
      case Left((_, msg)) => Result.Failure(msg)
      case Right(value) => readConfig(value)

  private def readConfig(table: Tbl): Result[Config, String] =
    Result:
      Config(
        scalaVersion = table.values.get("scalaVersion").flatMap({
          case Str(value) => Some(value)
          case _ => failure("scalaVersion must be a string")
        }),
        modules = table.values.get("modules").map({
          case Tbl(values) =>
            val parsed = values.toList.map(readModule)
            val graph = parsed.map(module => module.name -> module).toMap
            ModuleGraph.checkValid(graph)
            graph
          case _ => failure("modules must be a table")
        }).getOrElse(Map.empty)
      )

  private def readModule[T](key: String, value: toml.Value)(using CanError[String]): Module =
    value match
      case Tbl(values) =>
        val root = values.get("root").flatMap({
          case Str(value) => Some(value)
          case _ => failure(s"modules.${key}.root must be a string")
        }).getOrElse(key)
        val kind = values.get("kind").flatMap({
          case Str(value) => Some(value)
          case _ => failure(s"modules.${key}.kind must be a string")
        }).getOrElse("library")
        val dependsOn = values.get("dependsOn").flatMap({
          case Arr(values) => Some(values.flatMap({
            case Str(value) => Some(value)
            case _ => failure(s"modules.${key}.dependsOn must be a list of strings")
          }))
          case _ => failure(s"modules.${key}.dependsOn must be a list of strings")
        }).getOrElse(Nil)
        val mainClass = values.get("mainClass").flatMap({
          case Str(value) => Some(value)
          case _ => failure(s"modules.${key}.mainClass must be a string")
        })
        val moduleKind = kind match
          case "library" => ModuleKind.Library
          case "application" => ModuleKind.Application(mainClass = mainClass)
          case "resource" => ModuleKind.Resource
          case _ => failure(s"unknown module kind for modules.${key}.kind: $kind")
        if !moduleKind.isInstanceOf[ModuleKind.Application] && mainClass.nonEmpty then
          failure(s"modules.${key}.mainClass is only valid for application modules")
        Module(
          name = key,
          root = root,
          kind = moduleKind,
          dependsOn = dependsOn
        )
      case _ =>
        failure(s"module.$key must be a table")

case class Config(
  scalaVersion: Option[String] = None,
  modules: Map[String, Module] = Map.empty
) derives Writer

case class Module(
  name: String,
  root: String,
  kind: ModuleKind = ModuleKind.Library,
  dependsOn: List[String] = Nil
) derives Writer

enum ModuleKind derives Writer:
  case Library, Resource
  case Application(mainClass: Option[String])
