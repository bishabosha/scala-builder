package builder

import toml.Parse

import scala.util.control.NonLocalReturns.*

object Config:
  import toml.Codecs.{*, given}
  import toml.Value.*

  def parse(text: String): Either[String, Config] =
    toml.Toml.parse(text) match
      case Left((_, msg)) => Left(msg)
      case Right(value) => readConfig(value)

  private def readConfig(table: Tbl): Either[String, Config] = returning {
    Right(Config(
      scalaVersion = table.values.get("scalaVersion").flatMap({
        case Str(value) => Some(value)
        case _ => throwReturn(Left("scalaVersion must be a string"))
      }),
      modules = table.values.get("modules").flatMap({
        case Tbl(values) =>
          val parsed = values.toList.map((k, v) => readModule(k, v) match
            case Left(err) => throwReturn(Left(err))
            case Right(value) => value
          )
          val asSet = parsed.toSet
          val validated = parsed.map(module => validateModule(module, asSet - module) match
            case Left(err) => throwReturn(Left(err))
            case Right(_) => ()
          )
          Some(parsed)
        case _ => throwReturn(Left("modules must be a table"))
      }).getOrElse(Nil)
    ))
  }

  private def validateModule(module: Module, rest: Set[Module]): Either[String, Unit] =
    if module.dependsOn.contains(module.name) then
      Left(s"module ${module.name} cannot depend on itself")
    else if module.kind == ModuleKind.Application && rest.exists(_.dependsOn.contains(module)) then
      Left(s"module ${module.name} is depended on by ${rest.find(_.dependsOn.contains(module)).get.name} but is an application")
    else if module.dependsOn.exists(mod => !rest.exists(_.name == mod)) then
      Left(s"module ${module.name} depends on ${module.dependsOn.find(mod => !rest.exists(_.name == mod)).get} which does not exist")
    else
      Right(())

  private def readModule(key: String, value: toml.Value): Either[String, Module] = returning {
    value match
      case Tbl(values) =>
        val name = values.get("name").flatMap({
          case Str(value) => Some(value)
          case _ => throwReturn(Left(s"modules.${key}.name must be a string"))
        }).getOrElse(key)
        val root = values.get("root").flatMap({
          case Str(value) => Some(value)
          case _ => throwReturn(Left(s"modules.${key}.root must be a string"))
        }).getOrElse(key)
        val kind = values.get("kind").flatMap({
          case Str(value) => Some(value)
          case _ => throwReturn(Left(s"modules.${key}.kind must be a string"))
        }).getOrElse("library")
        val dependsOn = values.get("dependsOn").flatMap({
          case Arr(values) => Some(values.flatMap({
            case Str(value) => Some(value)
            case _ => throwReturn(Left(s"modules.${key}.dependsOn must be a list of strings"))
          }))
          case _ => throwReturn(Left(s"modules.${key}.dependsOn must be a list of strings"))
        }).getOrElse(Nil)
        Right(Module(
          name = name,
          root = root,
          kind = kind match
            case "library" => ModuleKind.Library
            case "application" => ModuleKind.Application
            case "resource" => ModuleKind.Resource
            case _ => throwReturn(Left(s"unknown module kind for modules.${key}.kind: $kind")),
          dependsOn = dependsOn
        ))
      case _ =>
        Left(s"module.$key must be a table")
  }

case class Config(
  scalaVersion: Option[String] = None,
  modules: List[Module] = Nil
)

case class Module(
  name: String,
  root: String,
  kind: ModuleKind = ModuleKind.Library,
  dependsOn: List[String] = Nil
)

enum ModuleKind:
  case Library, Application, Resource