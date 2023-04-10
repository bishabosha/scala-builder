package builder.targets

import upickle.default.ReadWriter
import builder.reporter

/** A graph of module names to associated targets */
case class Targets(graph: Map[String, TargetContext]) derives ReadWriter:
  def optLibrary(name: String): Option[Target.Library] =
    graph.get(name).flatMap(_.optLibrary)
  def optApplication(name: String): Option[Target.Application] =
    graph.get(name).flatMap(_.optApplication)
  def library(name: String): Target.Library =
    graph(name).library
  def application(name: String): Target.Application =
    graph(name).application

  def ++ (updates: Iterable[(String, TargetUpdate)]): Targets =
    val collected = updates.groupMap((module, _) => module)((module, update) =>
      reporter.info(s"updated ${update.target.describe(module)}")
      update
    )
    val graph0 = collected.foldLeft(graph) { case (graph, (module, updates)) =>
      val oldCtx = graph.get(module)
      val oldStates =
        (for ctx <- oldCtx yield
          Map.from(ctx.targets.map(state => state.ordinal -> state)))
        .getOrElse(Map.empty)
      val patches = updates.map(tu => tu.target.ordinal -> tu.target)
      val newValue = updates.map(_.project).toSet.ensuring(_.size == 1).head
      val newStates = oldStates ++ patches
      graph.updated(module, TargetContext(newValue, newStates.values.toSet))
    }
    Targets(graph0)

/** A unique token representing the state of a target */
final class TargetId

case class TargetUpdate(project: ujson.Value, target: Target)

case class TargetContext(project: ujson.Value, targets: Set[Target]) derives ReadWriter:
  def optLibrary: Option[Target.Library] = targets.collectFirst({ case l: Target.Library => l })
  def library: Target.Library = optLibrary.get
  def optApplication: Option[Target.Application] = targets.collectFirst({ case a: Target.Application => a })
  def application: Target.Application = optApplication.get

/** A target is a cacheable entity, associated with a module */
enum Target(val token: TargetId) derives ReadWriter:
  case Library(inputHash: String, depsClasspath: List[String], outClasspath: List[String]) extends Target(TargetId())
  case Application(inputHash: String, outCommand: List[String]) extends Target(TargetId())

  def describe(name: String): String = this match
    case Target.Library(_, _, _) => s"scala library target ${name}:main"
    case Target.Application(_, _) => s"scala application target ${name}:runner"
