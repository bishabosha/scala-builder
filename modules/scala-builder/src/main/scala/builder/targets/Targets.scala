package builder.targets

import upickle.default.ReadWriter
import builder.reporter
import builder.Settings
import builder.ScalaCommand.SubCommand
import builder.ModuleGraph
import builder.settings
import builder.Module
import builder.ModuleKind

import scala.collection.mutable
import scala.annotation.tailrec
import scala.collection.SeqMap

import builder.errors.*

enum TargetKind:
  case Library, Application

  def show: String = this match
    case Library => "main"
    case Application => "runner"
end TargetKind

case class Target(module: String, kind: TargetKind):
  def show: String = s"${module}:${kind.show}"

class TargetGraph private (private val targets: SeqMap[Target, collection.Seq[Target]]):

  def show: String =
    stages.zipWithIndex.map((stage, i) =>
      stage.map(_.show).mkString(s"${i + 1}. ", ", ", "")
    ).mkString("\n")

  def stages: List[List[Target]] = _stages

  private lazy val _stages: List[List[Target]] =
    val lookup = targets

    val dependencies =
      lookup.map((k, v) => k -> v.to(mutable.LinkedHashSet))

    val reverseDeps: mutable.Map[Target, mutable.LinkedHashSet[Target]] =
      val buf = mutable.LinkedHashMap.empty[Target, mutable.LinkedHashSet[Target]]
      for (k, v) <- lookup do
        buf.getOrElseUpdate(k, mutable.LinkedHashSet.empty[Target])
        for dep <- v do
          buf.getOrElseUpdate(dep, mutable.LinkedHashSet.empty[Target]).add(k)
      buf

    @tailrec
    def iterate(s1: List[Target], acc: List[List[Target]]): List[List[Target]] =
      val sNext = mutable.ListBuffer.empty[Target]
      for target <- s1 do
        val ndeps = dependencies(target)
        for d <- ndeps.toList do
          val incoming = reverseDeps(d)
          ndeps -= d
          incoming -= target
          if incoming.isEmpty then
            sNext += d
          end if
      if sNext.isEmpty then
        acc
      else
        val s2 = sNext.toList
        iterate(s2, s2 :: acc)

    val s0 = reverseDeps.collect({ case (node, incoming) if incoming.isEmpty => node }).toList
    iterate(s0, s0 :: Nil)
  end _stages

object TargetGraph:
  def compile(graph: Map[String, Module], targetModules: Seq[Module], subcommand: SubCommand): Result[TargetGraph, String] =
    Result:
      val excludeTarget = subcommand == SubCommand.Test

      // val reachableGraph = ModuleGraph.reachable(graph, targetModules, excludeTarget)

      extension (module: Module) def moduleDeps: List[String] =
        module.dependsOn // TODO: include other properties that depend on modules

      val buf = mutable.LinkedHashMap.empty[Target, mutable.ArrayBuffer[Target]]

      def step(level: Int, name: String): Target =
        val module = graph(name)
        val kind = module.kind match
          case ModuleKind.Library => TargetKind.Library
          case ModuleKind.Application(_) =>
            if level == 0 then
              if subcommand == SubCommand.Run then
                TargetKind.Application
              else if subcommand == SubCommand.Repl then
                TargetKind.Library
              else
                failure(s"cannot create target for ${module.name} with subcommand $subcommand")
            else failure(s"application ${module.name} cannot be a dependency of another module")

        val target = Target(module.name, kind)

        buf.getOrElseUpdate(target, mutable.ArrayBuffer.empty) // initialize parent

        for dep <- module.dependsOn do
          val depTarget = step(level + 1, dep)
          buf(target) += depTarget // register dep as child of parent

        target
      end step

      if !excludeTarget then
        for target <- targetModules do
          step(0, target.name)
      else
        val commonDeps = targetModules.flatMap(_.moduleDeps)
        for dep <- commonDeps do
          step(1, dep)

      TargetGraph(buf)
  end compile
end TargetGraph

/** A graph of module names to associated targets */
case class Targets(graph: Map[String, TargetContext]) derives ReadWriter:
  def optLibrary(name: String): Option[TargetState.Library] =
    graph.get(name).flatMap(_.optLibrary)
  def optApplication(name: String): Option[TargetState.Application] =
    graph.get(name).flatMap(_.optApplication)
  def library(name: String): TargetState.Library =
    graph(name).library
  def application(name: String): TargetState.Application =
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

case class TargetUpdate(project: ujson.Value, target: TargetState)

case class TargetContext(project: ujson.Value, targets: Set[TargetState]) derives ReadWriter:
  def optLibrary: Option[TargetState.Library] = targets.collectFirst({ case l: TargetState.Library => l })
  def library: TargetState.Library = optLibrary.get
  def optApplication: Option[TargetState.Application] = targets.collectFirst({ case a: TargetState.Application => a })
  def application: TargetState.Application = optApplication.get

/** A target is a cacheable entity, associated with a module */
enum TargetState(val token: TargetId) derives ReadWriter:
  case Library(inputHash: String, depsClasspath: List[String], outClasspath: List[String]) extends TargetState(TargetId())
  case Application(inputHash: String, outCommand: List[String]) extends TargetState(TargetId())

  def describe(name: String): String = this match
    case TargetState.Library(_, _, _) => s"scala library target ${name}:main"
    case TargetState.Application(_, _) => s"scala application target ${name}:runner"
