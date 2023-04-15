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
import builder.ResourceGenerator
import builder.PlatformKind

enum TargetKind(val weight: Int) derives ReadWriter:
  case Library(platform: PlatformKind) extends TargetKind(2)
  case Application extends TargetKind(2)
  case Package extends TargetKind(2)
  case Copy(target: Target) extends TargetKind(1)

  def show: String = this match
    case Library(kind) => s"main:$kind"
    case Application => "runner"
    case Package => "package"
    case Copy(target) => s"copy[${target.show}]"
end TargetKind

case class Target(module: String, kind: TargetKind) derives ReadWriter:
  def show: String = s"${module}:${kind.show}"

class TargetGraph private (private val targets: SeqMap[Target, collection.Seq[Target]]):

  def show: String =
    stages.zipWithIndex.map((stage, i) =>
      stage.map(_.show).mkString(s"${i + 1}. ", ", ", "")
    ).mkString("\n")

  def stages: List[List[Target]] = _stages

  private lazy val _stages: List[List[Target]] =
    val lookup = targets

    val outgoingEdges =
      lookup.map((k, v) => k -> v.map(dep => dep -> dep.kind.weight).to(mutable.LinkedHashMap))

    val incomingEdges: mutable.SeqMap[Target, mutable.LinkedHashMap[Target, Int]] =
      val buf = mutable.LinkedHashMap.empty[Target, mutable.LinkedHashMap[Target, Int]]
      for (k, v) <- lookup do
        buf.getOrElseUpdate(k, mutable.LinkedHashMap.empty[Target, Int])
        for dep <- v do
          buf.getOrElseUpdate(dep, mutable.LinkedHashMap.empty[Target, Int]).update(k, dep.kind.weight)
      buf

    @tailrec
    def iterate(s1: List[Target], s0: List[Target], acc: List[List[Target]]): List[List[Target]] =
      val sNext = mutable.LinkedHashSet.empty[Target]
      val sLeftOver = mutable.LinkedHashMap.empty[Target, mutable.LinkedHashSet[Target]]

      // reset weights of targets that are in s0
      for target <- s0 do
        val outgoing = outgoingEdges(target)
        for dep <- outgoing.keySet do
          val incoming = incomingEdges(dep)
          incoming(target) = dep.kind.weight
          outgoing(dep) = dep.kind.weight

      val sAll = s1 ::: s0

      val maxWeight = sAll.flatMap(t => outgoingEdges(t).values.maxOption).maxOption.getOrElse(1)
      val minWeight = sAll.flatMap(t => outgoingEdges(t).values.minOption).minOption.getOrElse(1)
      val decrementBy = minWeight

      extension (assoc: mutable.LinkedHashMap[Target, Int])
        def decrement(t: Target): Option[Int] =
          assoc.updateWith(t) {
            case Some(n) if n > decrementBy => Some(n - decrementBy)
            case _ => None
          }

      def once(): Unit =
        for target <- sAll do
          val outgoing = outgoingEdges(target)
          for dep <- outgoing.keySet do
            val incoming = incomingEdges(dep)
            ((outgoing.decrement(dep), incoming.decrement(target)): @unchecked) match
              case (None, None) =>
                sNext += dep
              case (Some(_), Some(_)) =>
                sLeftOver.getOrElseUpdate(dep, mutable.LinkedHashSet.empty) += target

      var iterated = 0
      var countDown = (maxWeight - minWeight) / decrementBy
      while
        once()
        iterated += 1
        sNext.isEmpty && countDown > 0
      do
        countDown -= 1
      if sNext.isEmpty then
        acc
      else
        val s2 = sNext.toList
        val sRest = (sLeftOver -- s2).values.flatten.toList.distinct
        iterate(s2, sRest, s2 :: acc)

    val s0 = incomingEdges.collect({ case (target, incoming) if incoming.isEmpty => target }).toList
    iterate(s0, Nil, s0 :: Nil)
  end _stages

object TargetGraph:
  def compile(graph: Map[String, Module], targetModules: Seq[Module], subcommand: SubCommand): Result[TargetGraph, String] =
    Result:
      val excludeTarget = subcommand == SubCommand.Test

      val buf = mutable.LinkedHashMap.empty[Target, mutable.ArrayBuffer[Target]]

      def stepResource(level: Int, parentModule: String, resource: ResourceGenerator): Target = resource match
        case ResourceGenerator.Copy(fromTarget, dest) =>
          val target = Target(parentModule, TargetKind.Copy(fromTarget))
          buf.getOrElseUpdate(target, mutable.ArrayBuffer.empty) += fromTarget

          // need to resolve the transitive dependencies of the target
          val Target(fromModule, kind) = fromTarget
          kind match
            case TargetKind.Package =>
              val fromPlatform =
                val options = graph(fromModule).platforms
                if options.sizeIs == 1 then options.head
                else failure(s"cannot copy target ${fromTarget.show} because it is a package target and has multiple platforms")

              stepModule(0, fromModule, fromPlatform, TargetKind.Package)
            case TargetKind.Library(_) => failure(s"cannot copy library target ${fromTarget.show}")
            case TargetKind.Application => failure(s"cannot copy application target ${fromTarget.show}")
            case TargetKind.Copy(target) => failure(s"cannot copy target ${fromTarget.show} because it is a copy target")

          target

      def stepModule(level: Int, name: String, platform: PlatformKind, targetKind: TargetKind): Target =

        val module = graph(name)

        val target = Target(module.name, targetKind)

        buf.getOrElseUpdate(target, mutable.ArrayBuffer.empty) // initialize parent

        for dep <- module.dependsOn do
          val depTarget = stepModule(level + 1, dep, platform, TargetKind.Library(platform))
          buf(target) += depTarget // register dep as child of parent

        for resource <- module.resourceGenerators do
          val resourceTarget = stepResource(level + 1, module.name, resource)
          buf(target) += resourceTarget // register resource as child of parent

        target
      end stepModule

      val rootKind = subcommand match
        case SubCommand.Run => TargetKind.Application
        case SubCommand.Repl => TargetKind.Library(PlatformKind.jvm)
        case SubCommand.Test => TargetKind.Library(PlatformKind.jvm)
        case SubCommand.Clean => failure("cannot create target graph for clean subcommand")

      if !excludeTarget then
        for target <- targetModules do
          stepModule(0, target.name, PlatformKind.jvm, rootKind)
      else
        val commonDeps = targetModules.flatMap(_.dependsOn)
        for dep <- commonDeps do
          stepModule(1, dep, PlatformKind.jvm, rootKind)

      TargetGraph(buf)
  end compile
end TargetGraph

/** A graph of module names to associated targets */
case class Targets(graph: Map[String, TargetContext]) derives ReadWriter:
  def optLibrary(name: String, platform: PlatformKind): Option[TargetState.Library] =
    graph.get(name).flatMap(_.optLibrary(platform))
  def optApplication(name: String): Option[TargetState.Application] =
    graph.get(name).flatMap(_.optApplication)
  def optPackage(name: String): Option[TargetState.Package] =
    graph.get(name).flatMap(_.optPackage)
  def library(name: String, platform: PlatformKind): TargetState.Library =
    graph(name).library(platform)
  def application(name: String): TargetState.Application =
    graph(name).application
  def getPackage(name: String): TargetState.Package =
    graph(name).getPackage

  def ++ (updates: Iterable[(String, TargetState)]): Targets =
    if updates.isEmpty then this
    else
      val collected = updates.groupMap((module, _) => module)((module, update) =>
        reporter.info(s"updated ${update.describe(module)}")
        update
      )
      extension (st: TargetState) def targetKind: TargetKind = st match
        case TargetState.Library(_, _, platform, _, _, _, _) => TargetKind.Library(platform)
        case TargetState.Application(_, _, _) => TargetKind.Application
        case TargetState.Package(_, _, _) => TargetKind.Package
        case TargetState.Copy(target) => TargetKind.Copy(target)

      val graph0 = collected.foldLeft(graph) { case (graph, (module, updates)) =>
        val oldStates =
          (for ctx <- graph.get(module) yield
            Map.from(ctx.targets.map(state => state.targetKind -> state)))
          .getOrElse(Map.empty)
        val patches = updates.map(target => target.targetKind -> target)
        val newStates = oldStates ++ patches
        graph.updated(module, TargetContext(newStates.values.toSet))
      }
      Targets(graph0)

/** A unique token representing the state of a target */
final class TargetId

// TODO: update, need to store the associated PlatformKind with the project
case class TargetContext(targets: Set[TargetState]) derives ReadWriter:
  def optLibrary(platform: PlatformKind): Option[TargetState.Library] =
    targets.collectFirst({ case l: TargetState.Library if l.platform == platform => l })
  def library(platform: PlatformKind): TargetState.Library = optLibrary(platform).get
  def optApplication: Option[TargetState.Application] = targets.collectFirst({ case a: TargetState.Application => a })
  def application: TargetState.Application = optApplication.get
  def optPackage: Option[TargetState.Package] = targets.collectFirst({ case p: TargetState.Package => p })
  def getPackage: TargetState.Package = optPackage.get

/** A target is a cacheable entity, associated with a module */
enum TargetState(val token: TargetId) derives ReadWriter:
  case Library(projectHash: String, sourcesHash: String, platform: PlatformKind, extraDependencies: List[String], extraClasspath: List[String], dependencies: List[String], classpath: List[String]) extends TargetState(TargetId())
  case Application(projectHash: String, sourcesHash: String, outCommand: List[String]) extends TargetState(TargetId())
  case Package(projectHash: String, sourcesHash: String, outPath: String) extends TargetState(TargetId())
  case Copy(target: Target) extends TargetState(TargetId())

  def describe(name: String): String = this match
    case TargetState.Library(_, _, platform, _, _, _, _) => s"scala library target $name:main:$platform"
    case TargetState.Application(_, _, _) => s"scala application target $name:runner"
    case TargetState.Package(_, _, _) => s"package target $name:package"
    case TargetState.Copy(target) => s"resource generator target $name:copy[${target.show}]"
