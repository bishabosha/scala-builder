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

enum TargetKind derives ReadWriter:
  case Library(platform: PlatformKind)
  case Application, Package
  case Copy(target: Target)

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

      def stepModule(level: Int, name: String, platform: PlatformKind, kind: TargetKind): Target =

        val module = graph(name)
        val targetKind = module.kind match
          case ModuleKind.Library => TargetKind.Library(platform)
          case ModuleKind.Application(_) =>
            if level == 0 then kind
            else failure(s"application ${module.name} cannot be a dependency of another module")

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
        val commonDeps = targetModules.flatMap(_.moduleDeps)
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

  def ++ (updates: Iterable[(String, TargetUpdate)]): Targets =
    val collected = updates.groupMap((module, _) => module)((module, update) =>
      reporter.info(s"updated ${update.target.describe(module)}")
      update
    )
    extension (st: TargetState) def targetKind: TargetKind = st match
      case TargetState.Library(_, platform, _, _, _, _) => TargetKind.Library(platform)
      case TargetState.Application(_, _) => TargetKind.Application
      case TargetState.Package(_, _) => TargetKind.Package

    val graph0 = collected.foldLeft(graph) { case (graph, (module, updates)) =>
      val oldCtx = graph.get(module)
      val oldStates =
        (for ctx <- oldCtx yield
          Map.from(ctx.targets.map(state => state.targetKind -> state)))
        .getOrElse(Map.empty)
      val oldProjects = oldCtx.map(_.projects).getOrElse(Map.empty)
      val patches = updates.map(tu => tu.target.targetKind -> tu.target)
      val projectPatches = updates.flatMap(_.project)
      val newStates = oldStates ++ patches
      val newProjects = oldProjects ++ projectPatches
      graph.updated(module, TargetContext(newProjects, newStates.values.toSet))
    }
    Targets(graph0)

/** A unique token representing the state of a target */
final class TargetId

case class TargetUpdate(project: Option[(PlatformKind, ujson.Value)], target: TargetState)

// TODO: update, need to store the associated PlatformKind with the project
case class TargetContext(projects: Map[PlatformKind, ujson.Value], targets: Set[TargetState]) derives ReadWriter:
  def optLibrary(platform: PlatformKind): Option[TargetState.Library] =
    targets.collectFirst({ case l: TargetState.Library if l.platform == platform => l })
  def library(platform: PlatformKind): TargetState.Library = optLibrary(platform).get
  def optApplication: Option[TargetState.Application] = targets.collectFirst({ case a: TargetState.Application => a })
  def application: TargetState.Application = optApplication.get
  def optPackage: Option[TargetState.Package] = targets.collectFirst({ case p: TargetState.Package => p })
  def getPackage: TargetState.Package = optPackage.get
  // def project(platform: PlatformKind): ujson.Value = projects(platform)
  def optProject(platform: PlatformKind): Option[ujson.Value] = projects.get(platform)

/** A target is a cacheable entity, associated with a module */
enum TargetState(val token: TargetId) derives ReadWriter:
  case Library(inputHash: String, platform: PlatformKind, depsDependencies: List[String], depsClasspath: List[String], outDependencies: List[String], outClasspath: List[String]) extends TargetState(TargetId())
  case Application(inputHash: String, outCommand: List[String]) extends TargetState(TargetId())
  case Package(inputHash: String, outPath: String) extends TargetState(TargetId())

  def describe(name: String): String = this match
    case TargetState.Library(_, platform, _, _, _, _) => s"scala library target $name:main:$platform"
    case TargetState.Application(_, _) => s"scala application target $name:runner"
    case TargetState.Package(_, _) => s"package target $name:package"
