package builder.targets

import builder.Settings
import builder.ModuleGraph
import builder.Module
import builder.ModuleKind
import builder.settings
import builder.errors.Result
import builder.errors.failure
import builder.errors.CanError
import builder.reporter
import builder.ScalaCommand
import builder.ScalaCommand.SubCommand

import scala.concurrent.*
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global
import builder.PlatformKind

/** A Plan operates over the target graph and produces a new one */
sealed trait Plan:
  /** executes the step, and signals if the project changed */
  def exec(initial: Targets)(using Settings): Result[Targets, String]

object Plan:

  def compile(targetModules: Set[Module], subcommand: SubCommand)(using Settings): Result[Plan, String] = Result:
    val graph = TargetGraph.compile(settings.config.modules, targetModules.toSeq, subcommand).?
    val stages = graph.stages

    reporter.debug(s"compilation plan for command ${subcommand} ${targetModules.map(_.name).mkString(", ")}:\n${graph.show}")

    def lookup(name: String) = settings.config.modules(name)

    val stepss = stages.map: stage =>
      val steps: List[Step] = stage.map: target =>
        target.kind match
          case TargetKind.Library(platform) => CompileScalaStep(lookup(target.module), target, platform)
          case TargetKind.Application => RunScalaStep(lookup(target.module), target)
          case TargetKind.Package => PackageScalaStep.of(lookup(target.module), target).?
          case TargetKind.Copy(fromTarget) => CopyResourceStep(lookup(target.module), target, fromTarget)
      steps

    if settings.sequential then
      new Plan:
        def exec(initial: Targets)(using Settings): Result[Targets, String] =
          Result:
            stepss.foldLeft(initial): (curr, steps) =>
              val updates = for step <- steps yield Shared.runStep(step, curr, initial).?
              curr ++ updates.flatten
      end new
    else
      new Plan:
        def exec(initial: Targets)(using Settings): Result[Targets, String] =
          Result:
            stepss.foldLeft(initial): (curr, steps) =>
              val updateResultsF = Future.sequence:
                for step <- steps yield
                  blocking:
                    Future:
                      Shared.runStep(step, curr, initial)
              val updateResults = Await.result(updateResultsF, Duration.Inf)
              curr ++ updateResults.flatMap(_.?)
      end new
    end if
  end compile
end Plan
