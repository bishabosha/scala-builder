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

/** A Plan operates over the target graph and produces a new one */
sealed trait Plan:
  /** executes the step, and signals if the project changed */
  def exec(initial: Targets)(using Settings): Result[Targets, String]

object Plan:

  def compile(targetModules: Set[Module], subcommand: SubCommand)(using Settings): Result[Plan, String] = Result:
    val graph = ModuleGraph.reachable(
      settings.config.modules,
      targetModules,
      excludeTarget = subcommand == SubCommand.Test
    )
    val stages = ModuleGraph.stages(graph)

    val stepss = stages.map: stage =>
      val steps: List[Step] = stage.map: module =>
        module.kind match
          case ModuleKind.Library => CompileScalaStep.of(module).?
          case info @ ModuleKind.Application(_) => subcommand match
            case SubCommand.Run => RunScalaStep.of(module, info).?
            case _ => CompileScalaStep.of(module).?
          case _ => failure(s"module ${module.name} is not a library or application")
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
