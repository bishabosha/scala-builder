package builder.errors

import scala.util.boundary, boundary.break

enum Result[+T, +E]:
  case Success[+T](value: T) extends Result[T, Nothing]
  case Failure[+E](error: E) extends Result[Nothing, E]

  inline def ?(using inline canError: CanError[E]): T = this match
    case Success(value) => value
    case err: Failure[E] => break(err)

object Result:
  inline def apply[T, E](inline body: CanError[E] ?=> T): Result[T, E] =
    boundary:
      Result.Success(body)