package builder.errors

import scala.util.boundary, boundary.{break, Label}

type CanError[E] = Label[Result.Failure[E]]
type CanAbort[T, E] = Label[Result[T, E]]

inline def failure[E](msg: E)(using inline canError: CanError[E]): Nothing =
  break[Result.Failure[E]](Result.Failure(msg))


extension [A, B, E](inline f: A => Result[B, E])
  inline def ?(using inline canError: CanError[E]): A => B = f(_).?