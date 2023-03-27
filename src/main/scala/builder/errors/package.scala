package builder.errors

import scala.util.boundary, boundary.{break, Label}

type CanError[E] = Label[Result.Failure[E]]

inline def failure[E](msg: E)(using inline canError: CanError[E]): Nothing =
  break[Result.Failure[E]](Result.Failure(msg))