package builder.errors

import scala.util.boundary, boundary.break, boundary.Label

object optional:

  object OptionSugar:
    extension [T](opt: Option[T])
      inline def some(using CanAbortNone): T =
        opt match
          case Some(value) => value
          case None => break(None)

  type CanAbortNone = Label[None.type]

  inline def apply[T](inline body: CanAbortNone ?=> T): Option[T] =
    boundary:
      Some(body)
