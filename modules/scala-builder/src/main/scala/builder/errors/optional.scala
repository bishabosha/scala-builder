package builder.errors

import scala.util.boundary, boundary.break, boundary.Label

object optional:

  type CanAbortNone = Label[None.type]

  inline def apply[T](inline body: CanAbortNone ?=> T): Option[T] =
    boundary:
      Some(body)
