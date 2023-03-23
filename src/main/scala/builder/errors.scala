package builder.errors

import scala.util.control.NonLocalReturns.*

type CanError[T] = ReturnThrowable[Either[String, T]]

def withErrors[T](body: CanError[T] ?=> Either[String, T]): Either[String, T] =
  returning(body)

def abortWithError[T](msg: String)(using CanError[T]): Nothing =
  throwReturn(Left(msg))
