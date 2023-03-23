package builder
object reporter:
  inline def debug(inline msg: String)(using Settings): Unit =
    if settings.debug then Console.err.println(s"[debug] $msg")
  inline def info(inline msg: String): Unit =
    Console.err.println(s"[info] $msg")
  inline def error(inline msg: String): Unit =
    Console.err.println(s"[error] $msg")
