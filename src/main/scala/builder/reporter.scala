package builder
object reporter:
  inline def debug(msg: String)(using Settings): Unit =
    if settings.debug then println(s"[debug] $msg")
