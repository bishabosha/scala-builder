package caskx.restApi

import cask.*
import cask.endpoints.*
import cask.router.*
import cask.internal.Util

export cask.getJson

// Source code copied from upickle, but modified to wrap the JSON body in a field called "jsonBody"
abstract class JsonBody(val path: String, override val subpath: Boolean = false)
  extends HttpEndpoint[Response[JsonData], ujson.Value] {
  val methods = Seq("post")
  type InputParser[T] = JsReader[T]

  def wrapFunction(ctx: Request, delegate: Delegate): Result[Response.Raw] = {
    val obj = for
      str <-
        try
          val boas = new java.io.ByteArrayOutputStream()
          Util.transferTo(ctx.exchange.getInputStream, boas)
          Right(new String(boas.toByteArray))
        catch
          case e: Throwable => Left(cask.model.Response(
            "Unable to deserialize input JSON text: " + e + "\n" + Util.stackTraceString(e),
            statusCode = 400
          ))
      json <-
        try Right(ujson.read(str))
        catch
          case e: Throwable => Left(cask.model.Response(
            "Input text is invalid JSON: " + e + "\n" + Util.stackTraceString(e),
            statusCode = 400
          ))
    yield Map("jsonBody" -> json)
    obj match
      case Left(r) => Result.Success(r.map(Response.Data.WritableData(_)))
      case Right(params) => delegate(params)
  }

  def wrapPathSegment(s: String): ujson.Value = ujson.Str(s)
}

class postJson(path: String, subpath: Boolean = false) extends JsonBody(path, subpath):
  override val methods = Seq("post")

class putJson(path: String, subpath: Boolean = false) extends JsonBody(path, subpath):
  override val methods = Seq("put")

class deleteJson(path: String, subpath: Boolean = false) extends cask.getJson(path, subpath):
  override val methods = Seq("delete")
