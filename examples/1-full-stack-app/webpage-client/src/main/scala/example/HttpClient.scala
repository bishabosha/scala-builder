package example

import org.scalajs.dom.*
import scala.scalajs.js

import java.io.IOException

import scala.concurrent.Future
import scala.concurrent.ExecutionContext

import upickle.default.*

class HttpClient(using ExecutionContext):

  private var _socket: Option[WebSocket] = None

  def subscribe(op: SubscriptionMessage => Unit): Unit =
    _socket match
      case None =>
        val socket = new WebSocket(s"ws://${window.location.host}/api/notes/subscribe")
        _socket = Some(socket)
        socket.onmessage = e =>
          val msg = read[SubscriptionMessage](e.data.toString)
          op(msg)
        socket.onclose = e =>
          _socket = None

      case Some(value) => // already subscribed

  def unsubscribe(): Unit =
    _socket match
      case Some(socket) =>
        socket.close()
        _socket = None

      case None => // already unsubscribed


  def getAllNotes(): Future[Seq[Note]] =
    for
      resp <- Fetch.fetch("./api/notes/all").toFuture
      notes <- resp.to[Seq[Note]]
    yield notes

  def createNote(title: String, content: String): Future[Note] =
    val request = Request(
      "./api/notes/create",
      new:
        method = HttpMethod.POST
        headers = js.Dictionary("Content-Type" -> "application/json")
        body = write(ujson.Obj("title" -> title, "content" -> content))
    )
    for
      resp <- Fetch.fetch(request).toFuture
      note <- resp.to[Note]
    yield note

  def deleteNote(id: String): Future[Boolean] =
    val request = Request(
      s"./api/notes/delete/$id",
      new:
        method = HttpMethod.DELETE
    )
    for
      resp <- Fetch.fetch(request).toFuture
      res <- resp.to[Boolean]
    yield res

  extension (resp: Response)
    private def to[T: Reader]: Future[T] =
      if resp.ok then
        for json <- resp.text().toFuture
        yield read[T](json)
      else Future.failed(new IOException(resp.statusText))
