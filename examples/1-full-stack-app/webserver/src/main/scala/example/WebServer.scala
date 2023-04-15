package example

import java.nio.file.Paths

import cask.*
import caskx.restApi
import caskx.websocketApi

import scala.util.chaining.*

import upickle.default.ReadWriter as Codec

import scala.concurrent.ExecutionContext.Implicits.global

object WebServer extends MainRoutes:

  case class AddNote(title: String, content: String) derives Codec

  private val repository = Repository(os.pwd / "repo-data")

  initialize()
  println(s"Server online at http://$host:$port/")

  @cask.get("/")
  def base() =
    StaticResource(
      "index.html",
      getClass.getClassLoader,
      Seq("Content-Type" -> "text/html")
    )

  @cask.get("/assets/:fileName", subpath = true)
  def getAsset(fileName: String) =
    val contentType =
      if fileName.endsWith(".js") then Some("text/javascript")
      else if fileName.endsWith(".js.map") then Some("application/json")
      else if fileName.endsWith(".html") then Some("text/html")
      else if fileName.endsWith(".css") then Some("text/css")
      else None
    val headers = contentType.map("Content-Type" -> _).toSeq
    StaticResource(s"assets/$fileName", getClass.getClassLoader, headers)

  private val subscribers = websocketApi.WsSubscriberActor[SubscriptionMessage]()

  @cask.websocket("api/notes/subscribe")
  def subscribe(): cask.WebsocketResult =
    cask.WsHandler(subscribers.subscribe)

  @restApi.getJson("api/notes/all")
  def getAllNotes(): Seq[Note] = repository.getAllNotes()

  @restApi.postJson("api/notes/create")
  def createNote(jsonBody: AddNote): Note =
    repository.createNote(jsonBody.title, jsonBody.content).tap { note =>
      subscribers.broadcast(SubscriptionMessage.Create(note))
    }

  @restApi.deleteJson("api/notes/delete/:id")
  def deleteNote(id: String): Boolean =
    repository.deleteNote(id).tap { deleted =>
      if deleted then subscribers.broadcast(SubscriptionMessage.Delete(id))
    }

  @restApi.putJson("api/notes/update/:id")
  def updateNote(jsonBody: Note, id: String): Boolean =
    assert(id == jsonBody.id)
    repository.updateNote(jsonBody).tap { updated =>
      if updated then subscribers.broadcast(SubscriptionMessage.Update(jsonBody))
    }
