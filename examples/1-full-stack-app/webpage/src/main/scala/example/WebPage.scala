package example

import org.scalajs.dom.html.Element
import org.scalajs.dom.document
import org.scalajs.dom.html.*

import DomHelper.*

import scala.concurrent.ExecutionContext
import scala.util.control.NonFatal

object WebPage:
  given ExecutionContext = ExecutionContext.global

  val activeNotes = collection.mutable.Map.empty[String, Element]

  val service = new HttpClient()

  val titleInput = input()
  val contentTextArea = textarea()

  val saveButton = button("Create Note")
  saveButton.onclick = _ =>
    service
      .createNote(titleInput.value, contentTextArea.value)
      .map(addNote)

  val form: Div = div(
    titleInput,
    contentTextArea,
    saveButton
  )
  form.className = "note-form"

  val appContainer: Div = div(
    h1("My Notepad"),
    form
  )
  appContainer.id = "app-container"

  def scanNotes(timeout: Int): Unit =
    for
      notes <- service.getAllNotes()
      note <- notes
    do
      addNote(note)
    service.subscribe {
      case SubscriptionMessage.Create(note) =>
        if !activeNotes.contains(note.id) then
          addNote(note)
      case SubscriptionMessage.Delete(id) =>
        deleteNote(id)
      case SubscriptionMessage.Update(note) =>
        for elem <- activeNotes.get(note.id) do
          patchNote(note, elem)
    }

  def deleteNote(id: String): Unit =
    activeNotes.updateWith(id) {
      case Some(elem) =>
        appContainer.removeChild(elem)
        None
      case None => None
    }

  def patchNote(note: Note, elem: Element): Unit =
    val title = elem.childNodes.head.asInstanceOf[Heading]
    val content = elem.childNodes(1).asInstanceOf[Paragraph]

    if title.textContent != note.title then
      title.textContent = note.title

    if content.textContent != note.content then
      content.textContent = note.content
  end patchNote

  def addNote(note: Note): Unit =
    def patch(): Unit =
      for elem <- activeNotes.get(note.id) do
        patchNote(note, elem)
    def append(): Unit =
      val elem = div(
        h2(note.title),
        p(note.content)
      )

      val deleteButton = button("Delete Note")
      deleteButton.onclick = _ =>
        service
          .deleteNote(note.id)
          .map(res =>
            if res then deleteNote(note.id)
          )

      elem.appendChild(deleteButton)
      elem.className = "note"
      activeNotes(note.id) = elem
      appContainer.appendChild(elem)
    end append
    if activeNotes.contains(note.id) then patch()
    else append()
  end addNote

  @main def start: Unit =
    document.body.appendChild(appContainer)
    scanNotes(300)
