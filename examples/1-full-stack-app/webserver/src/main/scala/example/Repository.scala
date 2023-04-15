package example

import scala.concurrent.{Future, ExecutionContext}
import scala.jdk.CollectionConverters.*

import java.nio.file.{Path, Files, StandardOpenOption}
import java.util.UUID

import upickle.default.*

trait Repository:
  def getAllNotes(): Seq[Note]
  def saveNote(note: Note): Unit
  def updateNote(note: Note): Boolean
  def deleteNote(id: String): Boolean

  def createNote(title: String, content: String): Note =
    val id = UUID.randomUUID().toString
    val note = Note(id, title, content)
    saveNote(note)
    note

object Repository:
  def apply(directory: os.Path): Repository =
    if !os.exists(directory) then os.makeDir.all(directory)
    new FileRepository(directory)

  private class FileRepository(directory: os.Path) extends Repository:
    def getAllNotes(): Seq[Note] =
      files.map { file =>
        val bytes = os.read(file)
        read[Note](bytes)
      }

    def saveNote(note: Note): Unit =
      val file = directory / s"${note.id}.json"
      val bytes = write(note).getBytes
      os.write.over(file, bytes)

    def updateNote(note: Note): Boolean =
      val file = directory / s"${note.id}.json"
      val bytes = write(note).getBytes
      if os.exists(file) then
        os.write.over(file, bytes)
        true
      else false

    def deleteNote(id: String): Boolean =
      val file = directory / s"$id.json"
      if os.exists(file) then
        os.remove(file)
        true
      else false

    private def files: Seq[os.Path] = os.list(directory)
