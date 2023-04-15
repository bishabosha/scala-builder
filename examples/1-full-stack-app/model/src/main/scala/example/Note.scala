package example

import upickle.default.{ReadWriter as Codec, *}

final case class Note(id: String, title: String, content: String) derives Codec
