package example

import upickle.default.{ReadWriter as Codec, *}

enum SubscriptionMessage derives Codec:
  case Delete(noteId: String)
  case Create(note: Note)
  case Update(note: Note)
