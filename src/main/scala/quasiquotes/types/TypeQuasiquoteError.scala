package quasiquotes.types

final case class TypeQuasiquoteError(message: String) extends RuntimeException(message)
