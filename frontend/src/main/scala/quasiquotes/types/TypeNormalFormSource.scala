package quasiquotes.types

import quasiquotes.parser.TinyTypeParser

/** Compiler-coupled source entry points for the compiler-free normal form.
  *
  * This is a frontend-owned internal boundary, not a second semantic model.
  */
object TypeNormalFormSource:
  def fromSource(
      source: String
  ): Either[TypeQuasiquoteError, TypeNormalForm] =
    TinyTypeParser
      .parse(source)
      .left
      .map(error => TypeQuasiquoteError(error.summary))
      .flatMap(parsed => TypeNormalForm.fromShape(parsed.shape))

  def equalSources(
      leftSource: String,
      rightSource: String
  ): Either[TypeQuasiquoteError, Boolean] =
    for
      left <- fromSource(leftSource)
      right <- fromSource(rightSource)
    yield left == right
