package quasiquotes.types

import scala.quoted.Quotes

/** Non-public module bridge for the accepted direct-reflection Type application. */
private[quasiquotes] object RuntimeTypeApplication:
  def build(using q: Quotes)(
      parts: Seq[String],
      constructor: q.reflect.TypeRepr,
      arguments: Seq[q.reflect.TypeRepr],
      additionalArgumentSequences: Seq[q.reflect.TypeRepr]*
  ): Either[String, q.reflect.TypeRepr] =
    for
      _ <- TypeSequenceSource.validate(
        parts,
        additionalArgumentSequences == null || additionalArgumentSequences.nonEmpty
      )
      result <- ReflectedTypeApplication.build(using q)(constructor, arguments)
    yield result
