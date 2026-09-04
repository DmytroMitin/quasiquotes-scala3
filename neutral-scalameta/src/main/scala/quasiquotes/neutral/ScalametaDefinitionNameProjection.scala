package quasiquotes.neutral

import _root_.quasiquotes.definitions.DefinitionName

import scala.meta.Term

/** Shared structural projection for reusable Scalameta definition names. */
private[quasiquotes] object ScalametaDefinitionNameProjection:
  def project(
      sourceName: Term.Name
  ): Either[NeutralProjectionError, DefinitionName] =
    for
      present <- Option(sourceName)
        .toRight(nameFailure)
      decoded <- Option(present.value)
        .toRight(nameFailure)
      tokenText = present.tokens.map(_.text).mkString
      source = if tokenText.nonEmpty then tokenText else decoded
      name <- DefinitionName
        .fromSource(source)
        .left
        .map(_ => nameFailure)
      _ <- Either.cond(name.decoded == decoded, (), nameFailure)
    yield name

  private def nameFailure: NeutralProjectionError =
    NeutralProjectionError(
      "NEUTRAL_DEFINITION_NAME_UNSUPPORTED",
      "the declaration name must satisfy the existing Core source-spelling policy."
    )
