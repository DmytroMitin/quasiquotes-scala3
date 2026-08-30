package quasiquotes.neutral

import quasiquotes.parser.TermShape

import scala.meta.*

/**
 * Compiler-free projection for integer literals and ordinary binary infix
 * terms only.
 */
object ScalametaTermProjection:
  def project(
      term: Term
  ): Either[NeutralProjectionError, ProjectedTermShape] =
    Option(term)
      .toRight(error("NEUTRAL_TERM_MISSING", "the Scalameta term must be present."))
      .flatMap(projectPresent)

  private def projectPresent(
      term: Term
  ): Either[NeutralProjectionError, ProjectedTermShape] =
    projectShape(term).map(ProjectedTermShape(_, truthfulSpan(term)))

  private def projectShape(
      term: Term
  ): Either[NeutralProjectionError, TermShape] =
    term match
      case Lit.Int(value) =>
        Right(TermShape.Literal(value.toString))
      case infix: Term.ApplyInfix =>
        for
          _ <- require(
            infix.targClause.values.isEmpty,
            "NEUTRAL_INFIX_TYPE_ARGUMENTS_UNSUPPORTED",
            "binary infix terms must not have type arguments."
          )
          right <- infix.argClause match
            case Term.ArgClause(value :: Nil, None) => Right(value)
            case _ =>
              Left(
                error(
                  "NEUTRAL_INFIX_ARGUMENT_CLAUSE_UNSUPPORTED",
                  "binary infix terms require exactly one ordinary RHS argument."
                )
              )
          leftShape <- projectShape(infix.lhs)
          rightShape <- projectShape(right)
        yield TermShape.Infix(leftShape, infix.op.value, rightShape)
      case other =>
        Left(
          error(
            "NEUTRAL_TERM_UNSUPPORTED",
            s"unsupported Scalameta term node: ${other.productPrefix}."
          )
        )

  private def truthfulSpan(tree: Tree): Option[NeutralSourceSpan] =
    tree.pos match
      case Position.None => None
      case position => Some(NeutralSourceSpan(position.start, position.end))

  private def require(
      condition: Boolean,
      code: String,
      detail: String
  ): Either[NeutralProjectionError, Unit] =
    Either.cond(condition, (), error(code, detail))

  private def error(code: String, detail: String): NeutralProjectionError =
    NeutralProjectionError(code, detail)
