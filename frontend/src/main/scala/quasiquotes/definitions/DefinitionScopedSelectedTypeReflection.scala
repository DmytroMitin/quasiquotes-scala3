package quasiquotes.definitions

import quasiquotes.types.ResolvedTypeReflection

import scala.quoted.*
import scala.util.control.NonFatal

/** Current-Dotty evidence bridge for one direct definition-parameter prefix.
  *
  * The enclosing DefDef supplies the exact parameter-symbol association. The
  * compiler-free plan supplies only scope-local BinderId structure and a
  * structured member declaration identity.
  */
private[quasiquotes] object DefinitionScopedSelectedTypeReflection:
  final case class Error(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  def inspect(using q: Quotes)(
      definition: q.reflect.DefDef,
      plan: DefinitionScopedSelectedTypePlan,
      target: q.reflect.TypeRepr
  ): Either[Error, Unit] =
    import q.reflect.*

    for
      parameterSymbols <- definitionParameterSymbols(definition, plan)
      _ <- target match
        case selected: TypeRef =>
          selected.qualifier match
            case prefix: TermRef =>
              val observedPosition = parameterSymbols.indexOf(prefix.termSymbol)
              if observedPosition < 0 then
                prefix.qualifier match
                  case _: TermRef =>
                    Left(
                      error(
                        "STABLE_SELECTED_TYPE_NESTED_PATH_UNSUPPORTED",
                        "the selected Type prefix is not a direct enclosing-definition parameter."
                      )
                    )
                  case _ =>
                    Left(
                      error(
                        "STABLE_SELECTED_TYPE_PREFIX_UNBOUND",
                        "the selected Type prefix is not bound by the enclosing definition."
                      )
                    )
              else if observedPosition != plan.prefixBinderPosition then
                Left(
                  error(
                    "STABLE_SELECTED_TYPE_PREFIX_MISMATCH",
                    "the selected Type uses a different enclosing-definition binder."
                  )
                )
              else if !selected.typeSymbol.isType then
                Left(
                  error(
                    "STABLE_SELECTED_TYPE_MEMBER_KIND_MISMATCH",
                    "the selected declaration is not a Type member."
                  )
                )
              else
                ResolvedTypeReflection
                  .deriveFromOwner(selected.typeSymbol.owner, selected.name)
                  .left
                  .map(value =>
                    error(
                      "STABLE_SELECTED_TYPE_MEMBER_IDENTITY_UNAVAILABLE",
                      value.message
                    )
                  )
                  .flatMap { observedMember =>
                    Either.cond(
                      plan.accepts(observedPosition, observedMember),
                      (),
                      error(
                        "STABLE_SELECTED_TYPE_MEMBER_MISMATCH",
                        "the selected Type member declaration differs from the scoped semantic plan."
                      )
                    )
                  }
            case _ =>
              Left(
                error(
                  "STABLE_SELECTED_TYPE_COMPILER_SHAPE_UNSUPPORTED",
                  "the selected Type does not have a stable TermRef prefix."
                )
              )
        case _ =>
          Left(
            error(
              "STABLE_SELECTED_TYPE_COMPILER_SHAPE_UNSUPPORTED",
              "the target is not one direct selected TypeRef."
            )
          )
    yield ()

  def rebuild(using q: Quotes)(
      definition: q.reflect.DefDef,
      plan: DefinitionScopedSelectedTypePlan,
      memberSymbol: q.reflect.Symbol
  ): Either[Error, q.reflect.TypeRepr] =
    import q.reflect.*

    for
      parameterSymbols <- definitionParameterSymbols(definition, plan)
      _ <- Either.cond(
        memberSymbol != Symbol.noSymbol && memberSymbol.isType,
        (),
        error(
          "STABLE_SELECTED_TYPE_MEMBER_KIND_MISMATCH",
          "exact reconstruction requires one Type-member symbol."
        )
      )
      memberIdentity <- ResolvedTypeReflection
        .deriveFromOwner(memberSymbol.owner, memberSymbol.name)
        .left
        .map(value =>
          error(
            "STABLE_SELECTED_TYPE_MEMBER_IDENTITY_UNAVAILABLE",
            value.message
          )
        )
      _ <- Either.cond(
        memberIdentity == plan.memberIdentity,
        (),
        error(
          "STABLE_SELECTED_TYPE_MEMBER_MISMATCH",
          "the exact Type-member symbol differs from the scoped semantic plan."
        )
      )
      selected <- selectMember(
        parameterSymbols(plan.prefixBinderPosition),
        memberSymbol
      )
      _ <- selected match
        case value: TypeRef =>
          value.qualifier match
            case prefix: TermRef
                if value.typeSymbol == memberSymbol &&
                  prefix.termSymbol == parameterSymbols(
                    plan.prefixBinderPosition
                  ) =>
              Right(())
            case _ =>
              Left(
                error(
                  "STABLE_SELECTED_TYPE_COMPILER_SHAPE_UNSUPPORTED",
                  "exact reconstruction did not retain the expected parameter prefix and Type-member symbol."
                )
              )
        case _ =>
          Left(
            error(
              "STABLE_SELECTED_TYPE_COMPILER_SHAPE_UNSUPPORTED",
              "exact reconstruction did not retain the expected parameter prefix and Type-member symbol."
            )
          )
    yield selected

  private def definitionParameterSymbols(using q: Quotes)(
      definition: q.reflect.DefDef,
      plan: DefinitionScopedSelectedTypePlan
  ): Either[Error, Vector[q.reflect.Symbol]] =
    val parameters = definition.termParamss.flatMap(_.params).map(_.symbol).toVector
    if parameters.size != plan.binderCount then
      Left(
        error(
          "STABLE_SELECTED_TYPE_SCOPE_MISMATCH",
          "the semantic binder count does not match the enclosing definition parameter count."
        )
      )
    else if parameters.distinct.size != parameters.size then
      Left(
        error(
          "STABLE_SELECTED_TYPE_SCOPE_MISMATCH",
          "the enclosing definition parameter symbols must be distinct."
        )
      )
    else if parameters.exists(_.owner != definition.symbol) then
      Left(
        error(
          "STABLE_SELECTED_TYPE_SCOPE_MISMATCH",
          "each admitted parameter symbol must be owned by the enclosing definition."
        )
      )
    else Right(parameters)

  private def selectMember(using q: Quotes)(
      parameterSymbol: q.reflect.Symbol,
      memberSymbol: q.reflect.Symbol
  ): Either[Error, q.reflect.TypeRepr] =
    try Right(parameterSymbol.termRef.select(memberSymbol))
    catch
      case NonFatal(_) =>
        Left(
          error(
            "STABLE_SELECTED_TYPE_COMPILER_SHAPE_UNSUPPORTED",
            "the exact compiler could not rebuild the direct parameter-prefix selection."
          )
        )

  private def error(code: String, detail: String): Error =
    Error(code, detail)
