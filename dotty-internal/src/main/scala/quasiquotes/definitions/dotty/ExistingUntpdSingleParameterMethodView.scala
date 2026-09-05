package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.core.StdNames.nme

/** Exact read-only handles for one admitted existing direct ordinary method. */
private[quasiquotes] object ExistingUntpdSingleParameterMethodView:
  final case class View private[dotty] (
      captured: ExistingUntpdClassMemberFilter.Capture,
      memberIndex: Int,
      method: untpd.DefDef,
      methodName: String,
      parameter: untpd.ValDef,
      parameterName: String,
      parameterType: untpd.Tree,
      resultType: untpd.Tree,
      rhs: untpd.Tree
  )

  def capture(
      captured: ExistingUntpdClassMemberFilter.Capture,
      memberIndex: Int
  )(using Context): Either[ExistingUntpdSingleParameterMethodViewError, View] =
    for
      presentCapture <- Option(captured).toRight(
        error("CAPTURE_REQUIRED", "the U023 captured class was null.")
      )
      _ <- validateCapture(presentCapture)
      member <- presentCapture.members.lift(memberIndex).toRight(
        error(
          "MEMBER_INDEX_NOT_CAPTURED",
          s"direct-member index $memberIndex was not captured from the original Template body."
        )
      )
      method <- member.tree match
        case value: untpd.DefDef => Right(value)
        case other =>
          Left(
            error(
              "SELECTED_MEMBER_NOT_METHOD",
              s"captured direct member $memberIndex was ${nodeKind(other)}, not untpd.DefDef."
            )
          )
      _ <- validateMethodRole(method)
      clauses <- Option(method.paramss).toRight(
        error(
          "ORDINARY_PARAMETER_CLAUSE_COUNT",
          "the selected method parameter-clause sequence was null."
        )
      )
      _ <- Either.cond(
        !clauses.exists(clause => Option(clause).exists(_.exists(_.isInstanceOf[untpd.TypeDef]))),
        (),
        error(
          "TYPE_PARAMETER_CLAUSE_UNSUPPORTED",
          "type-parameter clauses are outside the one ordinary value-parameter method envelope."
        )
      )
      _ <- Either.cond(
        !clauses.exists(clause =>
          Option(clause).exists(_.exists {
            case parameter: untpd.ValDef =>
              parameter.mods.is(Flags.Given) || parameter.mods.is(Flags.Implicit)
            case _ => false
          })
        ),
        (),
        error(
          "CONTEXTUAL_PARAMETER_UNSUPPORTED",
          "using/implicit parameters are outside the admitted ordinary parameter topology."
        )
      )
      _ <- Either.cond(
        clauses.size == 1 && clauses.head != null,
        (),
        error(
          "ORDINARY_PARAMETER_CLAUSE_COUNT",
          s"the selected method had ${clauses.size} parameter clauses; exactly one ordinary value-parameter clause is required."
        )
      )
      clause = clauses.head
      _ <- Either.cond(
        clause.size == 1,
        (),
        error(
          "PARAMETER_COUNT",
          s"the ordinary parameter clause had ${clause.size} parameters; exactly one is required."
        )
      )
      parameter <- Option(clause.head).filterNot(_.isEmpty) match
        case Some(value: untpd.ValDef) => Right(value)
        case _ =>
          Left(
            error(
              "PARAMETER_REQUIRED",
              "the sole ordinary parameter was null, EmptyTree, or not untpd.ValDef."
            )
          )
      _ <- Either.cond(
        !parameter.mods.is(Flags.Given) && !parameter.mods.is(Flags.Implicit),
        (),
        error(
          "CONTEXTUAL_PARAMETER_UNSUPPORTED",
          "using/implicit parameters are outside the admitted ordinary parameter topology."
        )
      )
      parameterRhs <- treeField(parameter.unforcedRhs, "PARAMETER_REQUIRED", "parameter RHS field")
      _ <- Either.cond(
        parameterRhs.isEmpty,
        (),
        error(
          "PARAMETER_RHS_UNEXPECTED",
          "the admitted method parameter must not carry a default or other RHS."
        )
      )
      parameterType <- nonEmptyTree(
        parameter.tpt,
        "PARAMETER_TYPE_REQUIRED",
        "the method parameter type"
      )
      resultType <- nonEmptyTree(
        method.tpt,
        "RESULT_TYPE_REQUIRED",
        "the method result type"
      )
      rhs <- nonEmptyTree(
        method.unforcedRhs,
        "RHS_REQUIRED",
        "the method RHS"
      )
      _ <- validatePreTyperMethod(method)
      view = View(
        presentCapture,
        memberIndex,
        method,
        method.name.toString,
        parameter,
        parameter.name.toString,
        parameterType,
        resultType,
        rhs
      )
      _ <- validate(view)
    yield view

  private[dotty] def validate(
      view: View
  )(using Context): Either[ExistingUntpdSingleParameterMethodViewError, Unit] =
    val invariantFailure = error(
      "VIEW_IDENTITY_INVARIANT_FAILED",
      "the capture, method, parameter, parameter type, result type, or RHS no longer matches its exact captured object identity."
    )
    Option(view).toRight(invariantFailure).flatMap { value =>
      validateCapture(value.captured).left.map(_ => invariantFailure).flatMap { _ =>
        val valid = Option(value.captured.members).exists { members =>
          members.lift(value.memberIndex).exists { member =>
            Option(member).exists { presentMember =>
              Option(value.method).exists { method =>
                val exactParameter =
                  Option(method.paramss)
                    .filter(_.size == 1)
                    .flatMap(_.headOption)
                    .filter(_ != null)
                    .filter(_.size == 1)
                    .flatMap(_.headOption)
                    .collect { case parameter: untpd.ValDef => parameter }
                Option(presentMember.tree).exists(_.eq(method)) &&
                exactParameter.exists { parameter =>
                  val exactRhs = Option(method.unforcedRhs.asInstanceOf[untpd.Tree])
                  parameter.eq(value.parameter) &&
                  Option(parameter.tpt).exists(_.eq(value.parameterType)) &&
                  Option(method.tpt).exists(_.eq(value.resultType)) &&
                  exactRhs.exists(_.eq(value.rhs))
                }
              }
            }
          }
        }
        Either.cond(valid, (), invariantFailure)
      }
    }

  private def validateMethodRole(
      method: untpd.DefDef
  ): Either[ExistingUntpdSingleParameterMethodViewError, Unit] =
    val unsupported =
      method.name == nme.CONSTRUCTOR ||
        !method.mods.is(Flags.Method) ||
        method.mods.is(Flags.Synthetic) ||
        method.mods.is(Flags.Artifact) ||
        method.mods.is(Flags.Accessor) ||
        method.mods.is(Flags.ExtensionMethod) ||
        method.mods.is(Flags.Given) ||
        method.mods.is(Flags.Implicit)
    Either.cond(
      !unsupported,
      (),
      error(
        "UNSUPPORTED_METHOD_ROLE",
        "constructors, contextual, synthetic/artifact/accessor, extension, and non-method DefDefs are outside the admitted ordinary-method role."
      )
    )

  private def validateCapture(
      captured: ExistingUntpdClassMemberFilter.Capture
  )(using Context): Either[ExistingUntpdSingleParameterMethodViewError, Unit] =
    val structurallyPresent = Option(captured).exists { value =>
      Option(value.originalRoot).exists { root =>
        Option(root.rhs).isDefined &&
        Option(value.originalTemplate).exists { template =>
          Option(template.body).exists { body =>
            Option(value.members).exists { members =>
              members.size == body.size &&
              members.forall(member =>
                member != null && member.tree != null
              )
            }
          }
        }
      }
    }
    if !structurallyPresent then
      Left(
        error(
          "CAPTURE_INVARIANT_FAILED",
          "the supplied U023 capture has a null or malformed structural field."
        )
      )
    else
      ExistingUntpdClassMemberFilter
        .validateCaptured(captured)
        .left
        .map(problem => error("CAPTURE_INVARIANT_FAILED", problem.detail))

  private def validatePreTyperMethod(
      method: untpd.DefDef
  )(using Context): Either[ExistingUntpdSingleParameterMethodViewError, Unit] =
    val graph = ExistingUntpdClassMemberFilter.allTrees(method)
    graph.find(_.isInstanceOf[untpd.TypedSplice]) match
      case Some(_) =>
        Left(
          error(
            "TYPED_SPLICE_METHOD_GRAPH",
            "the selected method graph contains TypedSplice and requires post-Typer repair."
          )
        )
      case None =>
        graph.find(_.symbol != NoSymbol) match
          case Some(tree) =>
            Left(
              error(
                "SYMBOL_BEARING_METHOD_GRAPH",
                s"the selected method contains symbol-bearing ${nodeKind(tree)} and requires owner or post-Typer repair."
              )
            )
          case None => Right(())

  private def treeField(
      tree: Any,
      code: String,
      label: String
  ): Either[ExistingUntpdSingleParameterMethodViewError, untpd.Tree] =
    tree match
      case value: untpd.Tree => Right(value)
      case _ => Left(error(code, s"$label was null or not an untpd.Tree."))

  private def nonEmptyTree(
      tree: Any,
      code: String,
      label: String
  ): Either[ExistingUntpdSingleParameterMethodViewError, untpd.Tree] =
    treeField(tree, code, label).flatMap(value =>
      Either.cond(!value.isEmpty, value, error(code, s"$label was EmptyTree."))
    )

  private def nodeKind(tree: untpd.Tree): String =
    Option(tree).fold("null")(_.getClass.getSimpleName)

  private def error(
      code: String,
      detail: String
  ): ExistingUntpdSingleParameterMethodViewError =
    ExistingUntpdSingleParameterMethodViewError(code, detail)
