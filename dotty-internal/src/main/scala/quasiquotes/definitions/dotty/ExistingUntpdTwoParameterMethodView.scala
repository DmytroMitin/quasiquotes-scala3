package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.core.StdNames.nme

/** Exact read-only handles for one admitted existing direct ordinary two-parameter method. */
private[quasiquotes] object ExistingUntpdTwoParameterMethodView:
  final case class View private[dotty] (
      captured: ExistingUntpdClassMemberFilter.Capture,
      memberIndex: Int,
      method: untpd.DefDef,
      methodName: String,
      firstParameter: untpd.ValDef,
      firstParameterName: String,
      firstParameterType: untpd.Tree,
      secondParameter: untpd.ValDef,
      secondParameterName: String,
      secondParameterType: untpd.Tree,
      resultType: untpd.Tree,
      rhs: untpd.Tree
  )

  def capture(
      captured: ExistingUntpdClassMemberFilter.Capture,
      memberIndex: Int
  )(using Context): Either[ExistingUntpdTwoParameterMethodViewError, View] =
    for
      view <- buildView(captured, memberIndex)
      _ <- validate(view)
    yield view

  private[dotty] def validate(
      view: View
  )(using Context): Either[ExistingUntpdTwoParameterMethodViewError, Unit] =
    val invariantFailure = error(
      "VIEW_IDENTITY_INVARIANT_FAILED",
      "the capture, method, parameters, parameter types, result type, or RHS no longer matches its exact captured object identity."
    )
    Option(view).toRight(invariantFailure).flatMap { value =>
      buildView(value.captured, value.memberIndex).left.map(_ => invariantFailure).flatMap { expected =>
        val valid =
          expected.captured.eq(value.captured) &&
            expected.memberIndex == value.memberIndex &&
            expected.method.eq(value.method) &&
            expected.firstParameter.eq(value.firstParameter) &&
            expected.firstParameterType.eq(value.firstParameterType) &&
            expected.secondParameter.eq(value.secondParameter) &&
            expected.secondParameterType.eq(value.secondParameterType) &&
            expected.resultType.eq(value.resultType) &&
            expected.rhs.eq(value.rhs)
        Either.cond(valid, (), invariantFailure)
      }
    }

  private def buildView(
      captured: ExistingUntpdClassMemberFilter.Capture,
      memberIndex: Int
  )(using Context): Either[ExistingUntpdTwoParameterMethodViewError, View] =
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
          "type-parameter clauses are outside the two ordinary value-parameter method envelope."
        )
      )
      _ <- Either.cond(
        !clauses.exists(clause =>
          Option(clause).exists(_.exists {
            case parameter: untpd.ValDef =>
              parameter.mods.is(Flags.Given) ||
                parameter.mods.is(Flags.Implicit) ||
                parameter.mods.is(Flags.Erased)
            case _ => false
          })
        ),
        (),
        error(
          "CONTEXTUAL_PARAMETER_UNSUPPORTED",
          "using, implicit, and erased parameters are outside the admitted ordinary parameter topology."
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
        clause.size == 2,
        (),
        error(
          "PARAMETER_COUNT",
          s"the ordinary parameter clause had ${clause.size} parameters; exactly two are required."
        )
      )
      firstParameter <- ordinaryParameter(
        clause(0),
        "FIRST_PARAMETER_REQUIRED",
        "the first ordinary parameter"
      )
      secondParameter <- ordinaryParameter(
        clause(1),
        "SECOND_PARAMETER_REQUIRED",
        "the second ordinary parameter"
      )
      _ <- validateParameterModifiers(firstParameter, "first")
      _ <- validateParameterModifiers(secondParameter, "second")
      firstParameterRhs <- treeField(
        firstParameter.unforcedRhs,
        "FIRST_PARAMETER_REQUIRED",
        "the first parameter RHS field"
      )
      secondParameterRhs <- treeField(
        secondParameter.unforcedRhs,
        "SECOND_PARAMETER_REQUIRED",
        "the second parameter RHS field"
      )
      _ <- Either.cond(
        firstParameterRhs.isEmpty && secondParameterRhs.isEmpty,
        (),
        error(
          "PARAMETER_RHS_UNEXPECTED",
          "the admitted method parameters must not carry defaults or other RHS values."
        )
      )
      firstParameterType <- nonEmptyTree(
        firstParameter.tpt,
        "FIRST_PARAMETER_TYPE_REQUIRED",
        "the first method parameter type"
      )
      secondParameterType <- nonEmptyTree(
        secondParameter.tpt,
        "SECOND_PARAMETER_TYPE_REQUIRED",
        "the second method parameter type"
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
      _ <- validatePreTyperOwner(presentCapture.originalRoot, method)
    yield View(
      presentCapture,
      memberIndex,
      method,
      method.name.toString,
      firstParameter,
      firstParameter.name.toString,
      firstParameterType,
      secondParameter,
      secondParameter.name.toString,
      secondParameterType,
      resultType,
      rhs
    )

  private def ordinaryParameter(
      tree: untpd.Tree,
      code: String,
      label: String
  ): Either[ExistingUntpdTwoParameterMethodViewError, untpd.ValDef] =
    Option(tree).filterNot(_.isEmpty) match
      case Some(value: untpd.ValDef) => Right(value)
      case _ => Left(error(code, s"$label was null, EmptyTree, or not untpd.ValDef."))

  private def validateParameterModifiers(
      parameter: untpd.ValDef,
      position: String
  ): Either[ExistingUntpdTwoParameterMethodViewError, Unit] =
    Either.cond(
      parameter.mods.flags == Flags.Param,
      (),
      error(
        "UNSUPPORTED_PARAMETER_MODIFIERS",
        s"the $position parameter was not one unmodified ordinary value parameter."
      )
    )

  private def validateMethodRole(
      method: untpd.DefDef
  ): Either[ExistingUntpdTwoParameterMethodViewError, Unit] =
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
  )(using Context): Either[ExistingUntpdTwoParameterMethodViewError, Unit] =
    val structurallyPresent = Option(captured).exists { value =>
      Option(value.originalRoot).exists { root =>
        Option(value.originalTemplate).exists { template =>
          Option(template.unforcedBody).collect {
            case body: List[?] => body.asInstanceOf[List[untpd.Tree]]
          }.exists { body =>
            Option(value.members).exists { members =>
              root.rhs.eq(template) &&
              members.size == body.size &&
              members.zipWithIndex.forall { case (member, index) =>
                member != null &&
                member.index == index &&
                member.tree != null &&
                member.tree.eq(body(index))
              }
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
        .left.map(problem => error("CAPTURE_INVARIANT_FAILED", problem.detail))

  private def validatePreTyperMethod(
      method: untpd.DefDef
  )(using Context): Either[ExistingUntpdTwoParameterMethodViewError, Unit] =
    if rawTreeGraphHasNull(method) then
      Left(
        error(
          "MALFORMED_METHOD_GRAPH",
          "the selected method graph contains a null tree, child, or child sequence."
        )
      )
    else
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

  private def validatePreTyperOwner(
      root: untpd.TypeDef,
      selectedMethod: untpd.DefDef
  )(using Context): Either[ExistingUntpdTwoParameterMethodViewError, Unit] =
    if rawTreeGraphHasNull(root) then
      Left(
        error(
          "MALFORMED_OWNER_GRAPH",
          "the captured owner graph contains a null tree, child, or child sequence."
        )
      )
    else
      val graph = ExistingUntpdClassMemberFilter.allTrees(root)
      graph.find(tree => !tree.eq(selectedMethod) && tree.isInstanceOf[untpd.TypedSplice]) match
        case Some(_) =>
          Left(
            error(
              "TYPED_SPLICE_OWNER_GRAPH",
              "the captured owner graph contains TypedSplice outside the selected method."
            )
          )
        case None =>
          graph.find(tree => !tree.eq(selectedMethod) && tree.symbol != NoSymbol) match
            case Some(tree) =>
              Left(
                error(
                  "SYMBOL_BEARING_OWNER_GRAPH",
                  s"the captured owner graph contains symbol-bearing ${nodeKind(tree)} outside the selected method."
                )
              )
            case None => Right(())

  private def rawTreeGraphHasNull(tree: untpd.Tree): Boolean =
    def containsNull(value: Any): Boolean = value match
      case null => true
      case current: untpd.Tree => current.productIterator.exists(containsNull)
      case values: Iterable[?] => values.iterator.exists(containsNull)
      case _ => false

    containsNull(tree)

  private def treeField(
      tree: Any,
      code: String,
      label: String
  ): Either[ExistingUntpdTwoParameterMethodViewError, untpd.Tree] =
    tree match
      case value: untpd.Tree => Right(value)
      case _ => Left(error(code, s"$label was null or not an untpd.Tree."))

  private def nonEmptyTree(
      tree: Any,
      code: String,
      label: String
  ): Either[ExistingUntpdTwoParameterMethodViewError, untpd.Tree] =
    treeField(tree, code, label).flatMap(value =>
      Either.cond(!value.isEmpty, value, error(code, s"$label was EmptyTree."))
    )

  private def nodeKind(tree: untpd.Tree): String =
    Option(tree).fold("null")(_.getClass.getSimpleName)

  private def error(
      code: String,
      detail: String
  ): ExistingUntpdTwoParameterMethodViewError =
    ExistingUntpdTwoParameterMethodViewError(code, detail)
