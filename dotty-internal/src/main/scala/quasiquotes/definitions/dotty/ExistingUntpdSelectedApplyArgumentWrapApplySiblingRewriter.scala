package quasiquotes.definitions.dotty

import scala.util.control.NonFatal

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.{NoSource, SourceFile}

/** Wraps one exact U014 argument with its exact original child and one fresh bounded Apply sibling. */
private[quasiquotes] object ExistingUntpdSelectedApplyArgumentWrapApplySiblingRewriter:
  final case class Result(
      validatedExisting: ExistingUntpdSelectedApplyArgumentRewriter.Result,
      wrapperApply: untpd.Apply,
      suppliedFreshSiblingApply: untpd.Apply,
      suppliedFreshSiblingFunction: untpd.Ident,
      suppliedFreshSiblingArguments: List[untpd.Tree],
      rebuiltRoot: untpd.TypeDef,
      rebuiltTemplate: untpd.Template,
      rebuiltTarget: untpd.DefDef,
      rebuiltApply: untpd.Apply
  ):
    def originalRoot: untpd.TypeDef = validatedExisting.originalRoot
    def originalTemplate: untpd.Template = validatedExisting.originalTemplate
    def originalTarget: untpd.DefDef = validatedExisting.originalTarget
    def originalApply: untpd.Apply = validatedExisting.originalApply
    def originalArgument: untpd.Tree = validatedExisting.originalArgument
    def argumentIndex: Int = validatedExisting.argumentIndex
    def wrapperFunction: untpd.Ident = wrapperApply.fun.asInstanceOf[untpd.Ident]
    def freshSiblingApply: untpd.Apply = suppliedFreshSiblingApply
    def freshSiblingFunction: untpd.Ident = suppliedFreshSiblingFunction
    def freshSiblingArguments: List[untpd.Tree] = suppliedFreshSiblingArguments
    def prefix: List[untpd.Tree] = validatedExisting.prefix
    def suffix: List[untpd.Tree] = validatedExisting.suffix
    def preservedArguments: List[untpd.Tree] = validatedExisting.preservedArguments

  def rewrite(
      root: untpd.TypeDef,
      exactTarget: untpd.DefDef,
      exactArgument: untpd.Tree,
      wrapperFunction: untpd.Tree,
      freshSibling: untpd.Tree
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentWrapApplySiblingRewriteError, Result] =
    try
      for
        validated <- validateExistingAndWrapper(root, exactTarget, exactArgument, wrapperFunction)
        (existing, wrapper) = validated
        sibling <- validateFreshSibling(freshSibling, wrapper, existing)
        result <- rebuild(existing, wrapper, sibling)
      yield result
    catch
      case NonFatal(exception) =>
        Left(error(
          "SELECTED_APPLY_ARGUMENT_WRAP_APPLY_SIBLING_REWRITE_FAILED",
          Option(exception.getMessage).filter(_.nonEmpty)
            .getOrElse(exception.getClass.getSimpleName)
        ))

  private def validateExistingAndWrapper(
      root: untpd.TypeDef,
      exactTarget: untpd.DefDef,
      exactArgument: untpd.Tree,
      wrapperFunction: untpd.Tree
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentWrapApplySiblingRewriteError,
    (ExistingUntpdSelectedApplyArgumentRewriter.Result, untpd.Ident)] =
    given SourceFile = NoSource
    val validationSibling = untpd.Number("0", untpd.NumberKind.Whole(10))
    ExistingUntpdSelectedApplyArgumentWrapSiblingRewriter
      .rewrite(root, exactTarget, exactArgument, wrapperFunction, validationSibling)
      .left.map(problem => error(problem.code, problem.detail))
      .map(result => (result.validatedExisting, result.wrapperFunction))

  private def validateFreshSibling(
      freshSibling: untpd.Tree,
      wrapperFunction: untpd.Ident,
      existing: ExistingUntpdSelectedApplyArgumentRewriter.Result
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentWrapApplySiblingRewriteError,
    untpd.Apply] =
    Option(freshSibling).toRight(
      error("FRESH_SIBLING_APPLY_REQUIRED", "the fresh child-bearing sibling was null.")
    ).flatMap {
      case apply: untpd.Apply => validateSiblingApply(apply, wrapperFunction, existing)
      case other => Left(error("FRESH_SIBLING_APPLY_REQUIRED",
        s"the fresh child-bearing sibling was ${nodeKind(other)}, not Apply."))
    }

  private def validateSiblingApply(
      sibling: untpd.Apply,
      wrapperFunction: untpd.Ident,
      existing: ExistingUntpdSelectedApplyArgumentRewriter.Result
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentWrapApplySiblingRewriteError,
    untpd.Apply] =
    val arguments = sibling.args
    val function = sibling.fun
    if arguments == null then
      Left(error("FRESH_SIBLING_ARGUMENTS_REQUIRED", "the fresh sibling argument list was null."))
    else if function == null then
      Left(error("FRESH_SIBLING_FUNCTION_REQUIRED", "the fresh sibling function was null."))
    else if !function.isInstanceOf[untpd.Ident] then
      Left(error("FRESH_SIBLING_FUNCTION_IDENT_REQUIRED",
        s"the fresh sibling function was ${nodeKind(function)}, not direct Ident."))
    else if arguments.size < 1 || arguments.size > 3 then
      Left(error("FRESH_SIBLING_ARGUMENT_COUNT",
        s"the fresh sibling Apply had ${arguments.size} arguments; expected 1 to 3."))
    else
      val nodes = sibling :: function :: arguments
      val outerFunctionNodes = allTrees(existing.originalApply.fun)
      val aliasesOriginal = nodes.exists(_.eq(existing.originalArgument))
      val aliasesExistingArgument = nodes.exists(node =>
        existing.originalApply.args.exists(_.eq(node))
      )
      val aliasesOuterFunction = nodes.exists(node => outerFunctionNodes.exists(_.eq(node)))
      val aliasesWrapper = nodes.exists(_.eq(wrapperFunction))
      val duplicateNodes = nodes.indices.exists(left =>
        nodes.indices.exists(right => left < right && nodes(left).eq(nodes(right)))
      )
      if aliasesOriginal then Left(error("FRESH_SIBLING_ALIASES_ORIGINAL",
        "a fresh sibling node aliases the preserved original argument."))
      else if aliasesExistingArgument then Left(error("FRESH_SIBLING_ALIASES_EXISTING_ARGUMENT",
        "a fresh sibling node aliases an existing outer argument."))
      else if aliasesOuterFunction then Left(error("FRESH_SIBLING_ALIASES_OUTER_FUNCTION",
        "a fresh sibling node aliases the preserved outer function subtree."))
      else if aliasesWrapper then Left(error("FRESH_SIBLING_ALIASES_WRAPPER_FUNCTION",
        "a fresh sibling node aliases the fresh wrapper function."))
      else if duplicateNodes then Left(error("FRESH_SIBLING_NODE_ALIAS",
        "fresh sibling Apply nodes must be distinct objects."))
      else validateSiblingProvenance(sibling, function.asInstanceOf[untpd.Ident], arguments)

  private def validateSiblingProvenance(
      sibling: untpd.Apply,
      function: untpd.Ident,
      arguments: List[untpd.Tree]
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentWrapApplySiblingRewriteError,
    untpd.Apply] =
    validateFreshNode(function, "FRESH_SIBLING_FUNCTION", "the fresh sibling function").flatMap { _ =>
      arguments.zipWithIndex.foldLeft[Either[
        ExistingUntpdSelectedApplyArgumentWrapApplySiblingRewriteError, Unit]](Right(())) {
        case (result, (argument, index)) => result.flatMap { _ =>
          Option(argument).toRight(error("FRESH_SIBLING_ARGUMENT_REQUIRED",
            s"fresh sibling argument $index was null.")).flatMap { present =>
            if present.isInstanceOf[untpd.TypedSplice] then
              Left(error("FRESH_SIBLING_ARGUMENT_TYPED_SPLICE_UNSUPPORTED",
                s"fresh sibling argument $index was TypedSplice."))
            else if !isAdmittedLeaf(present) then
              Left(error("FRESH_SIBLING_ARGUMENT_LEAF_REQUIRED",
                s"fresh sibling argument $index was ${nodeKind(present)}, not Ident, Number, or Literal."))
            else validateFreshNode(present, "FRESH_SIBLING_ARGUMENT",
              s"fresh sibling argument $index")
          }
        }
      }
    }.flatMap { _ =>
      validateFreshNode(sibling, "FRESH_SIBLING_APPLY", "the fresh sibling Apply")
    }.map(_ => sibling)

  private def validateFreshNode(
      tree: untpd.Tree,
      prefix: String,
      label: String
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentWrapApplySiblingRewriteError, Unit] =
    if tree.isInstanceOf[untpd.TypedSplice] then
      Left(error(s"${prefix}_TYPED_SPLICE_UNSUPPORTED", s"$label was TypedSplice."))
    else if tree.source.exists then
      Left(error(s"${prefix}_SOURCE_PROVENANCE", s"$label must not carry a source."))
    else if tree.span.exists then
      Left(error(s"${prefix}_SPAN_PROVENANCE", s"$label must not carry a span."))
    else if tree.symbol != NoSymbol then
      Left(error(s"${prefix}_SYMBOL_PROVENANCE", s"$label must not carry a symbol."))
    else Right(())

  private def rebuild(
      existing: ExistingUntpdSelectedApplyArgumentRewriter.Result,
      wrapperFunction: untpd.Ident,
      freshSibling: untpd.Apply
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentWrapApplySiblingRewriteError, Result] =
    given SourceFile = NoSource
    val wrapperApply = untpd.Apply(
      wrapperFunction,
      existing.originalArgument :: freshSibling :: Nil
    )
    val arguments = existing.originalApply.args.zipWithIndex.map {
      case (_, index) if index == existing.argumentIndex => wrapperApply
      case (argument, _) => argument
    }
    val rebuiltApply = untpd.Apply(existing.originalApply.fun, arguments)
    val rebuiltTarget = untpd.DefDef(
      existing.originalTarget.name,
      existing.originalTarget.paramss,
      existing.originalTarget.tpt,
      rebuiltApply
    ).withMods(existing.originalTarget.mods)
    val rebuiltTemplate = untpd.Template(
      existing.originalTemplate.constr,
      existing.originalTemplate.parentsOrDerived,
      existing.originalTemplate.derived,
      existing.originalTemplate.self,
      existing.prefix ::: rebuiltTarget :: existing.suffix
    )
    val rebuiltRoot = untpd.TypeDef(existing.originalRoot.name, rebuiltTemplate)
      .withMods(existing.originalRoot.mods)
    val result = Result(existing, wrapperApply, freshSibling,
      freshSibling.fun.asInstanceOf[untpd.Ident], freshSibling.args,
      rebuiltRoot, rebuiltTemplate, rebuiltTarget, rebuiltApply)
    verify(result).map(_ => result)

  private def verify(
      result: Result
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentWrapApplySiblingRewriteError, Unit] =
    val freshNodes = Vector[untpd.Tree](
      result.rebuiltRoot,
      result.rebuiltTemplate,
      result.rebuiltTarget,
      result.rebuiltApply,
      result.wrapperApply,
      result.wrapperFunction,
      result.freshSiblingApply,
      result.freshSiblingFunction
    ) ++ result.freshSiblingArguments
    val provenanceValid = freshNodes.forall(tree =>
      tree != null && !tree.source.exists && !tree.span.exists &&
        tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]
    )
    val wrapperValid =
      result.wrapperApply.fun.eq(result.wrapperFunction) &&
        result.wrapperApply.args != null && result.wrapperApply.args.size == 2 &&
        result.wrapperApply.args(0).eq(result.originalArgument) &&
        result.wrapperApply.args(1).eq(result.freshSiblingApply)
    val siblingValid =
      result.wrapperApply.args(1).eq(result.freshSiblingApply) &&
      result.freshSiblingApply.fun.eq(result.freshSiblingFunction) &&
        result.freshSiblingApply.args != null &&
        result.freshSiblingApply.args.size >= 1 && result.freshSiblingApply.args.size <= 3 &&
        result.freshSiblingApply.args.size == result.freshSiblingArguments.size &&
        result.freshSiblingApply.args.indices.forall(index =>
          result.freshSiblingApply.args(index).eq(result.freshSiblingArguments(index))
        )
    val argumentsValid =
      result.rebuiltApply.args != null &&
        result.rebuiltApply.args.size == result.originalApply.args.size &&
        result.originalApply.args.indices.forall { index =>
          if index == result.argumentIndex then
            result.rebuiltApply.args(index).eq(result.wrapperApply)
          else result.rebuiltApply.args(index).eq(result.originalApply.args(index))
        }
    val bodyValid =
      result.rebuiltTemplate.body != null &&
        result.rebuiltTemplate.body.size == result.originalTemplate.body.size &&
        result.rebuiltTemplate.body.indices.forall { index =>
          val original = result.originalTemplate.body(index)
          if original.eq(result.originalTarget) then result.rebuiltTemplate.body(index).eq(result.rebuiltTarget)
          else result.rebuiltTemplate.body(index).eq(original)
        }
    val graphValid =
      !result.rebuiltRoot.eq(result.originalRoot) &&
        !result.rebuiltTemplate.eq(result.originalTemplate) &&
        !result.rebuiltTarget.eq(result.originalTarget) &&
        !result.rebuiltApply.eq(result.originalApply) &&
        result.rebuiltRoot.name == result.originalRoot.name &&
        result.rebuiltRoot.mods.eq(result.originalRoot.mods) &&
        result.rebuiltRoot.rhs.eq(result.rebuiltTemplate) &&
        result.rebuiltTemplate.constr.eq(result.originalTemplate.constr) &&
        result.rebuiltTemplate.parentsOrDerived.eq(result.originalTemplate.parentsOrDerived) &&
        result.rebuiltTemplate.derived.eq(result.originalTemplate.derived) &&
        result.rebuiltTemplate.self.eq(result.originalTemplate.self) &&
        result.rebuiltTarget.name == result.originalTarget.name &&
        result.rebuiltTarget.mods.eq(result.originalTarget.mods) &&
        result.rebuiltTarget.paramss.eq(result.originalTarget.paramss) &&
        result.rebuiltTarget.tpt.eq(result.originalTarget.tpt) &&
        result.rebuiltTarget.rhs.eq(result.rebuiltApply) &&
        result.rebuiltApply.fun.eq(result.originalApply.fun) &&
        wrapperValid && siblingValid && argumentsValid && bodyValid
    Either.cond(
      provenanceValid && graphValid,
      (),
      error("RECONSTRUCTION_INVARIANT_FAILED",
        "the U020 mixed-provenance wrapper result violated its bounded identity/provenance contract.")
    )

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    val builder = Vector.newBuilder[untpd.Tree]
    val traverser = new untpd.UntypedTreeTraverser:
      override def traverse(current: untpd.Tree)(using Context): Unit =
        builder += current
        traverseChildren(current)
    traverser.traverse(tree)
    builder.result()

  private def isAdmittedLeaf(tree: untpd.Tree): Boolean =
    tree.isInstanceOf[untpd.Ident] ||
      tree.isInstanceOf[untpd.Number] ||
      tree.isInstanceOf[untpd.Literal]

  private def nodeKind(tree: untpd.Tree): String =
    Option(tree).map(_.getClass.getSimpleName).getOrElse("null")

  private def error(
      code: String,
      detail: String
  ): ExistingUntpdSelectedApplyArgumentWrapApplySiblingRewriteError =
    ExistingUntpdSelectedApplyArgumentWrapApplySiblingRewriteError(code, detail)
