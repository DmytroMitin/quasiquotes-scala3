package quasiquotes.definitions.dotty

import scala.util.control.NonFatal

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.{NoSource, SourceFile}

/** Wraps one exact U014 argument with one preserved child and one fresh sibling leaf. */
private[quasiquotes] object ExistingUntpdSelectedApplyArgumentWrapSiblingRewriter:
  final case class Result(
      validatedExisting: ExistingUntpdSelectedApplyArgumentRewriter.Result,
      wrapperApply: untpd.Apply,
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
    def freshSibling: untpd.Tree = wrapperApply.args(1)
    def prefix: List[untpd.Tree] = validatedExisting.prefix
    def suffix: List[untpd.Tree] = validatedExisting.suffix
    def preservedArguments: List[untpd.Tree] = validatedExisting.preservedArguments

  def rewrite(
      root: untpd.TypeDef,
      exactTarget: untpd.DefDef,
      exactArgument: untpd.Tree,
      wrapperFunction: untpd.Tree,
      freshSibling: untpd.Tree
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentWrapSiblingRewriteError, Result] =
    try
      for
        wrapper <- validateWrapperFunction(wrapperFunction)
        existing <- validateExisting(root, exactTarget, exactArgument)
        sibling <- validateFreshSibling(freshSibling, existing)
        result <- rebuild(existing, wrapper, sibling)
      yield result
    catch
      case NonFatal(exception) =>
        Left(error(
          "SELECTED_APPLY_ARGUMENT_WRAP_SIBLING_REWRITE_FAILED",
          Option(exception.getMessage).filter(_.nonEmpty)
            .getOrElse(exception.getClass.getSimpleName)
        ))

  private def validateExisting(
      root: untpd.TypeDef,
      exactTarget: untpd.DefDef,
      exactArgument: untpd.Tree
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentWrapSiblingRewriteError,
    ExistingUntpdSelectedApplyArgumentRewriter.Result] =
    given SourceFile = NoSource
    val validationLeaf = untpd.Number("0", untpd.NumberKind.Whole(10))
    ExistingUntpdSelectedApplyArgumentRewriter
      .rewrite(root, exactTarget, exactArgument, validationLeaf)
      .left.map(problem => error(problem.code, problem.detail))

  private def validateWrapperFunction(
      wrapperFunction: untpd.Tree
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentWrapSiblingRewriteError,
    untpd.Ident] =
    Option(wrapperFunction).toRight(
      error("WRAPPER_FUNCTION_REQUIRED", "the fresh wrapper function was null.")
    ).flatMap { wrapper =>
      if wrapper.isInstanceOf[untpd.TypedSplice] then
        Left(error("WRAPPER_FUNCTION_TYPED_SPLICE_UNSUPPORTED",
          "the fresh wrapper function was TypedSplice."))
      else wrapper match
        case ident: untpd.Ident if ident.source.exists =>
          Left(error("WRAPPER_FUNCTION_SOURCE_PROVENANCE",
            "the fresh wrapper function must not carry a source."))
        case ident: untpd.Ident if ident.span.exists =>
          Left(error("WRAPPER_FUNCTION_SPAN_PROVENANCE",
            "the fresh wrapper function must not carry a span."))
        case ident: untpd.Ident if ident.symbol != NoSymbol =>
          Left(error("WRAPPER_FUNCTION_SYMBOL_PROVENANCE",
            "the fresh wrapper function must not carry a symbol."))
        case ident: untpd.Ident => Right(ident)
        case other =>
          Left(error("WRAPPER_FUNCTION_IDENT_REQUIRED",
            s"the fresh wrapper function was ${nodeKind(other)}, not direct Ident."))
    }

  private def validateFreshSibling(
      freshSibling: untpd.Tree,
      existing: ExistingUntpdSelectedApplyArgumentRewriter.Result
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentWrapSiblingRewriteError,
    untpd.Tree] =
    Option(freshSibling).toRight(
      error("FRESH_SIBLING_REQUIRED", "the fresh sibling leaf was null.")
    ).flatMap { sibling =>
      if sibling.eq(existing.originalArgument) then
        Left(error("FRESH_SIBLING_ALIASES_ORIGINAL",
          "the fresh sibling leaf aliases the preserved original argument."))
      else if existing.originalApply.args.exists(argument => argument.eq(sibling)) then
        Left(error("FRESH_SIBLING_ALIASES_EXISTING_ARGUMENT",
          "the fresh sibling leaf aliases an existing outer argument."))
      else if sibling.isInstanceOf[untpd.TypedSplice] then
        Left(error("FRESH_SIBLING_TYPED_SPLICE_UNSUPPORTED",
          "the fresh sibling leaf was TypedSplice."))
      else if !isAdmittedLeaf(sibling) then
        Left(error("FRESH_SIBLING_LEAF_REQUIRED",
          s"the fresh sibling was ${nodeKind(sibling)}, not Ident, Number, or Literal."))
      else if sibling.source.exists then
        Left(error("FRESH_SIBLING_SOURCE_PROVENANCE",
          "the fresh sibling leaf must not carry a source."))
      else if sibling.span.exists then
        Left(error("FRESH_SIBLING_SPAN_PROVENANCE",
          "the fresh sibling leaf must not carry a span."))
      else if sibling.symbol != NoSymbol then
        Left(error("FRESH_SIBLING_SYMBOL_PROVENANCE",
          "the fresh sibling leaf must not carry a symbol."))
      else Right(sibling)
    }

  private def rebuild(
      existing: ExistingUntpdSelectedApplyArgumentRewriter.Result,
      wrapperFunction: untpd.Ident,
      freshSibling: untpd.Tree
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentWrapSiblingRewriteError, Result] =
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
    val result = Result(
      existing,
      wrapperApply,
      rebuiltRoot,
      rebuiltTemplate,
      rebuiltTarget,
      rebuiltApply
    )
    verify(result).map(_ => result)

  private def verify(
      result: Result
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentWrapSiblingRewriteError, Unit] =
    val freshNodes = Vector[untpd.Tree](
      result.rebuiltRoot,
      result.rebuiltTemplate,
      result.rebuiltTarget,
      result.rebuiltApply,
      result.wrapperApply,
      result.wrapperFunction,
      result.freshSibling
    )
    val provenanceValid = freshNodes.forall(tree =>
      tree != null && !tree.source.exists && !tree.span.exists &&
        tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]
    )
    val wrapperValid =
      result.wrapperApply.fun.eq(result.wrapperFunction) &&
        result.wrapperApply.args != null && result.wrapperApply.args.size == 2 &&
        result.wrapperApply.args(0).eq(result.originalArgument) &&
        result.wrapperApply.args(1).eq(result.freshSibling) &&
        !result.freshSibling.eq(result.originalArgument) &&
        !result.originalApply.args.exists(_.eq(result.freshSibling))
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
          if original.eq(result.originalTarget) then
            result.rebuiltTemplate.body(index).eq(result.rebuiltTarget)
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
        wrapperValid && argumentsValid && bodyValid
    Either.cond(
      provenanceValid && graphValid,
      (),
      error("RECONSTRUCTION_INVARIANT_FAILED",
        "the U019 mixed-provenance wrapper result violated its bounded identity/provenance contract.")
    )

  private def isAdmittedLeaf(tree: untpd.Tree): Boolean =
    tree.isInstanceOf[untpd.Ident] ||
      tree.isInstanceOf[untpd.Number] ||
      tree.isInstanceOf[untpd.Literal]

  private def nodeKind(tree: untpd.Tree): String =
    Option(tree).map(_.getClass.getSimpleName).getOrElse("null")

  private def error(
      code: String,
      detail: String
  ): ExistingUntpdSelectedApplyArgumentWrapSiblingRewriteError =
    ExistingUntpdSelectedApplyArgumentWrapSiblingRewriteError(code, detail)
