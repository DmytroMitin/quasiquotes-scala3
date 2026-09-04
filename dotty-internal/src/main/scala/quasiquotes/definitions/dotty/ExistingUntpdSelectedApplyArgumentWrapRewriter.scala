package quasiquotes.definitions.dotty

import scala.util.control.NonFatal

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.{NoSource, SourceFile}

/** Wraps one exact U014 argument in one fresh unary direct-Ident Apply. */
private[quasiquotes] object ExistingUntpdSelectedApplyArgumentWrapRewriter:
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
    def prefix: List[untpd.Tree] = validatedExisting.prefix
    def suffix: List[untpd.Tree] = validatedExisting.suffix
    def preservedArguments: List[untpd.Tree] = validatedExisting.preservedArguments

  def rewrite(
      root: untpd.TypeDef,
      exactTarget: untpd.DefDef,
      exactArgument: untpd.Tree,
      wrapperFunction: untpd.Tree
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentWrapRewriteError, Result] =
    try
      for
        wrapper <- validateWrapperFunction(wrapperFunction)
        existing <- validateExisting(root, exactTarget, exactArgument)
        result <- rebuild(existing, wrapper)
      yield result
    catch
      case NonFatal(exception) =>
        Left(
          error(
            "SELECTED_APPLY_ARGUMENT_WRAP_REWRITE_FAILED",
            Option(exception.getMessage).filter(_.nonEmpty)
              .getOrElse(exception.getClass.getSimpleName)
          )
        )

  private def validateExisting(
      root: untpd.TypeDef,
      exactTarget: untpd.DefDef,
      exactArgument: untpd.Tree
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentWrapRewriteError,
    ExistingUntpdSelectedApplyArgumentRewriter.Result] =
    given SourceFile = NoSource
    val validationLeaf = untpd.Number("0", untpd.NumberKind.Whole(10))
    ExistingUntpdSelectedApplyArgumentRewriter
      .rewrite(root, exactTarget, exactArgument, validationLeaf)
      .left.map(problem => error(problem.code, problem.detail))

  private def validateWrapperFunction(
      wrapperFunction: untpd.Tree
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentWrapRewriteError, untpd.Ident] =
    Option(wrapperFunction).toRight(
      error("WRAPPER_FUNCTION_REQUIRED", "the fresh wrapper function was null.")
    ).flatMap { wrapper =>
      if wrapper.isInstanceOf[untpd.TypedSplice] then
        Left(
          error(
            "WRAPPER_FUNCTION_TYPED_SPLICE_UNSUPPORTED",
            "the fresh wrapper function was TypedSplice."
          )
        )
      else wrapper match
        case ident: untpd.Ident if ident.source.exists =>
          Left(
            error(
              "WRAPPER_FUNCTION_SOURCE_PROVENANCE",
              "the fresh wrapper function must not carry a source."
            )
          )
        case ident: untpd.Ident if ident.span.exists =>
          Left(
            error(
              "WRAPPER_FUNCTION_SPAN_PROVENANCE",
              "the fresh wrapper function must not carry a span."
            )
          )
        case ident: untpd.Ident if ident.symbol != NoSymbol =>
          Left(
            error(
              "WRAPPER_FUNCTION_SYMBOL_PROVENANCE",
              "the fresh wrapper function must not carry a symbol."
            )
          )
        case ident: untpd.Ident => Right(ident)
        case other =>
          Left(
            error(
              "WRAPPER_FUNCTION_IDENT_REQUIRED",
              s"the fresh wrapper function was ${nodeKind(other)}, not direct Ident."
            )
          )
    }

  private def rebuild(
      existing: ExistingUntpdSelectedApplyArgumentRewriter.Result,
      wrapperFunction: untpd.Ident
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentWrapRewriteError, Result] =
    given SourceFile = NoSource
    val wrapperApply = untpd.Apply(wrapperFunction, existing.originalArgument :: Nil)
    val arguments = existing.originalApply.args.zipWithIndex.map {
      case (_, index) if index == existing.argumentIndex => wrapperApply
      case (argument, _) => argument
    }
    val rebuiltApply = untpd.Apply(existing.originalApply.fun, arguments)
    val rebuiltTarget = untpd
      .DefDef(
        existing.originalTarget.name,
        existing.originalTarget.paramss,
        existing.originalTarget.tpt,
        rebuiltApply
      )
      .withMods(existing.originalTarget.mods)
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
  )(using Context): Either[ExistingUntpdSelectedApplyArgumentWrapRewriteError, Unit] =
    val freshNodes = Vector[untpd.Tree](
      result.rebuiltRoot,
      result.rebuiltTemplate,
      result.rebuiltTarget,
      result.rebuiltApply,
      result.wrapperApply,
      result.wrapperFunction
    )
    val provenanceValid = freshNodes.forall(tree =>
      tree != null && !tree.source.exists && !tree.span.exists &&
        tree.symbol == NoSymbol && !tree.isInstanceOf[untpd.TypedSplice]
    )
    val wrapperValid =
      result.wrapperApply.fun.eq(result.wrapperFunction) &&
        result.wrapperApply.args != null && result.wrapperApply.args.size == 1 &&
        result.wrapperApply.args.head != null &&
        result.wrapperApply.args.head.eq(result.originalArgument)
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
    val identityValid =
      !result.rebuiltRoot.eq(result.originalRoot) &&
        !result.rebuiltTemplate.eq(result.originalTemplate) &&
        !result.rebuiltTarget.eq(result.originalTarget) &&
        !result.rebuiltApply.eq(result.originalApply) &&
        !result.wrapperApply.eq(result.originalArgument) &&
        !result.wrapperFunction.eq(result.originalArgument) &&
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
        argumentsValid && bodyValid
    Either.cond(
      provenanceValid && wrapperValid && identityValid,
      (),
      error(
        "RECONSTRUCTION_INVARIANT_FAILED",
        "the U018 mixed-provenance wrapper result violated its bounded identity/provenance contract."
      )
    )

  private def nodeKind(tree: untpd.Tree): String =
    Option(tree).map(_.getClass.getSimpleName).getOrElse("null")

  private def error(
      code: String,
      detail: String
  ): ExistingUntpdSelectedApplyArgumentWrapRewriteError =
    ExistingUntpdSelectedApplyArgumentWrapRewriteError(code, detail)
