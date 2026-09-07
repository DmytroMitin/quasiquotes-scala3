package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.SourceFile

import quasiquotes.definitions.{ConstructedDefinition, DefinitionShape, SemanticDefinition, SemanticDefinitionShapeAdapter}
import quasiquotes.terms.dotty.{ConstructedTermGeneratedOriginError, GeneratedOriginFragmentSupport}

/** Exact-version generated-origin lowering for public semantic Definitions. */
object DefinitionGeneratedOriginLowering:
  /** Stable public diagnostic boundary; callers branch on `code`. */
  final case class Failure(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  /** Fresh positioned member and its deterministic generated origin. */
  final class Lowered private[dotty] (
      val tree: untpd.MemberDef,
      val generatedSource: String,
      val sourceFile: SourceFile
  ):
    def virtualSourceName: String = sourceFile.path

  /** Lowers one admitted semantic Definition; placement and typing remain caller-owned. */
  def lower(
      definition: SemanticDefinition,
      virtualSourceName: String
  )(using Context): Either[Failure, Lowered] =
    for
      input <- Option(definition).toRight(Failure("MISSING_INPUT", "the semantic Definition must be present."))
      path <- Option(virtualSourceName).toRight(Failure("MISSING_INPUT", "the virtual source name must be present."))
      _ <- GeneratedOriginFragmentSupport.validateVirtualSourceName(path).left.map {
        case ConstructedTermGeneratedOriginError.InvalidVirtualSourceName(detail) =>
          Failure("INVALID_VIRTUAL_SOURCE", detail)
        case _ => invariant("the virtual-source validator returned an unexpected failure.")
      }
      shape <- contain(SemanticDefinitionShapeAdapter.adapt(input), classifyAdapterFailure)
      result <- finishLowering(shape, path, lowerShape(shape, path))
    yield result

  private def lowerShape(
      shape: DefinitionShape,
      path: String
  )(using Context): Either[Failure, GeneratedOriginDefinitionResult] =
    shape match
      case alias: DefinitionShape.SimpleTypeAlias =>
        contain(SimpleTypeAliasGeneratedOriginAdapter.lower(alias, path), classifyAliasFailure)
      case _: DefinitionShape.ImmutableVal | _: DefinitionShape.ParameterlessDef |
          _: DefinitionShape.SingleParameterDef | _: DefinitionShape.TwoParameterDef =>
        contain(
          ConstructedDefinition.fromShape(shape),
          problem => Option(problem)
            .map(p => Failure("EXACT_LOWERING_FAILED", p.message))
            .getOrElse(invariant("Definition completion returned a null failure."))
        ).flatMap { completed =>
          contain(ConstructedDefinitionGeneratedOriginAdapter.lower(completed, path), classifyConstructedFailure)
        }
      case null => Left(invariant("the semantic adapter returned an impossible Definition shape."))

  private def contain[E, A](
      result: Either[E, A],
      classify: E => Failure
  ): Either[Failure, A] =
    Option(result).toRight(invariant("a private authority returned no result container.")).flatMap {
      _.left.map(classify).flatMap(value =>
        Option(value).toRight(invariant("a private authority returned a null success value."))
      )
    }

  private def finishLowering(
      shape: DefinitionShape,
      path: String,
      lowered: Either[Failure, GeneratedOriginDefinitionResult]
  )(using Context): Either[Failure, Lowered] =
    for
      presentShape <- Option(shape).toRight(invariant("the adapted Definition shape was absent."))
      result <- contain(lowered, failure => Option(failure)
        .getOrElse(invariant("a private lowering stage returned a null failure.")))
      raw <- Option(result.tree).toRight(invariant("the generated member was absent."))
      source <- Option(result.sourceFile).toRight(invariant("the generated SourceFile was absent."))
      text <- Option(result.generatedSource).toRight(invariant("the generated source text was absent."))
      member <- raw match
        case value: untpd.MemberDef => Right(value)
        case _ => Left(invariant("the generated tree was not an untpd.MemberDef."))
      _ <- validateFamily(presentShape, member)
      _ <- validateOrigin(member, text, source, path)
    yield new Lowered(member, text, source)

  private def validateFamily(
      shape: DefinitionShape,
      member: untpd.MemberDef
  )(using Context): Either[Failure, Unit] =
    def present(tree: untpd.Tree): Boolean = tree != null && !tree.isEmpty
    def clean: Boolean = !member.mods.hasAnnotations && !member.mods.hasPrivateWithin
    def methodHeader(method: untpd.DefDef): Boolean =
      clean && method.mods.flags == Flags.Method && method.paramss != null &&
        present(method.tpt) && present(method.unforcedRhs.asInstanceOf[untpd.Tree])
    val sameName = shape.name != null && shape.name.decoded != null && member.name != null &&
      member.name.toString == shape.name.decoded
    val valid = sameName && ((shape, member) match
      case (_: DefinitionShape.ImmutableVal, value: untpd.ValDef) =>
        clean && value.mods.flags == Flags.EmptyFlags && present(value.tpt) &&
          present(value.unforcedRhs.asInstanceOf[untpd.Tree])
      case (_: DefinitionShape.ParameterlessDef, method: untpd.DefDef) =>
        methodHeader(method) && method.paramss.isEmpty
      case (expected: DefinitionShape.SingleParameterDef, method: untpd.DefDef) =>
        methodHeader(method) && method.paramss.size == 1 && method.paramss.head != null &&
          method.paramss.head.size == 1 && validParameter(method.paramss.head.head, expected.parameterName.decoded)
      case (expected: DefinitionShape.TwoParameterDef, method: untpd.DefDef) =>
        methodHeader(method) && method.paramss.size == 1 && method.paramss.head != null &&
          method.paramss.head.size == 2 &&
          validParameter(method.paramss.head.head, expected.firstParameterName.decoded) &&
          validParameter(method.paramss.head(1), expected.secondParameterName.decoded)
      case (_: DefinitionShape.SimpleTypeAlias, alias: untpd.TypeDef) =>
        clean && !alias.mods.hasFlags && alias.name.isTypeName && present(alias.rhs)
      case _ => false)
    Either.cond(valid, (), invariant("the generated member family, name, or modifiers contradicted the semantic Definition."))

  private def validParameter(tree: untpd.Tree, name: String)(using Context): Boolean =
    tree match
      case value: untpd.ValDef =>
        value.name != null && value.name.toString == name && value.mods.flags == Flags.Param &&
          !value.mods.hasAnnotations && !value.mods.hasPrivateWithin &&
          value.tpt != null && !value.tpt.isEmpty && value.rhs != null && value.rhs.isEmpty
      case _ => false

  private def validateOrigin(
      member: untpd.MemberDef,
      text: String,
      source: SourceFile,
      path: String
  )(using Context): Either[Failure, Unit] =
    if source.path != path || source.content.mkString != text then
      Left(origin("the generated source content or effective virtual path disagreed with the requested origin."))
    else if !member.span.exists || member.span.start != 0 || member.span.end != text.length then
      Left(origin("the generated member span did not cover its complete generated source."))
    else
      val trees = member match
        case alias: untpd.TypeDef => alias +: GeneratedOriginFragmentSupport.allTrees(alias.rhs)
        case ordinary => GeneratedOriginFragmentSupport.allTrees(ordinary)
      trees.collectFirst {
        case tree if !(tree.source eq source) =>
          "a generated tree did not belong to the returned SourceFile."
        case tree if !tree.span.exists || tree.span.start < 0 || tree.span.start > tree.span.point ||
            tree.span.point > tree.span.end || tree.span.end > text.length =>
          "a generated tree had a missing or out-of-bounds span."
        case _: untpd.TypedSplice => "the generated member contained a TypedSplice."
        case tree if tree.symbol != NoSymbol => "a generated tree carried a symbol before Typer."
        case tree: untpd.MemberDef if tree.mods.hasAnnotations || tree.mods.hasPrivateWithin =>
          "a generated member carried an unexpected annotation or private qualifier."
      }.toLeft(()).left.map(origin)

  private def classifyAdapterFailure(problem: SemanticDefinitionShapeAdapter.Error): Failure =
    Option(problem).map { present =>
      present.code match
        case "MISSING_INPUT" | "MALFORMED_SEMANTIC_VALUE" |
            "UNSUPPORTED_SEMANTIC_VALUE" | "SEMANTIC_ADAPTER_FAILED" =>
          Failure(present.code, present.detail)
        case _ => invariant("the semantic adapter returned an unreviewed failure code.")
    }.getOrElse(invariant("the semantic adapter returned a null failure."))

  private def classifyConstructedFailure(problem: ConstructedDefinitionGeneratedOriginError): Failure =
    import ConstructedDefinitionGeneratedOriginError.*
    problem match
      case InvalidVirtualSourceName(detail) => Failure("INVALID_VIRTUAL_SOURCE", detail)
      case RawDefinitionLoweringFailure(detail) => Failure("EXACT_LOWERING_FAILED", detail)
      case _: UnsupportedConstructedDefinitionVariant =>
        invariant("the generated-origin backend rejected an impossible constructed family.")
      case expected @ (_: DefinitionNameRenderingFailure | _: DefinitionTypePlanningFailure |
          _: DefinitionBodyPlanningFailure | _: RawDefinitionPlanMismatch |
          _: InvalidDefinitionStructuralPlan | _: IncompleteDefinitionPositionMap) =>
        origin(expected.message)
      case null => invariant("the constructed generated-origin backend returned a null failure.")

  private def classifyAliasFailure(problem: SimpleTypeAliasGeneratedOriginError): Failure =
    import SimpleTypeAliasGeneratedOriginError.*
    problem match
      case MissingDefinitionShape | _: WrongDefinitionShapeFamily =>
        invariant("the alias generated-origin backend received an impossible shape.")
      case AliasCompletionFailure(null) | CompletedTypeExactLoweringFailure(null) |
          GeneratedTypePlanningFailure(null) =>
        invariant("the alias generated-origin backend returned a failure without its required cause.")
      case expected @ (_: AliasCompletionFailure | _: AliasNameFailure |
          _: CompletedTypeExactLoweringFailure | _: SourceFreeInvariantFailure) =>
        Failure("EXACT_LOWERING_FAILED", expected.message)
      case InvalidVirtualSourceName(detail) => Failure("INVALID_VIRTUAL_SOURCE", detail)
      case expected @ (_: GeneratedTypePlanningFailure | _: GeneratedSourcePlanMismatch |
          _: RawTopologyMismatch | _: GeneratedOriginPositioningFailure | _: PositionedInvariantFailure) =>
        origin(expected.message)
      case null => invariant("the alias generated-origin backend returned a null failure.")

  private def origin(detail: String): Failure = Failure("GENERATED_ORIGIN_FAILED", detail)
  private def invariant(detail: String): Failure = Failure("INTERNAL_INVARIANT_FAILED", detail)
