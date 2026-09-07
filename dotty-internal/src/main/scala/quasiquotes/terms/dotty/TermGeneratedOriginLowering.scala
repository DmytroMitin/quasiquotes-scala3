package quasiquotes.terms.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.util.SourceFile
import quasiquotes.parser.TermShape

/** Exact-version generated-source and provenance lowering for public semantic Terms. */
object TermGeneratedOriginLowering:
  /** Stable public diagnostic boundary; callers branch on `code`. */
  final case class Failure(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  /** Fresh positioned Term and its deterministic generated origin. */
  final class Lowered private[dotty] (
      val tree: untpd.Tree,
      val generatedSource: String,
      val sourceFile: SourceFile
  ):
    def virtualSourceName: String = sourceFile.path

  /** Lower an admitted semantic Term; placement and typing remain caller-owned. */
  def lower(
      term: TermShape,
      virtualSourceName: String
  )(using Context): Either[Failure, Lowered] =
    for
      input <- Option(term).toRight(Failure("MISSING_INPUT", "the semantic TermShape must be present."))
      path <- Option(virtualSourceName).toRight(Failure("MISSING_INPUT", "the virtual source name must be present."))
      _ <- contain(GeneratedOriginFragmentSupport.validateVirtualSourceName(path), {
        case ConstructedTermGeneratedOriginError.InvalidVirtualSourceName(detail) => Failure("INVALID_VIRTUAL_SOURCE", detail)
        case _ => invariant("the virtual-source validator returned an unexpected failure.")
      })
      checked <- contain(CheckedTermUntypedLowering.lower(input), problem =>
        Option(problem).map(p => Failure(p.code, p.detail)).getOrElse(invariant("checked Term lowering returned a null failure.")))
      completed <- Option(checked.completed).toRight(invariant("the checked completed Term was absent."))
      raw <- Option(checked.raw).toRight(invariant("the checked raw Term was absent."))
      _ <- contain(TermGeneratedOriginAdmission.validate(completed.root), identity)
      fragment <- contain(GeneratedOriginFragmentSupport.planTerm(completed), classifyOriginFailure)
      text <- Option(fragment.source).toRight(invariant("the structural plan returned absent source text."))
      source = SourceFile.virtual(path, text)
      positioned <- contain(GeneratedOriginFragmentSupport.positionTerm(raw, fragment, source, baseOffset = 0), classifyOriginFailure)
      _ <- contain(GeneratedOriginFragmentSupport.validatePositionedTree(positioned, source, 0, text.length), classifyOriginFailure)
      _ <- contain(GeneratedOriginFragmentSupport.validatePositionedTermAgainstCheckedRaw(raw, positioned, fragment), classifyOriginFailure)
      _ <- validateIdentity(positioned, text, source, path)
    yield new Lowered(positioned, text, source)

  private def contain[E, A](result: Either[E, A], classify: E => Failure): Either[Failure, A] =
    Option(result).toRight(invariant("a private authority returned no result container.")).flatMap {
      case Left(null) => Left(invariant("a private authority returned a null failure."))
      case Left(problem) => Left(Option(classify(problem)).getOrElse(invariant("a private classifier returned a null failure.")))
      case Right(value) => Option(value).toRight(invariant("a private authority returned a null success value."))
    }

  private def validateIdentity(tree: untpd.Tree, text: String, source: SourceFile, path: String)(using Context): Either[Failure, Unit] =
    if source.path != path || source.content.mkString != text then
      Left(origin("the generated source content or effective path disagreed with the requested origin."))
    else if GeneratedOriginFragmentSupport.allTrees(tree).exists(node => !(node.source eq source)) then
      Left(origin("a generated material tree did not belong to the returned SourceFile."))
    else Right(())

  private def classifyOriginFailure(problem: ConstructedTermGeneratedOriginError): Failure =
    import ConstructedTermGeneratedOriginError.*
    problem match
      case expected @ (_: UnrenderableName | _: InvalidConstructorName | _: UnsupportedTermNode |
          _: UnsupportedUnaryOperator | _: UnsupportedInterpolationPrefix | _: UnsupportedTypeSidecar |
          NestedLambda1Unsupported) => Failure("UNSUPPORTED_SEMANTIC_VALUE", expected.message)
      case expected @ (_: MalformedConstructorArguments | _: NullConstructorArgument | _: MalformedBlock |
          _: MalformedLocalDef | _: MalformedInterpolation | _: NullInterpolationPart |
          _: NullInterpolationArgument | _: OutOfScopeBoundReference | _: MalformedBinderScope) =>
        Failure("MALFORMED_SEMANTIC_VALUE", expected.message)
      case expected: UnsupportedLiteral => Failure("EXACT_LOWERING_FAILED", expected.message)
      case expected @ (_: InvalidStructuralPlan | _: RawTreePlanMismatch | _: IncompletePositionMap) => origin(expected.message)
      case expected @ (MissingConstructedTerm | MissingTermShape | _: MissingTypeSidecar |
          _: UnconsumedTypeSidecars | _: RawLoweringFailure | _: InvalidVirtualSourceName) => invariant(expected.message)
      case null => invariant("the generated-origin authority returned a null failure.")

  private def origin(detail: String): Failure = Failure("GENERATED_ORIGIN_FAILED", detail)
  private def invariant(detail: String): Failure = Failure("INTERNAL_INVARIANT_FAILED", detail)
