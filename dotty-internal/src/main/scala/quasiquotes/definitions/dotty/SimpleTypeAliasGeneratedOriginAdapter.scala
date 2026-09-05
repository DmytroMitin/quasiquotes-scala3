package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.Spans.Span

import quasiquotes.definitions.DefinitionShape
import quasiquotes.terms.dotty.{
  ConstructedTermGeneratedOriginError,
  GeneratedOriginFragmentSupport
}

/** Generated origin for exactly the generic U022 SimpleTypeAlias family. */
private[quasiquotes] object SimpleTypeAliasGeneratedOriginAdapter:
  import DefinitionShapeUntypedLowererError.{
    RawInvariantFailure,
    SimpleTypeAliasCompletedTypeFailure as U022CompletedTypeFailure,
    SimpleTypeAliasCompletionFailure as U022CompletionFailure,
    SimpleTypeAliasNameFailure as U022NameFailure
  }
  import SimpleTypeAliasGeneratedOriginError.*

  private final case class AliasPlan(
      generatedSource: String,
      nameStart: Int,
      nameEnd: Int,
      rhsStart: Int,
      rootPoint: Int,
      rhsFragment: GeneratedOriginFragmentSupport.TypeFragment
  ):
    def rootSpan: Span = Span(0, generatedSource.length, rootPoint)

  def lower(
      shape: DefinitionShape,
      virtualSourceName: String
  )(using Context): Either[
    SimpleTypeAliasGeneratedOriginError,
    GeneratedOriginDefinitionResult
  ] =
    for
      present <- Option(shape).toRight(MissingDefinitionShape)
      alias <- present match
        case value: DefinitionShape.SimpleTypeAlias => Right(value)
        case other => Left(WrongDefinitionShapeFamily(other.getClass.getSimpleName))
      authority <- DefinitionShapeUntypedLowerer
        .lowerSimpleTypeAlias(alias)
        .left
        .map(classifySemanticFailure)
      result <- lowerAuthority(authority, authority.tree, virtualSourceName)
    yield result

  private def lowerAuthority(
      authority: DefinitionShapeUntypedLowerer.LoweredSimpleTypeAlias,
      raw: untpd.Tree,
      virtualSourceName: String
  )(using Context): Either[
    SimpleTypeAliasGeneratedOriginError,
    GeneratedOriginDefinitionResult
  ] =
    for
      _ <- DefinitionShapeUntypedLowerer
        .validateRawInvariant(raw, "simple type alias generated-origin input")
        .left
        .map(problem => SourceFreeInvariantFailure(problem.message))
      virtualName <- Option(virtualSourceName).toRight(
        InvalidVirtualSourceName("the name was null")
      )
      _ <- GeneratedOriginFragmentSupport
        .validateVirtualSourceName(virtualName)
        .left
        .map(problem => InvalidVirtualSourceName(problem.message))
      rhsFragment <- GeneratedOriginFragmentSupport
        .planType(authority.normalForm)
        .left
        .map(GeneratedTypePlanningFailure.apply)
      plan = assemble(authority.name.source, rhsFragment)
      _ <- validatePlan(plan, authority.name.source)
      rawAlias <- Option(raw).toRight(
        RawTopologyMismatch("the source-free raw tree was null.")
      ).flatMap {
        case value: untpd.TypeDef => Right(value)
        case other =>
          Left(
            RawTopologyMismatch(
              s"expected TypeDef but found ${other.getClass.getSimpleName}."
            )
          )
      }
      source = SourceFile.virtual(virtualName, plan.generatedSource)
      positionedRhs <- GeneratedOriginFragmentSupport
        .positionType(rawAlias.rhs, plan.rhsFragment, source, plan.rhsStart)
        .left
        .map(classifyPositioningFailure)
      positioned = untpd
        .TypeDef(rawAlias.name, positionedRhs)
        .withMods(rawAlias.mods)
        .cloneIn(source)
        .withSpan(plan.rootSpan)
      _ <- validatePositioned(authority, positioned, plan, source)
    yield new GeneratedOriginDefinitionResult(
      positioned,
      plan.generatedSource,
      source
    )

  private[dotty] def positionRawForTest(
      raw: untpd.Tree,
      alias: DefinitionShape.SimpleTypeAlias,
      virtualSourceName: String
  )(using Context): Either[
    SimpleTypeAliasGeneratedOriginError,
    GeneratedOriginDefinitionResult
  ] =
    DefinitionShapeUntypedLowerer
      .lowerSimpleTypeAlias(alias)
      .left
      .map(classifySemanticFailure)
      .flatMap(lowerAuthority(_, raw, virtualSourceName))

  private[dotty] def validateGeneratedSourceForTest(
      alias: DefinitionShape.SimpleTypeAlias,
      generatedSource: String
  )(using Context): Either[SimpleTypeAliasGeneratedOriginError, Unit] =
    for
      authority <- DefinitionShapeUntypedLowerer
        .lowerSimpleTypeAlias(alias)
        .left
        .map(classifySemanticFailure)
      rhsFragment <- GeneratedOriginFragmentSupport
        .planType(authority.normalForm)
        .left
        .map(GeneratedTypePlanningFailure.apply)
      plan = assemble(authority.name.source, rhsFragment).copy(
        generatedSource = generatedSource
      )
      _ <- validatePlan(plan, authority.name.source)
    yield ()

  private[dotty] def validatePositionedForTest(
      alias: DefinitionShape.SimpleTypeAlias,
      tree: untpd.Tree,
      result: GeneratedOriginDefinitionResult
  )(using Context): Either[SimpleTypeAliasGeneratedOriginError, Unit] =
    for
      authority <- DefinitionShapeUntypedLowerer
        .lowerSimpleTypeAlias(alias)
        .left
        .map(classifySemanticFailure)
      rhsFragment <- GeneratedOriginFragmentSupport
        .planType(authority.normalForm)
        .left
        .map(GeneratedTypePlanningFailure.apply)
      plan = assemble(authority.name.source, rhsFragment)
      _ <- Either.cond(
        result.generatedSource == plan.generatedSource,
        (),
        GeneratedSourcePlanMismatch(
          "the supplied positioned result did not carry the deterministic source."
        )
      )
      positioned <- tree match
        case value: untpd.TypeDef => Right(value)
        case null => Left(PositionedInvariantFailure("the positioned tree was null."))
        case other =>
          Left(
            PositionedInvariantFailure(
              s"expected positioned TypeDef but found ${other.getClass.getSimpleName}."
            )
          )
      _ <- validatePositioned(authority, positioned, plan, result.sourceFile)
    yield ()

  private def assemble(
      nameSource: String,
      rhsFragment: GeneratedOriginFragmentSupport.TypeFragment
  ): AliasPlan =
    val nameStart = "type ".length
    val nameEnd = nameStart + nameSource.length
    val rhsStart = nameEnd + " = ".length
    val generatedSource = s"type $nameSource = ${rhsFragment.source}"
    val rootPoint =
      nameStart + Option.when(nameSource.startsWith("`"))(1).getOrElse(0)
    AliasPlan(
      generatedSource,
      nameStart,
      nameEnd,
      rhsStart,
      rootPoint,
      rhsFragment
    )

  private def validatePlan(
      plan: AliasPlan,
      nameSource: String
  ): Either[SimpleTypeAliasGeneratedOriginError, Unit] =
    val rhsEnd = plan.rhsStart + plan.rhsFragment.source.length
    val valid =
      plan.nameStart == "type ".length &&
        plan.nameStart <= plan.nameEnd &&
        plan.nameEnd <= plan.rhsStart &&
        rhsEnd == plan.generatedSource.length &&
        plan.generatedSource.startsWith("type ") &&
        plan.generatedSource.slice(plan.nameStart, plan.nameEnd) == nameSource &&
        plan.generatedSource.slice(plan.nameEnd, plan.rhsStart) == " = " &&
        plan.generatedSource.slice(plan.rhsStart, rhsEnd) == plan.rhsFragment.source &&
        plan.rootPoint >= plan.nameStart && plan.rootPoint <= plan.nameEnd
    Either.cond(
      valid,
      (),
      GeneratedSourcePlanMismatch(
        "the deterministic type/name/RHS slices did not cover the generated source."
      )
    )

  private def validatePositioned(
      authority: DefinitionShapeUntypedLowerer.LoweredSimpleTypeAlias,
      positioned: untpd.TypeDef,
      plan: AliasPlan,
      source: SourceFile
  )(using Context): Either[SimpleTypeAliasGeneratedOriginError, Unit] =
    for
      _ <- validateTopology(authority.tree, positioned)
      _ <- GeneratedOriginFragmentSupport
        .validatePositionedTypeAgainstPlan(
        positioned.rhs,
        plan.rhsFragment,
        plan.rhsStart
      )
        .left
        .map(problem => PositionedInvariantFailure(problem.message))
      _ <- GeneratedOriginFragmentSupport
        .validatePositionedTree(
          positioned.rhs,
          source,
          plan.rhsStart,
          plan.generatedSource.length
        )
        .left
        .map(problem => PositionedInvariantFailure(problem.message))
      _ <- validateCompleteGraph(positioned, plan, source)
    yield ()

  private def validateCompleteGraph(
      positioned: untpd.TypeDef,
      plan: AliasPlan,
      source: SourceFile
  )(using Context): Either[SimpleTypeAliasGeneratedOriginError, Unit] =
    val trees = positioned +: GeneratedOriginFragmentSupport.allTrees(positioned.rhs)
    val invalid = trees.collectFirst {
      case tree
          if !tree.source.exists || tree.source.path != source.path ||
            tree.source.content.mkString != plan.generatedSource =>
        s"${tree.getClass.getSimpleName} did not retain the one generated SourceFile."
      case tree
          if !tree.span.exists || tree.span.start < 0 ||
            tree.span.start > tree.span.point || tree.span.point > tree.span.end ||
            tree.span.end > plan.generatedSource.length =>
        s"${tree.getClass.getSimpleName} had an invalid generated span."
      case tree if tree.symbol != NoSymbol =>
        s"${tree.getClass.getSimpleName} retained a symbol before Typer."
      case _: untpd.TypedSplice =>
        "the positioned graph contained a TypedSplice."
    }
    if positioned.span != plan.rootSpan then
      Left(PositionedInvariantFailure("the TypeDef did not cover the full generated source."))
    else invalid.toLeft(()).left.map(PositionedInvariantFailure.apply)

  private def validateTopology(
      sourceFree: untpd.Tree,
      positioned: untpd.Tree
  )(using Context): Either[SimpleTypeAliasGeneratedOriginError, Unit] =
    val mismatch = (sourceFree, positioned) match
      case (left: untpd.TypeDef, right: untpd.TypeDef) =>
        Option.when(
          left.name != right.name || left.mods.hasFlags != right.mods.hasFlags ||
            left.mods.hasAnnotations != right.mods.hasAnnotations ||
            left.mods.hasPrivateWithin != right.mods.hasPrivateWithin
        )("TypeDef name or modifiers changed")
          .orElse(validateTopology(left.rhs, right.rhs).left.toOption.map(_.message))
      case (left: untpd.Ident, right: untpd.Ident) =>
        Option.when(left.name != right.name)("type identifier name changed")
      case (left: untpd.AppliedTypeTree, right: untpd.AppliedTypeTree) =>
        compareChildren(left.tpt +: left.args, right.tpt +: right.args)
      case (left: untpd.Tuple, right: untpd.Tuple) =>
        compareChildren(left.trees, right.trees)
      case (left: untpd.Function, right: untpd.Function) =>
        compareChildren(left.args :+ left.body, right.args :+ right.body)
      case _ =>
        Some(
          s"${sourceFree.getClass.getSimpleName} became ${positioned.getClass.getSimpleName}"
        )
    mismatch.toLeft(()).left.map(RawTopologyMismatch.apply)

  private def compareChildren(
      sourceFree: List[untpd.Tree],
      positioned: List[untpd.Tree]
  )(using Context): Option[String] =
    if sourceFree.size != positioned.size then
      Some(s"child count ${sourceFree.size} became ${positioned.size}")
    else
      sourceFree
        .zip(positioned)
        .collectFirst(Function.unlift { case (left, right) =>
          validateTopology(left, right).left.toOption.map(_.message)
        })

  private def classifySemanticFailure(
      problem: DefinitionShapeUntypedLowererError
  ): SimpleTypeAliasGeneratedOriginError =
    problem match
      case U022CompletionFailure(cause) => AliasCompletionFailure(cause)
      case U022NameFailure(detail) => AliasNameFailure(detail)
      case U022CompletedTypeFailure(cause) =>
        CompletedTypeExactLoweringFailure(cause)
      case RawInvariantFailure(_, detail) => SourceFreeInvariantFailure(detail)
      case other => SourceFreeInvariantFailure(other.message)

  private def classifyPositioningFailure(
      problem: ConstructedTermGeneratedOriginError
  ): SimpleTypeAliasGeneratedOriginError =
    problem match
      case ConstructedTermGeneratedOriginError.RawTreePlanMismatch(detail) =>
        RawTopologyMismatch(detail)
      case other => GeneratedOriginPositioningFailure(other.message)
