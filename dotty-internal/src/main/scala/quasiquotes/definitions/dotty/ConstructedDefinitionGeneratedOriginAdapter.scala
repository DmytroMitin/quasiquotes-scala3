package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.Spans.Span

import quasiquotes.definitions.{ConstructedDefinition, DefinitionName}
import quasiquotes.terms.ConstructedTerm
import quasiquotes.terms.dotty.GeneratedOriginFragmentSupport
import quasiquotes.types.TypeNormalForm

private[quasiquotes] object ConstructedDefinitionGeneratedOriginAdapter:
  import ConstructedDefinitionGeneratedOriginError.*

  private enum DefinitionKind:
    case ParameterlessDef
    case ImmutableVal

  private final case class DefinitionParts(
      kind: DefinitionKind,
      name: DefinitionName,
      definitionType: TypeNormalForm,
      body: ConstructedTerm
  )

  private final case class DefinitionPlan(
      parts: DefinitionParts,
      generatedSource: String,
      nameStart: Int,
      nameEnd: Int,
      typeStart: Int,
      bodyStart: Int,
      rootPoint: Int,
      typeFragment: GeneratedOriginFragmentSupport.TypeFragment,
      bodyFragment: GeneratedOriginFragmentSupport.TermFragment
  )

  def lower(
      constructed: ConstructedDefinition,
      virtualSourceName: String
  )(using Context): Either[
    ConstructedDefinitionGeneratedOriginError,
    GeneratedOriginDefinitionResult
  ] =
    for
      _ <- GeneratedOriginFragmentSupport
        .validateVirtualSourceName(virtualSourceName)
        .left
        .map {
          case quasiquotes.terms.dotty.ConstructedTermGeneratedOriginError
                .InvalidVirtualSourceName(detail) =>
            InvalidVirtualSourceName(detail)
          case other =>
            InvalidVirtualSourceName(other.message)
        }
      parts <- extractParts(constructed)
      _ <- validateName(parts.name)
      typeFragment <- GeneratedOriginFragmentSupport
        .planType(parts.definitionType)
        .left
        .map(error => DefinitionTypePlanningFailure(error.message))
      bodyFragment <- GeneratedOriginFragmentSupport
        .planDefinitionBody(parts.body)
        .left
        .map(error => DefinitionBodyPlanningFailure(error.message))
      plan = assemble(parts, typeFragment, bodyFragment)
      _ <- validatePlan(plan)
      raw <- ConstructedDefinitionUntypedBackend
        .lower(constructed)
        .left
        .map(error => RawDefinitionLoweringFailure(error.message))
      source = SourceFile.virtual(virtualSourceName, plan.generatedSource)
      positioned <- position(raw, plan, source)
      _ <- validatePositioned(positioned, plan, source)
    yield
      new GeneratedOriginDefinitionResult(
        positioned,
        plan.generatedSource,
        source
      )

  private[dotty] def validatePositionedForTest(
      tree: untpd.Tree,
      generatedSource: String,
      expectedSource: SourceFile,
      expectedNameSource: String,
      expectedNameDecoded: String
  )(using Context): Either[
    ConstructedDefinitionGeneratedOriginError,
    Unit
  ] =
    val kindAndChildren =
      tree match
        case method: untpd.DefDef =>
          Right(
            (
              DefinitionKind.ParameterlessDef,
              method.name.toString,
              method.tpt,
              method.rhs
            )
          )
        case value: untpd.ValDef =>
          Right(
            (
              DefinitionKind.ImmutableVal,
              value.name.toString,
              value.tpt,
              value.rhs
            )
          )
        case other =>
          Left(
            RawDefinitionPlanMismatch(
              s"expected DefDef or ValDef but found ${other.getClass.getSimpleName}"
            )
          )
    kindAndChildren.flatMap { case (_, actualName, definitionType, body) =>
      validateCompleteMap(
        tree,
        definitionType,
        body,
        generatedSource,
        expectedSource,
        expectedNameSource,
        expectedNameDecoded,
        actualName
      )
    }

  private[dotty] def positionRawForTest(
      raw: untpd.Tree,
      constructed: ConstructedDefinition,
      virtualSourceName: String
  )(using Context): Either[
    ConstructedDefinitionGeneratedOriginError,
    untpd.Tree
  ] =
    for
      parts <- extractParts(constructed)
      typeFragment <- GeneratedOriginFragmentSupport
        .planType(parts.definitionType)
        .left
        .map(error => DefinitionTypePlanningFailure(error.message))
      bodyFragment <- GeneratedOriginFragmentSupport
        .planDefinitionBody(parts.body)
        .left
        .map(error => DefinitionBodyPlanningFailure(error.message))
      plan = assemble(parts, typeFragment, bodyFragment)
      source = SourceFile.virtual(virtualSourceName, plan.generatedSource)
      positioned <- position(raw, plan, source)
    yield positioned

  private def extractParts(
      constructed: ConstructedDefinition
  ): Either[ConstructedDefinitionGeneratedOriginError, DefinitionParts] =
    Option(constructed)
      .toRight(
        RawDefinitionLoweringFailure(
          "the completed definition was null."
        )
      )
      .flatMap {
        case method: ConstructedDefinition.ParameterlessDef =>
          Right(
            DefinitionParts(
              DefinitionKind.ParameterlessDef,
              method.name,
              method.resultType,
              method.body
            )
          )
        case value: ConstructedDefinition.ImmutableVal =>
          Right(
            DefinitionParts(
              DefinitionKind.ImmutableVal,
              value.name,
              value.declaredType,
              value.rhs
            )
          )
        case unsupported =>
          Left(
            UnsupportedConstructedDefinitionVariant(
              unsupported.getClass.getSimpleName
            )
          )
      }

  private def validateName(
      name: DefinitionName
  ): Either[ConstructedDefinitionGeneratedOriginError, Unit] =
    Option(name)
      .flatMap(value =>
        DefinitionName
          .fromSource(value.source)
          .toOption
          .filter(_.decoded == value.decoded)
      )
      .toRight(
        DefinitionNameRenderingFailure(
          "the validated source/decoded spelling invariant was not satisfied."
        )
      )
      .map(_ => ())

  private def assemble(
      parts: DefinitionParts,
      typeFragment: GeneratedOriginFragmentSupport.TypeFragment,
      bodyFragment: GeneratedOriginFragmentSupport.TermFragment
  ): DefinitionPlan =
    val keyword =
      parts.kind match
        case DefinitionKind.ParameterlessDef => "def"
        case DefinitionKind.ImmutableVal => "val"
    val nameStart = keyword.length + 1
    val nameEnd = nameStart + parts.name.source.length
    val typeStart = nameEnd + 2
    val bodyStart = typeStart + typeFragment.source.length + 3
    val generatedSource =
      s"$keyword ${parts.name.source}: ${typeFragment.source} = ${bodyFragment.source}"
    val rootPoint =
      nameStart + Option.when(parts.name.source.startsWith("`"))(1).getOrElse(0)
    DefinitionPlan(
      parts,
      generatedSource,
      nameStart,
      nameEnd,
      typeStart,
      bodyStart,
      rootPoint,
      typeFragment,
      bodyFragment
    )

  private def validatePlan(
      plan: DefinitionPlan
  ): Either[ConstructedDefinitionGeneratedOriginError, Unit] =
    val source = plan.generatedSource
    val typeEnd = plan.typeStart + plan.typeFragment.source.length
    val bodyEnd = plan.bodyStart + plan.bodyFragment.source.length
    val keyword =
      plan.parts.kind match
        case DefinitionKind.ParameterlessDef => "def"
        case DefinitionKind.ImmutableVal => "val"
    val errors = Vector.newBuilder[String]
    if source.substring(0, keyword.length) != keyword then
      errors += "definition keyword slice does not match the variant"
    if source.slice(plan.nameStart, plan.nameEnd) != plan.parts.name.source then
      errors += "definition name slice does not match DefinitionName.source"
    if source.slice(plan.nameEnd, plan.typeStart) != ": " then
      errors += "name/type punctuation is not `: `"
    if source.slice(plan.typeStart, typeEnd) != plan.typeFragment.source then
      errors += "definition type slice does not match the shared fragment"
    if source.slice(typeEnd, plan.bodyStart) != " = " then
      errors += "type/body punctuation is not ` = `"
    if source.slice(plan.bodyStart, bodyEnd) != plan.bodyFragment.source then
      errors += "definition body slice does not match the shared fragment"
    if bodyEnd != source.length then
      errors += s"definition body end $bodyEnd does not cover source length ${source.length}"
    if plan.rootPoint < plan.nameStart || plan.rootPoint >= plan.nameEnd then
      errors += "definition root point is not inside the exact name slice"
    val result = errors.result()
    Either.cond(
      result.isEmpty,
      (),
      InvalidDefinitionStructuralPlan(result.mkString("; "))
    )

  private def position(
      raw: untpd.Tree,
      plan: DefinitionPlan,
      source: SourceFile
  )(using Context): Either[
    ConstructedDefinitionGeneratedOriginError,
    untpd.Tree
  ] =
    raw match
      case method: untpd.DefDef
          if plan.parts.kind == DefinitionKind.ParameterlessDef =>
        for
          definitionType <- GeneratedOriginFragmentSupport
            .positionType(
              method.tpt,
              plan.typeFragment,
              source,
              plan.typeStart
            )
            .left
            .map(error => RawDefinitionPlanMismatch(error.message))
          body <- GeneratedOriginFragmentSupport
            .positionTerm(
              method.rhs,
              plan.bodyFragment,
              source,
              plan.bodyStart
            )
            .left
            .map(error => RawDefinitionPlanMismatch(error.message))
        yield
          untpd.cpy
            .DefDef(method)(
              method.name,
              method.paramss,
              definitionType,
              body
            )
            .cloneIn(source)
            .withSpan(
              Span(0, plan.generatedSource.length, plan.rootPoint)
            )
      case value: untpd.ValDef
          if plan.parts.kind == DefinitionKind.ImmutableVal =>
        for
          definitionType <- GeneratedOriginFragmentSupport
            .positionType(
              value.tpt,
              plan.typeFragment,
              source,
              plan.typeStart
            )
            .left
            .map(error => RawDefinitionPlanMismatch(error.message))
          body <- GeneratedOriginFragmentSupport
            .positionTerm(
              value.rhs,
              plan.bodyFragment,
              source,
              plan.bodyStart
            )
            .left
            .map(error => RawDefinitionPlanMismatch(error.message))
        yield
          untpd.cpy
            .ValDef(value)(
              value.name,
              definitionType,
              body
            )
            .cloneIn(source)
            .withSpan(
              Span(0, plan.generatedSource.length, plan.rootPoint)
            )
      case other =>
        Left(
          RawDefinitionPlanMismatch(
            s"raw ${other.getClass.getSimpleName} does not match ${plan.parts.kind}"
          )
        )

  private def validatePositioned(
      tree: untpd.Tree,
      plan: DefinitionPlan,
      source: SourceFile
  )(using Context): Either[
    ConstructedDefinitionGeneratedOriginError,
    Unit
  ] =
    val structural =
      (tree, plan.parts.kind) match
        case (method: untpd.DefDef, DefinitionKind.ParameterlessDef) =>
          Either.cond(
            method.name.toString == plan.parts.name.decoded &&
              method.paramss.isEmpty &&
              method.mods.flags == Flags.Method &&
              !method.mods.hasAnnotations &&
              !method.mods.hasPrivateWithin,
            (method.name.toString, method.tpt, method.rhs),
            RawDefinitionPlanMismatch(
              "positioned parameterless method diverged from the Phase 48 raw contract"
            )
          )
        case (value: untpd.ValDef, DefinitionKind.ImmutableVal) =>
          Either.cond(
            value.name.toString == plan.parts.name.decoded &&
              !value.mods.hasFlags &&
              !value.mods.hasAnnotations &&
              !value.mods.hasPrivateWithin,
            (value.name.toString, value.tpt, value.rhs),
            RawDefinitionPlanMismatch(
              "positioned immutable value diverged from the Phase 48 raw contract"
            )
          )
        case _ =>
          Left(
            RawDefinitionPlanMismatch(
              "positioned definition variant does not match the structural plan"
            )
          )
    structural.flatMap { case (actualName, definitionType, body) =>
      validateCompleteMap(
        tree,
        definitionType,
        body,
        plan.generatedSource,
        source,
        plan.parts.name.source,
        plan.parts.name.decoded,
        actualName
      )
    }

  private def validateCompleteMap(
      tree: untpd.Tree,
      definitionType: untpd.Tree,
      body: untpd.Tree,
      generatedSource: String,
      source: SourceFile,
      expectedNameSource: String,
      expectedNameDecoded: String,
      actualName: String
  )(using Context): Either[
    ConstructedDefinitionGeneratedOriginError,
    Unit
  ] =
    val errors = Vector.newBuilder[String]
    if !tree.span.exists ||
        tree.span.start != 0 ||
        tree.span.end != generatedSource.length
    then
      errors +=
        s"root ${tree.getClass.getSimpleName} does not cover generated source 0..${generatedSource.length}"
    if actualName != expectedNameDecoded then
      errors +=
        s"semantic name `$actualName` does not match `$expectedNameDecoded`"
    if tree.span.exists then
      val nameStart =
        tree.span.point -
          Option.when(expectedNameSource.startsWith("`"))(1).getOrElse(0)
      val nameEnd = nameStart + expectedNameSource.length
      if nameStart < 0 ||
          nameEnd > generatedSource.length ||
          generatedSource.slice(nameStart, nameEnd) != expectedNameSource
      then errors += "root point does not identify the exact definition-name slice"
    val rootChildren = GeneratedOriginFragmentSupport.directChildren(tree)
    if rootChildren != Vector(definitionType, body) then
      errors += "definition root has children beyond the explicit type and body"
    if definitionType.span.exists &&
        body.span.exists &&
        definitionType.span.end > body.span.start
    then errors += "definition type and body overlap or are out of source order"

    GeneratedOriginFragmentSupport.allTrees(tree).foreach { current =>
      if !current.source.exists then
        errors += s"${current.getClass.getSimpleName} has no source"
      else if current.source.path != source.path then
        errors +=
          s"${current.getClass.getSimpleName} has source `${current.source.path}` instead of `${source.path}`"
      if !current.span.exists then
        errors += s"${current.getClass.getSimpleName} has no span"
      else if current.span.start < 0 ||
          current.span.start > current.span.point ||
          current.span.point > current.span.end ||
          current.span.end > generatedSource.length
      then
        errors +=
          s"${current.getClass.getSimpleName} has out-of-bounds span ${current.span.start}..${current.span.point}..${current.span.end}"
      if current.symbol != NoSymbol then
        errors += s"${current.getClass.getSimpleName} unexpectedly has a symbol"
      if current.isInstanceOf[untpd.TypedSplice] then
        errors += "positioned definition contains a TypedSplice"
      val children = GeneratedOriginFragmentSupport.directChildren(current)
      if current.span.exists then
        children.foreach { child =>
          if child.span.exists &&
              (child.span.start < current.span.start ||
                child.span.end > current.span.end)
          then
            errors +=
              s"${current.getClass.getSimpleName} span does not contain ${child.getClass.getSimpleName}"
        }
      children.zip(children.drop(1)).foreach { case (left, right) =>
        if left.span.exists &&
            right.span.exists &&
            left.span.end > right.span.start
        then
          errors +=
            s"${current.getClass.getSimpleName} children overlap or are out of source order"
      }
    }
    val result = errors.result()
    Either.cond(
      result.isEmpty,
      (),
      IncompleteDefinitionPositionMap(result.mkString("; "))
    )
