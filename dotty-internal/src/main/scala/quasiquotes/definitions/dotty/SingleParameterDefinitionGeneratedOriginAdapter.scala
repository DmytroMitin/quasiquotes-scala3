package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.Spans.Span

import quasiquotes.definitions.{ConstructedDefinition, DefinitionName}
import quasiquotes.terms.dotty.GeneratedOriginFragmentSupport

private[dotty] object SingleParameterDefinitionGeneratedOriginAdapter:
  import ConstructedDefinitionGeneratedOriginError.*

  private final case class Plan(
      generatedSource: String,
      rootPoint: Int,
      methodNameStart: Int,
      methodNameEnd: Int,
      parameterNameStart: Int,
      parameterNameEnd: Int,
      parameterStart: Int,
      parameterEnd: Int,
      parameterPoint: Int,
      parameterTypeStart: Int,
      resultTypeStart: Int,
      bodyStart: Int,
      parameterType: GeneratedOriginFragmentSupport.TypeFragment,
      resultType: GeneratedOriginFragmentSupport.TypeFragment,
      body: GeneratedOriginFragmentSupport.TermFragment
  )

  def lower(
      method: ConstructedDefinition.SingleParameterDef,
      virtualSourceName: String
  )(using Context): Either[
    ConstructedDefinitionGeneratedOriginError,
    GeneratedOriginDefinitionResult
  ] =
    for
      _ <- GeneratedOriginFragmentSupport
        .validateVirtualSourceName(virtualSourceName)
        .left
        .map(error => InvalidVirtualSourceName(error.message))
      _ <- validateName(method.name, "method")
      _ <- validateName(method.parameterName, "parameter")
      parameterType <- GeneratedOriginFragmentSupport
        .planType(method.parameterType)
        .left
        .map(error =>
          DefinitionTypePlanningFailure(s"parameter type: ${error.message}")
        )
      resultType <- GeneratedOriginFragmentSupport
        .planType(method.resultType)
        .left
        .map(error =>
          DefinitionTypePlanningFailure(s"result type: ${error.message}")
        )
      body <- GeneratedOriginFragmentSupport
        .planDefinitionBodyInScope(
          method.body,
          method.parameterBinderId,
          method.parameterName.source
        )
        .left
        .map(error => DefinitionBodyPlanningFailure(error.message))
      plan = assemble(method, parameterType, resultType, body)
      _ <- validatePlan(method, plan)
      raw <- ConstructedDefinitionUntypedBackend
        .lower(method)
        .left
        .map(error => RawDefinitionLoweringFailure(error.message))
      source = SourceFile.virtual(virtualSourceName, plan.generatedSource)
      positioned <- position(raw, plan, source)
      _ <- validatePositioned(method, positioned, plan, source)
    yield new GeneratedOriginDefinitionResult(positioned, plan.generatedSource, source)

  private def validateName(
      name: DefinitionName,
      role: String
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
          s"the validated $role source/decoded spelling invariant was not satisfied."
        )
      )
      .map(_ => ())

  private def assemble(
      method: ConstructedDefinition.SingleParameterDef,
      parameterType: GeneratedOriginFragmentSupport.TypeFragment,
      resultType: GeneratedOriginFragmentSupport.TypeFragment,
      body: GeneratedOriginFragmentSupport.TermFragment
  ): Plan =
    val prefix = s"def ${method.name.source}("
    val parameterNameStart = prefix.length
    val parameterNameEnd = parameterNameStart + method.parameterName.source.length
    val parameterTypeStart = parameterNameEnd + 2
    val parameterEnd = parameterTypeStart + parameterType.source.length
    val resultTypeStart = parameterEnd + 3
    val bodyStart = resultTypeStart + resultType.source.length + 3
    val generatedSource =
      s"${prefix}${method.parameterName.source}: ${parameterType.source}): ${resultType.source} = ${body.source}"
    val methodNameStart = 4
    Plan(
      generatedSource,
      rootPoint = methodNameStart + backtickOffset(method.name.source),
      methodNameStart,
      methodNameEnd = methodNameStart + method.name.source.length,
      parameterNameStart,
      parameterNameEnd,
      parameterStart = parameterNameStart,
      parameterEnd,
      parameterPoint = parameterNameStart + backtickOffset(method.parameterName.source),
      parameterTypeStart,
      resultTypeStart,
      bodyStart,
      parameterType,
      resultType,
      body
    )

  private def validatePlan(
      method: ConstructedDefinition.SingleParameterDef,
      plan: Plan
  ): Either[ConstructedDefinitionGeneratedOriginError, Unit] =
    val parameterTypeEnd = plan.parameterTypeStart + plan.parameterType.source.length
    val resultTypeEnd = plan.resultTypeStart + plan.resultType.source.length
    val bodyEnd = plan.bodyStart + plan.body.source.length
    val source = plan.generatedSource
    val errors = Vector.newBuilder[String]
    if source.slice(plan.methodNameStart, plan.methodNameEnd) != method.name.source then
      errors += "method name slice does not match DefinitionName.source"
    if source.slice(plan.parameterNameStart, plan.parameterNameEnd) != method.parameterName.source then
      errors += "parameter name slice does not match DefinitionName.source"
    if source.slice(plan.parameterNameEnd, plan.parameterTypeStart) != ": " then
      errors += "parameter name/type punctuation is not `: `"
    if source.slice(plan.parameterTypeStart, parameterTypeEnd) != plan.parameterType.source then
      errors += "parameter type slice does not match the shared type fragment"
    if source.slice(parameterTypeEnd, plan.resultTypeStart) != "): " then
      errors += "parameter/result punctuation is not `): `"
    if source.slice(plan.resultTypeStart, resultTypeEnd) != plan.resultType.source then
      errors += "result type slice does not match the shared type fragment"
    if source.slice(resultTypeEnd, plan.bodyStart) != " = " then
      errors += "result/body punctuation is not ` = `"
    if source.slice(plan.bodyStart, bodyEnd) != plan.body.source then
      errors += "body slice does not match the binder-aware term fragment"
    if bodyEnd != source.length then
      errors += s"body end $bodyEnd does not cover source length ${source.length}"
    val result = errors.result()
    Either.cond(
      result.isEmpty,
      (),
      InvalidDefinitionStructuralPlan(result.mkString("; "))
    )

  private def position(
      raw: untpd.Tree,
      plan: Plan,
      source: SourceFile
  )(using Context): Either[
    ConstructedDefinitionGeneratedOriginError,
    untpd.DefDef
  ] =
    raw match
      case method: untpd.DefDef
          if method.paramss.size == 1 && method.paramss.head.size == 1 =>
        val parameter = method.paramss.head.head.asInstanceOf[untpd.ValDef]
        for
          parameterType <- GeneratedOriginFragmentSupport
            .positionType(
              parameter.tpt,
              plan.parameterType,
              source,
              plan.parameterTypeStart
            )
            .left
            .map(error => RawDefinitionPlanMismatch(error.message))
          resultType <- GeneratedOriginFragmentSupport
            .positionType(
              method.tpt,
              plan.resultType,
              source,
              plan.resultTypeStart
            )
            .left
            .map(error => RawDefinitionPlanMismatch(error.message))
          body <- GeneratedOriginFragmentSupport
            .positionTerm(method.rhs, plan.body, source, plan.bodyStart)
            .left
            .map(error => RawDefinitionPlanMismatch(error.message))
        yield
          val positionedParameter = untpd
            .ValDef(parameter.name, parameterType, untpd.EmptyTree)
            .withMods(parameter.mods)
            .cloneIn(source)
            .withSpan(
              Span(plan.parameterStart, plan.parameterEnd, plan.parameterPoint)
            )
          untpd.cpy
            .DefDef(method)(
              method.name,
              List(List(positionedParameter)),
              resultType,
              body
            )
            .cloneIn(source)
            .withSpan(Span(0, plan.generatedSource.length, plan.rootPoint))
      case other =>
        Left(
          RawDefinitionPlanMismatch(
            s"expected a one-clause one-parameter DefDef, found ${other.getClass.getSimpleName}"
          )
        )

  private def validatePositioned(
      expected: ConstructedDefinition.SingleParameterDef,
      tree: untpd.DefDef,
      plan: Plan,
      source: SourceFile
  )(using Context): Either[ConstructedDefinitionGeneratedOriginError, Unit] =
    val errors = Vector.newBuilder[String]
    val parameter = tree.paramss.head.head.asInstanceOf[untpd.ValDef]
    if tree.name.toString != expected.name.decoded then
      errors += "method name does not match the semantic declaration"
    if tree.mods.flags != Flags.Method || tree.paramss.map(_.size) != List(1) then
      errors += "method flags or ordinary parameter-clause shape diverged"
    if parameter.name.toString != expected.parameterName.decoded ||
        parameter.mods.flags != Flags.Param || !parameter.rhs.isEmpty
    then errors += "ordinary parameter name, flags, or RHS diverged"
    val materialChildren = Vector(parameter, tree.tpt, tree.rhs)
    if materialChildren.map(_.span.start) != materialChildren.map(_.span.start).sorted then
      errors += "parameter, result type, and body are not in source order"
    allDefinitionTrees(tree).foreach { current =>
      if !current.source.exists || current.source.path != source.path then
        errors += s"${current.getClass.getSimpleName} has the wrong virtual source"
      if !current.span.exists || current.span.start < 0 ||
          current.span.start > current.span.point || current.span.point > current.span.end ||
          current.span.end > plan.generatedSource.length
      then errors += s"${current.getClass.getSimpleName} has an invalid span"
      if current.symbol != NoSymbol then
        errors += s"${current.getClass.getSimpleName} unexpectedly has a symbol"
      if current.isInstanceOf[untpd.TypedSplice] then
        errors += "positioned definition contains a TypedSplice"
    }
    val result = errors.result()
    Either.cond(
      result.isEmpty,
      (),
      IncompleteDefinitionPositionMap(result.mkString("; "))
    )

  private def allDefinitionTrees(
      tree: untpd.Tree
  )(using Context): Vector[untpd.Tree] =
    tree match
      case method: untpd.DefDef =>
        method +: (
          method.paramss.flatten.toVector ++ Vector(method.tpt, method.rhs)
        ).flatMap(allDefinitionTrees)
      case _ =>
        GeneratedOriginFragmentSupport.allTrees(tree)

  private def backtickOffset(source: String): Int =
    Option.when(source.startsWith("`"))(1).getOrElse(0)
