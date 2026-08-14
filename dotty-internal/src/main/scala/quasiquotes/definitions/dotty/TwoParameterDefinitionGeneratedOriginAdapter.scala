package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.Spans.Span

import quasiquotes.definitions.{ConstructedDefinition, DefinitionName}
import quasiquotes.terms.dotty.GeneratedOriginFragmentSupport

private[dotty] object TwoParameterDefinitionGeneratedOriginAdapter:
  import ConstructedDefinitionGeneratedOriginError.*

  private final case class ParameterPlan(
      nameStart: Int,
      nameEnd: Int,
      start: Int,
      end: Int,
      point: Int,
      typeStart: Int,
      tpe: GeneratedOriginFragmentSupport.TypeFragment
  )

  private final case class Plan(
      generatedSource: String,
      rootPoint: Int,
      methodNameStart: Int,
      methodNameEnd: Int,
      first: ParameterPlan,
      second: ParameterPlan,
      resultTypeStart: Int,
      bodyStart: Int,
      resultType: GeneratedOriginFragmentSupport.TypeFragment,
      body: GeneratedOriginFragmentSupport.TermFragment
  )

  def lower(
      method: ConstructedDefinition.TwoParameterDef,
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
      _ <- validateName(method.firstParameterName, "first parameter")
      _ <- validateName(method.secondParameterName, "second parameter")
      firstType <- planType(method.firstParameterType, "first parameter")
      secondType <- planType(method.secondParameterType, "second parameter")
      resultType <- planType(method.resultType, "result")
      body <- Option(method.body)
        .toRight(DefinitionBodyPlanningFailure("the completed definition body was null."))
        .flatMap(
          GeneratedOriginFragmentSupport
            .planDefinitionBodyInScopes(
              _,
              Vector(
                method.firstParameterBinderId -> method.firstParameterName.source,
                method.secondParameterBinderId -> method.secondParameterName.source
              )
            )
            .left
            .map(error => DefinitionBodyPlanningFailure(error.message))
        )
      plan = assemble(method, firstType, secondType, resultType, body)
      _ <- validatePlan(method, plan)
      raw <- ConstructedDefinitionUntypedBackend
        .lower(method)
        .left
        .map(error => RawDefinitionLoweringFailure(error.message))
      source = SourceFile.virtual(virtualSourceName, plan.generatedSource)
      positioned <- position(raw, plan, source)
      _ <- validatePositioned(method, positioned, plan, source)
    yield new GeneratedOriginDefinitionResult(positioned, plan.generatedSource, source)

  private def planType(
      normalForm: quasiquotes.types.TypeNormalForm,
      role: String
  ): Either[
    ConstructedDefinitionGeneratedOriginError,
    GeneratedOriginFragmentSupport.TypeFragment
  ] =
    Option(normalForm)
      .toRight(DefinitionTypePlanningFailure(s"$role type: the completed type was null."))
      .flatMap(
        GeneratedOriginFragmentSupport
          .planType(_)
          .left
          .map(error =>
            DefinitionTypePlanningFailure(s"$role type: ${error.message}")
          )
      )

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
      method: ConstructedDefinition.TwoParameterDef,
      firstType: GeneratedOriginFragmentSupport.TypeFragment,
      secondType: GeneratedOriginFragmentSupport.TypeFragment,
      resultType: GeneratedOriginFragmentSupport.TypeFragment,
      body: GeneratedOriginFragmentSupport.TermFragment
  ): Plan =
    val prefix = s"def ${method.name.source}("
    val firstNameStart = prefix.length
    val firstNameEnd = firstNameStart + method.firstParameterName.source.length
    val firstTypeStart = firstNameEnd + 2
    val firstEnd = firstTypeStart + firstType.source.length
    val secondNameStart = firstEnd + 2
    val secondNameEnd = secondNameStart + method.secondParameterName.source.length
    val secondTypeStart = secondNameEnd + 2
    val secondEnd = secondTypeStart + secondType.source.length
    val resultTypeStart = secondEnd + 3
    val bodyStart = resultTypeStart + resultType.source.length + 3
    val generatedSource =
      s"${prefix}${method.firstParameterName.source}: ${firstType.source}, ${method.secondParameterName.source}: ${secondType.source}): ${resultType.source} = ${body.source}"
    val methodNameStart = 4
    Plan(
      generatedSource,
      rootPoint = methodNameStart + backtickOffset(method.name.source),
      methodNameStart,
      methodNameEnd = methodNameStart + method.name.source.length,
      ParameterPlan(
        firstNameStart,
        firstNameEnd,
        start = firstNameStart,
        end = firstEnd,
        point = firstNameStart + backtickOffset(method.firstParameterName.source),
        typeStart = firstTypeStart,
        tpe = firstType
      ),
      ParameterPlan(
        secondNameStart,
        secondNameEnd,
        start = secondNameStart,
        end = secondEnd,
        point = secondNameStart + backtickOffset(method.secondParameterName.source),
        typeStart = secondTypeStart,
        tpe = secondType
      ),
      resultTypeStart,
      bodyStart,
      resultType,
      body
    )

  private def validatePlan(
      method: ConstructedDefinition.TwoParameterDef,
      plan: Plan
  ): Either[ConstructedDefinitionGeneratedOriginError, Unit] =
    val firstTypeEnd = plan.first.typeStart + plan.first.tpe.source.length
    val secondTypeEnd = plan.second.typeStart + plan.second.tpe.source.length
    val resultTypeEnd = plan.resultTypeStart + plan.resultType.source.length
    val bodyEnd = plan.bodyStart + plan.body.source.length
    val source = plan.generatedSource
    val errors = Vector.newBuilder[String]
    if source.slice(plan.methodNameStart, plan.methodNameEnd) != method.name.source then
      errors += "method name slice does not match DefinitionName.source"
    if source.slice(plan.first.nameStart, plan.first.nameEnd) != method.firstParameterName.source then
      errors += "first parameter name slice does not match DefinitionName.source"
    if source.slice(plan.first.nameEnd, plan.first.typeStart) != ": " then
      errors += "first parameter name/type punctuation is not `: `"
    if source.slice(plan.first.typeStart, firstTypeEnd) != plan.first.tpe.source then
      errors += "first parameter type slice does not match its type fragment"
    if source.slice(firstTypeEnd, plan.second.nameStart) != ", " then
      errors += "parameter separator punctuation is not `, `"
    if source.slice(plan.second.nameStart, plan.second.nameEnd) != method.secondParameterName.source then
      errors += "second parameter name slice does not match DefinitionName.source"
    if source.slice(plan.second.nameEnd, plan.second.typeStart) != ": " then
      errors += "second parameter name/type punctuation is not `: `"
    if source.slice(plan.second.typeStart, secondTypeEnd) != plan.second.tpe.source then
      errors += "second parameter type slice does not match its type fragment"
    if source.slice(secondTypeEnd, plan.resultTypeStart) != "): " then
      errors += "parameter/result punctuation is not `): `"
    if source.slice(plan.resultTypeStart, resultTypeEnd) != plan.resultType.source then
      errors += "result type slice does not match its type fragment"
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
          if method.paramss.size == 1 && method.paramss.head.size == 2 =>
        val first = method.paramss.head(0).asInstanceOf[untpd.ValDef]
        val second = method.paramss.head(1).asInstanceOf[untpd.ValDef]
        for
          firstType <- GeneratedOriginFragmentSupport
            .positionType(first.tpt, plan.first.tpe, source, plan.first.typeStart)
            .left
            .map(error => RawDefinitionPlanMismatch(error.message))
          secondType <- GeneratedOriginFragmentSupport
            .positionType(second.tpt, plan.second.tpe, source, plan.second.typeStart)
            .left
            .map(error => RawDefinitionPlanMismatch(error.message))
          resultType <- GeneratedOriginFragmentSupport
            .positionType(method.tpt, plan.resultType, source, plan.resultTypeStart)
            .left
            .map(error => RawDefinitionPlanMismatch(error.message))
          body <- GeneratedOriginFragmentSupport
            .positionTerm(method.rhs, plan.body, source, plan.bodyStart)
            .left
            .map(error => RawDefinitionPlanMismatch(error.message))
        yield
          val positionedFirst = positionParameter(first, firstType, plan.first, source)
          val positionedSecond = positionParameter(second, secondType, plan.second, source)
          untpd.cpy
            .DefDef(method)(
              method.name,
              List(List(positionedFirst, positionedSecond)),
              resultType,
              body
            )
            .cloneIn(source)
            .withSpan(Span(0, plan.generatedSource.length, plan.rootPoint))
      case other =>
        Left(
          RawDefinitionPlanMismatch(
            s"expected a one-clause exact-two-parameter DefDef, found ${other.getClass.getSimpleName}"
          )
        )

  private def positionParameter(
      parameter: untpd.ValDef,
      parameterType: untpd.Tree,
      plan: ParameterPlan,
      source: SourceFile
  )(using Context): untpd.ValDef =
    untpd
      .ValDef(parameter.name, parameterType, untpd.EmptyTree)
      .withMods(parameter.mods)
      .cloneIn(source)
      .withSpan(Span(plan.start, plan.end, plan.point))

  private def validatePositioned(
      expected: ConstructedDefinition.TwoParameterDef,
      tree: untpd.DefDef,
      plan: Plan,
      source: SourceFile
  )(using Context): Either[ConstructedDefinitionGeneratedOriginError, Unit] =
    val errors = Vector.newBuilder[String]
    val parameters = tree.paramss.head.map(_.asInstanceOf[untpd.ValDef])
    if tree.name.toString != expected.name.decoded then
      errors += "method name does not match the semantic declaration"
    if tree.mods.flags != Flags.Method || tree.paramss.map(_.size) != List(2) then
      errors += "method flags or exact-two parameter-clause shape diverged"
    val expectedParameters = Vector(
      expected.firstParameterName.decoded,
      expected.secondParameterName.decoded
    )
    if parameters.map(_.name.toString).toVector != expectedParameters ||
        parameters.exists(parameter =>
          parameter.mods.flags != Flags.Param || !parameter.rhs.isEmpty
        )
    then errors += "ordinary parameter names, flags, or RHSs diverged"
    val materialChildren = Vector(
      parameters(0),
      parameters(0).tpt,
      parameters(1),
      parameters(1).tpt,
      tree.tpt,
      tree.rhs
    )
    if materialChildren.map(_.span.start) != materialChildren.map(_.span.start).sorted then
      errors += "parameter, result type, and body nodes are not in source order"
    allDefinitionTrees(tree).foreach { current =>
      if !current.source.exists || current.source.path != source.path then
        errors += s"${current.getClass.getSimpleName} has the wrong virtual source"
      if !current.span.exists || current.span.start < 0 ||
          current.span.start > current.span.point ||
          current.span.point > current.span.end ||
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
      case _ => GeneratedOriginFragmentSupport.allTrees(tree)

  private def backtickOffset(source: String): Int =
    Option.when(source.startsWith("`"))(1).getOrElse(0)
