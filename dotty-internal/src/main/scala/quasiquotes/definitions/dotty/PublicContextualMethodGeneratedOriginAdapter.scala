package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.Spans.Span

import quasiquotes.publicapi.{CompletedType, DefinitionResultView}
import quasiquotes.terms.dotty.GeneratedOriginFragmentSupport

private[quasiquotes] object PublicContextualMethodGeneratedOriginAdapter:
  import PublicContextualMethodGeneratedOriginError.*

  private enum TypePlanKind:
    case Identifier
    case Applied

  private final case class TypePlan(
      kind: TypePlanKind,
      source: String,
      start: Int,
      end: Int,
      point: Int,
      children: Vector[TypePlan]
  )

  private final case class MethodPlan(
      generatedSource: String,
      methodNameStart: Int,
      methodNameEnd: Int,
      typeParameterStart: Int,
      typeParameterEnd: Int,
      contextualParameterStart: Int,
      contextualParameterNameEnd: Int,
      contextualType: TypePlan,
      resultType: TypePlan,
      bodyStart: Int,
      bodyEnd: Int
  )

  def lower(
      result: DefinitionResultView,
      virtualSourceName: String
  )(using Context): Either[
    PublicContextualMethodGeneratedOriginError,
    GeneratedOriginDefinitionResult
  ] =
    for
      _ <- GeneratedOriginFragmentSupport
        .validateVirtualSourceName(virtualSourceName)
        .left
        .map(error => InvalidVirtualSourceName(error.message))
      plan <- plan(result)
      raw <- PublicContextualMethodUntypedBackend
        .lower(result)
        .left
        .map(error => RawLoweringFailure(error.message))
      source = SourceFile.virtual(virtualSourceName, plan.generatedSource)
      positioned <- position(raw, plan, source)
      _ <- validatePositioned(positioned, result, plan, source)
    yield
      new GeneratedOriginDefinitionResult(
        positioned,
        plan.generatedSource,
        source
      )

  private def plan(
      result: DefinitionResultView
  ): Either[PublicContextualMethodGeneratedOriginError, MethodPlan] =
    Option(result)
      .toRight(ProjectionPlanningFailure("the definition result was null."))
      .flatMap { definition =>
        val builder = new StringBuilder("def ")
        val methodNameStart = builder.length
        builder.append(definition.name)
        val methodNameEnd = builder.length
        builder.append('[')
        val typeParameterStart = builder.length
        builder.append(definition.typeParameterName)
        val typeParameterEnd = builder.length
        builder.append("](using ")
        val contextualParameterStart = builder.length
        builder.append(definition.contextualParameterName)
        val contextualParameterNameEnd = builder.length
        builder.append(": ")
        for
          contextualType <- renderType(
            definition.contextualParameterType,
            definition.typeParameterName,
            builder,
            "contextual-parameter"
          )
          _ = builder.append("): ")
          resultType <- renderType(
            definition.resultType,
            definition.typeParameterName,
            builder,
            "result"
          )
          _ = builder.append(" = ")
          bodyStart = builder.length
          _ = builder.append(definition.body.referenceName)
          bodyEnd = builder.length
          generatedSource = builder.toString
          methodPlan = MethodPlan(
            generatedSource,
            methodNameStart,
            methodNameEnd,
            typeParameterStart,
            typeParameterEnd,
            contextualParameterStart,
            contextualParameterNameEnd,
            contextualType,
            resultType,
            bodyStart,
            bodyEnd
          )
          _ <- validatePlan(methodPlan)
        yield methodPlan
      }

  private def renderType(
      value: CompletedType,
      declaredTypeParameter: String,
      builder: StringBuilder,
      anchor: String
  ): Either[PublicContextualMethodGeneratedOriginError, TypePlan] =
    Option(value)
      .toRight(ProjectionPlanningFailure(s"$anchor type was null."))
      .flatMap { completed =>
        completed.kindCode match
          case "named" | "type-parameter" =>
            completed.name
              .toRight(
                ProjectionPlanningFailure(
                  s"$anchor ${completed.kindCode} type has no name projection."
                )
              )
              .flatMap { name =>
                Either.cond(
                  completed.kindCode != "type-parameter" ||
                    name == declaredTypeParameter,
                  {
                    val start = builder.length
                    builder.append(name)
                    TypePlan(
                      TypePlanKind.Identifier,
                      name,
                      start,
                      builder.length,
                      start,
                      Vector.empty
                    )
                  },
                  ProjectionPlanningFailure(
                    s"$anchor type parameter `$name` does not name binder `$declaredTypeParameter`."
                  )
                )
              }
          case "applied" =>
            for
              constructor <- completed.constructor.toRight(
                ProjectionPlanningFailure(
                  s"$anchor applied type has no constructor projection."
                )
              )
              _ <- Either.cond(
                constructor.kindCode == "named" && completed.arguments.nonEmpty,
                (),
                ProjectionPlanningFailure(
                  s"$anchor applied type must have one named constructor and nonempty arguments."
                )
              )
              start = builder.length
              constructorPlan <- renderType(
                constructor,
                declaredTypeParameter,
                builder,
                s"$anchor constructor"
              )
              _ = builder.append('[')
              argumentPlans <- completed.arguments.zipWithIndex.foldLeft(
                Right(Vector.empty): Either[
                  PublicContextualMethodGeneratedOriginError,
                  Vector[TypePlan]
                ]
              ) { case (accumulated, (argument, index)) =>
                accumulated.flatMap { plans =>
                  if index > 0 then builder.append(", ")
                  renderType(
                    argument,
                    declaredTypeParameter,
                    builder,
                    s"$anchor argument ${index + 1}"
                  ).map(plans :+ _)
                }
              }
              _ = builder.append(']')
              end = builder.length
            yield TypePlan(
              TypePlanKind.Applied,
              builder.substring(start, end),
              start,
              end,
              constructorPlan.point,
              constructorPlan +: argumentPlans
            )
          case other =>
            Left(
              ProjectionPlanningFailure(
                s"$anchor type has unsupported projection kind `${String.valueOf(other)}`."
              )
            )
      }

  private def validatePlan(
      plan: MethodPlan
  ): Either[PublicContextualMethodGeneratedOriginError, Unit] =
    val errors = Vector.newBuilder[String]
    val length = plan.generatedSource.length
    if plan.methodNameStart != 4 || plan.methodNameEnd <= plan.methodNameStart then
      errors += "method-name slice is invalid"
    if plan.typeParameterStart <= plan.methodNameEnd ||
        plan.typeParameterEnd <= plan.typeParameterStart
    then errors += "type-parameter slice is invalid"
    if plan.contextualParameterStart <= plan.typeParameterEnd ||
        plan.contextualParameterNameEnd <= plan.contextualParameterStart
    then errors += "contextual-parameter name slice is invalid"
    validateTypePlan(plan.contextualType, length, errors)
    validateTypePlan(plan.resultType, length, errors)
    if plan.contextualType.end > plan.resultType.start then
      errors += "contextual and result type plans overlap"
    if plan.resultType.end > plan.bodyStart || plan.bodyEnd != length then
      errors += "result type/body order or generated-source coverage is invalid"
    val result = errors.result()
    Either.cond(
      result.isEmpty,
      (),
      ProjectionPlanningFailure(result.mkString("; "))
    )

  private def validateTypePlan(
      plan: TypePlan,
      sourceLength: Int,
      errors: scala.collection.mutable.Builder[String, Vector[String]]
  ): Unit =
    if plan.start < 0 || plan.start > plan.point || plan.point > plan.end ||
        plan.end > sourceLength
    then errors += s"${plan.kind} has invalid span ${plan.start}..${plan.point}..${plan.end}"
    plan.children.foreach { child =>
      if child.start < plan.start || child.end > plan.end then
        errors += s"${plan.kind} does not contain ${child.kind}"
      validateTypePlan(child, sourceLength, errors)
    }
    plan.children.zip(plan.children.drop(1)).foreach { case (left, right) =>
      if left.end > right.start then
        errors += s"${plan.kind} children overlap or are out of order"
    }

  private def position(
      raw: untpd.DefDef,
      plan: MethodPlan,
      source: SourceFile
  )(using Context): Either[
    PublicContextualMethodGeneratedOriginError,
    untpd.DefDef
  ] =
    (raw.leadingTypeParams, raw.trailingParamss) match
      case (List(typeParameter: untpd.TypeDef), List(List(parameter: untpd.ValDef))) =>
        for
          bounds <- positionBounds(typeParameter.rhs, plan.typeParameterStart, source)
          contextualType <- positionType(parameter.tpt, plan.contextualType, source)
          resultType <- positionType(raw.tpt, plan.resultType, source)
          body <- positionBody(raw.rhs, plan, source)
        yield
          val positionedTypeParameter =
            untpd
              .TypeDef(typeParameter.name, bounds)
              .withMods(typeParameter.mods)
              .cloneIn(source)
              .withSpan(
                Span(
                  plan.typeParameterStart,
                  plan.typeParameterEnd,
                  plan.typeParameterStart
                )
              )
          val positionedContextualParameter =
            untpd
              .ValDef(parameter.name, contextualType, untpd.EmptyTree)
              .withMods(parameter.mods)
              .cloneIn(source)
              .withSpan(
                Span(
                  plan.contextualParameterStart,
                  plan.contextualType.end,
                  plan.contextualParameterStart
                )
              )
          untpd
            .DefDef(
              raw.name,
              List(
                positionedTypeParameter :: Nil,
                positionedContextualParameter :: Nil
              ),
              resultType,
              body
            )
            .withMods(raw.mods)
            .cloneIn(source)
            .withSpan(
              Span(0, plan.generatedSource.length, plan.methodNameStart)
            )
      case other =>
        Left(
          RawTreePlanMismatch(
            s"expected one TypeDef clause and one contextual ValDef clause, found $other."
          )
        )

  private def positionBounds(
      raw: untpd.Tree,
      binderStart: Int,
      source: SourceFile
  ): Either[PublicContextualMethodGeneratedOriginError, untpd.Tree] =
    raw match
      case untpd.WildcardTypeBoundsTree() =>
        Right(
          raw
            .cloneIn(source)
            .withSpan(Span(binderStart, binderStart, binderStart))
        )
      case other =>
        Left(
          RawTreePlanMismatch(
            s"expected WildcardTypeBoundsTree, found ${other.getClass.getSimpleName}."
          )
        )

  private def positionType(
      raw: untpd.Tree,
      plan: TypePlan,
      source: SourceFile
  )(using Context): Either[PublicContextualMethodGeneratedOriginError, untpd.Tree] =
    (raw, plan.kind) match
      case (identifier: untpd.Ident, TypePlanKind.Identifier) =>
        Right(identifier.cloneIn(source).withSpan(planSpan(plan)))
      case (applied: untpd.AppliedTypeTree, TypePlanKind.Applied)
          if plan.children.nonEmpty =>
        for
          constructor <- positionType(applied.tpt, plan.children.head, source)
          arguments <- applied.args.zip(plan.children.tail).foldLeft(
            Right(List.empty): Either[
              PublicContextualMethodGeneratedOriginError,
              List[untpd.Tree]
            ]
          ) { case (accumulated, (argument, argumentPlan)) =>
            for
              values <- accumulated
              positioned <- positionType(argument, argumentPlan, source)
            yield values :+ positioned
          }
          _ <- Either.cond(
            applied.args.size == plan.children.tail.size,
            (),
            RawTreePlanMismatch(
              "applied type argument count does not match the generated-source plan."
            )
          )
        yield
          untpd
            .AppliedTypeTree(constructor, arguments)
            .cloneIn(source)
            .withSpan(planSpan(plan))
      case (other, _) =>
        Left(
          RawTreePlanMismatch(
            s"type ${other.getClass.getSimpleName} does not match ${plan.kind}."
          )
        )

  private def positionBody(
      raw: untpd.Tree,
      plan: MethodPlan,
      source: SourceFile
  ): Either[PublicContextualMethodGeneratedOriginError, untpd.Tree] =
    raw match
      case identifier: untpd.Ident =>
        Right(
          identifier
            .cloneIn(source)
            .withSpan(Span(plan.bodyStart, plan.bodyEnd, plan.bodyStart))
        )
      case other =>
        Left(
          RawTreePlanMismatch(
            s"body ${other.getClass.getSimpleName} is not the planned stable reference."
          )
        )

  private def planSpan(plan: TypePlan): Span =
    Span(plan.start, plan.end, plan.point)

  private def validatePositioned(
      tree: untpd.DefDef,
      result: DefinitionResultView,
      plan: MethodPlan,
      source: SourceFile
  )(using Context): Either[PublicContextualMethodGeneratedOriginError, Unit] =
    val errors = Vector.newBuilder[String]
    val typeParameter = tree.leadingTypeParams.head.asInstanceOf[untpd.TypeDef]
    val contextualParameter =
      tree.trailingParamss.head.head.asInstanceOf[untpd.ValDef]

    if tree.name.toString != result.name || tree.mods.flags != Flags.Method then
      errors += "root method name or flags diverged from the public/raw contract"
    if tree.paramss.map(_.size) != List(1, 1) then
      errors += "root parameter clauses diverged from the one-plus-one contract"
    if typeParameter.name.toString != result.typeParameterName ||
        typeParameter.mods.flags != Flags.Param
    then errors += "type parameter name or flags diverged from the public/raw contract"
    if contextualParameter.name.toString != result.contextualParameterName ||
        contextualParameter.mods.flags != (Flags.Param | Flags.Given)
    then errors += "contextual parameter name or flags diverged from the public/raw contract"
    if !contextualParameter.rhs.isEmpty || contextualParameter.rhs.span.exists ||
        contextualParameter.rhs.source.exists
    then errors += "synthetic contextual-parameter RHS gained source or position"

    validateSpan(tree, 0, plan.generatedSource.length, plan.methodNameStart, "root", errors)
    validateSlice(plan.methodNameStart, plan.methodNameEnd, result.name, "method name", plan, errors)
    validateSpan(
      typeParameter,
      plan.typeParameterStart,
      plan.typeParameterEnd,
      plan.typeParameterStart,
      "type parameter",
      errors
    )
    validateSlice(
      plan.typeParameterStart,
      plan.typeParameterEnd,
      result.typeParameterName,
      "type parameter",
      plan,
      errors
    )
    validateSpan(
      typeParameter.rhs,
      plan.typeParameterStart,
      plan.typeParameterStart,
      plan.typeParameterStart,
      "wildcard bounds",
      errors
    )
    typeParameter.rhs match
      case bounds: untpd.TypeBoundsTree =>
        Vector(bounds.lo, bounds.hi).foreach { empty =>
          if !empty.isEmpty || empty.span.exists || empty.source.exists then
            errors += "synthetic wildcard bound child gained source or position"
        }
      case _ => errors += "type parameter no longer contains TypeBoundsTree"
    validateSpan(
      contextualParameter,
      plan.contextualParameterStart,
      plan.contextualType.end,
      plan.contextualParameterStart,
      "contextual parameter",
      errors
    )
    validateSlice(
      plan.contextualParameterStart,
      plan.contextualParameterNameEnd,
      result.contextualParameterName,
      "contextual parameter name",
      plan,
      errors
    )
    validateTypeTree(contextualParameter.tpt, plan.contextualType, plan, errors)
    validateTypeTree(tree.tpt, plan.resultType, plan, errors)
    validateSpan(tree.rhs, plan.bodyStart, plan.bodyEnd, plan.bodyStart, "body", errors)
    validateSlice(plan.bodyStart, plan.bodyEnd, result.body.referenceName, "body", plan, errors)

    if source.path != tree.source.path || source.content.mkString != plan.generatedSource then
      errors += "virtual source identity or content diverged from the generated source"

    nonEmptyTrees(tree).foreach { current =>
      if !current.source.exists || current.source.path != source.path then
        errors += s"${current.getClass.getSimpleName} does not use the generated virtual source"
      if !current.span.exists || current.span.start < 0 ||
          current.span.start > current.span.point ||
          current.span.point > current.span.end ||
          current.span.end > plan.generatedSource.length
      then errors += s"${current.getClass.getSimpleName} has an invalid or absent span"
      if current.symbol != NoSymbol then
        errors += s"${current.getClass.getSimpleName} unexpectedly has a symbol"
      if current.isInstanceOf[untpd.TypedSplice] then
        errors += "positioned method contains a TypedSplice"
      positionedChildren(current).foreach { child =>
        if child.span.start < current.span.start || child.span.end > current.span.end then
          errors += s"${current.getClass.getSimpleName} does not contain ${child.getClass.getSimpleName}"
      }
      positionedChildren(current)
        .zip(positionedChildren(current).drop(1))
        .foreach { case (left, right) =>
          if left.span.end > right.span.start then
            errors += s"${current.getClass.getSimpleName} children overlap or are out of order"
        }
    }

    val validationErrors = errors.result()
    Either.cond(
      validationErrors.isEmpty,
      (),
      IncompletePositionMap(validationErrors.mkString("; "))
    )

  private def validateTypeTree(
      tree: untpd.Tree,
      plan: TypePlan,
      methodPlan: MethodPlan,
      errors: scala.collection.mutable.Builder[String, Vector[String]]
  )(using Context): Unit =
    validateSpan(tree, plan.start, plan.end, plan.point, plan.kind.toString, errors)
    validateSlice(plan.start, plan.end, plan.source, plan.kind.toString, methodPlan, errors)
    val children = positionedChildren(tree)
    if children.size != plan.children.size then
      errors += s"${plan.kind} tree/plan child counts differ"
    children.zip(plan.children).foreach { case (child, childPlan) =>
      validateTypeTree(child, childPlan, methodPlan, errors)
    }

  private def validateSpan(
      tree: untpd.Tree,
      start: Int,
      end: Int,
      point: Int,
      role: String,
      errors: scala.collection.mutable.Builder[String, Vector[String]]
  ): Unit =
    if !tree.span.exists || tree.span.start != start || tree.span.end != end ||
        tree.span.point != point
    then errors += s"$role span does not equal $start..$point..$end"

  private def validateSlice(
      start: Int,
      end: Int,
      expected: String,
      role: String,
      plan: MethodPlan,
      errors: scala.collection.mutable.Builder[String, Vector[String]]
  ): Unit =
    if start < 0 || end > plan.generatedSource.length ||
        plan.generatedSource.slice(start, end) != expected
    then errors += s"$role does not select exact source `$expected`"

  private def nonEmptyTrees(
      tree: untpd.Tree
  )(using Context): Vector[untpd.Tree] =
    if tree.isEmpty then Vector.empty
    else tree +: positionedChildren(tree).flatMap(nonEmptyTrees)

  private def positionedChildren(
      tree: untpd.Tree
  )(using Context): Vector[untpd.Tree] =
    tree match
      case value: untpd.DefDef =>
        value.paramss.flatten.toVector ++ Vector(value.tpt, value.rhs)
      case value: untpd.TypeDef => Vector(value.rhs)
      case value: untpd.TypeBoundsTree =>
        Vector(value.lo, value.hi).filterNot(_.isEmpty)
      case value: untpd.ValDef => Vector(value.tpt, value.rhs).filterNot(_.isEmpty)
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case _ => Vector.empty
