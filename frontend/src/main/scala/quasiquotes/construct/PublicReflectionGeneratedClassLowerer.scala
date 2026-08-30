package quasiquotes.construct

import scala.annotation.experimental
import scala.quoted.*

@experimental
private[quasiquotes] object PublicReflectionGeneratedClassLowerer:
  def lower(using q: Quotes)(
      plan: GeneratedClassPlan,
      parentType: q.reflect.TypeRepr,
      parameterType: q.reflect.TypeRepr,
      resultType: q.reflect.TypeRepr,
      capturedTerm: q.reflect.Term,
      invocationArgument: q.reflect.Term
  ): Either[GeneratedClassLoweringError, q.reflect.Term] =
    import q.reflect.*

    for
      _ <- GeneratedClassPlanValidation.validate(plan)
      _ <- rejectOverloadedParentMember(parentType, plan.overrideMethod.displayName)
      _ <- rejectOwnedDefinitionCapture(capturedTerm)
      lowered <- lowerValidated(
        plan,
        parentType,
        parameterType,
        resultType,
        capturedTerm,
        invocationArgument
      )
    yield lowered

  private def lowerValidated(using q: Quotes)(
      plan: GeneratedClassPlan,
      parentType: q.reflect.TypeRepr,
      parameterType: q.reflect.TypeRepr,
      resultType: q.reflect.TypeRepr,
      capturedTerm: q.reflect.Term,
      invocationArgument: q.reflect.Term
  ): Either[GeneratedClassLoweringError, q.reflect.Term] =
    import q.reflect.*

    val methodPlan = plan.overrideMethod
    val methodType = MethodType(List(methodPlan.parameter.displayName))(
      _ => List(parameterType),
      _ => resultType
    )
    var overrideMethod = Symbol.noSymbol
    val generatedClass = Symbol.newClass(
      Symbol.spliceOwner,
      Symbol.freshName(plan.classDisplayName),
      List(TypeRepr.of[Object], parentType),
      cls =>
        overrideMethod = Symbol.newMethod(
          cls,
          methodPlan.displayName,
          methodType,
          Flags.Override,
          Symbol.noSymbol
        )
        List(overrideMethod),
      selfType = None
    )

    if overrideMethod.owner != generatedClass then
      Left(
        GeneratedClassLoweringError(
          "DETACHED_METHOD_OWNER",
          "The reflected override symbol is not owned by the generated class."
        )
      )
    else
      var parameterFailure: Option[GeneratedClassLoweringError] = None
      val overrideDefinition = DefDef(overrideMethod, parameterClauses =>
        parameterClauses match
          case List(List(parameter)) =>
            val generatedParameter = Ref(parameter.symbol)
            val body = Select.overloaded(
              capturedTerm,
              "+",
              Nil,
              List(generatedParameter),
              resultType
            )
            Some(body)
          case _ =>
            parameterFailure = Some(
              GeneratedClassLoweringError(
                "MALFORMED_GENERATED_PARAMETER",
                "The generated override callback did not provide exactly one ordinary parameter."
              )
            )
            Some(Literal(UnitConstant()))
      )

      parameterFailure match
        case Some(error) => Left(error)
        case None =>
          val parentTree = typeTreeOf(parentType)
          val classDefinition = ClassDef(
            generatedClass,
            List(TypeTree.of[Object], parentTree),
            List(overrideDefinition)
          )
          val constructor = generatedClass.primaryConstructor
          val constructed = Apply(
            Select(New(TypeIdent(generatedClass)), constructor),
            Nil
          )
          val asParent = Typed(constructed, typeTreeOf(parentType))
          val invocation = Apply(
            Select(asParent, overrideMethod),
            List(invocationArgument)
          )
          Right(Block(List(classDefinition), invocation))

  private def typeTreeOf(using q: Quotes)(
      typeRepr: q.reflect.TypeRepr
  ): q.reflect.TypeTree =
    typeRepr.asType match
      case '[value] => q.reflect.TypeTree.of[value]

  private def rejectOverloadedParentMember(using q: Quotes)(
      parentType: q.reflect.TypeRepr,
      methodName: String
  ): Either[GeneratedClassLoweringError, Unit] =
    val inherited = parentType.typeSymbol.methodMember(methodName)
    if inherited.size <= 1 then Right(())
    else
      Left(
        GeneratedClassLoweringError(
          "OVERLOADED_PARENT_MEMBER",
          s"The inherited method name `$methodName` is overloaded; this lowerer does not select overloads."
        )
      )

  private def rejectOwnedDefinitionCapture(using q: Quotes)(
      capturedTerm: q.reflect.Term
  ): Either[GeneratedClassLoweringError, Unit] =
    import q.reflect.*
    var foundKind: Option[String] = None
    val traverser = new TreeTraverser:
      override def traverseTree(tree: Tree)(owner: Symbol): Unit =
        tree match
          case _: ValDef if foundKind.isEmpty => foundKind = Some("ValDef")
          case _: DefDef if foundKind.isEmpty => foundKind = Some("DefDef")
          case _: ClassDef if foundKind.isEmpty => foundKind = Some("ClassDef")
          case _ if foundKind.isEmpty => super.traverseTree(tree)(owner)
          case _ => ()
    traverser.traverseTree(capturedTerm)(Symbol.spliceOwner)
    foundKind match
      case None => Right(())
      case Some(kind) =>
        Left(
          GeneratedClassLoweringError(
            "CAPTURE_CONTAINS_OWNED_DEFINITION",
            s"A captured Term containing an owned $kind requires a separate rebuild/reownership contract."
          )
        )
