package quasiquotes.phase139

import scala.annotation.experimental
import scala.quoted.*

import quasiquotes.construct.*

trait Phase139Mapper:
  def combine(value: Int): Int

trait Phase139NoMatchingMember:
  def other(value: Int): Int

trait Phase139OverloadedMapper:
  def combine(value: Int): Int
  def combine(value: String): String

trait Phase139StringParameterMapper:
  def combine(value: String): Int

trait Phase139StringResultMapper:
  def combine(value: Int): String

trait Phase139FinalMapper:
  final def combine(value: Int): Int = value

final case class Phase139GeneratedEvidence(
    result: Int,
    classOwnedBySplice: Boolean,
    methodOwnedByClass: Boolean,
    constructorOwnedByClass: Boolean,
    methodHasOverrideFlag: Boolean,
    overridesRequestedParentMember: Boolean,
    callerTermObjectRetained: Boolean,
    invocationArgumentObjectRetained: Boolean,
    bodyUsesGeneratedParameterRefExactlyOnce: Boolean,
    classTreeUsesRequestedParent: Boolean,
    invocationUsesPrimaryConstructor: Boolean,
    generatedClassNameUsesDisplayPrefix: Boolean,
    generatedMethodName: String,
    generatedParameterName: String
)

@experimental
object Phase139GeneratedClassLowererProbe:
  inline def generated[P <: Phase139Mapper](
      inline captured: Int,
      inline input: Int,
      inline classDisplayName: String,
      inline methodDisplayName: String,
      inline parameterDisplayName: String
  ): Phase139GeneratedEvidence =
    ${ generatedImpl[P]('captured, 'input, 'classDisplayName, 'methodDisplayName, 'parameterDisplayName) }

  inline def rejectDetachedMethodOwner(): Unit =
    ${ rejectDetachedMethodOwnerImpl }

  inline def rejectOverloadedParent(): Unit =
    ${ rejectOverloadedParentImpl }

  inline def rejectInvalidDisplayNames(): Unit =
    ${ rejectInvalidDisplayNamesImpl }

  inline def rejectMalformedBodyBinder(): Unit =
    ${ rejectMalformedBodyBinderImpl }

  inline def rejectOwnedValCapture(): Unit =
    ${ rejectOwnedValCaptureImpl }

  inline def rejectOwnedDefCapture(): Unit =
    ${ rejectOwnedDefCaptureImpl }

  inline def rejectOwnedClassCapture(): Unit =
    ${ rejectOwnedClassCaptureImpl }

  inline def compilerRejectsNoMatchingMember(): Int =
    ${ invalidOverrideImpl[Phase139NoMatchingMember]("combine", Type.of[Int], Type.of[Int]) }

  inline def compilerRejectsIncompatibleParameter(): Int =
    ${ invalidOverrideImpl[Phase139StringParameterMapper]("combine", Type.of[Int], Type.of[Int]) }

  inline def compilerRejectsIncompatibleResult(): Int =
    ${ invalidOverrideImpl[Phase139StringResultMapper]("combine", Type.of[Int], Type.of[Int]) }

  inline def compilerRejectsFinalMember(): Int =
    ${ invalidOverrideImpl[Phase139FinalMapper]("combine", Type.of[Int], Type.of[Int]) }

  private def generatedImpl[P <: Phase139Mapper: Type](
      captured: Expr[Int],
      input: Expr[Int],
      classDisplayNameExpression: Expr[String],
      methodDisplayNameExpression: Expr[String],
      parameterDisplayNameExpression: Expr[String]
  )(using Quotes): Expr[Phase139GeneratedEvidence] =
    import quotes.reflect.*

    val classDisplayName = classDisplayNameExpression.valueOrAbort
    val methodDisplayName = methodDisplayNameExpression.valueOrAbort
    val parameterDisplayName = parameterDisplayNameExpression.valueOrAbort
    val plan = validPlan(classDisplayName, methodDisplayName, parameterDisplayName)
    val originalCapturedTerm = captured.asTerm
    val originalInvocationArgument = input.asTerm
    val lowered = PublicReflectionGeneratedClassLowerer
      .lower(
        plan,
        TypeRepr.of[P],
        TypeRepr.of[Int],
        TypeRepr.of[Int],
        originalCapturedTerm,
        originalInvocationArgument
      )
      .fold(abortPlan, identity)

    val classDefinition = lowered match
      case Block(List(definition: ClassDef), _) => definition
      case other => report.errorAndAbort(s"PHASE139_UNEXPECTED_LOWERED_TREE: ${other.show}")
    val overrideDefinition = classDefinition.body.collectFirst {
      case definition: DefDef if definition.symbol.flags.is(Flags.Override) => definition
    }.getOrElse(report.errorAndAbort("PHASE139_MISSING_OVERRIDE_DEFINITION"))
    val generatedClass = classDefinition.symbol
    val overrideMethod = overrideDefinition.symbol
    val constructor = generatedClass.primaryConstructor
    val parameterDefinition: ValDef = overrideDefinition.termParamss match
      case List(List(parameter)) => parameter.asInstanceOf[ValDef]
      case _ => report.errorAndAbort("PHASE139_UNEXPECTED_GENERATED_PARAMETER_SHAPE")
    var callerTermObjectRetained = false
    var invocationArgumentObjectRetained = false
    val identityCheck = new TreeTraverser:
      override def traverseTree(tree: Tree)(owner: Symbol): Unit =
        if tree.asInstanceOf[AnyRef] eq originalCapturedTerm.asInstanceOf[AnyRef] then
          callerTermObjectRetained = true
        if tree.asInstanceOf[AnyRef] eq originalInvocationArgument.asInstanceOf[AnyRef] then
          invocationArgumentObjectRetained = true
        super.traverseTree(tree)(owner)
    identityCheck.traverseTree(lowered)(Symbol.spliceOwner)

    var generatedParameterRefCount = 0
    val parameterRefCheck = new TreeTraverser:
      override def traverseTree(tree: Tree)(owner: Symbol): Unit =
        tree match
          case term: Term if term.symbol == parameterDefinition.symbol =>
            generatedParameterRefCount += 1
          case _ => ()
        super.traverseTree(tree)(owner)
    overrideDefinition.rhs.foreach(parameterRefCheck.traverseTree(_)(overrideMethod))

    val classTreeUsesRequestedParent = classDefinition.parents.exists {
      case parent: TypeTree => parent.tpe =:= TypeRepr.of[P]
      case _ => false
    }
    val invocationUsesPrimaryConstructor = lowered match
      case Block(_, Apply(Select(Typed(Apply(selection: Select, Nil), _), _), _)) =>
        selection.qualifier match
          case New(_) => selection.symbol == constructor
          case _ => false
      case _ => false

    val result = lowered.asExprOf[Int]
    '{
      Phase139GeneratedEvidence(
        $result,
        ${ Expr(generatedClass.owner == Symbol.spliceOwner) },
        ${ Expr(overrideMethod.owner == generatedClass) },
        ${ Expr(constructor.owner == generatedClass) },
        ${ Expr(overrideMethod.flags.is(Flags.Override)) },
        ${ Expr(overrideMethod.allOverriddenSymbols.map(_.name).toList == List(methodDisplayName)) },
        ${ Expr(callerTermObjectRetained) },
        ${ Expr(invocationArgumentObjectRetained) },
        ${ Expr(generatedParameterRefCount == 1) },
        ${ Expr(classTreeUsesRequestedParent) },
        ${ Expr(invocationUsesPrimaryConstructor) },
        ${ Expr(generatedClass.name.startsWith(classDisplayName)) },
        ${ Expr(overrideMethod.name) },
        ${ Expr(parameterDefinition.name) }
      )
    }

  private def rejectDetachedMethodOwnerImpl(using Quotes): Expr[Unit] =
    import quotes.reflect.*
    val plan = validPlan("DetachedMapper", "combine", "value").copy(
      overrideMethod = validPlan("DetachedMapper", "combine", "value").overrideMethod.copy(
        owner = GeneratedMethodOwnerPlan.ActiveSplice
      )
    )
    lowerUnit(plan, TypeRepr.of[Phase139Mapper], Literal(IntConstant(1)))

  private def rejectOverloadedParentImpl(using Quotes): Expr[Unit] =
    import quotes.reflect.*
    lowerUnit(
      validPlan("OverloadedMapper", "combine", "value"),
      TypeRepr.of[Phase139OverloadedMapper],
      Literal(IntConstant(1))
    )

  private def rejectInvalidDisplayNamesImpl(using Quotes): Expr[Unit] =
    import quotes.reflect.*
    val invalidPlans = List(
      validPlan("Invalid Mapper", "combine", "value"),
      validPlan("ValidMapper", "class", "value"),
      validPlan("ValidMapper", "combine", "value-name")
    )
    invalidPlans.foreach { plan =>
      PublicReflectionGeneratedClassLowerer
        .lower(
          plan,
          TypeRepr.of[Phase139Mapper],
          TypeRepr.of[Int],
          TypeRepr.of[Int],
          Literal(IntConstant(1)),
          Literal(IntConstant(1))
        ) match
        case Left(_) => ()
        case Right(_) => report.errorAndAbort("PHASE139_INVALID_DISPLAY_NAME_WAS_ADMITTED")
    }
    report.errorAndAbort("PHASE139_PLAN_REJECTED_INVALID_DISPLAY_NAMES")

  private def rejectMalformedBodyBinderImpl(using Quotes): Expr[Unit] =
    import quotes.reflect.*
    val valid = validPlan("MalformedBinderMapper", "combine", "value")
    val malformed = valid.copy(
      overrideMethod = valid.overrideMethod.copy(
        body = GeneratedMethodBodyPlan.CapturedTermPlusParameter(GeneratedParameterBinderId(99))
      )
    )
    lowerUnit(malformed, TypeRepr.of[Phase139Mapper], Literal(IntConstant(1)))

  private def rejectOwnedValCaptureImpl(using Quotes): Expr[Unit] =
    import quotes.reflect.*
    lowerUnit(
      validPlan("OwnedValMapper", "combine", "value"),
      TypeRepr.of[Phase139Mapper],
      '{ val owned = 1; owned }.asTerm
    )

  private def rejectOwnedDefCaptureImpl(using Quotes): Expr[Unit] =
    import quotes.reflect.*
    lowerUnit(
      validPlan("OwnedDefMapper", "combine", "value"),
      TypeRepr.of[Phase139Mapper],
      '{ def owned: Int = 1; owned }.asTerm
    )

  private def rejectOwnedClassCaptureImpl(using Quotes): Expr[Unit] =
    import quotes.reflect.*
    lowerUnit(
      validPlan("OwnedClassMapper", "combine", "value"),
      TypeRepr.of[Phase139Mapper],
      '{ class Owned; 1 }.asTerm
    )

  private def invalidOverrideImpl[P: Type](
      methodName: String,
      parameterType: Type[?],
      resultType: Type[?]
  )(using Quotes): Expr[Int] =
    import quotes.reflect.*
    val parameterRepr = parameterType match
      case '[parameter] => TypeRepr.of[parameter]
    val resultRepr = resultType match
      case '[result] => TypeRepr.of[result]
    PublicReflectionGeneratedClassLowerer
      .lower(
        validPlan("CompilerRejectedMapper", methodName, "value"),
        TypeRepr.of[P],
        parameterRepr,
        resultRepr,
        Literal(IntConstant(1)),
        Literal(IntConstant(1))
      )
      .fold(abortPlan, _.asExprOf[Int])

  private def validPlan(
      classDisplayName: String,
      methodDisplayName: String,
      parameterDisplayName: String
  ): GeneratedClassPlan =
    val binder = GeneratedParameterBinderId(0)
    GeneratedClassPlan(
      classDisplayName,
      GeneratedClassOwnerPlan.ActiveSplice,
      GeneratedParentPlan.CallerProvidedCompleteType,
      OverrideMethodPlan(
        methodDisplayName,
        GeneratedMethodOwnerPlan.GeneratedClass,
        GeneratedParameterPlan(parameterDisplayName, binder),
        GeneratedMethodBodyPlan.CapturedTermPlusParameter(binder)
      ),
      GeneratedConstructorPlan.ParameterlessPrimary
    )

  private def lowerUnit(using q: Quotes)(
      plan: GeneratedClassPlan,
      parentType: q.reflect.TypeRepr,
      captured: q.reflect.Term
  ): Expr[Unit] =
    import q.reflect.*
    PublicReflectionGeneratedClassLowerer
      .lower(
        plan,
        parentType,
        TypeRepr.of[Int],
        TypeRepr.of[Int],
        captured,
        Literal(IntConstant(1))
      ) match
      case Left(error) => report.errorAndAbort(s"PHASE139_PLAN_REJECTED_${error.code}: ${error.message}")
      case Right(_) => report.errorAndAbort("PHASE139_EXPECTED_PLAN_REJECTION")

  private def abortPlan(error: GeneratedClassLoweringError)(using Quotes): Nothing =
    quotes.reflect.report.errorAndAbort(s"PHASE139_PLAN_REJECTED_${error.code}: ${error.message}")
