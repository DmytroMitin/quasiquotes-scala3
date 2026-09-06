package quasiquotes.neutral

import _root_.quasiquotes.definitions.*
import _root_.quasiquotes.parser.TermShape
import _root_.quasiquotes.terms.*
import _root_.quasiquotes.types.*

import scala.annotation.nowarn
import scala.meta.*

@nowarn("cat=deprecation")
final class ScalametaPublicSemanticDefinitionAuthoringTest extends munit.FunSuite:
  test("authors all five public semantic families and round-trips through the public projector"):
    val recursive = TypeNormalForm.STypeApply(
      TypeNormalForm.STypeIdent("Either"),
      List(
        TypeNormalForm.STypeApply(
          TypeNormalForm.STypeIdent("List"),
          List(intType)
        ),
        TypeNormalForm.STypeApply(
          TypeNormalForm.STypeIdent("Option"),
          List(stringType)
        )
      )
    )
    val value = immutable("answer", recursive, TermShape.Identifier("source", false))
    val parameterless = method("`type`", Vector.empty, intType)(_ => Right(TermShape.Literal("42")))
    val single = method("identity", Vector("value" -> intType), intType) { scope =>
      scope.reference(0, 0)
    }
    val pair = method(
      "pair",
      Vector("left" -> intType, "right" -> stringType),
      TypeNormalForm.STypeTuple(List(stringType, intType))
    ) { scope =>
      for
        left <- scope.reference(0, 0)
        right <- scope.reference(0, 1)
      yield TermShape.Tuple(List(right, left))
    }
    val alias = definitionRight(
      SemanticDefinition.typeAlias(name("Result"), recursive)
    )

    List(value, parameterless, single, pair, alias).foreach(assertPublicRoundTrip)

    val authoredParameterless = author(parameterless).asInstanceOf[Defn.Def]
    assertEquals(authoredParameterless.paramClauseGroups, Nil)

    val projectedPair = project(author(pair)).definition.asMethod.get
    val pairBody = projectedPair.body.get.asInstanceOf[TermShape.Tuple]
    val first = binder(projectedPair.parameterScope.reference(0, 0))
    val second = binder(projectedPair.parameterScope.reference(0, 1))
    assertEquals(
      projectedPair.parameterClauses.head.parameters.map(_.name.source),
      Vector("left", "right")
    )
    assert(boundBinder(pairBody.elements.head) == second)
    assert(boundBinder(pairBody.elements(1)) == first)
    assert(first != second)

  test("preserves one parameter through every admitted nested term position"):
    val definition = method("nested", Vector("value" -> intType), intType) { scope =>
      scope.reference(0, 0).map { reference =>
        TermShape.Tuple(
          List(
            reference,
            TermShape.Select(reference, "member"),
            TermShape.Apply(TermShape.Identifier("consume", false), List(reference)),
            TermShape.Infix(reference, "+", TermShape.Literal("1")),
            TermShape.If(TermShape.Identifier("condition", false), reference, TermShape.Literal("0")),
            TermShape.InterpolatedString("s", List("", ""), List(reference)),
            TermShape.Typed(reference, "Int")
          )
        )
      }
    }

    val projected = project(author(definition)).definition.asMethod.get
    val expected = binder(projected.parameterScope.reference(0, 0))
    assertEquals(collectBoundBinders(projected.body.get), List.fill(7)(expected))
    assertEquals(projected.parameterClauses.head.parameters.head.name.source, "value")

  test("keeps a same-spelling selected member non-binding"):
    val definition = method("selected", Vector("member" -> intType), intType) { _ =>
      Right(TermShape.Select(TermShape.Identifier("service", false), "member"))
    }

    val projected = project(author(definition)).definition.asMethod.get
    assertEquals(collectBoundBinders(projected.body.get), Nil)
    assertEquals(
      projected.body,
      Some(TermShape.Select(TermShape.Identifier("service", false), "member"))
    )

  test("fails closed for missing input broader Core terms resolved Types and Parenthesized Outcome-B"):
    assertCode(
      ScalametaDefinitionAuthoring.author(null),
      "NEUTRAL_DEFINITION_AUTHORING_MISSING"
    )

    val lambda = method("lambda", Vector.empty, intType) { _ =>
      TermShapeBindings
        .lambda(Vector(TermParameterSpec("inner", intType))) { scope =>
          scope.reference(scope.parameterBinders.head.head)
        }
        .left
        .map(problem => DefinitionSemanticError(problem.code, problem.detail))
    }
    assertCode(
      ScalametaDefinitionAuthoring.author(lambda),
      "NEUTRAL_DEFINITION_AUTHORING_PRIVATE_ADAPTER_FAILED"
    )

    val resolved = TypeNormalForm.STypeResolved(
      ResolvedTypeNameId(
        Vector(ResolvedTypeOwnerSegment(ResolvedTypeOwnerKind.Package, "scala")),
        "Int"
      )
    )
    assertCode(
      ScalametaDefinitionAuthoring.author(
        immutable("resolved", resolved, TermShape.Literal("1"))
      ),
      "NEUTRAL_DEFINITION_AUTHORING_TYPE_ADAPTER_FAILED"
    )

    val parenthesized = immutable(
      "parenthesized",
      intType,
      TermShape.Parenthesized(TermShape.Identifier("source", false))
    )
    assertCode(
      ScalametaDefinitionAuthoring.author(parenthesized),
      "NEUTRAL_DEFINITION_AUTHORING_PRIVATE_ADAPTER_FAILED"
    )

  test("rejects a free same-spelling identifier instead of rebinding it by name"):
    val definition = method("free", Vector("value" -> intType), intType) { _ =>
      Right(TermShape.Identifier("value", false))
    }

    assertCode(
      ScalametaDefinitionAuthoring.author(definition),
      "NEUTRAL_DEFINITION_AUTHORING_PRIVATE_ADAPTER_FAILED"
    )

  test("declaration snapshot catches parameter spelling drift beyond semantic equality"):
    val expected = method("identity", Vector("left" -> intType), intType)(_.reference(0, 0))
    val drifted = freshSingleParameterMethod("identity", "right")
    val projected = project(drifted).definition

    assertEquals(projected, expected)
    assertCode(
      ScalametaDefinitionAuthoring.validatePublicRoundTrip(expected, drifted),
      "NEUTRAL_DEFINITION_AUTHORING_ROUNDTRIP_FAILED"
    )

  test("declaration snapshot catches two-parameter source-order drift beyond semantic equality"):
    val expected = method(
      "pair",
      Vector("left" -> intType, "right" -> intType),
      intType
    ) { scope =>
      for
        first <- scope.reference(0, 0)
        second <- scope.reference(0, 1)
      yield TermShape.Tuple(List(first, second))
    }
    val drifted = freshTwoParameterMethod("pair", "right", "left")
    val projected = project(drifted).definition

    assertEquals(projected, expected)
    assertCode(
      ScalametaDefinitionAuthoring.validatePublicRoundTrip(expected, drifted),
      "NEUTRAL_DEFINITION_AUTHORING_ROUNDTRIP_FAILED"
    )

  private val intType = TypeNormalForm.STypeIdent("Int")
  private val stringType = TypeNormalForm.STypeIdent("String")

  private def assertPublicRoundTrip(definition: SemanticDefinition): Unit =
    val authored = author(definition)
    val projected = project(authored)
    assertEquals(projected.definition, definition)
    assertEquals(projected.sourceSpan, None)
    assert(allTrees(authored).forall(_.pos == Position.None))

  private def immutable(
      sourceName: String,
      declaredType: TypeNormalForm,
      body: TermShape
  ): SemanticDefinition =
    definitionRight(
      SemanticDefinition.immutableValue(name(sourceName), declaredType, body)
    )

  private def method(
      sourceName: String,
      parameters: Vector[(String, TypeNormalForm)],
      resultType: TypeNormalForm
  )(
      body: DefinitionParameterScope => Either[DefinitionSemanticError, TermShape]
  ): SemanticDefinition =
    val clauses =
      if parameters.isEmpty then Vector.empty
      else
        Vector(
          definitionRight(
            DefinitionParameterClause.ordinary(
              parameters.map { case (parameterName, parameterType) =>
                DefinitionParameter(name(parameterName), parameterType)
              }
            )
          )
        )
    definitionRight(
      SemanticDefinition.concreteMethod(name(sourceName), clauses, resultType)(body)
    )

  private def name(source: String): DefinitionName =
    definitionRight(DefinitionName.fromSource(source))

  private def author(definition: SemanticDefinition): Defn =
    ScalametaDefinitionAuthoring.author(definition)
      .fold(problem => fail(problem.message), identity)

  private def project(definition: Defn): ProjectedDefinition =
    ScalametaDefinitionProjection.project(definition)
      .fold(problem => fail(problem.message), identity)

  private def definitionRight[A](result: Either[DefinitionSemanticError, A]): A =
    result.fold(problem => fail(problem.message), identity)

  private def bindingRight[A](result: Either[TermBindingFailure, A]): A =
    result.fold(problem => fail(problem.message), identity)

  private def binder(result: Either[DefinitionSemanticError, TermShape]): TermBinder =
    boundBinder(definitionRight(result))

  private def boundBinder(shape: TermShape): TermBinder =
    bindingRight(TermShapeBindingView.inspect(shape)).boundReference
      .getOrElse(fail(s"expected bound reference, found ${shape.render}"))
      .binder

  private def collectBoundBinders(shape: TermShape): List[TermBinder] =
    val direct = bindingRight(TermShapeBindingView.inspect(shape)).boundReference.toList.map(_.binder)
    direct ++ (shape match
      case TermShape.Select(qualifier, _) => collectBoundBinders(qualifier)
      case TermShape.Apply(function, arguments) =>
        collectBoundBinders(function) ++ arguments.flatMap(collectBoundBinders)
      case TermShape.Infix(left, _, right) =>
        collectBoundBinders(left) ++ collectBoundBinders(right)
      case TermShape.Unary(_, operand) => collectBoundBinders(operand)
      case TermShape.InterpolatedString(_, _, arguments) => arguments.flatMap(collectBoundBinders)
      case TermShape.Typed(expression, _) => collectBoundBinders(expression)
      case TermShape.Tuple(elements) => elements.flatMap(collectBoundBinders)
      case TermShape.If(condition, thenBranch, elseBranch) =>
        collectBoundBinders(condition) ++ collectBoundBinders(thenBranch) ++ collectBoundBinders(elseBranch)
      case TermShape.Parenthesized(expression) => collectBoundBinders(expression)
      case _ => Nil)

  private def freshSingleParameterMethod(methodName: String, parameterName: String): Defn.Def =
    Defn.Def(
      Nil,
      Term.Name(methodName),
      List(
        Member.ParamClauseGroup(
          Type.ParamClause(Nil),
          List(
            Term.ParamClause(
              List(Term.Param(Nil, Term.Name(parameterName), Some(Type.Name("Int")), None))
            )
          )
        )
      ),
      Some(Type.Name("Int")),
      Term.Name(parameterName)
    )

  private def freshTwoParameterMethod(
      methodName: String,
      firstParameterName: String,
      secondParameterName: String
  ): Defn.Def =
    Defn.Def(
      Nil,
      Term.Name(methodName),
      List(
        Member.ParamClauseGroup(
          Type.ParamClause(Nil),
          List(
            Term.ParamClause(
              List(
                Term.Param(Nil, Term.Name(firstParameterName), Some(Type.Name("Int")), None),
                Term.Param(Nil, Term.Name(secondParameterName), Some(Type.Name("Int")), None)
              )
            )
          )
        )
      ),
      Some(Type.Name("Int")),
      Term.Tuple(List(Term.Name(firstParameterName), Term.Name(secondParameterName)))
    )

  private def assertCode[A](
      result: Either[ScalametaDefinitionAuthoring.Error, A],
      expected: String
  ): Unit =
    val problem = result.left.toOption.getOrElse(fail(s"expected $expected"))
    assertEquals(problem.code, expected)
    assertEquals(problem.message, s"${problem.code}: ${problem.detail}")

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
