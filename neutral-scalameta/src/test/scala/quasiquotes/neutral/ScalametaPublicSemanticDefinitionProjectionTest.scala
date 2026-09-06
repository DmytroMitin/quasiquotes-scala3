package quasiquotes.neutral

import _root_.quasiquotes.definitions.*
import _root_.quasiquotes.parser.{BinderId, BlockStatement, TermShape, TypeShape}
import _root_.quasiquotes.terms.{TermBinder, TermBindingFailure, TermShapeBindingView}
import _root_.quasiquotes.types.TypeNormalForm

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaPublicSemanticDefinitionProjectionTest extends munit.FunSuite:
  test("projects all five admitted families to exact public semantic views"):
    val value = project("val answer: Int = (42: Int)")
    val parameterless = project("def `type`: Int = 42")
    val single = project("def id(`type`: Int): Int = `type`")
    val pair = project("def choose(left: Int, right: Int): Int = (right, left)")
    val alias = project("type `type` = Either[List[Int], Option[String]]")

    assertEquals(value.definition.kind.code, "value")
    assertEquals(value.definition.name.source, "answer")
    assertEquals(value.definition.modifiers, DefinitionModifiers.empty)
    assertEquals(value.definition.asValue.get.declaredType, intType)
    assertEquals(
      value.definition.asValue.get.body,
      Some(TermShape.Typed(TermShape.Literal("42"), "Int"))
    )

    assertEquals(parameterless.definition.kind.code, "method")
    assertEquals(parameterless.definition.name.source, "`type`")
    assertEquals(parameterless.definition.modifiers, DefinitionModifiers.empty)
    assertEquals(parameterless.definition.asMethod.get.parameterClauses, Vector.empty)
    assertEquals(parameterless.definition.asMethod.get.resultType, intType)
    assertEquals(parameterless.definition.asMethod.get.body, Some(TermShape.Literal("42")))

    val singleView = single.definition.asMethod.get
    assertEquals(single.definition.kind.code, "method")
    assertEquals(single.definition.name.source, "id")
    assertEquals(single.definition.modifiers, DefinitionModifiers.empty)
    assertEquals(singleView.parameterClauses.map(_.parameters.map(_.name.source)), Vector(Vector("`type`")))
    assertEquals(singleView.parameterClauses.head.parameters.head.declaredType, intType)
    assertEquals(singleView.resultType, intType)
    assert(
      boundBinder(singleView.body.get) ==
        definitionRight(singleView.parameterScope.binder(0, 0))
    )

    val pairView = pair.definition.asMethod.get
    val tuple = pairView.body.get.asInstanceOf[TermShape.Tuple]
    val first = definitionRight(pairView.parameterScope.binder(0, 0))
    val second = definitionRight(pairView.parameterScope.binder(0, 1))
    val firstReference = boundBinder(
      definitionRight(pairView.parameterScope.reference(0, 0))
    )
    val secondReference = boundBinder(
      definitionRight(pairView.parameterScope.reference(0, 1))
    )
    assertEquals(
      pairView.parameterClauses.head.parameters.map(_.name.source),
      Vector("left", "right")
    )
    assertEquals(pair.definition.kind.code, "method")
    assertEquals(pair.definition.modifiers, DefinitionModifiers.empty)
    assert(boundBinder(tuple.elements.head) == second)
    assert(boundBinder(tuple.elements(1)) == first)
    assert(firstReference == first)
    assert(secondReference == second)
    assertNotEquals(first, second)

    assertEquals(alias.definition.kind.code, "type-member")
    assertEquals(alias.definition.name.source, "`type`")
    assertEquals(alias.definition.modifiers, DefinitionModifiers.empty)
    assertEquals(
      alias.definition.asType.get.aliasedType,
      Some(
        TypeNormalForm.STypeApply(
          TypeNormalForm.STypeIdent("Either"),
          List(
            TypeNormalForm.STypeApply(TypeNormalForm.STypeIdent("List"), List(intType)),
            TypeNormalForm.STypeApply(TypeNormalForm.STypeIdent("Option"), List(stringType))
          )
        )
      )
    )

  test("maps one method parameter through every admitted nested body position"):
    val rows = List(
      "def select(x: Int): Int = x.value",
      "def apply(x: Int): Int = f(x)",
      "def combine(x: Int): Int = x + 1",
      "def tuple(x: Int): Int = (x, 1)",
      "def branch(x: Int): Int = if cond then x else 0",
      "def interpolate(x: Int): Int = s\"$x\"",
      "def typed(x: Int): Int = (x: Int)"
    )

    rows.foreach { source =>
      val view = project(source).definition.asMethod.get
      val parameter = definitionRight(view.parameterScope.binder(0, 0))
      val references = collectBoundBinders(view.body.get)
      assert(references == List(parameter), clues(source, view.body))
    }

    val selectedMember = project("def selected(x: Int): Int = obj.x").definition.asMethod.get
    assertEquals(collectBoundBinders(selectedMember.body.get), Nil)
    assertEquals(selectedMember.body, Some(TermShape.Select(TermShape.Identifier("obj", false), "x")))

  test("preserves exact root provenance for parsed and fresh definitions"):
    val sources = List(
      "val answer: Int = 42",
      "def answer: Int = 42",
      "def id(x: Int): Int = x",
      "def choose(left: Int, right: Int): Int = right",
      "type Result = Either[List[Int], Option[String]]"
    )

    sources.foreach { source =>
      val parsedDefinition = parsed(source)
      val positioned = ScalametaDefinitionProjection.project(parsedDefinition)
        .fold(problem => fail(problem.message), identity)
      assertEquals(
        positioned.sourceSpan,
        Some(NeutralSourceSpan(parsedDefinition.pos.start, parsedDefinition.pos.end)),
        clues(source)
      )
      assertEquals(
        positioned.sourceSpan,
        ScalametaDefinitionProjection.projectShape(parsedDefinition).toOption.get.sourceSpan,
        clues(source)
      )

      val freshDefinition = fresh(parsed(source))
      assertEquals(freshDefinition.pos, Position.None, clues(source))
      assertEquals(
        ScalametaDefinitionProjection.project(freshDefinition).toOption.map(_.sourceSpan),
        Some(None),
        clues(source)
      )
    }

  test("preserves outer references and nested Lambda P2 and P3 binder roles without capture"):
    val outerPrivate = BinderId(99)

    val lambdaView = adaptedMethod(
      TermShape.Lambda1(
        BinderId(0),
        "inner",
        "Int",
        TermShape.Tuple(
          List(
            TermShape.BoundReference(BinderId(0), "inner"),
            TermShape.BoundReference(outerPrivate, "outer")
          )
        )
      ),
      outerPrivate
    )
    val lambdaBody = lambdaView.body.get
    val lambda = bindingRight(TermShapeBindingView.inspect(lambdaBody)).lambda.get
    val lambdaTuple = lambda.body.asInstanceOf[TermShape.Tuple]
    assert(boundBinder(lambdaTuple.elements.head) == lambda.parameters.head.binder)
    assert(boundBinder(lambdaTuple.elements(1)) == outerBinder(lambdaView))
    assert(lambda.parameters.head.binder != outerBinder(lambdaView))

    val p2View = adaptedMethod(
      TermShape.Block(
        List(
          BlockStatement.LocalVal(
            BinderId(0),
            "local",
            "Int",
            TermShape.BoundReference(outerPrivate, "outer")
          )
        ),
        TermShape.Tuple(
          List(
            TermShape.BoundReference(BinderId(0), "local"),
            TermShape.BoundReference(outerPrivate, "outer")
          )
        )
      ),
      outerPrivate
    )
    val p2 = bindingRight(TermShapeBindingView.inspect(p2View.body.get)).block.get
    val p2Tuple = p2.result.asInstanceOf[TermShape.Tuple]
    assert(boundBinder(p2.locals.head.body.get) == outerBinder(p2View))
    assert(boundBinder(p2Tuple.elements.head) == p2.locals.head.binder)
    assert(boundBinder(p2Tuple.elements(1)) == outerBinder(p2View))
    assert(p2.locals.head.binder != outerBinder(p2View))

    val p3View = adaptedMethod(
      TermShape.Block(
        List(
          BlockStatement.LocalDef(
            BinderId(0),
            "identity",
            BinderId(1),
            "value",
            TypeShape.Identifier("Int"),
            TypeShape.Identifier("Int"),
            TermShape.Tuple(
              List(
                TermShape.BoundReference(BinderId(1), "value"),
                TermShape.BoundReference(outerPrivate, "outer")
              )
            )
          )
        ),
        TermShape.Apply(
          TermShape.BoundReference(BinderId(0), "identity"),
          List(TermShape.BoundReference(outerPrivate, "outer"))
        )
      ),
      outerPrivate
    )
    val p3 = bindingRight(TermShapeBindingView.inspect(p3View.body.get)).block.get
    val localMethod = p3.locals.head
    val localTuple = localMethod.body.get.asInstanceOf[TermShape.Tuple]
    val resultApply = p3.result.asInstanceOf[TermShape.Apply]
    assert(
      boundBinder(localTuple.elements.head) ==
        localMethod.parameterClauses.head.head.binder
    )
    assert(boundBinder(localTuple.elements(1)) == outerBinder(p3View))
    assert(boundBinder(resultApply.function) == localMethod.binder)
    assert(boundBinder(resultApply.arguments.head) == outerBinder(p3View))
    assert(localMethod.binder != outerBinder(p3View))

  test("retains private dispatcher rejection categories at the public boundary"):
    val rows = List(
      parsed("var answer: Int = 42") -> "NEUTRAL_DEFINITION_FAMILY_UNSUPPORTED",
      parsed("class Answer") -> "NEUTRAL_DEFINITION_FAMILY_UNSUPPORTED",
      parsed("def answer(): Int = 42") -> "NEUTRAL_DEFINITION_FAMILY_UNSUPPORTED",
      parsed("def wide(a: Int, b: Int, c: Int): Int = a") ->
        "NEUTRAL_DEFINITION_FAMILY_UNSUPPORTED",
      parsed("def contextual(using value: Int): Int = value") ->
        "NEUTRAL_SINGLE_PARAMETER_DEF_TOPOLOGY_UNSUPPORTED",
      parsed("def specialized(using value: Int): Int = summon[Int]") ->
        "NEUTRAL_SINGLE_PARAMETER_DEF_TOPOLOGY_UNSUPPORTED",
      parsed("opaque type Result = Int") -> "NEUTRAL_SIMPLE_TYPE_ALIAS_TOPOLOGY_UNSUPPORTED",
      parsed("type Result[A] = A") -> "NEUTRAL_SIMPLE_TYPE_ALIAS_TOPOLOGY_UNSUPPORTED",
      parsed("type Result >: Int <: AnyVal = Int") -> "NEUTRAL_SIMPLE_TYPE_ALIAS_TOPOLOGY_UNSUPPORTED",
      parsed("private val answer: Int = 42") -> "NEUTRAL_TYPED_VAL_TOPOLOGY_UNSUPPORTED",
      parsed("val answer = 42") -> "NEUTRAL_TYPED_VAL_TOPOLOGY_UNSUPPORTED",
      parsed("def answer = 42") -> "NEUTRAL_PARAMETERLESS_DEF_TOPOLOGY_UNSUPPORTED",
      parsed("def id[A](x: A): A = x") -> "NEUTRAL_SINGLE_PARAMETER_DEF_TOPOLOGY_UNSUPPORTED",
      parsed("inline def answer: Int = 42") -> "NEUTRAL_PARAMETERLESS_DEF_TOPOLOGY_UNSUPPORTED",
      parsed("def recurse(x: Int): Int = recurse") ->
        "NEUTRAL_DEFINITION_RECURSION_UNSUPPORTED",
      parsed("def id(x: Int): Int = value match { case _ => x }") ->
        "NEUTRAL_DEFINITION_BODY_UNSUPPORTED"
    )

    rows.foreach { case (definition, code) =>
      assertEquals(
        ScalametaDefinitionProjection.project(definition).left.toOption.map(_.code),
        Some(code),
        clues(definition)
      )
    }
    assertEquals(
      ScalametaDefinitionProjection.project(null).left.toOption.map(_.code),
      Some("NEUTRAL_DEFINITION_MISSING")
    )

  private val intType = TypeNormalForm.STypeIdent("Int")
  private val stringType = TypeNormalForm.STypeIdent("String")

  private def project(source: String): ProjectedDefinition =
    ScalametaDefinitionProjection.project(parsed(source))
      .fold(problem => fail(s"$source: ${problem.message}"), identity)

  private def parsed(source: String): Defn =
    Scala3(source).parse[Stat].get match
      case definition: Defn => definition
      case other => fail(s"expected Defn, found ${other.productPrefix}")

  private def fresh(definition: Defn): Defn =
    definition match
      case value: Defn.Val => value.copy()
      case method: Defn.Def => method.copy()
      case alias: Defn.Type => alias.copy()
      case other => fail(s"expected admitted Defn, found ${other.productPrefix}")

  private def definitionRight[A](
      result: Either[DefinitionSemanticError, A]
  ): A =
    result.fold(problem => fail(problem.message), identity)

  private def adaptedMethod(
      body: TermShape,
      outerPrivate: BinderId
  ): MethodDefinitionView =
    val parameter = DefinitionParameter(
      DefinitionName.plain("outer").toOption.get,
      intType
    )
    val clause = definitionRight(DefinitionParameterClause.ordinary(Vector(parameter)))
    definitionRight(
      SemanticDefinition.concreteMethod(
        DefinitionName.plain("method").toOption.get,
        Vector(clause),
        intType
      ) { scope =>
        scope.reference(0, 0).map { reference =>
          ScalametaDefinitionProjection.substituteDefinitionParameters(
            body,
            Map(outerPrivate -> reference)
          )
        }
      }
    ).asMethod.get

  private def outerBinder(view: MethodDefinitionView): TermBinder =
    definitionRight(view.parameterScope.binder(0, 0))

  private def bindingRight[A](result: Either[TermBindingFailure, A]): A =
    result.fold(problem => fail(problem.message), identity)

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
      case TermShape.InterpolatedString(_, _, arguments) =>
        arguments.flatMap(collectBoundBinders)
      case TermShape.Typed(expression, _) => collectBoundBinders(expression)
      case TermShape.Tuple(elements) => elements.flatMap(collectBoundBinders)
      case TermShape.If(condition, thenBranch, elseBranch) =>
        collectBoundBinders(condition) ++
          collectBoundBinders(thenBranch) ++
          collectBoundBinders(elseBranch)
      case TermShape.Parenthesized(expression) => collectBoundBinders(expression)
      case _ => Nil)
