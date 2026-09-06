package quasiquotes.neutral

import _root_.quasiquotes.publicapi.{
  CompletedTerm,
  CompletedType,
  DefinitionConstruction,
  DefinitionResultView
}

import scala.meta.{Position, Tree}

final class ScalametaContextualMethodAuthoringTest extends munit.FunSuite:
  test("authors the full positive matrix through an exact unpositioned reprojection"):
    val rows = List(
      contextualMethod("identity", "A", "ctx", typeParameter("A"), typeParameter("A")),
      contextualMethod("constant", "A", "ctx", named("Context"), named("Result")),
      contextualMethod("consume", "A", "ctx", applied("Ctx", typeParameter("A")), named("Result")),
      contextualMethod("produce", "A", "ctx", named("Context"), applied("Out", typeParameter("A"))),
      contextualMethod(
        "nested",
        "A",
        "ctx",
        applied("Ctx", applied("List", typeParameter("A"))),
        applied("Out", applied("Option", typeParameter("A")))
      ),
      contextualMethod("sameSpelling", "A", "A", typeParameter("A"), typeParameter("A"))
    )

    rows.foreach { expected =>
      val authored = author(expected)
      assert(allTrees(authored).forall(_.pos == Position.None), clues(expected))
      assertEquals(
        ScalametaContextualMethodProjection.project(authored),
        Right(ProjectedContextualMethod(expected, None))
      )
    }

  test("reports the stable missing category for a null root"):
    assertErrorCode(
      ScalametaContextualMethodAuthoring.author(null),
      "NEUTRAL_CONTEXTUAL_METHOD_AUTHORING_MISSING"
    )

  test("rejects a constructible definition-parameter body without erasing its role"):
    val input = contextualMethod(
      "identity",
      "A",
      "ctx",
      typeParameter("A"),
      typeParameter("A"),
      definitionParameterReference("ctx")
    )

    assertErrorCode(
      ScalametaContextualMethodAuthoring.author(input),
      "NEUTRAL_CONTEXTUAL_METHOD_AUTHORING_BODY_UNSUPPORTED"
    )

  test("rejects ordinary named occurrences that collide with the declared Type parameter"):
    val rows = List(
      contextualMethod("parameterCollision", "A", "ctx", named("A"), named("Result")),
      contextualMethod("resultCollision", "A", "ctx", named("Context"), named("A")),
      contextualMethod(
        "constructorCollision",
        "A",
        "ctx",
        applied(named("A"), typeParameter("A")),
        named("Result")
      )
    )

    rows.foreach { input =>
      assertErrorCode(
        ScalametaContextualMethodAuthoring.author(input),
        "NEUTRAL_CONTEXTUAL_METHOD_AUTHORING_TYPE_UNSUPPORTED",
        input.name
      )
    }

  test("keeps existing wrong-clause and wrong-topology projection controls rejected"):
    val authored = author(
      contextualMethod("identity", "A", "ctx", typeParameter("A"), typeParameter("A"))
    )
    val group = authored.paramClauseGroups.head
    val clause = group.paramClauses.head
    val ordinary = withParameterGroups(
      authored,
      List(group.copy(paramClauses = List(clause.copy(mod = None))))
    )
    val duplicateGroup = withParameterGroups(authored, List(group, group))

    assertProjectionCode(ordinary, "NEUTRAL_CONTEXTUAL_CLAUSE_UNSUPPORTED")
    assertProjectionCode(duplicateGroup, "NEUTRAL_PARAMETER_GROUPS_UNSUPPORTED")

  private def contextualMethod(
      methodName: String,
      typeParameterName: String,
      contextualParameterName: String,
      contextualType: CompletedType,
      resultType: CompletedType,
      body: CompletedTerm = null
  ): DefinitionResultView =
    val completedBody = Option(body).getOrElse(reference(contextualParameterName))
    DefinitionConstruction
      .contextualMethod(
        methodName,
        typeParameterName,
        contextualParameterName,
        contextualType,
        resultType,
        completedBody
      )
      .fold(problem => fail(problem.message), identity)

  private def named(value: String): CompletedType =
    CompletedType.named(value).fold(problem => fail(problem.message), identity)

  private def typeParameter(value: String): CompletedType =
    CompletedType.typeParameter(value).fold(problem => fail(problem.message), identity)

  private def applied(constructor: String, argument: CompletedType): CompletedType =
    applied(named(constructor), argument)

  private def applied(constructor: CompletedType, argument: CompletedType): CompletedType =
    CompletedType
      .applied(constructor, Vector(argument))
      .fold(problem => fail(problem.message), identity)

  private def reference(value: String): CompletedTerm =
    CompletedTerm.reference(value).fold(problem => fail(problem.message), identity)

  private def definitionParameterReference(value: String): CompletedTerm =
    CompletedTerm
      .definitionParameterReference(value)
      .fold(problem => fail(problem.message), identity)

  private def author(input: DefinitionResultView): scala.meta.Defn.Def =
    ScalametaContextualMethodAuthoring
      .author(input)
      .fold(problem => fail(problem.message), identity)

  private def assertErrorCode[A](
      result: Either[ScalametaContextualMethodAuthoring.Error, A],
      expected: String,
      clue: String = ""
  ): Unit =
    assertEquals(result.left.toOption.map(_.code), Some(expected), clues(clue, result))

  private def assertProjectionCode(
      definition: scala.meta.Defn.Def,
      expected: String
  ): Unit =
    assertEquals(
      ScalametaContextualMethodProjection.project(definition).left.toOption.map(_.code),
      Some(expected)
    )

  private def withParameterGroups(
      definition: scala.meta.Defn.Def,
      groups: List[scala.meta.Member.ParamClauseGroup]
  ): scala.meta.Defn.Def =
    scala.meta.Defn.Def(
      definition.mods,
      definition.name,
      groups,
      definition.decltpe,
      definition.body
    )

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
