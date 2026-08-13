package quasiquotes.definitions

import quasiquotes.parser.{BinderId, TermShape, TypeShape}
import quasiquotes.source.*
import quasiquotes.terms.*
import quasiquotes.types.{TypeNormalForm, TypeTemplate}

class SingleParameterDefinitionCoreTest extends munit.FunSuite:
  private val methodName = name("f")
  private val xName = name("x")
  private val yName = name("y")
  private val intShape = TypeShape.Identifier("Int")
  private val stringShape = TypeShape.Identifier("String")
  private val intForm = TypeNormalForm.STypeIdent("Int")
  private val stringForm = TypeNormalForm.STypeIdent("String")
  private val booleanForm = TypeNormalForm.STypeIdent("Boolean")

  test("single-parameter shapes admit id inc and keep without exposing binder identity") {
    val id = shape(0, xName, intShape, intShape, bound(0, "x"))
    val inc = shape(
      1,
      xName,
      intShape,
      intShape,
      TermShape.Infix(bound(1, "x"), "+", TermShape.Literal("1"))
    )
    val keep = shape(2, xName, stringShape, stringShape, bound(2, "x"))

    assertEquals(id.parameterName, xName)
    assertEquals(id.parameterType, intShape)
    assertEquals(id.resultType, intShape)
    assertEquals(id.body, bound(0, "x"))
    assertEquals(inc.body.render, "Infix(BoundRef(x), +, Literal(1))")
    assertEquals(keep.parameterType, stringShape)
    assert(!id.render.contains("BinderId"))
    assert(!id.render.contains("binder identity"))
  }

  test("shape equality is alpha-structural and keeps free same-text references distinct") {
    val left = shape(0, xName, intShape, intShape, bound(0, "x"))
    val renamed = shape(17, yName, intShape, intShape, bound(17, "y"))
    val free = shape(17, yName, intShape, intShape, ident("x"))
    val otherResult = shape(17, yName, intShape, stringShape, bound(17, "y"))

    assertEquals(left, renamed)
    assertEquals(left.hashCode, renamed.hashCode)
    assertNotEquals(left, free)
    assertNotEquals(left, otherResult)
  }

  test("shape factory rejects foreign scope and unsupported parameter or result types") {
    val foreign = DefinitionShape.singleParameterDef(
      methodName,
      BinderId(0),
      xName,
      intShape,
      intShape,
      bound(1, "x")
    )
    val unsupportedParameter = DefinitionShape.singleParameterDef(
      methodName,
      BinderId(0),
      xName,
      TypeShape.Identifier("Long"),
      intShape,
      bound(0, "x")
    )
    val unsupportedResult = DefinitionShape.singleParameterDef(
      methodName,
      BinderId(0),
      xName,
      intShape,
      TypeShape.Identifier("Long"),
      bound(0, "x")
    )

    assertEquals(
      foreign.left.toOption.get.message,
      "Unsupported method body: bound references must resolve to the single ordinary method parameter."
    )
    assertEquals(
      unsupportedParameter,
      Left(DefinitionError.UnsupportedDefinitionType("method parameter type"))
    )
    assertEquals(
      unsupportedResult,
      Left(DefinitionError.UnsupportedDefinitionType("method result type"))
    )
    assert(!foreign.left.toOption.get.message.exists(_.isDigit))
  }

  test("parameterless methods preserve their existing bound-reference rejection") {
    assertEquals(
      DefinitionShape.parameterlessDef(methodName, intShape, bound(0, "x")),
      Left(
        DefinitionError.UnsupportedDefinitionBody(
          "method body",
          "lambda-bound references are only valid inside the bounded Lambda1 term tranche"
        )
      )
    )
  }

  test("single-parameter templates traverse parameter result and body bindings deterministically") {
    val binder = BinderId(0)
    val body = scopedTemplate(
      binder,
      TermShape.Tuple(
        List(
          bound(0, "x"),
          TermShape.Typed(ident("__term_tail", placeholder = true), "__type_body")
        )
      ),
      termEntries = Vector("tail" -> "__term_tail"),
      termOccurrences = Vector(TermHoleOccurrence("tail", 0)),
      typeEntries = Vector("bodyType" -> "__type_body"),
      ascriptions = Vector(TypeTemplate.TTHole("bodyType"))
    )
    val template = DefinitionTemplate
      .singleParameterDef(
        methodName,
        binder,
        xName,
        TypeTemplate.TTHole("parameterType"),
        TypeTemplate.TTHole("resultType"),
        body
      )
      .fold(error => fail(error.message), identity)

    assertEquals(template.requiredTermBindings, Vector("tail"))
    assertEquals(
      template.requiredTypeBindings,
      Vector("parameterType", "resultType", "bodyType")
    )
    assertEquals(
      template.complete(Map.empty, Map.empty),
      Left(DefinitionConstructionError.MissingTermBinding("tail"))
    )
    assertEquals(
      template.complete(
        Map("tail" -> constructed(ident("x"))),
        Map.empty
      ),
      Left(DefinitionConstructionError.MissingTypeBinding("parameterType"))
    )
    val completed = template
      .complete(
        Map("tail" -> constructed(ident("x"))),
        Map(
          "parameterType" -> intForm,
          "resultType" -> stringForm,
          "bodyType" -> booleanForm
        )
      )
      .toOption
      .get
      .asInstanceOf[ConstructedDefinition.SingleParameterDef]
    assertEquals(completed.parameterType, intForm)
    assertEquals(completed.resultType, stringForm)
    assertEquals(completed.body.ascriptionTypes, Vector(booleanForm))
  }

  test("template completion preserves bound and free same-text references without capture") {
    val binder = BinderId(0)
    val body = scopedTemplate(
      binder,
      TermShape.Tuple(
        List(bound(0, "x"), ident("__term_tail", placeholder = true))
      ),
      termEntries = Vector("tail" -> "__term_tail"),
      termOccurrences = Vector(TermHoleOccurrence("tail", 0))
    )
    val template = DefinitionTemplate
      .singleParameterDef(
        methodName,
        binder,
        xName,
        TypeTemplate.TTHole("parameterType"),
        TypeTemplate.TTHole("resultType"),
        body
      )
      .toOption
      .get
    val completed = template
      .complete(
        Map("tail" -> constructed(ident("x"))),
        Map("parameterType" -> intForm, "resultType" -> stringForm)
      )
      .toOption
      .get
      .asInstanceOf[ConstructedDefinition.SingleParameterDef]

    assertEquals(completed.parameterName, xName)
    assertEquals(completed.parameterType, intForm)
    assertEquals(completed.resultType, stringForm)
    assertEquals(
      completed.body.root,
      TermShape.Tuple(List(bound(0, "x"), ident("x")))
    )
  }

  test("template and completed equality deliberately follow method-parameter alpha identity") {
    val leftTemplate = definitionTemplate(0, xName, bound(0, "x"))
    val renamedTemplate = definitionTemplate(17, yName, bound(17, "y"))
    val freeTemplate = definitionTemplate(17, yName, ident("x"))

    assertEquals(leftTemplate, renamedTemplate)
    assertEquals(leftTemplate.hashCode, renamedTemplate.hashCode)
    assertNotEquals(leftTemplate, freeTemplate)

    val leftCompleted = leftTemplate.complete(Map.empty, Map.empty).toOption.get
    val renamedCompleted = renamedTemplate.complete(Map.empty, Map.empty).toOption.get
    val freeCompleted = freeTemplate.complete(Map.empty, Map.empty).toOption.get

    assertEquals(leftCompleted, renamedCompleted)
    assertEquals(leftCompleted.hashCode, renamedCompleted.hashCode)
    assertNotEquals(leftCompleted, freeCompleted)
  }

  test("shape conversion preserves one parameter and rejects located-source overclaim") {
    val parsed = shape(0, xName, intShape, intShape, bound(0, "x"))
    val completed = ConstructedDefinition
      .fromShape(parsed)
      .toOption
      .get
      .asInstanceOf[ConstructedDefinition.SingleParameterDef]
    val components = DefinitionComponentSpans
      .create(
        SourceSpan(0, 24),
        SourceSpan(4, 5),
        SourceSpan(10, 13),
        SourceSpan(20, 21)
      )
      .toOption
      .get

    assertEquals(completed.parameterName, xName)
    assertEquals(completed.parameterType, intForm)
    assertEquals(completed.resultType, intForm)
    assertEquals(
      LocatedDefinitionShape
        .create(parsed, SourceId("single-parameter"), components)
        .left
        .toOption
        .get
        .message,
      "Invalid definition source metadata: single-parameter definitions require separate parameter-name and parameter-type evidence."
    )
  }

  private def shape(
      binderValue: Int,
      parameterName: DefinitionName,
      parameterType: TypeShape,
      resultType: TypeShape,
      body: TermShape
  ): DefinitionShape.SingleParameterDef =
    DefinitionShape
      .singleParameterDef(
        methodName,
        BinderId(binderValue),
        parameterName,
        parameterType,
        resultType,
        body
      )
      .fold(error => fail(error.message), identity)

  private def definitionTemplate(
      binderValue: Int,
      parameterName: DefinitionName,
      body: TermShape
  ): DefinitionTemplate.SingleParameterDef =
    val binder = BinderId(binderValue)
    DefinitionTemplate
      .singleParameterDef(
        methodName,
        binder,
        parameterName,
        TypeTemplate.TTIdent("Int"),
        TypeTemplate.TTIdent("Int"),
        scopedTemplate(binder, body)
      )
      .fold(error => fail(error.message), identity)

  private def scopedTemplate(
      binder: BinderId,
      root: TermShape,
      termEntries: Vector[(String, String)] = Vector.empty,
      termOccurrences: Vector[TermHoleOccurrence] = Vector.empty,
      typeEntries: Vector[(String, String)] = Vector.empty,
      ascriptions: Vector[TypeTemplate] = Vector.empty
  ): TermTemplate =
    TermTemplate
      .createInScope(
        root,
        binder,
        index(termEntries, HoleRole.TermTemplate),
        termOccurrences,
        index(typeEntries, HoleRole.TypeTemplate),
        ascriptions
      )
      .fold(error => fail(error.message), identity)

  private def constructed(shape: TermShape): ConstructedTerm =
    ConstructedTerm.fromShape(shape).fold(error => fail(error.message), identity)

  private def index(
      entries: Vector[(String, String)],
      role: HoleRole
  ): GeneratedHoleIndex =
    GeneratedHoleIndex.fromOccurrences(
      entries.zipWithIndex.map { case ((semanticName, generatedName), ordinal) =>
        HoleOccurrence(
          semanticName,
          generatedName,
          SourceSpan(ordinal, ordinal + 1),
          SourceSpan(ordinal, ordinal + 1),
          role
        )
      }
    )

  private def name(source: String): DefinitionName =
    DefinitionName.plain(source).fold(error => fail(error.message), identity)

  private def ident(name: String, placeholder: Boolean = false): TermShape =
    TermShape.Identifier(name, placeholder)

  private def bound(id: Int, displayName: String): TermShape =
    TermShape.BoundReference(BinderId(id), displayName)
