package quasiquotes.definitions

import quasiquotes.parser.{BinderId, TermShape, TypeShape}
import quasiquotes.source.*
import quasiquotes.terms.*
import quasiquotes.types.{TypeNormalForm, TypeTemplate}

class TwoParameterDefinitionCoreTest extends munit.FunSuite:
  private val methodName = name("pair")
  private val xName = name("x")
  private val yName = name("y")
  private val aName = name("a")
  private val bName = name("b")
  private val intShape = TypeShape.Identifier("Int")
  private val stringShape = TypeShape.Identifier("String")
  private val pairShape = TypeShape.Tuple(List(intShape, stringShape))
  private val intForm = TypeNormalForm.STypeIdent("Int")
  private val stringForm = TypeNormalForm.STypeIdent("String")
  private val booleanForm = TypeNormalForm.STypeIdent("Boolean")
  private val pairForm = TypeNormalForm.STypeTuple(List(intForm, stringForm))

  test("two-parameter shapes are alpha-structural and preserve ordered binder position") {
    val left = shape(
      10,
      xName,
      intShape,
      20,
      yName,
      stringShape,
      pairShape,
      TermShape.Tuple(List(bound(10, "second-looking"), bound(20, "first-looking")))
    )
    val renamed = shape(
      41,
      aName,
      intShape,
      99,
      bName,
      stringShape,
      pairShape,
      TermShape.Tuple(List(bound(41, "y"), bound(99, "x")))
    )
    val swapped = shape(
      41,
      aName,
      intShape,
      99,
      bName,
      stringShape,
      pairShape,
      TermShape.Tuple(List(bound(99, "x"), bound(41, "y")))
    )

    assertEquals(left, renamed)
    assertEquals(left.hashCode, renamed.hashCode)
    assertNotEquals(left, swapped)
    assertEquals(left.firstParameterName, xName)
    assertEquals(left.secondParameterName, yName)
    assert(!left.render.contains("BinderId"))
  }

  test("equal parameter types still distinguish parameter one parameter two and free same-text") {
    val first = shape(
      1,
      xName,
      intShape,
      2,
      yName,
      intShape,
      intShape,
      bound(1, "y")
    )
    val second = shape(
      1,
      xName,
      intShape,
      2,
      yName,
      intShape,
      intShape,
      bound(2, "x")
    )
    val free = shape(
      1,
      xName,
      intShape,
      2,
      yName,
      intShape,
      intShape,
      ident("x")
    )

    assertNotEquals(first, second)
    assertNotEquals(first, free)
    assertNotEquals(second, free)
  }

  test("shape factory rejects duplicate names duplicate binder identities and foreign references") {
    val duplicateNames = DefinitionShape.twoParameterDef(
      methodName,
      BinderId(1),
      xName,
      intShape,
      BinderId(2),
      xName,
      stringShape,
      pairShape,
      bound(1, "x")
    )
    val duplicateBinders = DefinitionShape.twoParameterDef(
      methodName,
      BinderId(1),
      xName,
      intShape,
      BinderId(1),
      yName,
      stringShape,
      pairShape,
      bound(1, "x")
    )
    val foreign = DefinitionShape.twoParameterDef(
      methodName,
      BinderId(1),
      xName,
      intShape,
      BinderId(2),
      yName,
      stringShape,
      pairShape,
      bound(3, "x")
    )

    assertEquals(
      duplicateNames,
      Left(DefinitionError.InvalidTwoParameterList("declared parameter names must be distinct"))
    )
    assertEquals(
      duplicateBinders,
      Left(DefinitionError.InvalidTwoParameterList("parameter binder identities must be distinct"))
    )
    assertEquals(
      foreign.left.toOption.get.message,
      "Unsupported method body: bound references must resolve to one of the two ordinary method parameters."
    )
  }

  test("shape conversion preserves both ordered parameters") {
    val parsed = shape(
      3,
      xName,
      intShape,
      4,
      yName,
      stringShape,
      pairShape,
      TermShape.Tuple(List(bound(3, "hostile-y"), bound(4, "hostile-x")))
    )
    val completed = ConstructedDefinition
      .fromShape(parsed)
      .toOption
      .get
      .asInstanceOf[ConstructedDefinition.TwoParameterDef]

    assertEquals(completed.firstParameterName, xName)
    assertEquals(completed.firstParameterType, intForm)
    assertEquals(completed.secondParameterName, yName)
    assertEquals(completed.secondParameterType, stringForm)
    assertEquals(completed.resultType, pairForm)
  }

  test("template type preorder is parameter one parameter two result then body preorder") {
    val binders = Vector(BinderId(5), BinderId(6))
    val body = scopedTemplate(
      binders,
      TermShape.Tuple(
        List(
          TermShape.Typed(bound(5, "x"), "__type_bodyFirst"),
          TermShape.Lambda1(
            BinderId(70),
            "z",
            "__type_lambda",
            TermShape.Typed(bound(6, "y"), "__type_bodySecond")
          )
        )
      ),
      typeEntries = Vector(
        "bodyFirst" -> "__type_bodyFirst",
        "lambdaType" -> "__type_lambda",
        "bodySecond" -> "__type_bodySecond"
      ),
      ascriptions = Vector(
        TypeTemplate.TTHole("bodyFirst"),
        TypeTemplate.TTHole("lambdaType"),
        TypeTemplate.TTHole("bodySecond")
      )
    )
    val template = DefinitionTemplate
      .twoParameterDef(
        methodName,
        binders(0),
        xName,
        TypeTemplate.TTHole("shared"),
        binders(1),
        yName,
        TypeTemplate.TTApply(TypeTemplate.TTIdent("List"), List(TypeTemplate.TTHole("shared"))),
        TypeTemplate.TTHole("result"),
        body
      )
      .fold(error => fail(error.message), identity)

    assertEquals(
      template.requiredTypeBindings,
      Vector("shared", "result", "bodyFirst", "lambdaType", "bodySecond")
    )
    assertEquals(
      template.complete(Map.empty, Map.empty),
      Left(DefinitionConstructionError.MissingTypeBinding("shared"))
    )
    assertEquals(
      template.complete(
        Map.empty,
        Map(
          "shared" -> intForm,
          "result" -> pairForm,
          "bodyFirst" -> booleanForm,
          "lambdaType" -> intForm,
          "bodySecond" -> stringForm,
          "extra" -> intForm
        )
      ),
      Left(DefinitionConstructionError.UnexpectedTypeBinding("extra"))
    )
    val completed = template
      .complete(
        Map.empty,
        Map(
          "shared" -> intForm,
          "result" -> pairForm,
          "bodyFirst" -> booleanForm,
          "lambdaType" -> intForm,
          "bodySecond" -> stringForm
        )
      )
      .toOption
      .get
      .asInstanceOf[ConstructedDefinition.TwoParameterDef]
    assertEquals(completed.firstParameterType, intForm)
    assertEquals(
      completed.secondParameterType,
      TypeNormalForm.STypeApply(TypeNormalForm.STypeIdent("List"), List(intForm))
    )
    assertEquals(completed.resultType, pairForm)
    assertEquals(completed.body.ascriptionTypes, Vector(booleanForm, intForm, stringForm))
  }

  test("template completion preserves both bound references and inserted same-text free identifiers") {
    val binders = Vector(BinderId(8), BinderId(9))
    val body = scopedTemplate(
      binders,
      TermShape.Tuple(
        List(
          bound(8, "y"),
          bound(9, "x"),
          ident("__term_freeX", placeholder = true),
          ident("__term_freeY", placeholder = true)
        )
      ),
      termEntries = Vector("freeX" -> "__term_freeX", "freeY" -> "__term_freeY"),
      termOccurrences = Vector(
        TermHoleOccurrence("freeX", 0),
        TermHoleOccurrence("freeY", 1)
      )
    )
    val template = DefinitionTemplate
      .twoParameterDef(
        methodName,
        binders(0),
        xName,
        TypeTemplate.TTIdent("Int"),
        binders(1),
        yName,
        TypeTemplate.TTIdent("String"),
        TypeTemplate.TTTuple(List(TypeTemplate.TTIdent("Int"), TypeTemplate.TTIdent("String"))),
        body
      )
      .toOption
      .get
    val completed = template
      .complete(
        Map("freeX" -> constructed(ident("x")), "freeY" -> constructed(ident("y"))),
        Map.empty
      )
      .toOption
      .get
      .asInstanceOf[ConstructedDefinition.TwoParameterDef]

    assertEquals(
      completed.body.root,
      TermShape.Tuple(
        List(bound(8, "y"), bound(9, "x"), ident("x"), ident("y"))
      )
    )
  }

  test("template equality uses ordered ambient binders rather than display text") {
    val left = simpleTemplate(
      Vector(BinderId(15), BinderId(16)),
      xName,
      yName,
      TermShape.Tuple(List(bound(15, "y"), bound(16, "x")))
    )
    val renamed = simpleTemplate(
      Vector(BinderId(25), BinderId(26)),
      aName,
      bName,
      TermShape.Tuple(List(bound(25, "right"), bound(26, "left")))
    )
    val swapped = simpleTemplate(
      Vector(BinderId(25), BinderId(26)),
      aName,
      bName,
      TermShape.Tuple(List(bound(26, "left"), bound(25, "right")))
    )

    assertEquals(left, renamed)
    assertEquals(left.hashCode, renamed.hashCode)
    assertNotEquals(left, swapped)
  }

  test("constructed and template factories reject duplicate exact-two declaration state first") {
    val binder = BinderId(30)
    val completedBody = ConstructedTerm
      .fromShapeInScope(bound(30, "x"), binder)
      .toOption
      .get
    val constructedResult = ConstructedDefinition.twoParameterDef(
      methodName,
      binder,
      xName,
      intForm,
      binder,
      yName,
      stringForm,
      pairForm,
      completedBody
    )
    val templateBody = scopedTemplate(
      Vector(BinderId(31), BinderId(32)),
      bound(31, "x")
    )
    val templateResult = DefinitionTemplate.twoParameterDef(
      methodName,
      BinderId(31),
      xName,
      TypeTemplate.TTIdent("Int"),
      BinderId(32),
      xName,
      TypeTemplate.TTIdent("String"),
      TypeTemplate.TTIdent("Int"),
      templateBody
    )

    assertEquals(
      constructedResult,
      Left(
        DefinitionConstructionError.InvalidTwoParameterList(
          "parameter binder identities must be distinct"
        )
      )
    )
    assertEquals(
      templateResult,
      Left(
        DefinitionConstructionError.InvalidTwoParameterList(
          "declared parameter names must be distinct"
        )
      )
    )
  }

  test("one body ascription follows all three declaration types and invalid bindings fail first") {
    val binders = Vector(BinderId(35), BinderId(36))
    val body = scopedTemplate(
      binders,
      TermShape.Typed(bound(35, "x"), "__type_body"),
      typeEntries = Vector("body" -> "__type_body"),
      ascriptions = Vector(TypeTemplate.TTHole("body"))
    )
    val template = DefinitionTemplate
      .twoParameterDef(
        methodName,
        binders(0),
        xName,
        TypeTemplate.TTHole("first"),
        binders(1),
        yName,
        TypeTemplate.TTHole("second"),
        TypeTemplate.TTHole("result"),
        body
      )
      .toOption
      .get

    assertEquals(
      template.requiredTypeBindings,
      Vector("first", "second", "result", "body")
    )
    val invalid = template.complete(
      Map.empty,
      Map(
        "first" -> TypeNormalForm.STypeIdent("Long"),
        "second" -> stringForm,
        "result" -> intForm,
        "body" -> booleanForm
      )
    )
    assertEquals(
      invalid.left.toOption.get.asInstanceOf[DefinitionConstructionError.InvalidTypeBinding].name,
      "first"
    )
  }

  test("ordered ambient binders and nested Lambda1 remain alpha-equivalent without aliasing") {
    val left = constructedDefinitionWithLambda(
      Vector(BinderId(11), BinderId(12)),
      BinderId(13),
      xName,
      yName
    )
    val renamed = constructedDefinitionWithLambda(
      Vector(BinderId(101), BinderId(102)),
      BinderId(103),
      aName,
      bName
    )

    assertEquals(left, renamed)
    assertEquals(left.hashCode, renamed.hashCode)
  }

  test("legacy located metadata rejects exact-two shapes with a controlled truthful reason") {
    val parsed = shape(
      1,
      xName,
      intShape,
      2,
      yName,
      stringShape,
      pairShape,
      TermShape.Tuple(List(bound(1, "x"), bound(2, "y")))
    )
    val components = DefinitionComponentSpans
      .create(
        SourceSpan(0, 45),
        SourceSpan(4, 8),
        SourceSpan(28, 41),
        SourceSpan(44, 45)
      )
      .toOption
      .get

    assertEquals(
      LocatedDefinitionShape
        .create(parsed, SourceId("two-parameter"), components)
        .left
        .toOption
        .get
        .message,
      "Invalid definition source metadata: two-parameter definitions require separate name and type evidence for both parameter declarations."
    )
  }

  private def shape(
      firstBinderValue: Int,
      firstParameterName: DefinitionName,
      firstParameterType: TypeShape,
      secondBinderValue: Int,
      secondParameterName: DefinitionName,
      secondParameterType: TypeShape,
      resultType: TypeShape,
      body: TermShape
  ): DefinitionShape.TwoParameterDef =
    DefinitionShape
      .twoParameterDef(
        methodName,
        BinderId(firstBinderValue),
        firstParameterName,
        firstParameterType,
        BinderId(secondBinderValue),
        secondParameterName,
        secondParameterType,
        resultType,
        body
      )
      .fold(error => fail(error.message), identity)

  private def constructedDefinitionWithLambda(
      binders: Vector[BinderId],
      lambdaBinder: BinderId,
      firstName: DefinitionName,
      secondName: DefinitionName
  ): ConstructedDefinition.TwoParameterDef =
    val body = ConstructedTerm
      .createInScope(
        TermShape.Lambda1(
          lambdaBinder,
          "z",
          "Int",
          TermShape.Tuple(
            List(
              bound(binders(0).value, "second-looking"),
              bound(binders(1).value, "first-looking"),
              TermShape.BoundReference(lambdaBinder, "outer-looking")
            )
          )
        ),
        Vector(intForm),
        binders
      )
      .fold(error => fail(error.message), identity)
    ConstructedDefinition
      .twoParameterDef(
        methodName,
        binders(0),
        firstName,
        intForm,
        binders(1),
        secondName,
        stringForm,
        pairForm,
        body
      )
      .fold(error => fail(error.message), identity)

  private def simpleTemplate(
      binders: Vector[BinderId],
      firstName: DefinitionName,
      secondName: DefinitionName,
      body: TermShape
  ): DefinitionTemplate.TwoParameterDef =
    DefinitionTemplate
      .twoParameterDef(
        methodName,
        binders(0),
        firstName,
        TypeTemplate.TTIdent("Int"),
        binders(1),
        secondName,
        TypeTemplate.TTIdent("String"),
        TypeTemplate.TTIdent("Int"),
        scopedTemplate(binders, body)
      )
      .fold(error => fail(error.message), identity)

  private def scopedTemplate(
      binders: Vector[BinderId],
      root: TermShape,
      termEntries: Vector[(String, String)] = Vector.empty,
      termOccurrences: Vector[TermHoleOccurrence] = Vector.empty,
      typeEntries: Vector[(String, String)] = Vector.empty,
      ascriptions: Vector[TypeTemplate] = Vector.empty
  ): TermTemplate =
    TermTemplate
      .createInScope(
        root,
        binders,
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
