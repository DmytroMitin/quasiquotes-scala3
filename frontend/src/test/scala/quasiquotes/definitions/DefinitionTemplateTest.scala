package quasiquotes.definitions

import quasiquotes.parser.TermShape
import quasiquotes.terms.*
import quasiquotes.terms.parser.{
  CategorizedHoleOccurrence,
  TermTemplateHoleCategory,
  TermTemplateSourceAdapter
}
import quasiquotes.types.{TypeNormalForm, TypeTemplate}

class DefinitionTemplateTest extends munit.FunSuite:
  import DefinitionConstructionTestFixtures.*

  private val intForm = TypeNormalForm.STypeIdent("Int")
  private val stringForm = TypeNormalForm.STypeIdent("String")
  private val booleanForm = TypeNormalForm.STypeIdent("Boolean")

  private def parameterless(
      resultType: TypeTemplate,
      body: TermTemplate,
      name: DefinitionName = plainName
  ): DefinitionTemplate.ParameterlessDef =
    DefinitionTemplate
      .parameterlessDef(name, resultType, body)
      .fold(error => fail(error.message), identity)

  private def immutable(
      declaredType: TypeTemplate,
      rhs: TermTemplate,
      name: DefinitionName = plainName
  ): DefinitionTemplate.ImmutableVal =
    DefinitionTemplate
      .immutableVal(name, declaredType, rhs)
      .fold(error => fail(error.message), identity)

  test("factories preserve both exact variants and fixed names") {
    val body = termTemplate(TermShape.Literal("1"))
    val method = parameterless(TypeTemplate.TTIdent("Int"), body)
    val value =
      immutable(TypeTemplate.TTIdent("String"), body, keywordName)

    assertEquals(method.name, plainName)
    assertEquals(method.resultType, TypeTemplate.TTIdent("Int"))
    assertEquals(method.body, body)
    assertEquals(value.name, keywordName)
    assertEquals(value.declaredType, TypeTemplate.TTIdent("String"))
    assertEquals(value.rhs, body)
    assert(method != value)
  }

  test("factories reject an invalid definition type template") {
    val result =
      DefinitionTemplate.parameterlessDef(
        plainName,
        TypeTemplate.TTIdent("AnyVal"),
        termTemplate(TermShape.Literal("1"))
      )

    assert(result.isLeft)
    assert(
      result.left.toOption.get
        .isInstanceOf[
          DefinitionConstructionError.InvalidDefinitionTypeTemplate
        ]
    )
  }

  test("factories reject an invalid direct definition type-hole name") {
    val result =
      DefinitionTemplate.parameterlessDef(
        plainName,
        TypeTemplate.TTHole(""),
        termTemplate(TermShape.Literal("1"))
      )

    assert(
      result.left.toOption.get
        .isInstanceOf[
          DefinitionConstructionError.InvalidDefinitionTypeTemplate
        ]
    )
  }

  test("hole-free templates complete with empty maps") {
    val body = termTemplate(TermShape.Literal("1"))
    val method =
      parameterless(TypeTemplate.TTIdent("Int"), body)
        .complete(Map.empty, Map.empty)
        .toOption
        .get
    val value =
      immutable(TypeTemplate.TTIdent("String"), body)
        .complete(Map.empty, Map.empty)
        .toOption
        .get

    assert(method.isInstanceOf[ConstructedDefinition.ParameterlessDef])
    assert(value.isInstanceOf[ConstructedDefinition.ImmutableVal])
  }

  test("term-only body holes complete and repeated occurrences share one binding") {
    val generated = "__term_value"
    val body =
      termTemplate(
        TermShape.Tuple(List(ident(generated), ident(generated))),
        termEntries = Vector("body" -> generated),
        termOccurrences = Vector(
          TermHoleOccurrence("body", 0),
          TermHoleOccurrence("body", 1)
        )
      )
    val template =
      parameterless(TypeTemplate.TTIdent("Int"), body)
    val bound = constructed(TermShape.Literal("1"))
    val completed =
      template.complete(Map("body" -> bound), Map.empty).toOption.get
        .asInstanceOf[ConstructedDefinition.ParameterlessDef]

    assertEquals(template.requiredTermBindings, Vector("body"))
    assertEquals(completed.body.root, TermShape.Tuple(List(bound.root, bound.root)))
  }

  test("definition-type-only type holes complete") {
    val template =
      parameterless(
        TypeTemplate.TTApply(
          TypeTemplate.TTIdent("List"),
          List(TypeTemplate.TTHole("element"))
        ),
        termTemplate(TermShape.Literal("1"))
      )
    val completed =
      template
        .complete(Map.empty, Map("element" -> stringForm))
        .toOption
        .get
        .asInstanceOf[ConstructedDefinition.ParameterlessDef]

    assertEquals(template.requiredTypeBindings, Vector("element"))
    assertEquals(
      completed.resultType,
      TypeNormalForm.STypeApply(
        TypeNormalForm.STypeIdent("List"),
        List(stringForm)
      )
    )
  }

  test("body-sidecar-only type holes complete") {
    val generated = "__type_body"
    val body =
      termTemplate(
        TermShape.Typed(ident("rhs"), generated),
        typeEntries = Vector("bodyType" -> generated),
        ascriptions = Vector(TypeTemplate.TTHole("bodyType"))
      )
    val completed =
      immutable(TypeTemplate.TTIdent("Int"), body)
        .complete(Map.empty, Map("bodyType" -> booleanForm))
        .toOption
        .get
        .asInstanceOf[ConstructedDefinition.ImmutableVal]

    assertEquals(completed.rhs.ascriptionTypes, Vector(booleanForm))
  }

  test("one shared type hole across definition and body uses one binding") {
    val generated = "__type_shared"
    val body =
      termTemplate(
        TermShape.Typed(ident("rhs"), generated),
        typeEntries = Vector("shared" -> generated),
        ascriptions = Vector(TypeTemplate.TTHole("shared"))
      )
    val template =
      parameterless(TypeTemplate.TTHole("shared"), body)
    val completed =
      template
        .complete(Map.empty, Map("shared" -> stringForm))
        .toOption
        .get
        .asInstanceOf[ConstructedDefinition.ParameterlessDef]

    assertEquals(template.requiredTypeBindings, Vector("shared"))
    assertEquals(completed.resultType, stringForm)
    assertEquals(completed.body.ascriptionTypes, Vector(stringForm))
  }

  test("distinct component type holes form a deterministic exact union") {
    val generated = "__type_body"
    val body =
      termTemplate(
        TermShape.Typed(ident("rhs"), generated),
        typeEntries = Vector("bodyType" -> generated),
        ascriptions = Vector(TypeTemplate.TTHole("bodyType"))
      )
    val template =
      immutable(TypeTemplate.TTHole("declared"), body)

    assertEquals(
      template.requiredTypeBindings,
      Vector("declared", "bodyType")
    )
    assert(
      template
        .complete(
          Map.empty,
          Map("declared" -> intForm, "bodyType" -> stringForm)
        )
        .isRight
    )
  }

  test("same textual name remains independent in term and type namespaces") {
    val termGenerated = "__term_same"
    val typeGenerated = "__type_same"
    val body =
      termTemplate(
        TermShape.Typed(ident(termGenerated), typeGenerated),
        termEntries = Vector("same" -> termGenerated),
        termOccurrences = Vector(TermHoleOccurrence("same", 0)),
        typeEntries = Vector("same" -> typeGenerated),
        ascriptions = Vector(TypeTemplate.TTHole("same"))
      )
    val template =
      parameterless(TypeTemplate.TTHole("same"), body)
    val completed =
      template
        .complete(
          Map("same" -> constructed(TermShape.Literal("1"))),
          Map("same" -> intForm)
        )
        .toOption
        .get
        .asInstanceOf[ConstructedDefinition.ParameterlessDef]

    assertEquals(completed.resultType, intForm)
    assertEquals(
      completed.body.root,
      TermShape.Typed(TermShape.Literal("1"), "Int")
    )
  }

  test("body receives only its relevant type-binding subset") {
    val generated = "__type_body"
    val body =
      termTemplate(
        TermShape.Typed(ident("rhs"), generated),
        typeEntries = Vector("bodyType" -> generated),
        ascriptions = Vector(TypeTemplate.TTHole("bodyType"))
      )
    val result =
      parameterless(TypeTemplate.TTHole("resultType"), body)
        .complete(
          Map.empty,
          Map("resultType" -> intForm, "bodyType" -> stringForm)
        )

    assert(result.isRight)
  }

  test("missing and unexpected binding errors follow the documented order") {
    val termGenerated = "__term_required"
    val typeGenerated = "__type_required"
    val body =
      termTemplate(
        TermShape.Typed(ident(termGenerated), typeGenerated),
        termEntries = Vector("term" -> termGenerated),
        termOccurrences = Vector(TermHoleOccurrence("term", 0)),
        typeEntries = Vector("bodyType" -> typeGenerated),
        ascriptions = Vector(TypeTemplate.TTHole("bodyType"))
      )
    val template =
      parameterless(TypeTemplate.TTHole("resultType"), body)
    val bound = constructed(TermShape.Literal("1"))

    assertEquals(
      template.complete(
        Map("extraTerm" -> bound),
        Map("extraType" -> intForm)
      ),
      Left(DefinitionConstructionError.MissingTermBinding("term"))
    )
    assertEquals(
      template.complete(
        Map("term" -> bound, "extraTerm" -> bound),
        Map("extraType" -> intForm)
      ),
      Left(DefinitionConstructionError.UnexpectedTermBinding("extraTerm"))
    )
    assertEquals(
      template.complete(
        Map("term" -> bound),
        Map("extraType" -> intForm)
      ),
      Left(DefinitionConstructionError.MissingTypeBinding("resultType"))
    )
    assertEquals(
      template.complete(
        Map("term" -> bound),
        Map(
          "resultType" -> intForm,
          "bodyType" -> stringForm,
          "extraType" -> intForm
        )
      ),
      Left(DefinitionConstructionError.UnexpectedTypeBinding("extraType"))
    )
  }

  test("missing names use first semantic occurrence and extras use lexical order") {
    val firstGenerated = "__term_z"
    val secondGenerated = "__term_a"
    val body =
      termTemplate(
        TermShape.Tuple(
          List(ident(firstGenerated), ident(secondGenerated))
        ),
        termEntries = Vector(
          "zFirst" -> firstGenerated,
          "aSecond" -> secondGenerated
        ),
        termOccurrences = Vector(
          TermHoleOccurrence("zFirst", 0),
          TermHoleOccurrence("aSecond", 1)
        )
      )
    val template =
      parameterless(TypeTemplate.TTIdent("Int"), body)
    val bound = constructed(TermShape.Literal("1"))

    assertEquals(
      template.complete(Map.empty, Map.empty),
      Left(DefinitionConstructionError.MissingTermBinding("zFirst"))
    )
    assertEquals(
      template.complete(
        Map(
          "zFirst" -> bound,
          "aSecond" -> bound,
          "zExtra" -> bound,
          "aExtra" -> bound
        ),
        Map.empty
      ),
      Left(DefinitionConstructionError.UnexpectedTermBinding("aExtra"))
    )
  }

  test("invalid type bindings are checked after exact binding sets") {
    val template =
      parameterless(TypeTemplate.TTHole("resultType"), termTemplate(TermShape.Literal("1")))
    val invalid = TypeNormalForm.STypeIdent("AnyVal")

    assert(
      template
        .complete(Map.empty, Map("resultType" -> invalid))
        .left
        .toOption
        .get
        .isInstanceOf[DefinitionConstructionError.InvalidTypeBinding]
    )
  }

  test("Phase 45Q source templates compose without a production parser dependency") {
    import TermTemplateHoleCategory.*

    val located =
      TermTemplateSourceAdapter
        .parseLocated(
          "($rhs: Option[$element])",
          Vector(
            CategorizedHoleOccurrence("rhs", Term),
            CategorizedHoleOccurrence("element", Type)
          )
        )
        .fold(error => fail(error.diagnostic.message), identity)
    val template =
      immutable(
        TypeTemplate.TTApply(
          TypeTemplate.TTIdent("List"),
          List(TypeTemplate.TTHole("element"))
        ),
        located.template
      )
    val completed =
      template
        .complete(
          Map("rhs" -> constructed(ident("value"))),
          Map("element" -> stringForm)
        )
        .toOption
        .get
        .asInstanceOf[ConstructedDefinition.ImmutableVal]

    assertEquals(
      completed.declaredType,
      TypeNormalForm.STypeApply(
        TypeNormalForm.STypeIdent("List"),
        List(stringForm)
      )
    )
    assertEquals(
      completed.rhs.ascriptionTypes,
      Vector(
        TypeNormalForm.STypeApply(
          TypeNormalForm.STypeIdent("Option"),
          List(stringForm)
        )
      )
    )
    assertEquals(located.template, template.rhs)
  }

  test("template equality is structural and ignores generated transport names") {
    def body(termTransport: String, typeTransport: String): TermTemplate =
      termTemplate(
        TermShape.Typed(ident(termTransport), typeTransport),
        termEntries = Vector("term" -> termTransport),
        termOccurrences = Vector(TermHoleOccurrence("term", 0)),
        typeEntries = Vector("tpe" -> typeTransport),
        ascriptions = Vector(TypeTemplate.TTHole("tpe"))
      )

    val first =
      parameterless(TypeTemplate.TTHole("tpe"), body("__term_a", "__type_a"))
    val second =
      parameterless(TypeTemplate.TTHole("tpe"), body("__term_b", "__type_b"))
    val third =
      parameterless(TypeTemplate.TTHole("tpe"), body("__term_c", "__type_c"))

    assertEquals(first, second)
    assertEquals(second, third)
    assertEquals(first, third)
    assertEquals(first.hashCode, second.hashCode)
    assertEquals(second.hashCode, third.hashCode)
  }

  test("template equality distinguishes variant name type body and occurrence position") {
    val literalBody = termTemplate(TermShape.Literal("1"))
    val otherBody = termTemplate(TermShape.Literal("2"))
    val method =
      parameterless(TypeTemplate.TTIdent("Int"), literalBody)
    val value =
      immutable(TypeTemplate.TTIdent("Int"), literalBody)
    val otherName =
      parameterless(TypeTemplate.TTIdent("Int"), literalBody, keywordName)
    val otherType =
      parameterless(TypeTemplate.TTIdent("String"), literalBody)
    val otherStructure =
      parameterless(TypeTemplate.TTIdent("Int"), otherBody)

    assert(method != value)
    assert(method != otherName)
    assert(method != otherType)
    assert(method != otherStructure)
    assertEquals(method, method)
    assertEquals(method.toString, method.render)
  }

  test("template equality preserves logical term-hole occurrence positions") {
    val generated = "__term_position"
    val first =
      parameterless(
        TypeTemplate.TTIdent("Int"),
        termTemplate(
          TermShape.Tuple(List(ident(generated), ident("ordinary"))),
          termEntries = Vector("hole" -> generated),
          termOccurrences = Vector(TermHoleOccurrence("hole", 0))
        )
      )
    val second =
      parameterless(
        TypeTemplate.TTIdent("Int"),
        termTemplate(
          TermShape.Tuple(List(ident("ordinary"), ident(generated))),
          termEntries = Vector("hole" -> generated),
          termOccurrences = Vector(TermHoleOccurrence("hole", 1))
        )
      )

    assert(first != second)
  }
