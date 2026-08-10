package quasiquotes.definitions.parser

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import quasiquotes.definitions.DefinitionTemplate
import quasiquotes.parser.TermShape
import quasiquotes.source.{
  DiagnosticPrecision,
  HoleRole,
  SourceOrigin,
  SourceSpan
}
import quasiquotes.terms.ConstructedTerm
import quasiquotes.types.{TypeNormalForm, TypeTemplate}

class DefinitionTemplateSourceAdapterTest extends munit.FunSuite:
  import DefinitionTemplateHoleCategory.*
  import DefinitionTemplateSourceAdapterError.*

  private def definitionType(
      name: String
  ): CategorizedDefinitionHoleOccurrence =
    CategorizedDefinitionHoleOccurrence(name, DefinitionType)

  private def bodyTerm(
      name: String
  ): CategorizedDefinitionHoleOccurrence =
    CategorizedDefinitionHoleOccurrence(name, BodyTerm)

  private def bodyType(
      name: String
  ): CategorizedDefinitionHoleOccurrence =
    CategorizedDefinitionHoleOccurrence(name, BodyType)

  private def parsed(
      source: String,
      occurrences: CategorizedDefinitionHoleOccurrence*
  ) =
    DefinitionTemplateSourceAdapter
      .parseLocated(source, occurrences.toVector)
      .fold(error => fail(error.diagnostic.message), identity)

  private def error(
      source: String,
      occurrences: CategorizedDefinitionHoleOccurrence*
  ) =
    DefinitionTemplateSourceAdapter
      .parseLocated(source, occurrences.toVector)
      .swap
      .fold(value => fail(s"Expected adapter failure, received $value"), identity)

  Vector(
    "def answer: Int = 1",
    "val answer: String = \"text\"",
    "def `type`: List[Int] = service.answer",
    "val `val`: Option[String] = (value: String)"
  ).foreach { source =>
    test(s"accepts hole-free canonical definition: $source") {
      val located = parsed(source)
      assertEquals(located.definitionTypeOccurrences, Vector.empty)
      assertEquals(located.body.termOccurrences, Vector.empty)
      assertEquals(located.body.typeOccurrences, Vector.empty)
      assertEquals(
        DefinitionTemplateSourceAdapter.parse(source, Vector.empty),
        Right(located.template)
      )
    }
  }

  test("extracts definition type body term and body type categories in one source") {
    val source =
      "def `type`: List[$T] = ($left + $right: Option[$T])"
    val located =
      parsed(
        source,
        definitionType("T"),
        bodyTerm("left"),
        bodyTerm("right"),
        bodyType("T")
      )

    val method =
      located.template
        .asInstanceOf[DefinitionTemplate.ParameterlessDef]
    assertEquals(
      method.resultType,
      TypeTemplate.TTApply(
        TypeTemplate.TTIdent("List"),
        List(TypeTemplate.TTHole("T"))
      )
    )
    assertEquals(
      located.definitionTypeOccurrences.map(_.name),
      Vector("T")
    )
    assertEquals(
      located.body.termOccurrences.map(_.semantic.name),
      Vector("left", "right")
    )
    assertEquals(
      located.body.typeOccurrences.map(_.name),
      Vector("T")
    )
  }

  test("same semantic name stays independent in all three namespaces") {
    val located =
      parsed(
        "def answer: $same = ($same: $same)",
        definitionType("same"),
        bodyTerm("same"),
        bodyType("same")
      )
    val generated =
      Vector(
        located.definitionTypeOccurrences.head.generatedName,
        located.body.termOccurrences.head.source.generatedName,
        located.body.typeOccurrences.head.generatedName
      )

    assertEquals(generated.distinct.size, 3)
    assertEquals(
      located.template.requiredTermBindings,
      Vector("same")
    )
    assertEquals(
      located.template.requiredTypeBindings,
      Vector("same")
    )
  }

  test("repeated occurrences share category-specific generated identities") {
    val located =
      parsed(
        "val answer: ($T, $T) = ($x, $x)",
        definitionType("T"),
        definitionType("T"),
        bodyTerm("x"),
        bodyTerm("x")
      )

    assertEquals(
      located.definitionTypeOccurrences.map(_.generatedName).distinct.size,
      1
    )
    assertEquals(
      located.body.termOccurrences
        .map(_.source.generatedName)
        .distinct
        .size,
      1
    )
  }

  test("ordinary generated-looking identifiers remain ordinary under collision") {
    val located =
      parsed(
        "val answer: Int = (__qq_dt_type_x, __qq_dt_body_term_x, __qq_dt_body_type_x, $x)",
        bodyTerm("x")
      )
    assertEquals(
      located.body.termOccurrences.head.source.generatedName,
      "__qq_dt_body_term_x_1"
    )
  }

  test("CR LF and supplementary Unicode retain UTF-16 occurrence offsets") {
    val source =
      "def answer: Int = (\"😀\",\r\n $later)"
    val located = parsed(source, bodyTerm("later"))
    assertEquals(
      located.body.termOccurrences.head.source.originalSpan,
      SourceSpan(
        source.indexOf("$later"),
        source.indexOf("$later") + "$later".length
      )
    )
  }

  test("all admitted definition type families and representative bodies remain available") {
    val sources =
      Vector(
        "def scalar: Int = -(-1)",
        "val applied: Option[String] = service.value",
        "def tuple: (Int, String) = (1, \"x\")",
        "val triple: (Int, String, Boolean) = (1, \"x\", true)",
        "def function: Int => String = if ready then value else fallback",
        "val binary: (Int, String) => Boolean = predicate",
        "def call: List[Int] = service.compute(1, 2 + 3)"
      )
    sources.foreach(source => assert(parsed(source).template != null))
  }

  test("scanner ignores comments strings and backticks across the full definition") {
    val located =
      parsed(
        """def /* $comment */ answer: Int =
          |  ($value, "$string", `$backtick`)""".stripMargin,
        bodyTerm("value")
      )
    assertEquals(
      located.body.termOccurrences.map(_.semantic.name),
      Vector("value")
    )
  }

  test("source map is complete and roles retain exact original occurrences") {
    val source = "def answer: List[$T] = ($x: Option[$U])"
    val located =
      parsed(
        source,
        definitionType("T"),
        bodyTerm("x"),
        bodyType("U")
      )
    val segments = located.sourceMap.segments

    assertEquals(segments.head.generatedSpan.start, 0)
    assertEquals(
      segments.last.generatedSpan.end,
      located.sourceMap.generatedSource.length
    )
    assert(
      segments.zip(segments.drop(1)).forall { case (left, right) =>
        left.generatedSpan.end == right.generatedSpan.start
      }
    )
    assertEquals(
      located.definitionTypeOccurrences.map(_.role),
      Vector(HoleRole.DefinitionTypeTemplate)
    )
    assertEquals(
      located.body.termOccurrences.map(_.source.role),
      Vector(HoleRole.DefinitionBodyTermTemplate)
    )
    assertEquals(
      located.body.typeOccurrences.map(_.role),
      Vector(HoleRole.DefinitionBodyTypeTemplate)
    )
    assertEquals(
      located.definitionTypeOccurrences.head.originalSpan,
      SourceSpan(source.indexOf("$T"), source.indexOf("$T") + 2)
    )
  }

  test("located completion reports the unique definition type occurrence") {
    val located =
      parsed(
        "def answer: $T = 1",
        definitionType("T")
      )
    val failure =
      located.complete(Map.empty, Map.empty).swap.toOption.get

    assertEquals(
      failure.location.map(_.precision),
      Some(DiagnosticPrecision.ExactOccurrence)
    )
    assert(
      failure.location.toVector.flatMap(_.origins).exists {
        case SourceOrigin.RewrittenHole(
              _,
              _,
              "T",
              HoleRole.DefinitionTypeTemplate
            ) =>
          true
        case _ =>
          false
      }
    )
  }

  test("completed template unifies a shared type binding across definition and body") {
    val located =
      parsed(
        "def answer: List[$T] = ($x: Option[$T])",
        definitionType("T"),
        bodyTerm("x"),
        bodyType("T")
      )
    val one =
      ConstructedTerm
        .fromShape(TermShape.Literal("1"))
        .toOption
        .get
    val result =
      located
        .complete(
          Map("x" -> one),
          Map("T" -> TypeNormalForm.STypeIdent("String"))
        )
        .toOption
        .get
        .asInstanceOf[
          quasiquotes.definitions.ConstructedDefinition.ParameterlessDef
        ]

    assertEquals(
      result.resultType,
      TypeNormalForm.STypeApply(
        TypeNormalForm.STypeIdent("List"),
        List(TypeNormalForm.STypeIdent("String"))
      )
    )
    assertEquals(
      result.body.ascriptionTypes,
      Vector(
        TypeNormalForm.STypeApply(
          TypeNormalForm.STypeIdent("Option"),
          List(TypeNormalForm.STypeIdent("String"))
        )
      )
    )
  }

  test("invalid plan names dollar syntax counts and names fail before parsing") {
    assert(error("def answer: Int = 1", bodyTerm("")).diagnostic
      .isInstanceOf[InvalidHoleName])
    assert(error("def answer: Int = $", Vector.empty*).diagnostic
      .isInstanceOf[InvalidDollarSyntax])
    assert(error("def answer: Int = $x").diagnostic
      .isInstanceOf[OccurrenceCountMismatch])
    assert(
      error(
        "def answer: Int = $x",
        bodyTerm("x"),
        bodyTerm("x")
      ).diagnostic.isInstanceOf[OccurrenceCountMismatch]
    )
    assert(
      error("def answer: Int = $x", bodyTerm("y")).diagnostic
        .isInstanceOf[OccurrenceNameMismatch]
    )
  }

  test("category and parsed component mismatches are occurrence-local") {
    val wrongDefinition =
      error(
        "def answer: $T = 1",
        bodyType("T")
      )
    val wrongBody =
      error(
        "def answer: Int = $x",
        definitionType("x")
      )
    val name =
      error(
        "def $name: Int = 1",
        bodyTerm("name")
      )

    Vector(wrongDefinition, wrongBody, name).foreach { failure =>
      assert(
        failure.diagnostic.isInstanceOf[CategoryComponentMismatch]
      )
      assertEquals(
        failure.location.map(_.precision),
        Some(DiagnosticPrecision.ExactOccurrence)
      )
    }
  }

  test("all six category crossings into name or the other component fail") {
    val cases =
      Vector(
        "def answer: Int = $x" -> definitionType("x"),
        "def $x: Int = 1" -> definitionType("x"),
        "def answer: $x = 1" -> bodyTerm("x"),
        "def $x: Int = 1" -> bodyTerm("x"),
        "def answer: $x = 1" -> bodyType("x"),
        "def $x: Int = 1" -> bodyType("x")
      )
    cases.foreach { case (source, plan) =>
      assert(
        error(source, plan).diagnostic
          .isInstanceOf[CategoryComponentMismatch],
        source
      )
    }
  }

  test("body term and body type syntax are not interchangeable") {
    assert(
      error(
        "def answer: Int = ($value: $T)",
        bodyTerm("value"),
        bodyTerm("T")
      ).diagnostic.isInstanceOf[InvalidDefinitionBodyTemplate]
    )
    assert(
      error(
        "def answer: Int = ($value: Int)",
        bodyType("value")
      ).diagnostic.isInstanceOf[InvalidDefinitionBodyTemplate]
    )
  }

  test("unsupported variants malformed source and multiple definitions are rejected") {
    Vector(
      "def answer(): Int = 1",
      "def answer[A]: Int = 1",
      "lazy val answer: Int = 1",
      "var answer: Int = 1",
      "private val answer: Int = 1",
      "val (left, right) = (1, 2)",
      "def answer = 1",
      "class Answer",
      "val first: Int = 1\nval second: Int = 2"
    ).foreach { source =>
      assert(
        error(source).diagnostic
          .isInstanceOf[UnsupportedDefinitionVariant],
        source
      )
    }
    assert(
      error("def answer: Int =").diagnostic
        .isInstanceOf[DefinitionParserFailure]
    )
  }

  test("unsupported fixed names and unsupported child shapes stay distinct") {
    Vector(
      "def `answer`: Int = 1",
      "def `not a keyword`: Int = 1",
      "def +: Int = 1",
      "def naïve: Int = 1"
    ).foreach { source =>
      error(source).diagnostic match
        case _: InvalidDefinitionName |
            _: DefinitionParserFailure |
            _: UnsupportedDefinitionVariant =>
          ()
        case other =>
          fail(s"Unexpected fixed-name failure for `$source`: $other")
    }
    assert(
      error("def answer: scala.Int = 1").diagnostic
        .isInstanceOf[InvalidDefinitionTypeTemplate]
    )
    assert(
      error("def answer: Int = { 1 }").diagnostic
        .isInstanceOf[InvalidDefinitionBodyTemplate]
    )
  }

  test("definition type diagnostics stay actionable without generated markers") {
    val failure =
      error(
        "def answer: Vector[$T] = 1",
        definitionType("T")
      )
    assertEquals(
      failure.diagnostic.message,
      "Invalid definition type template: Unsupported applied type constructor `Vector`; supported constructors are List/1, Option/1, Either/2."
    )
    assert(!failure.diagnostic.message.contains("__qq_dt_type_"))
  }

  test("adapter diagnostics do not expose compiler or peer implementation names") {
    val messages =
      Vector(
        error("class Answer").diagnostic.message,
        error("def answer: Int =").diagnostic.message,
        error(
          "def answer: Int = ($x: Int)",
          bodyType("x")
        ).diagnostic.message
      )
    messages.foreach { message =>
      assert(!message.contains("dotty.tools"))
      assert(!message.contains("DefDef"))
      assert(!message.toLowerCase.contains("macroparadise"))
    }
  }

  test("compiler-coupled frontend and shared body helper retain package boundaries") {
    val frontend =
      Files.readString(
        Path.of(
          "frontend",
          "src",
          "main",
          "scala",
          "quasiquotes",
          "definitions",
          "parser",
          "DefinitionTemplateSourceAdapter.scala"
        ),
        StandardCharsets.UTF_8
      )
    Vector(
      "scala.quoted",
      "quotes.reflect",
      "Expr[",
      "TypeRepr",
      "ConstructedDefinitionUntypedBackend",
      "ConstructedDefinitionGeneratedOriginAdapter",
      "MacroParadise",
      "trait Backend"
    ).foreach(value => assert(!frontend.contains(value), value))

    val sharedBody =
      Files.readString(
        Path.of(
          "frontend",
          "src",
          "main",
          "scala",
          "quasiquotes",
          "terms",
          "parser",
          "TermTemplateSourceAdapter.scala"
        ),
        StandardCharsets.UTF_8
      )
    val helper =
      sharedBody.substring(
        sharedBody.indexOf(
          "private[quasiquotes] object RawTermTemplateAdapter"
        )
      )
    assert(!helper.contains("quasiquotes.definitions"))
    assert(!helper.contains("DefinitionTemplate"))
  }
