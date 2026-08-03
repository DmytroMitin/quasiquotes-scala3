package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.SourceFile

import quasiquotes.definitions.*
import quasiquotes.parser.{TermShape, TypeShape}
import quasiquotes.source.*

class RawDefinitionParserTest extends munit.FunSuite:
  private val sourceId = SourceId("raw-definition-test")

  test("standalone parameterless def returns only the located project-owned shape") {
    val source = "def answer: Int = 42"
    val result = accepted(source)

    val expected =
      DefinitionShape
        .parameterlessDef(
          DefinitionName.plain("answer").toOption.get,
          TypeShape.Identifier("Int"),
          TermShape.Literal("42")
        )
        .toOption
        .get
    assertEquals(result.shape, expected)
    assertEquals(result.sourceId, sourceId)
    assertEquals(result.components.definition, SourceSpan(0, source.length))
    assertEquals(result.components.name, SourceSpan(4, 10))
    assertEquals(result.components.declaredType, SourceSpan(12, 15))
    assertEquals(result.components.body, SourceSpan(18, 20))
    assertEquals(result.originMap, None)
    assert(!result.getClass.getName.contains("dotty.tools"))
  }

  test("standalone immutable val preserves its distinct shape and exact component spans") {
    val source = "val answer: Int = service.value"
    val result = accepted(source)

    val value = result.shape.asInstanceOf[DefinitionShape.ImmutableVal]
    assertEquals(value.name.source, "answer")
    assertEquals(value.declaredType, TypeShape.Identifier("Int"))
    assertEquals(
      value.rhs,
      TermShape.Select(TermShape.Identifier("service", false), "value")
    )
    assertEquals(source.substring(result.components.name.start, result.components.name.end), "answer")
    assertEquals(source.substring(result.components.declaredType.start, result.components.declaredType.end), "Int")
    assertEquals(source.substring(result.components.body.start, result.components.body.end), "service.value")
  }

  test("bounded name scanning skips comments and ignores misleading type and body substrings") {
    val source = "def /* answer and Int */ actual: Option[Int] = actual"
    val result = accepted(source)
    val methodWithContainingBody = accepted("def answer: Int = answerValue")
    val valueWithContainingBody = accepted("val answer: Int = answerValue")

    assertEquals(result.shape.name.decoded, "actual")
    assertEquals(
      source.substring(result.components.name.start, result.components.name.end),
      "actual"
    )
    assertEquals(
      source.substring(result.components.declaredType.start, result.components.declaredType.end),
      "Option[Int]"
    )
    assertEquals(source.substring(result.components.body.start, result.components.body.end), "actual")
    assertEquals(methodWithContainingBody.shape.name.source, "answer")
    assertEquals(methodWithContainingBody.components.name, SourceSpan(4, 10))
    assertEquals(valueWithContainingBody.shape.name.source, "answer")
    assertEquals(valueWithContainingBody.components.name, SourceSpan(4, 10))
  }

  test("nested block comments before a backticked keyword preserve exact source spelling") {
    val source = "val /* outer /* inner */ done */ `type`: Int = 1"
    val result = accepted(source)

    assertEquals(result.shape.name.decoded, "type")
    assertEquals(result.shape.name.source, "`type`")
    assertEquals(
      source.substring(result.components.name.start, result.components.name.end),
      "`type`"
    )
  }

  test("supported type structures are converted through the existing inspector") {
    val applied = accepted("def applied: Option[Int] = value").shape
    val tuple = accepted("val tuple: (Int, String) = (1, \"x\")").shape
    val function = accepted("def function: (Int, String) => Boolean = predicate").shape

    assertEquals(
      applied.asInstanceOf[DefinitionShape.ParameterlessDef].resultType,
      TypeShape.Apply(TypeShape.Identifier("Option"), List(TypeShape.Identifier("Int")))
    )
    assertEquals(
      tuple.asInstanceOf[DefinitionShape.ImmutableVal].declaredType,
      TypeShape.Tuple(List(TypeShape.Identifier("Int"), TypeShape.Identifier("String")))
    )
    assertEquals(
      function.asInstanceOf[DefinitionShape.ParameterlessDef].resultType,
      TypeShape.Function(
        List(TypeShape.Identifier("Int"), TypeShape.Identifier("String")),
        TypeShape.Identifier("Boolean")
      )
    )
  }

  test("supported term structures are converted through the existing inspector") {
    val selected = accepted("val selected: Int = service.value").shape
    val applied = accepted("def applied: Int = service.compute(1)").shape
    val unary = accepted("val unary: Int = -value").shape
    val tuple = accepted("def tuple: (Int, Int) = (1, 2)").shape
    val conditional = accepted("val choice: Int = if ready then 1 else 2").shape

    assert(selected.asInstanceOf[DefinitionShape.ImmutableVal].rhs.isInstanceOf[TermShape.Select])
    assert(applied.asInstanceOf[DefinitionShape.ParameterlessDef].body.isInstanceOf[TermShape.Apply])
    assert(unary.asInstanceOf[DefinitionShape.ImmutableVal].rhs.isInstanceOf[TermShape.Unary])
    assert(tuple.asInstanceOf[DefinitionShape.ParameterlessDef].body.isInstanceOf[TermShape.Tuple])
    assert(conditional.asInstanceOf[DefinitionShape.ImmutableVal].rhs.isInstanceOf[TermShape.If])
  }

  test("inferred method and value types are rejected") {
    assertError("def answer = 42", RawDefinitionAdapterError.MissingExplicitType)
    assertError("val answer = 42", RawDefinitionAdapterError.MissingExplicitType)
  }

  test("a bodyless method declaration is rejected as missing its body") {
    assertError("def answer: Int", RawDefinitionAdapterError.MissingDefinitionBody)
  }

  test("method type parameters and every parameter-clause form are rejected") {
    assertError("def answer[A]: Int = 42", RawDefinitionAdapterError.UnsupportedTypeParameters)
    assertError("def answer(): Int = 42", RawDefinitionAdapterError.UnsupportedParameterClauses)
    assertError("def answer(value: Int): Int = value", RawDefinitionAdapterError.UnsupportedParameterClauses)
    assertError("def answer(using value: Int): Int = value", RawDefinitionAdapterError.UnsupportedParameterClauses)
  }

  test("mutable lazy and pattern values have distinct errors") {
    assertError("var answer: Int = 42", RawDefinitionAdapterError.UnsupportedMutableValue)
    assertError("lazy val answer: Int = 42", RawDefinitionAdapterError.UnsupportedLazyValue)
    assertError("val (left, right) = (1, 2)", RawDefinitionAdapterError.UnsupportedPatternValue)
  }

  test("annotations and source modifiers are rejected at the whole-definition location") {
    val candidates = List(
      "@deprecated def answer: Int = 42",
      "private def answer: Int = 42",
      "protected val answer: Int = 42",
      "final def answer: Int = 42",
      "inline def answer: Int = 42"
    )

    candidates.foreach { source =>
      val error = rejected(source)
      assertEquals(error.diagnostic, RawDefinitionAdapterError.UnsupportedDefinitionModifiers)
      assertEquals(error.location.map(_.precision), Some(DiagnosticPrecision.WholeSource))
    }
  }

  test("non-definition roots wrappers imports empty input and multiple statements are rejected") {
    assertError("type Answer = Int", RawDefinitionAdapterError.UnsupportedRawDefinitionKind)
    assertError("class Answer", RawDefinitionAdapterError.UnsupportedRawDefinitionKind)
    assertError("object Answer", RawDefinitionAdapterError.UnsupportedRawDefinitionKind)
    assertError("package example\nval answer: Int = 42", RawDefinitionAdapterError.UnsupportedRawDefinitionKind)
    assertError("import example.*", RawDefinitionAdapterError.UnsupportedRawDefinitionKind)
    assertError("", RawDefinitionAdapterError.ExpectedExactlyOneDefinition(0))
    assertError(
      "import example.*\nval answer: Int = 42",
      RawDefinitionAdapterError.ExpectedExactlyOneDefinition(2)
    )
    assertError(
      "val first: Int = 1\nval second: Int = 2",
      RawDefinitionAdapterError.ExpectedExactlyOneDefinition(2)
    )
  }

  test("unsupported selected types and block bodies retain exact component locations") {
    val typeError = rejected("def answer: scala.Int = 42")
    val bodyError = rejected("val answer: Int = { 42 }")

    assert(typeError.diagnostic.isInstanceOf[RawDefinitionAdapterError.UnsupportedDefinitionType])
    assertEquals(typeError.location.map(_.span), Some(SourceSpan(12, 21)))
    assertEquals(typeError.location.map(_.precision), Some(DiagnosticPrecision.ExactOccurrence))
    assert(bodyError.diagnostic.isInstanceOf[RawDefinitionAdapterError.UnsupportedDefinitionBody])
    assertEquals(bodyError.location.map(_.span), Some(SourceSpan(18, 24)))
    assertEquals(bodyError.location.map(_.precision), Some(DiagnosticPrecision.ExactOccurrence))
  }

  test("non-keyword backticks and out-of-policy plain names are rejected at the exact name occurrence") {
    val backticked = rejected("def `answer`: Int = 42")
    val outOfPolicy = rejected("def $answer: Int = 42")

    assert(backticked.diagnostic.isInstanceOf[RawDefinitionAdapterError.InvalidDefinitionName])
    assertEquals(backticked.location.map(_.span), Some(SourceSpan(4, 12)))
    assert(outOfPolicy.diagnostic.isInstanceOf[RawDefinitionAdapterError.InvalidDefinitionName])
    assertEquals(outOfPolicy.location.map(_.span), Some(SourceSpan(4, 11)))
  }

  test("parser failures use a stable adapter error without compiler diagnostic text") {
    val result = rejected("def answer: Int =")

    assertEquals(result.diagnostic, RawDefinitionAdapterError.DefinitionParseFailure)
    assertEquals(result.diagnostic.message, "The source could not be parsed as one raw definition.")
    assert(!result.diagnostic.message.contains("E040"))
    assert(!result.diagnostic.message.contains("DefDef"))
  }

  test("isolated raw-tree entrypoint accepts a supplied definition without exposing raw output") {
    val source = "val supplied: Int = 7"
    val base = new ContextBase
    val reporter = new StoreReporter(null)
    given Context = base.initialCtx.fresh.setReporter(reporter)
    val parsed = new Parser(SourceFile.virtual("Supplied.scala", source)).parse()
    val tree = parsed.asInstanceOf[untpd.PackageDef].stats.head

    val result = RawDefinitionAdapter.adaptIsolated(tree, source, sourceId).toOption.get

    assert(result.shape.isInstanceOf[DefinitionShape.ImmutableVal])
    assertEquals(result.shape.name.source, "supplied")
    assertEquals(result.originMap, None)
  }

  test("adapter error messages do not expose raw node names dumps or peer ownership") {
    val errors = List(
      rejected("class Answer").diagnostic,
      rejected("def answer: scala.Int = 42").diagnostic,
      rejected("val answer: Int = { 42 }").diagnostic
    )

    errors.foreach { error =>
      assert(!error.message.contains("TypeDef"))
      assert(!error.message.contains("Block"))
      assert(!error.message.contains("dotty"))
      assert(!error.message.toLowerCase.contains("macroparadise"))
    }
  }

  private def accepted(source: String): LocatedDefinitionShape =
    RawDefinitionParser.parseStandalone(source, sourceId) match
      case Right(value) => value
      case Left(error) => fail(s"Expected acceptance but received: ${error.diagnostic.message}")

  private def rejected(source: String): LocatedDiagnostic[RawDefinitionAdapterError] =
    RawDefinitionParser.parseStandalone(source, sourceId) match
      case Left(error) => error
      case Right(value) => fail(s"Expected rejection but received: ${value.render}")

  private def assertError(source: String, expected: RawDefinitionAdapterError): Unit =
    assertEquals(rejected(source).diagnostic, expected)
