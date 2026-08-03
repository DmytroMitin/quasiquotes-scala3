package quasiquotes.definitions.parser

import quasiquotes.definitions.dotty.RawDefinitionParser
import quasiquotes.source.SourceId

class DefinitionTemplateSourceAdapterOneParseTest extends munit.FunSuite:
  import DefinitionTemplateHoleCategory.*

  private def bodyTerm(name: String) =
    CategorizedDefinitionHoleOccurrence(name, BodyTerm)

  test("successful definition frontend invokes the full-definition parser exactly once") {
    var calls = 0
    val result =
      DefinitionTemplateSourceAdapter.parseLocatedUsing(
        "def answer: Int = $value",
        Vector(bodyTerm("value"))
      ) { (source, sourceId) =>
        calls += 1
        RawDefinitionParser.parseEnvelopeStandalone(source, sourceId)
      }

    assert(result.isRight)
    assertEquals(calls, 1)
  }

  test("parser failure still invokes the full-definition parser exactly once") {
    var calls = 0
    val result =
      DefinitionTemplateSourceAdapter.parseLocatedUsing(
        "def answer: Int =",
        Vector.empty
      ) { (source, sourceId) =>
        calls += 1
        RawDefinitionParser.parseEnvelopeStandalone(source, sourceId)
      }

    assert(result.isLeft)
    assertEquals(calls, 1)
  }

  test("invalid pre-parse plans invoke no parser") {
    var calls = 0
    val result =
      DefinitionTemplateSourceAdapter.parseLocatedUsing(
        "def answer: Int = $value",
        Vector.empty
      ) { (source, sourceId) =>
        calls += 1
        RawDefinitionParser.parseEnvelopeStandalone(source, sourceId)
      }

    assert(result.isLeft)
    assertEquals(calls, 0)
  }

  test("a corrupted raw definition type cannot introduce an unowned generated marker") {
    val result =
      DefinitionTemplateSourceAdapter.parseLocatedUsing(
        "def answer: Int = 1",
        Vector.empty
      ) { (_, _) =>
        RawDefinitionParser.parseEnvelopeStandalone(
          "def answer: __qq_dt_type_intruder = 1",
          SourceId.VirtualDefinitionTemplateParserInput
        )
      }

    assert(
      result.left.toOption.get.diagnostic
        .isInstanceOf[
          DefinitionTemplateSourceAdapterError.UnknownGeneratedMarker
        ]
    )
  }
