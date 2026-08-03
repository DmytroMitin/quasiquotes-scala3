package quasiquotes.definitions.parser

import quasiquotes.parser.TermShape
import quasiquotes.terms.ConstructedTerm
import quasiquotes.types.TypeNormalForm

class DefinitionTemplateSourceAdapterCompositionTest
    extends munit.FunSuite:
  import DefinitionTemplateHoleCategory.*

  private def occurrence(
      name: String,
      category: DefinitionTemplateHoleCategory
  ) =
    CategorizedDefinitionHoleOccurrence(name, category)

  private def term(shape: TermShape): ConstructedTerm =
    ConstructedTerm.fromShape(shape).toOption.get

  test("categorized source completes at the frontend-to-core boundary") {
    val located =
      DefinitionTemplateSourceAdapter
        .parseLocated(
          "def `type`: List[$T] = if $condition then ($left: Option[$T]) else $right",
          Vector(
            occurrence("T", DefinitionType),
            occurrence("condition", BodyTerm),
            occurrence("left", BodyTerm),
            occurrence("T", BodyType),
            occurrence("right", BodyTerm)
          )
        )
        .fold(error => fail(error.diagnostic.message), identity)
    val completed =
      located
        .complete(
          Map(
            "condition" -> term(
              TermShape.Identifier("ready", false)
            ),
            "left" -> term(TermShape.Literal("1")),
            "right" -> term(TermShape.Literal("2"))
          ),
          Map("T" -> TypeNormalForm.STypeIdent("String"))
        )
        .fold(error => fail(error.diagnostic.message), identity)

    assertEquals(completed.name.render, "BacktickedKeywordName(`type`)")
    completed match
      case method: quasiquotes.definitions.ConstructedDefinition.ParameterlessDef =>
        assertEquals(
          method.resultType,
          TypeNormalForm.STypeApply(
            TypeNormalForm.STypeIdent("List"),
            List(TypeNormalForm.STypeIdent("String"))
          )
        )
        assertEquals(
          method.body.root,
          TermShape.If(
            TermShape.Identifier("ready", false),
            TermShape.Parenthesized(
              TermShape.Typed(TermShape.Literal("1"), "Option[String]")
            ),
            TermShape.Literal("2")
          )
        )
        assertEquals(
          method.body.ascriptionTypes,
          Vector(
            TypeNormalForm.STypeApply(
              TypeNormalForm.STypeIdent("Option"),
              List(TypeNormalForm.STypeIdent("String"))
            )
          )
        )
      case other => fail(s"Expected a parameterless def, received $other")
  }
