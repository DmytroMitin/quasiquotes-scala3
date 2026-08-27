package quasiquotes.construct

import scala.quoted.staging.{Compiler, withQuotes}

import quasiquotes.parser.TinyTermParser

final class NullarySelectedCallCompositionProbeTest extends munit.FunSuite:
  test("source parser retains an explicit empty Apply outside the selected capacity method"):
    val parsed = TinyTermParser.parseOrThrow(
      "new java.lang.StringBuilder(__qq_term_hole_0).capacity()"
    )

    assertEquals(
      parsed.rawStructure,
      "Apply(Select(Apply(Select(New(Select(Select(Ident(java), lang), StringBuilder)), <init>), [Ident(__qq_term_hole_0)]), capacity), [])"
    )

  test("eagerly normalizing the selected capacity method consumes its empty parameter list"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*

      val constructor = Select.overloaded(
        New(TypeTree.ref(Symbol.requiredClass("java.lang.StringBuilder"))),
        "<init>",
        Nil,
        Literal(IntConstant(16)) :: Nil
      )
      val selected = Select.unique(constructor, "capacity")
      val selectedMethod = selected.tpe.widen match
        case method: MethodType => method
        case other => fail(s"Expected capacity to select a MethodType, obtained ${other.show}")
      val normalized = selected.appliedToNone

      assertEquals(selectedMethod.paramNames, Nil)
      assert(normalized.tpe.widen =:= TypeRepr.of[Int])
