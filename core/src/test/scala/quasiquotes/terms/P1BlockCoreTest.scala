package quasiquotes.terms

import quasiquotes.matching.TermPattern
import quasiquotes.parser.TermShape

class P1BlockCoreTest extends munit.FunSuite:
  import TermCoreTestFixtures.*

  private val firstTransport = "__first_transport"
  private val resultTransport = "__result_transport"

  test("P1 block structure preserves ordered prefixes and a distinct final result") {
    val block = TermShape.Block(
      List(
        TermShape.Apply(ident("first"), Nil),
        TermShape.Apply(ident("second"), List(TermShape.Literal("2")))
      ),
      ident("result")
    )

    assertEquals(
      block.render,
      "Block([Apply(Ident(first), []), Apply(Ident(second), [Literal(2)])], Ident(result))"
    )
    assertNotEquals(
      template(block).toOption.get,
      template(
        TermShape.Block(
          List(
            TermShape.Apply(ident("second"), List(TermShape.Literal("2"))),
            TermShape.Apply(ident("first"), Nil)
          ),
          ident("result")
        )
      ).toOption.get
    )
    assertNotEquals(
      template(block).toOption.get,
      template(
        TermShape.Block(
          List(
            TermShape.Apply(ident("first"), Nil),
            ident("result")
          ),
          TermShape.Apply(ident("second"), List(TermShape.Literal("2")))
        )
      ).toOption.get
    )
  }

  test("P1 block completion substitutes prefix and result holes in source order") {
    val source = TermShape.Block(
      List(ident(firstTransport)),
      ident(resultTransport)
    )
    val blockTemplate = template(
      source,
      termEntries = Vector(
        "first" -> firstTransport,
        "result" -> resultTransport
      ),
      termOccurrences = Vector(
        TermHoleOccurrence("first", 0),
        TermHoleOccurrence("result", 1)
      )
    ).toOption.get

    val completed = blockTemplate.complete(
      Map(
        "first" -> ConstructedTerm.fromShape(TermShape.Literal("1")).toOption.get,
        "result" -> ConstructedTerm.fromShape(TermShape.Literal("2")).toOption.get
      ),
      Map.empty
    ).toOption.get

    assertEquals(
      completed.root,
      TermShape.Block(List(TermShape.Literal("1")), TermShape.Literal("2"))
    )
  }

  test("P1 block pattern rendering keeps prefix and result positions distinct") {
    val pattern = TermPattern.Block(
      List(TermPattern.Hole("first"), TermPattern.Identifier("consume")),
      TermPattern.Hole("result")
    )

    assertEquals(
      pattern.render,
      "Block([Hole($first), Ident(consume)], Hole($result))"
    )
  }
