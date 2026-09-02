package quasiquotes.terms.dotty

import dotty.tools.dotc.ast.untpd

import quasiquotes.parser.TinyTermParser

class P1BlockRawCharacterizationTest extends munit.FunSuite:
  private val cases = Vector(
    "{ 1; 2 }" ->
      "Block([Number(1,Whole(10))], Number(2,Whole(10)))",
    "{ first(); second(); result }" ->
      "Block([Apply(Ident(first), []), Apply(Ident(second), [])], Ident(result))",
    "{ 1; { 2; 3 }; 4 }" ->
      "Block([Number(1,Whole(10)), Block([Number(2,Whole(10))], Number(3,Whole(10)))], Number(4,Whole(10)))",
    "{ if cond then 1 else 2; f(3) }" ->
      "Block([If(Ident(cond),Number(1,Whole(10)),Number(2,Whole(10)))], Apply(Ident(f), [Number(3,Whole(10))]))"
  )

  cases.foreach { (source, expected) =>
    test(s"parser keeps binder-free P1 prefix/result topology: $source") {
      val parsed = TinyTermParser.parseOrThrow(source)

      assertEquals(parsed.rawStructure, expected)
      parsed.rawTree match
        case untpd.Block(prefix, result) =>
          assert(prefix.nonEmpty)
          assertEquals(prefix.size + 1, directChildren(parsed.rawTree).size)
          assertEquals(directChildren(parsed.rawTree).last, result)
          assertEquals(parsed.rawTree.span.start, 0)
          assertEquals(parsed.rawTree.span.end, source.length)
          directChildren(parsed.rawTree).foreach { child =>
            assert(child.span.exists)
            assert(child.span.start >= parsed.rawTree.span.start)
            assert(child.span.end <= parsed.rawTree.span.end)
          }
        case other =>
          fail(s"expected Block, found ${other.getClass.getSimpleName}")
    }
  }

  test("parser keeps a raw P0 wrapper while Core inspection remains transparent") {
    val source = "{ 1 }"
    val parsed = TinyTermParser.parseOrThrow(source)

    assertEquals(parsed.rawStructure, "Block([], Number(1,Whole(10)))")
    assertEquals(parsed.shape.render, "Literal(1)")
    parsed.rawTree match
      case untpd.Block(Nil, result: untpd.Number) =>
        assertEquals(parsed.rawTree.span.start, 0)
        assertEquals(parsed.rawTree.span.end, source.length)
        assertEquals(result.span.start, 2)
        assertEquals(result.span.end, 3)
      case other =>
        fail(s"expected transparent raw P0 Block, found ${other.getClass.getSimpleName}")
  }

  test("parser exposes exact spans for the canonical two-expression P1") {
    val parsed = TinyTermParser.parseOrThrow("{ 1; 2 }")

    parsed.rawTree match
      case block @ untpd.Block(first :: Nil, result) =>
        assertEquals((block.span.start, block.span.point, block.span.end), (0, 0, 8))
        assertEquals((first.span.start, first.span.point, first.span.end), (2, 2, 3))
        assertEquals((result.span.start, result.span.point, result.span.end), (5, 5, 6))
      case other =>
        fail(s"expected two-expression Block, found ${other.getClass.getSimpleName}")
  }

  private def directChildren(tree: untpd.Tree): Vector[untpd.Tree] =
    tree match
      case untpd.Block(prefix, result) => prefix.toVector :+ result
      case _ => Vector.empty
