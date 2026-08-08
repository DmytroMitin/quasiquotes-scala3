package quasiquotes.parser

import dotty.tools.dotc.ast.untpd
import quasiquotes.source.SourceSpan

class InterpolatedStringRawPreflightTest extends munit.FunSuite:
  private val cases = List(
    "s\"plain\"",
    "s\"hello $name\"",
    "s\"value = ${foo(x)}\"",
    "s\"$a / $b\"",
    "s\"prefix ${foo(x)} suffix\"",
    "s\"literal $$ dollar\"",
    "s\"${-x}\"",
    "s\"${foo(x)}\"",
    "s\"${(x, y)}\"",
    "s\"${if cond then x else y}\""
  )

  cases.foreach { source =>
    test(s"raw parser confirms interpolation root and semantic invariant for $source") {
      val parsed = TinyTermParser.parseOrThrow(source)
      val shape = parsed.shape.asInstanceOf[TermShape.InterpolatedString]
      assertEquals(shape.prefix, "s")
      assertEquals(shape.parts.size, shape.arguments.size + 1)
      assertEquals(DottySourceSpanAdapter.fromTree(parsed.rawTree), Some(SourceSpan(0, source.length)))
      assert(parsed.rawStructure.startsWith("InterpolatedString(s,"))
    }
  }

  test("raw direct and braced forms normalize to the same argument structure") {
    val direct = TinyTermParser.parseOrThrow("s\"$name\"").shape
    val braced = TinyTermParser.parseOrThrow("s\"${name}\"").shape
    assertEquals(direct, braced)
  }

  test("raw interpolation argument trees retain focused component spans") {
    val source = "s\"prefix ${foo(x)} suffix\""
    val parsed = TinyTermParser.parseOrThrow(source)
    parsed.rawTree match
      case interpolation: untpd.InterpolatedString =>
        val argument = interpolation.segments.collectFirst {
          case untpd.Thicket(_ :: value :: Nil) => value
        }.getOrElse(fail("missing interpolation argument"))
        val span = DottySourceSpanAdapter.fromTree(argument).getOrElse(fail("missing argument span"))
        assertEquals(source.slice(span.start, span.end), "{foo(x)}")
      case other => fail(s"expected InterpolatedString, got ${other.getClass.getSimpleName}")
  }

  test("unsupported prefixes remain visible but outside the admitted s tranche") {
    List("raw\"hello $name\"", "f\"value = $x%d\"").foreach { source =>
      assert(TinyTermParser.parseOrThrow(source).shape.isInstanceOf[TermShape.Unsupported])
    }
  }
