package quasiquotes.terms.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags

import quasiquotes.parser.{TermShapeInspector, TinyTermParser}

class P2LocalValRawCharacterizationTest extends munit.FunSuite:
  private final case class Oracle(
      source: String,
      structure: String,
      definitionSpan: (Int, Int, Int),
      declaredTypeSpan: (Int, Int, Int),
      initializerSpan: (Int, Int, Int),
      laterStatementSpans: Vector[(Int, Int, Int)],
      resultSpan: (Int, Int, Int)
  )

  private val emptyFlags = Flags.EmptyFlags.toString
  private val parameterFlags = Flags.Param.toString

  private val cases = Vector(
    Oracle(
      "{ val x: Int = 1; x }",
      s"Block([ValDef(x,Ident(Int),Number(1,Whole(10)),flags=$emptyFlags)], Ident(x))",
      (2, 6, 16),
      (9, 9, 12),
      (15, 15, 16),
      Vector.empty,
      (18, 18, 19)
    ),
    Oracle(
      "{ val x: String = \"a\"; x }",
      s"Block([ValDef(x,Ident(String),Literal(String(\"a\")),flags=$emptyFlags)], Ident(x))",
      (2, 6, 21),
      (9, 9, 15),
      (18, 18, 21),
      Vector.empty,
      (23, 23, 24)
    ),
    Oracle(
      "{ val x: Boolean = true; if x then 1 else 0 }",
      s"Block([ValDef(x,Ident(Boolean),Literal(Boolean(true)),flags=$emptyFlags)], If(Ident(x),Number(1,Whole(10)),Number(0,Whole(10))))",
      (2, 6, 23),
      (9, 9, 16),
      (19, 19, 23),
      Vector.empty,
      (25, 25, 43)
    ),
    Oracle(
      "{ val x: Int = 1; f(x); x + 1 }",
      s"Block([ValDef(x,Ident(Int),Number(1,Whole(10)),flags=$emptyFlags), Apply(Ident(f), [Ident(x)])], InfixOp(Ident(x),Ident(+),Number(1,Whole(10))))",
      (2, 6, 16),
      (9, 9, 12),
      (15, 15, 16),
      Vector((18, 19, 22)),
      (24, 26, 29)
    ),
    Oracle(
      "{ val x: Int = 1; (inner: Int) => x + inner }",
      s"Block([ValDef(x,Ident(Int),Number(1,Whole(10)),flags=$emptyFlags)], Function([ValDef(inner,Ident(Int),EmptyTree,flags=$parameterFlags)], Block([], InfixOp(Ident(x),Ident(+),Ident(inner)))))",
      (2, 6, 16),
      (9, 9, 12),
      (15, 15, 16),
      Vector.empty,
      (18, 31, 43)
    )
  )

  cases.foreach { oracle =>
    test(s"parser exposes exact P2 raw topology and spans: ${oracle.source}") {
      withContext {
        val parsed = TinyTermParser.parseOrThrow(oracle.source)

        assertEquals(exactStructure(parsed.rawTree), oracle.structure)
        parsed.rawTree match
          case block @ untpd.Block((definition: untpd.ValDef) :: later, result) =>
            assertEquals(definition.name.toString, "x")
            assertEquals(definition.mods.flags, Flags.EmptyFlags)
            assertEquals(span(block), (0, 0, oracle.source.length))
            assertEquals(span(definition), oracle.definitionSpan)
            assertEquals(span(definition.tpt), oracle.declaredTypeSpan)
            assertEquals(span(definition.rhs), oracle.initializerSpan)
            assertEquals(later.map(span).toVector, oracle.laterStatementSpans)
            assertEquals(span(result), oracle.resultSpan)
          case other =>
            fail(s"expected Block with leading ValDef, found ${other.getClass.getSimpleName}")
      }
    }
  }

  test("parser exposes exact P2-to-Lambda child topology and points") {
    withContext {
      val parsed = TinyTermParser.parseOrThrow(
        "{ val x: Int = 1; (inner: Int) => x + inner }"
      )

      parsed.rawTree match
        case untpd.Block(
              (_: untpd.ValDef) :: Nil,
              untpd.Function(
                (parameter: untpd.ValDef) :: Nil,
                bodyBlock @ untpd.Block(Nil, body: untpd.InfixOp)
              )
            ) =>
          assertEquals(parameter.name.toString, "inner")
          assertEquals(parameter.mods.flags, Flags.Param)
          assert(parameter.rhs.isEmpty)
          assertEquals(span(parameter), (19, 19, 29))
          assertEquals(span(parameter.tpt), (26, 26, 29))
          assertEquals(span(bodyBlock), (34, 34, 43))
          assertEquals(span(body), (34, 36, 43))
          assertEquals(exactStructure(body), "InfixOp(Ident(x),Ident(+),Ident(inner))")
        case other => fail(s"unexpected P2/Lambda raw tree: ${other.getClass.getSimpleName}")
    }
  }

  private def exactStructure(tree: untpd.Tree)(using Context): String =
    tree match
      case definition: untpd.ValDef =>
        val rhs = if definition.rhs.isEmpty then "EmptyTree" else exactStructure(definition.rhs)
        s"ValDef(${definition.name},${exactStructure(definition.tpt)},$rhs,flags=${definition.mods.flags})"
      case untpd.Function(parameters, body) =>
        s"Function([${parameters.map(exactStructure).mkString(", ")}], ${exactStructure(body)})"
      case untpd.Block(statements, result) =>
        s"Block([${statements.map(exactStructure).mkString(", ")}], ${exactStructure(result)})"
      case untpd.Apply(function, arguments) =>
        s"Apply(${exactStructure(function)}, [${arguments.map(exactStructure).mkString(", ")}])"
      case untpd.InfixOp(left, operator, right) =>
        s"InfixOp(${exactStructure(left)},${exactStructure(operator)},${exactStructure(right)})"
      case untpd.If(condition, thenBranch, elseBranch) =>
        s"If(${exactStructure(condition)},${exactStructure(thenBranch)},${exactStructure(elseBranch)})"
      case other => TermShapeInspector.rawStructure(other)

  private def span(tree: untpd.Tree): (Int, Int, Int) =
    (tree.span.start, tree.span.point, tree.span.end)

  private def withContext(body: Context ?=> Unit): Unit =
    val base = new ContextBase
    given Context = base.initialCtx
    body
