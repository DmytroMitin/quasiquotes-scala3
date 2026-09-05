package quasiquotes.terms.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags.EmptyFlags
import dotty.tools.dotc.core.Names.{termName, typeName}
import dotty.tools.dotc.core.Symbols.{NoSymbol, newSymbol}
import dotty.tools.dotc.core.Types.NoType
import dotty.tools.dotc.util.{NoSource, SourceFile}
import dotty.tools.dotc.util.Spans.Span

import quasiquotes.neutral.{ScalametaTermProjection, ScalametaTermShapeAuthoring}
import quasiquotes.parser.{TermShape, TermShapeInspector, TinyTermParser}

import scala.meta.{Term, Type}

class PrimitiveTypedTermShapeUntypedLowererTest extends munit.FunSuite:
  import CoreTermShapeUntypedLowererError.*
  import PrimitiveTypedTermShapeUntypedLowererError.*

  private val fixtures = Vector(
    (
      TermShape.Typed(TermShape.Identifier("x", false), "Int"),
      "(x: Int)",
      "Typed(Ident(x),Ident(Int))"
    ),
    (
      TermShape.Typed(TermShape.Literal("\"x\""), "String"),
      "(\"x\": String)",
      "Typed(Literal(String(\"x\")),Ident(String))"
    ),
    (
      TermShape.Typed(TermShape.Literal("true"), "Boolean"),
      "(true: Boolean)",
      "Typed(Literal(Boolean(true)),Ident(Boolean))"
    ),
    (
      TermShape.Typed(
        TermShape.Apply(
          TermShape.Identifier("f", false),
          List(TermShape.Literal("1"))
        ),
        "Int"
      ),
      "(f(1): Int)",
      "Typed(Apply(Ident(f), [Number(1,Whole(10))]),Ident(Int))"
    ),
    (
      TermShape.Typed(
        TermShape.Tuple(List(TermShape.Literal("1"), TermShape.Literal("2"))),
        "String"
      ),
      "((1, 2): String)",
      "Typed(Tuple([Number(1,Whole(10)), Number(2,Whole(10))]),Ident(String))"
    )
  )

  test("lowers exactly the bounded primitive Typed matrix through existing child semantics") {
    withContext {
      fixtures.foreach { case (shape, _, expected) =>
        val first = lowerOrFail(shape)
        val second = lowerOrFail(shape)

        assertEquals(TermShapeInspector.rawStructure(first), expected)
        assertEquals(TermShapeInspector.inspect(first), shape)
        assert(!first.asInstanceOf[AnyRef].eq(second.asInstanceOf[AnyRef]))
        assert(!first.expr.asInstanceOf[AnyRef].eq(second.expr.asInstanceOf[AnyRef]))
        allTrees(first).foreach { tree =>
          assert(!tree.source.exists, clues(shape, tree.getClass.getSimpleName))
          assert(!tree.span.exists, clues(shape, tree.getClass.getSimpleName))
          assertEquals(tree.symbol, NoSymbol, clues(shape, tree.getClass.getSimpleName))
          assert(!tree.isInstanceOf[untpd.TypedSplice])
        }
      }
    }
  }

  test("agrees with the parser oracle after removing only the source Parens wrapper") {
    withContext {
      fixtures.foreach { case (shape, source, _) =>
        val parserInner = TinyTermParser.parseOrThrow(source).rawTree match
          case untpd.Parens(inner: untpd.Typed) => inner
          case other => fail(s"expected Parens(Typed), found $other")

        assertEquals(
          TermShapeInspector.rawStructure(lowerOrFail(shape)),
          TermShapeInspector.rawStructure(parserInner),
          clues(source)
        )
      }
    }
  }

  test("accepts the semantic value supplied by neutral authoring and projection") {
    withContext {
      fixtures.foreach { case (shape, _, expected) =>
        val authored = ScalametaTermShapeAuthoring
          .author(shape)
          .fold(problem => fail(problem.message), identity)
        val projected = ScalametaTermProjection
          .project(authored)
          .fold(problem => fail(problem.message), _.shape)

        assertEquals(projected, shape)
        assertEquals(
          TermShapeInspector.rawStructure(lowerOrFail(projected)),
          expected
        )
      }
    }
  }

  test("keeps the direct Core lowerer and public Scalameta bridge closed for Typed") {
    withContext {
      val shape = fixtures.head._1
      assertEquals(
        CoreTermShapeUntypedLowerer.lower(shape),
        Left(UnsupportedTermShape("Typed"))
      )

      val publicResult = ScalametaTermUntypedBridge.lower(
        Term.Ascribe(Term.Name("x"), Type.Name("Int"))
      )
      val failure = publicResult.left.toOption.getOrElse(
        fail("the public bridge unexpectedly admitted primitive Typed")
      )
      assertEquals(failure.code, "EXACT_LOWERING_FAILED")
      assert(failure.detail.contains("Unsupported core TermShape"))
      assert(failure.detail.endsWith("Typed."))
    }
  }

  test("returns structured boundary and primitive-type failures") {
    withContext {
      assertEquals(
        PrimitiveTypedTermShapeUntypedLowerer.lower(null),
        Left(PrimitiveTypedTermShapeUntypedLowererError.MissingTermShape)
      )
      assertEquals(
        PrimitiveTypedTermShapeUntypedLowerer.lower(TermShape.Literal("1")),
        Left(WrongTermShapeFamily("Literal"))
      )
      assertEquals(
        PrimitiveTypedTermShapeUntypedLowerer.lower(TermShape.Typed(null, "Int")),
        Left(MissingTypedExpression)
      )
      assertEquals(
        PrimitiveTypedTermShapeUntypedLowerer.lower(
          TermShape.Typed(TermShape.Literal("1"), null)
        ),
        Left(MissingPrimitiveTypeName)
      )

      Vector(
        "",
        "Long",
        "Any",
        "AnyVal",
        "scala.Int",
        "java.lang.String",
        "Option[Int]",
        "Either[Int, String]",
        "(Int, String)",
        "Int => String",
        "UserType"
      ).foreach { invalid =>
        assertEquals(
          PrimitiveTypedTermShapeUntypedLowerer.lower(
            TermShape.Typed(TermShape.Literal("1"), invalid)
          ),
          Left(UnsupportedPrimitiveTypeName(invalid)),
          clues(invalid)
        )
      }
    }
  }

  test("preserves exact existing child-lowering failures without widening recursion") {
    withContext {
      val failures = Vector(
        TermShape.Identifier("_", true) -> PlaceholderIdentifier("_"),
        TermShape.Parenthesized(TermShape.Literal("1")) ->
          UnsupportedTermShape("Parenthesized"),
        TermShape.Unsupported("Hostile", "test") -> UnsupportedTermShape("Unsupported"),
        TermShape.Apply(
          TermShape.Apply(TermShape.Identifier("f", false), Nil),
          List(TermShape.Literal("1"))
        ) -> MultipleApplicationLists,
        TermShape.Typed(TermShape.Identifier("x", false), "Int") ->
          UnsupportedTermShape("Typed")
      )

      failures.foreach { case (child, expected) =>
        assertEquals(
          PrimitiveTypedTermShapeUntypedLowerer.lower(
            TermShape.Typed(child, "Int")
          ),
          Left(ExpressionLoweringFailure(expected)),
          clues(child)
        )
      }
    }
  }

  test("rejects malformed or contaminated raw graphs through the narrow test seam") {
    withContext {
      given SourceFile = NoSource
      val cleanExpression = untpd.Ident(termName("x"))
      val cleanType = untpd.Ident(typeName("Int"))

      assertEquals(
        CoreTermShapeUntypedLowerer.verifySourceFreeForTest(null),
        Left(SourceFreeInvariantViolation("null", "the node is null."))
      )

      Vector[untpd.Tree](
        null,
        untpd.Typed(cleanExpression, null),
        untpd.Typed(null, cleanType)
      ).foreach { malformed =>
        val result = PrimitiveTypedTermShapeUntypedLowerer
          .validateRawForTest(malformed, "Int")
        assert(
          result.left.toOption.exists(_.isInstanceOf[RawTopologyMismatch]),
          clues(malformed, result)
        )
      }

      assert(
        PrimitiveTypedTermShapeUntypedLowerer
          .validateRawForTest(untpd.Ident(termName("x")), "Int")
          .left
          .toOption
          .exists(_.isInstanceOf[RawTopologyMismatch])
      )
      assert(
        PrimitiveTypedTermShapeUntypedLowerer
          .validateRawForTest(
            untpd.Typed(cleanExpression, untpd.Ident(typeName("String"))),
            "Int"
          )
          .left
          .toOption
          .exists(_.isInstanceOf[RawTopologyMismatch])
      )

      val sourceful = cleanExpression.cloneIn(
        SourceFile.virtual("U027Corrupt.scala", "x")
      )
      val spanned = untpd.Ident(termName("x")).withSpan(Span(0, 1))
      val symbol = newSymbol(NoSymbol, termName("u027Symbol"), EmptyFlags, NoType)
      val symbolBearing = untpd.Ident(termName("x")).withType(symbol.termRef)
      val typedSplice = untpd.TypedSplice(symbolBearing)

      Vector(sourceful, spanned, symbolBearing, typedSplice).foreach { corrupt =>
        val result = PrimitiveTypedTermShapeUntypedLowerer.validateRawForTest(
          untpd.Typed(corrupt, cleanType),
          "Int"
        )
        assert(
          result.left.toOption.exists(_.isInstanceOf[SourceFreeInvariantFailure]),
          clues(corrupt, result)
        )
      }
    }
  }

  private def lowerOrFail(
      shape: TermShape
  )(using Context): untpd.Typed =
    PrimitiveTypedTermShapeUntypedLowerer
      .lower(shape)
      .fold(problem => fail(problem.message), identity)

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree +: (tree match
      case untpd.Typed(expression, typeTree) =>
        allTrees(expression) ++ allTrees(typeTree)
      case untpd.Apply(function, arguments) =>
        allTrees(function) ++ arguments.toVector.flatMap(allTrees)
      case untpd.Tuple(elements) => elements.toVector.flatMap(allTrees)
      case _ => Vector.empty)

  private def withContext[A](body: Context ?=> A): A =
    val base = new ContextBase
    given Context = base.initialCtx
    body
