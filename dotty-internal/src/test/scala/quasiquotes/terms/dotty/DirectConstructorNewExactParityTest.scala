package quasiquotes.terms.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.Spans.NoSpan
import dotty.tools.dotc.util.{NoSource, SourceFile}

import quasiquotes.neutral.ScalametaTermProjection
import quasiquotes.parser.{TermShape, TermShapeInspector, TinyTermParser}
import quasiquotes.terms.ConstructedTerm

import scala.meta.*
import scala.meta.dialects.Scala3

class DirectConstructorNewExactParityTest extends munit.FunSuite:
  import CoreTermShapeUntypedLowererError.*

  private val fixtures = Vector(
    "new java.lang.StringBuilder()" ->
      TermShape.New("java.lang.StringBuilder", Nil),
    "new java.lang.StringBuilder(16)" ->
      TermShape.New(
        "java.lang.StringBuilder",
        List(TermShape.Literal("16"))
      ),
    "new java.lang.RuntimeException(\"boom\")" ->
      TermShape.New(
        "java.lang.RuntimeException",
        List(TermShape.Literal("\"boom\""))
      ),
    "new java.lang.StringBuilder(foo(x))" ->
      TermShape.New(
        "java.lang.StringBuilder",
        List(
          TermShape.Apply(
            ident("foo"),
            List(ident("x"))
          )
        )
      ),
    "new java.lang.StringBuilder(if cond then 8 else 16)" ->
      TermShape.New(
        "java.lang.StringBuilder",
        List(
          TermShape.If(
            ident("cond"),
            TermShape.Literal("8"),
            TermShape.Literal("16")
          )
        )
      ),
    "new synthetic.unresolved.Widget(new other.missing.Value(1))" ->
      TermShape.New(
        "synthetic.unresolved.Widget",
        List(
          TermShape.New(
            "other.missing.Value",
            List(TermShape.Literal("1"))
          )
        )
      )
  )

  test("direct exact lowering admits valid zero ordinary and nested constructor arguments") {
    withContext {
      fixtures.foreach { case (source, shape) =>
        val expected = TinyTermParser.parseOrThrow(source).rawStructure
        val actual = CoreTermShapeUntypedLowerer
          .lower(shape)
          .map(TermShapeInspector.rawStructure)

        assertEquals(actual, Right(expected), clues(source, shape))
      }
    }
  }

  test("parser and direct constructor paths use term qualifiers and a terminal type name") {
    withContext {
      val source = fixtures.head._1
      val expected = TinyTermParser.parseOrThrow(source).rawTree
      val actual = lowerOrFail(fixtures.head._2)

      assertEquals(constructorNameRoles(expected), Vector(false, false, true))
      assertEquals(constructorNameRoles(actual), constructorNameRoles(expected))
    }
  }

  test("direct exact constructor lowering agrees with the richer source-free backend") {
    withContext {
      fixtures.foreach { case (_, shape) =>
        val direct = lowerOrFail(shape)
        val constructed = ConstructedTerm.fromShape(shape).toOption.get
        val richer = ConstructedTermUntypedBackend.lower(constructed).toOption.get

        assertEquals(
          TermShapeInspector.rawStructure(direct),
          TermShapeInspector.rawStructure(richer),
          clues(shape)
        )
      }
    }
  }

  test("constructor lowering is recursively source span symbol and TypedSplice free") {
    withContext {
      val raw = lowerOrFail(fixtures.last._2)
      val trees = allTrees(raw)

      assertEquals(trees.count(_.isInstanceOf[untpd.New]), 2)
      assert(
        trees.collect { case selected: untpd.Select => selected.name.toString }
          .contains("Widget")
      )
      assert(
        trees.collect { case selected: untpd.Select => selected.name.toString }
          .contains("Value")
      )
      trees.foreach { tree =>
        assert(!tree.source.exists, clues(tree.getClass.getSimpleName))
        assert(!tree.span.exists, clues(tree.getClass.getSimpleName))
        assertEquals(tree.symbol, NoSymbol, clues(tree.getClass.getSimpleName))
        assert(!tree.isInstanceOf[untpd.TypedSplice])
      }
    }
  }

  test("source-free verification descends through New tpt") {
    val source = "new java.lang.StringBuilder()"
    val base = new ContextBase
    val reporter = new StoreReporter(null)
    given Context = base.initialCtx.fresh.setReporter(reporter)
    val sourceFile = SourceFile.virtual("U009VerifierOracle.scala", source)
    val parsed = new Parser(sourceFile).expr()
    val sourcedType = parsed match
      case untpd.Apply(untpd.Select(fresh: untpd.New, _), Nil) => fresh.tpt
      case other => fail(s"unexpected parser constructor tree: ${other.show}")
    val sourceFreeRoot =
      locally {
        given SourceFile = NoSource
        untpd.New(sourcedType).withSpan(NoSpan)
      }

    val verification =
      CoreTermShapeUntypedLowerer.verifySourceFreeForTest(sourceFreeRoot)
    assert(
      verification
        .left
        .toOption
        .exists {
          case SourceFreeInvariantViolation(nodeKind, detail) =>
            nodeKind == "Select" && detail == "the node has a span."
          case _ => false
        },
      clues(verification, sourceFreeRoot.source, sourceFreeRoot.span, sourcedType.source, sourcedType.span)
    )
  }

  test("direct constructor boundary reuses the exact shared name policy") {
    val invalidNames = Vector(
      null,
      "StringBuilder",
      ".java.lang.StringBuilder",
      "java.lang.StringBuilder.",
      "java..lang.StringBuilder",
      "java.lang.StringBuilder[Int]",
      "java.lang.`StringBuilder`",
      "java.lang.StringBuilder$"
    )

    withContext {
      invalidNames.foreach { name =>
        assert(
          CoreTermShapeUntypedLowerer
            .lower(TermShape.New(name, Nil))
            .left
            .toOption
            .exists {
              case InvalidConstructorName(actual, _) =>
                actual == String.valueOf(name)
              case _ => false
            },
          clues(name)
        )
      }
    }
  }

  test("direct constructor arguments fail closed with stable index and child errors") {
    withContext {
      assertEquals(
        CoreTermShapeUntypedLowerer.lower(
          TermShape.New("java.lang.StringBuilder", null)
        ),
        Left(MalformedConstructorArguments(-1))
      )
      assertEquals(
        CoreTermShapeUntypedLowerer.lower(
          TermShape.New("java.lang.StringBuilder", List(null))
        ),
        Left(NullConstructorArgument(0))
      )
      assertEquals(
        CoreTermShapeUntypedLowerer.lower(
          TermShape.New(
            "java.lang.StringBuilder",
            List(TermShape.Literal("1"), null)
          )
        ),
        Left(NullConstructorArgument(1))
      )
      assertEquals(
        CoreTermShapeUntypedLowerer.lower(
          TermShape.New(
            "java.lang.StringBuilder",
            List(TermShape.Unsupported("Match", "unsupported child"))
          )
        ),
        Left(UnsupportedTermShape("Unsupported"))
      )
      assert(
        CoreTermShapeUntypedLowerer
          .lower(
            TermShape.New(
              "java.lang.StringBuilder",
              List(TermShape.New("Value", Nil))
            )
          )
          .left
          .toOption
          .exists(_.isInstanceOf[InvalidConstructorName])
      )
    }
  }

  test("accepted N008 constructors compose directly into parser-equivalent exact trees") {
    val sources = Vector(
      "new java.lang.StringBuilder()",
      "new java.lang.StringBuilder(16)",
      "new synthetic.unresolved.Widget(1, 2)",
      "new java.lang.StringBuilder(foo(x))",
      "new java.lang.StringBuilder(if cond then 8 else 16)",
      "new synthetic.unresolved.Widget(new other.missing.Value(1))"
    )

    withContext {
      sources.foreach { source =>
        val meta = Scala3(source).parse[Term].get
        val projected = ScalametaTermProjection.project(meta).toOption.get
        val direct = lowerOrFail(projected.shape)

        assertEquals(
          TermShapeInspector.rawStructure(direct),
          TinyTermParser.parseOrThrow(source).rawStructure,
          clues(source, projected.shape)
        )
      }
    }
  }

  test("N008-owned constructor near misses remain rejected before direct lowering") {
    val cases = Vector(
      "new StringBuilder()" -> "NEUTRAL_NEW_CONSTRUCTOR_NAME_UNSUPPORTED",
      "new java.lang.`StringBuilder`()" -> "NEUTRAL_NEW_CONSTRUCTOR_NAME_UNSUPPORTED",
      "new java.lang.StringBuilder[Int](16)" -> "NEUTRAL_NEW_CONSTRUCTOR_TYPE_UNSUPPORTED",
      "new java.lang.StringBuilder(capacity = 16)" -> "NEUTRAL_NEW_ARGUMENT_UNSUPPORTED",
      "new java.lang.StringBuilder(values*)" -> "NEUTRAL_NEW_ARGUMENT_UNSUPPORTED",
      "new java.lang.StringBuilder(16)(17)" -> "NEUTRAL_NEW_ARGUMENT_LIST_UNSUPPORTED",
      "new java.lang.StringBuilder(16) { }" -> "NEUTRAL_NEW_ANONYMOUS_UNSUPPORTED"
    )

    cases.foreach { case (source, expectedCode) =>
      val projected = ScalametaTermProjection.project(
        Scala3(source).parse[Term].get
      )
      assertEquals(
        projected.left.toOption.map(_.code),
        Some(expectedCode),
        clues(source)
      )
    }
  }

  private def lowerOrFail(shape: TermShape)(using Context): untpd.Tree =
    CoreTermShapeUntypedLowerer.lower(shape).fold(
      error => fail(error.message),
      identity
    )

  private def ident(name: String): TermShape =
    TermShape.Identifier(name, isPlaceholder = false)

  private def allTrees(tree: untpd.Tree): Vector[untpd.Tree] =
    tree +: directChildren(tree).flatMap(allTrees)

  private def directChildren(tree: untpd.Tree): Vector[untpd.Tree] =
    tree match
      case selected: untpd.Select => Vector(selected.qualifier)
      case application: untpd.Apply => application.fun +: application.args.toVector
      case fresh: untpd.New => Vector(fresh.tpt)
      case conditional: untpd.If =>
        Vector(conditional.cond, conditional.thenp, conditional.elsep)
      case _ => Vector.empty

  private def constructorNameRoles(tree: untpd.Tree): Vector[Boolean] =
    tree match
      case untpd.Apply(untpd.Select(fresh: untpd.New, _), _) =>
        typePathNames(fresh.tpt).map(_.isTypeName)
      case other => fail(s"unexpected constructor topology: ${other.getClass.getSimpleName}")

  private def typePathNames(tree: untpd.Tree): Vector[dotty.tools.dotc.core.Names.Name] =
    tree match
      case identifier: untpd.Ident => Vector(identifier.name)
      case selected: untpd.Select => typePathNames(selected.qualifier) :+ selected.name
      case other => fail(s"unexpected constructor type path: ${other.getClass.getSimpleName}")

  private def withContext[A](body: Context ?=> A): A =
    val base = new ContextBase
    given Context = base.initialCtx
    body
