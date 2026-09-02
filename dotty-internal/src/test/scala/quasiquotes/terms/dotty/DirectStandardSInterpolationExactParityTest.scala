package quasiquotes.terms.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Names.termName
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.{NoSource, SourceFile, Spans}

import quasiquotes.neutral.ScalametaTermProjection
import quasiquotes.parser.{TermShape, TinyTermParser}
import quasiquotes.terms.ConstructedTerm

import scala.meta.*
import scala.meta.dialects.Scala3

final class DirectStandardSInterpolationExactParityTest extends munit.FunSuite:
  private final case class Fixture(
      label: String,
      source: String,
      shape: TermShape.InterpolatedString
  )

  private val fixtures = Vector(
    Fixture("plain", "s\"plain\"", interpolation(List("plain"))),
    Fixture(
      "direct identifier",
      "s\"hello $name\"",
      interpolation(List("hello ", ""), ident("name"))
    ),
    Fixture(
      "braced application",
      "s\"value=${foo(x)}\"",
      interpolation(
        List("value=", ""),
        TermShape.Apply(ident("foo"), List(ident("x")))
      )
    ),
    Fixture(
      "braced if",
      "s\"value=${if cond then 1 else 2}\"",
      interpolation(
        List("value=", ""),
        TermShape.If(ident("cond"), literal("1"), literal("2"))
      )
    ),
    Fixture(
      "braced constructor",
      "s\"new=${new java.lang.StringBuilder(\"x\")}\"",
      interpolation(
        List("new=", ""),
        TermShape.New(
          "java.lang.StringBuilder",
          List(literal("\"x\""))
        )
      )
    ),
    Fixture(
      "multiple arguments",
      "s\"a=$x b=$y\"",
      interpolation(List("a=", " b=", ""), ident("x"), ident("y"))
    ),
    Fixture(
      "semantic escapes",
      "s\"quote=\\\";slash=\\\\;line=\\n;return=\\r;tab=\\t;back=\\b;form=\\f;control=\\u0001;del=\\u007f;dollar=$$;unicode=λ\"",
      interpolation(
        List(
          "quote=\";slash=\\;line=\n;return=\r;tab=\t;back=\b;form=\f;control=\u0001;del=\u007f;dollar=$;unicode=λ"
        )
      )
    ),
    Fixture(
      "nested admitted children",
      "s\"nested=${outer(if cond then inner(1) else inner(2))}\"",
      interpolation(
        List("nested=", ""),
        TermShape.Apply(
          ident("outer"),
          List(
            TermShape.If(
              ident("cond"),
              TermShape.Apply(ident("inner"), List(literal("1"))),
              TermShape.Apply(ident("inner"), List(literal("2")))
            )
          )
        )
      )
    )
  )

  test("direct lowering closes parser and richer exact parity for the bounded standard-s family"):
    withContext:
      fixtures.foreach { fixture =>
        val direct = lowerOrFail(fixture.shape)
        val richer = ConstructedTermUntypedBackend
          .lower(ConstructedTerm.fromShape(fixture.shape).toOption.get)
          .fold(error => fail(error.message), identity)
        val parser = TinyTermParser.parseOrThrow(fixture.source).rawTree

        assertEquals(rawSummary(direct), rawSummary(parser), clues(fixture.label))
        assertEquals(rawSummary(direct), rawSummary(richer), clues(fixture.label))
        direct match
          case untpd.InterpolatedString(prefix, _) => assert(prefix.isTermName)
          case other => fail(s"expected InterpolatedString, found ${other.getClass.getSimpleName}")
        assertSourceFree(direct)
      }

  test("direct lowering preserves encoded raw literal payloads and exact direct or braced topology"):
    withContext:
      fixtures.foreach { fixture =>
        val direct = lowerOrFail(fixture.shape)
        val parser = TinyTermParser.parseOrThrow(fixture.source).rawTree
        assertEquals(rawLiteralValues(direct), rawLiteralValues(parser), clues(fixture.label))
        assertEquals(argumentWrapperKinds(direct), argumentWrapperKinds(parser), clues(fixture.label))
      }

  test("the missing direct case is distinct from child rejection"):
    withContext:
      val valid = interpolation(List("hello ", ""), ident("name"))
      assert(CoreTermShapeUntypedLowerer.lower(valid).isRight)

      val unsupportedChild = interpolation(
        List("", ""),
        TermShape.Unsupported("Hostile", "payload")
      )
      assertEquals(errorName(unsupportedChild), "UnsupportedTermShape")

      val malformedNestedApply = interpolation(
        List("", ""),
        TermShape.Apply(ident("f"), null)
      )
      assertEquals(errorName(malformedNestedApply), "MissingApplyArguments")

      val malformedNestedConstructor = interpolation(
        List("", ""),
        TermShape.New(null, Nil)
      )
      assertEquals(errorName(malformedNestedConstructor), "InvalidConstructorName")

  test("direct interpolation fails closed for every malformed carrier boundary"):
    val validArgument = ident("x")
    val cases = Vector(
      malformed(null, List("plain"), Nil) -> "UnsupportedInterpolationPrefix",
      malformed("raw", List("plain"), Nil) -> "UnsupportedInterpolationPrefix",
      malformed("f", List("plain"), Nil) -> "UnsupportedInterpolationPrefix",
      malformed("custom", List("plain"), Nil) -> "UnsupportedInterpolationPrefix",
      malformed("s", null, Nil) -> "MalformedInterpolation",
      malformed("s", List("plain"), null) -> "MalformedInterpolation",
      malformed("s", List("only"), List(validArgument)) -> "MalformedInterpolation",
      malformed("s", List(null), Nil) -> "NullInterpolationPart",
      malformed("s", List("", null, ""), List(validArgument, validArgument)) ->
        "NullInterpolationPart",
      malformed("s", List("", ""), List(null)) -> "NullInterpolationArgument",
      malformed("s", List("", "", ""), List(validArgument, null)) ->
        "NullInterpolationArgument"
    )

    withContext:
      cases.foreach { case (shape, expected) =>
        assertEquals(errorName(shape), expected)
      }

  test("source-free verification descends through interpolation thickets and braced blocks"):
    val base = new ContextBase
    given Context = base.initialCtx
    given SourceFile = NoSource
    val foreignSource = SourceFile.virtual("SourcedInterpolationChild.scala", "x")
    val sourced = untpd.Ident(termName("x")).cloneIn(foreignSource).withSpan(Spans.Span(0, 1))
    val raw = untpd.InterpolatedString(
      termName("s"),
      List(
        untpd.Thicket(
          List(
            untpd.Literal(Constant("value=")),
            untpd.Block(Nil, sourced)
          )
        ),
        untpd.Literal(Constant(""))
      )
    )

    assertEquals(
      CoreTermShapeUntypedLowerer.verifySourceFreeForTest(raw),
      Left(
        CoreTermShapeUntypedLowererError.SourceFreeInvariantViolation(
          "Ident",
          "the node has a source."
        )
      )
    )

  test("accepted N009R projection composes into direct exact lowering"):
    val sources = Vector(
      "s\"plain\"",
      "s\"hello $name\"",
      "s\"hello ${name}\"",
      "s\"value=${foo(x)}\"",
      "s\"a=$x b=$y\"",
      "s\"line=\\n$name tab=\\t\"",
      "s\"literal $$ dollar\"",
      "s\"new=${new java.lang.StringBuilder(\"x\")}\""
    )

    withContext:
      val directName = projected("s\"hello $name\"")
      val bracedName = projected("s\"hello ${name}\"")
      assertEquals(directName, bracedName)
      sources.foreach { source =>
        val shape = projected(source)
        val direct = lowerOrFail(shape)
        val canonical = ConstructedTermGeneratedOriginAdapter
          .lower(
            ConstructedTerm.fromShape(shape).toOption.get,
            "<u011-neutral-direct>"
          )
          .fold(error => fail(error.message), _.generatedSource)
        assertEquals(
          rawSummary(direct),
          rawSummary(TinyTermParser.parseOrThrow(canonical).rawTree),
          clues(source, canonical)
        )
      }

  test("N009R near misses remain rejected before direct lowering"):
    val rejected = Vector(
      "raw\"plain\"",
      "f\"value=$x%d\"",
      "custom\"plain\"",
      "s\"\"\"plain\"\"\"",
      "s\"${value match { case _ => 1 }}\""
    )
    rejected.foreach { source =>
      val term = source.parse[Term].get
      assert(ScalametaTermProjection.project(term).isLeft, clues(source))
    }
    val malformedEscape = Term.Interpolate(
      Term.Name("s"),
      List(Lit.String("\\q")),
      Nil
    )
    assert(ScalametaTermProjection.project(malformedEscape).isLeft)

  private def interpolation(
      parts: List[String],
      arguments: TermShape*
  ): TermShape.InterpolatedString =
    TermShape.InterpolatedString("s", parts, arguments.toList)

  private def ident(name: String): TermShape.Identifier =
    TermShape.Identifier(name, isPlaceholder = false)

  private def literal(value: String): TermShape.Literal =
    TermShape.Literal(value)

  private def projected(source: String): TermShape =
    ScalametaTermProjection
      .project(source.parse[Term].get)
      .fold(error => fail(error.message), _.shape)

  private def lowerOrFail(shape: TermShape)(using Context): untpd.Tree =
    CoreTermShapeUntypedLowerer
      .lower(shape)
      .fold(error => fail(error.message), identity)

  private def errorName(shape: TermShape)(using Context): String =
    CoreTermShapeUntypedLowerer
      .lower(shape)
      .left
      .toOption
      .fold("<success>")(_.getClass.getSimpleName.stripSuffix("$"))

  private def withContext[A](body: Context ?=> A): A =
    val base = new ContextBase
    given Context = base.initialCtx
    body

  private def assertSourceFree(tree: untpd.Tree)(using Context): Unit =
    allTrees(tree).foreach { current =>
      assert(!current.source.exists, clues(current.getClass.getSimpleName))
      assert(!current.span.exists, clues(current.getClass.getSimpleName))
      assertEquals(current.symbol, NoSymbol)
      assert(!current.isInstanceOf[untpd.TypedSplice])
    }

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree +: children(tree).flatMap(allTrees)

  private def children(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree match
      case value: untpd.InterpolatedString => value.segments.toVector
      case value: untpd.Thicket => value.trees.toVector
      case value: untpd.Block => value.stats.toVector :+ value.expr
      case value: untpd.Select => Vector(value.qualifier)
      case value: untpd.New => Vector(value.tpt)
      case value: untpd.Apply => value.fun +: value.args.toVector
      case value: untpd.InfixOp => Vector(value.left, value.op, value.right)
      case value: untpd.PrefixOp => Vector(value.op, value.od)
      case value: untpd.Tuple => value.trees.toVector
      case value: untpd.If => Vector(value.cond, value.thenp, value.elsep)
      case _ => Vector.empty

  private def rawSummary(tree: untpd.Tree)(using Context): String =
    val detail = tree match
      case untpd.InterpolatedString(prefix, segments) => s"($prefix,${segments.size})"
      case value: untpd.Thicket => s"(${value.trees.size})"
      case value: untpd.Literal => s"(${escape(String.valueOf(value.const.value))})"
      case value: untpd.Number => s"(${value.digits},${value.kind})"
      case value: untpd.Ident => s"(${value.name})"
      case value: untpd.Select => s"(${value.name})"
      case value: untpd.Apply => s"(${value.args.size})"
      case value: untpd.Block => s"(${value.stats.size})"
      case _ => ""
    s"${tree.getClass.getSimpleName}$detail[${children(tree).map(rawSummary).mkString(",")}]"

  private def rawLiteralValues(tree: untpd.Tree)(using Context): Vector[String] =
    allTrees(tree).collect { case value: untpd.Literal =>
      String.valueOf(value.const.value)
    }

  private def argumentWrapperKinds(tree: untpd.Tree)(using Context): Vector[String] =
    tree match
      case value: untpd.InterpolatedString =>
        value.segments.toVector.collect { case segment: untpd.Thicket =>
          segment.trees match
            case _ :: argument :: Nil => argument.getClass.getSimpleName
            case other => s"Malformed(${other.size})"
        }
      case _ => Vector.empty

  private def escape(value: String): String =
    value.flatMap {
      case '\\' => "\\\\"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      case other => other.toString
    }

  private def malformed(
      prefix: String,
      parts: List[String],
      arguments: List[TermShape]
  ): TermShape.InterpolatedString =
    val unsafeField = classOf[sun.misc.Unsafe].getDeclaredField("theUnsafe")
    unsafeField.setAccessible(true)
    val unsafe = unsafeField.get(null).asInstanceOf[sun.misc.Unsafe]
    val result = unsafe
      .allocateInstance(classOf[TermShape.InterpolatedString])
      .asInstanceOf[TermShape.InterpolatedString]
    def set(name: String, value: AnyRef): Unit =
      val field = classOf[TermShape.InterpolatedString].getDeclaredField(name)
      unsafe.putObject(result, unsafe.objectFieldOffset(field), value)
    set("prefix", prefix)
    set("parts", parts)
    set("arguments", arguments)
    result
