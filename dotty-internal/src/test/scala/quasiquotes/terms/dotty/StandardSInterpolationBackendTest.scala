package quasiquotes.terms.dotty

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol

import quasiquotes.parser.{TermShape, TinyTermParser}
import quasiquotes.terms.ConstructedTerm
import quasiquotes.types.TypeNormalForm

class StandardSInterpolationBackendTest extends munit.FunSuite:
  import TypeNormalForm.*

  private final case class Fixture(
      label: String,
      shape: TermShape.InterpolatedString,
      sidecars: Vector[TypeNormalForm] = Vector.empty,
      expectedSource: String
  )

  private val fixtures = Vector(
    Fixture("plain", interpolation(List("plain")), expectedSource = "s\"plain\""),
    Fixture(
      "one direct identifier",
      interpolation(List("hello ", ""), ident("name")),
      expectedSource = "s\"hello $name\""
    ),
    Fixture(
      "two direct identifiers",
      interpolation(List("", " / ", ""), ident("a"), ident("b")),
      expectedSource = "s\"$a / $b\""
    ),
    Fixture(
      "application",
      interpolation(
        List("value = ", ""),
        TermShape.Apply(ident("foo"), List(ident("x")))
      ),
      expectedSource = "s\"value = ${foo(x)}\""
    ),
    Fixture(
      "application with suffix",
      interpolation(
        List("prefix ", " suffix"),
        TermShape.Apply(ident("foo"), List(ident("x")))
      ),
      expectedSource = "s\"prefix ${foo(x)} suffix\""
    ),
    Fixture(
      "literal dollar",
      interpolation(List("literal $ dollar")),
      expectedSource = "s\"literal $$ dollar\""
    ),
    Fixture(
      "unary",
      interpolation(List("", ""), TermShape.Unary("-", ident("x"))),
      expectedSource = "s\"${-x}\""
    ),
    Fixture(
      "application only",
      interpolation(
        List("", ""),
        TermShape.Apply(ident("foo"), List(ident("x")))
      ),
      expectedSource = "s\"${foo(x)}\""
    ),
    Fixture(
      "tuple",
      interpolation(
        List("", ""),
        TermShape.Tuple(List(ident("x"), ident("y")))
      ),
      expectedSource = "s\"${(x, y)}\""
    ),
    Fixture(
      "conditional",
      interpolation(
        List("", ""),
        TermShape.If(ident("cond"), ident("x"), ident("y"))
      ),
      expectedSource = "s\"${if cond then x else y}\""
    ),
    Fixture(
      "typed parenthesized",
      interpolation(
        List("", ""),
        TermShape.Parenthesized(TermShape.Typed(ident("x"), "Int"))
      ),
      Vector(STypeIdent("Int")),
      expectedSource = "s\"${(x: Int)}\""
    ),
    Fixture(
      "nested application",
      interpolation(
        List("nested ", " end"),
        TermShape.Apply(
          ident("foo"),
          List(
            TermShape.Unary("-", ident("x")),
            TermShape.Tuple(List(ident("a"), ident("b")))
          )
        )
      ),
      expectedSource = "s\"nested ${foo(-x, (a, b))} end\""
    )
  )

  fixtures.foreach { fixture =>
    test(s"source-free and generated-origin parser-oracle closure: ${fixture.label}") {
      withContext {
        val constructed = construct(fixture.shape, fixture.sidecars)
        val raw = ConstructedTermUntypedBackend.lower(constructed).toOption.get
        val result =
          ConstructedTermGeneratedOriginAdapter
            .lower(
              constructed,
              s"<generated-s-${fixture.label.replace(' ', '-')}>"
            )
            .fold(error => fail(error.message), identity)
        val oracle = TinyTermParser.parseOrThrow(result.generatedSource).rawTree

        assertEquals(result.generatedSource, fixture.expectedSource)
        assertEquals(rawSummary(raw), rawSummary(oracle))
        assertEquals(rawSummary(result.tree), rawSummary(oracle))
        assertEquals(spanSummary(result.tree), spanSummary(oracle))
        assertEquals(semanticParts(result.tree), fixture.shape.parts)
        assertEquals(interpolationPrefix(result.tree), "s")
        assertSourceFree(raw)
        assertPositioned(result)
      }
    }
  }

  test("encodes every semantic interpolation-part category canonically") {
    val matrix = Vector(
      "" -> "",
      "ordinary ASCII" -> "ordinary ASCII",
      "space !?,.;:" -> "space !?,.;:",
      "$" -> "$$",
      "$$" -> "$$$$",
      "\\" -> "\\\\",
      "\"" -> "\\\"",
      "\n" -> "\\n",
      "\r" -> "\\r",
      "\t" -> "\\t",
      "\b" -> "\\b",
      "\f" -> "\\f",
      "\u0001" -> "\\u0001",
      "\u007f" -> "\\u007f",
      "λ😀" -> "λ😀"
    )

    withContext {
      matrix.zipWithIndex.foreach { case ((semantic, encoded), index) =>
        val shape = interpolation(List(semantic))
        val constructed = construct(shape, Vector.empty)
        val raw = ConstructedTermUntypedBackend.lower(constructed).toOption.get
        val result =
          ConstructedTermGeneratedOriginAdapter
            .lower(constructed, s"<generated-s-escaping-$index>")
            .fold(error => fail(error.message), identity)
        val oracle = TinyTermParser.parseOrThrow(result.generatedSource).rawTree

        assertEquals(result.generatedSource, "s\"" + encoded + "\"")
        assertEquals(rawSummary(raw), rawSummary(oracle))
        assertEquals(spanSummary(result.tree), spanSummary(oracle))
        assertEquals(semanticParts(result.tree), List(semantic))
        assertPositioned(result)
      }
    }
  }

  test("uses direct syntax only for simple safe identifiers") {
    val values = Vector(
      ident("name") -> "s\"$name\"",
      ident("class") -> "s\"${`class`}\"",
      TermShape.Select(ident("service"), "name") -> "s\"${service.name}\"",
      TermShape.Apply(ident("name"), Nil) -> "s\"${name()}\""
    )

    withContext {
      values.zipWithIndex.foreach { case ((argument, source), index) =>
        val result =
          ConstructedTermGeneratedOriginAdapter
            .lower(
              construct(interpolation(List("", ""), argument), Vector.empty),
              s"<generated-s-policy-$index>"
            )
            .fold(error => fail(error.message), identity)
        assertEquals(result.generatedSource, source)
        assertEquals(
          rawSummary(result.tree),
          rawSummary(TinyTermParser.parseOrThrow(source).rawTree)
        )
      }
    }
  }

  test("consumes typed sidecars inside one and multiple arguments in preorder") {
    val shape =
      interpolation(
        List("first=", ", second=", ""),
        TermShape.Typed(ident("first"), "Int"),
        TermShape.Parenthesized(
          TermShape.Typed(
            TermShape.Apply(ident("foo"), List(ident("second"))),
            "String"
          )
        )
      )
    val sidecars = Vector(STypeIdent("Int"), STypeIdent("String"))

    withContext {
      val constructed = construct(shape, sidecars)
      val raw = ConstructedTermUntypedBackend.lower(constructed).toOption.get
      val result =
        ConstructedTermGeneratedOriginAdapter
          .lower(constructed, "<generated-s-sidecar-order>")
          .fold(error => fail(error.message), identity)
      val oracle = TinyTermParser.parseOrThrow(result.generatedSource).rawTree

      assertEquals(
        result.generatedSource,
        "s\"first=${first: Int}, second=${(foo(second): String)}\""
      )
      assertEquals(rawSummary(raw), rawSummary(oracle))
      assertEquals(rawSummary(result.tree), rawSummary(oracle))
      assertEquals(spanSummary(result.tree), spanSummary(oracle))
    }
  }

  test("returns controlled errors for invalid interpolation shapes") {
    Vector("raw", "f", "custom").foreach { prefix =>
      val invalidPrefix =
        corrupt(interpolationWithPrefix(prefix, List("plain")), Vector.empty)
      assertEquals(
        ConstructedTermUntypedBackend.lower(invalidPrefix),
        Left(
          ConstructedTermUntypedBackendError
            .UnsupportedInterpolationPrefix(prefix)
        )
      )

      withContext {
        assertEquals(
          ConstructedTermGeneratedOriginAdapter.lower(
            invalidPrefix,
            s"<generated-s-invalid-prefix-$prefix>"
          ),
          Left(
            ConstructedTermGeneratedOriginError
              .UnsupportedInterpolationPrefix(prefix)
          )
        )
      }
    }

    val nullPart = corrupt(interpolation(List(null)), Vector.empty)
    assertEquals(
      ConstructedTermUntypedBackend.lower(nullPart),
      Left(ConstructedTermUntypedBackendError.NullInterpolationPart(0))
    )
    withContext {
      assertEquals(
        ConstructedTermGeneratedOriginAdapter.lower(
          nullPart,
          "<generated-s-null-part>"
        ),
        Left(ConstructedTermGeneratedOriginError.NullInterpolationPart(0))
      )
    }

    val nullArgument =
      corrupt(interpolation(List("", ""), null), Vector.empty)
    assertEquals(
      ConstructedTermUntypedBackend.lower(nullArgument),
      Left(ConstructedTermUntypedBackendError.NullInterpolationArgument(0))
    )
    withContext {
      assertEquals(
        ConstructedTermGeneratedOriginAdapter.lower(
          nullArgument,
          "<generated-s-null-argument>"
        ),
        Left(ConstructedTermGeneratedOriginError.NullInterpolationArgument(0))
      )
    }

    val unsupportedArgument = corrupt(
      interpolation(
        List("", ""),
        TermShape.Unsupported("ArbitraryUnsupported", "payload")
      ),
      Vector.empty
    )
    assertEquals(
      ConstructedTermUntypedBackend.lower(unsupportedArgument),
      Left(
        ConstructedTermUntypedBackendError
          .UnsupportedTermNode("ArbitraryUnsupported")
      )
    )
    withContext {
      assertEquals(
        ConstructedTermGeneratedOriginAdapter.lower(
          unsupportedArgument,
          "<generated-s-unsupported-argument>"
        ),
        Left(
          ConstructedTermGeneratedOriginError
            .UnsupportedTermNode("ArbitraryUnsupported")
        )
      )
    }
  }

  test("keeps production interpolation lowering parser-free and non-desugared") {
    val root =
      Path.of(
        "dotty-internal",
        "src",
        "main",
        "scala",
        "quasiquotes",
        "terms",
        "dotty"
      )
    val files = Vector(
      "ConstructedTermUntypedBackend.scala",
      "GeneratedOriginFragmentSupport.scala",
      "StandardSInterpolationEncoding.scala"
    )
    val forbidden = Vector(
      "dotty.tools.dotc.parsing",
      "TinyTermParser",
      "Scala3ParserBridge",
      "StringContext(",
      "scala.quoted",
      "Quotes"
    )

    files.foreach { file =>
      val source =
        Files.readString(root.resolve(file), StandardCharsets.UTF_8)
      forbidden.foreach(value =>
        assert(!source.contains(value), clues(file, value))
      )
    }
  }

  private def interpolation(
      parts: List[String],
      arguments: TermShape*
  ): TermShape.InterpolatedString =
    TermShape.InterpolatedString("s", parts, arguments.toList)

  private def interpolationWithPrefix(
      prefix: String,
      parts: List[String],
      arguments: TermShape*
  ): TermShape.InterpolatedString =
    TermShape.InterpolatedString(prefix, parts, arguments.toList)

  private def ident(name: String): TermShape =
    TermShape.Identifier(name, isPlaceholder = false)

  private def construct(
      shape: TermShape,
      sidecars: Vector[TypeNormalForm]
  ): ConstructedTerm =
    ConstructedTerm.create(shape, sidecars).toOption.get

  private def withContext(body: Context ?=> Unit): Unit =
    val base = new ContextBase
    given Context = base.initialCtx
    body

  private def interpolationPrefix(tree: untpd.Tree): String =
    tree match
      case untpd.InterpolatedString(prefix, _) => prefix.toString
      case other =>
        fail(
          s"expected InterpolatedString, found ${other.getClass.getSimpleName}"
        )

  private def semanticParts(tree: untpd.Tree): List[String] =
    tree match
      case untpd.InterpolatedString(_, segments) =>
        segments.map {
          case untpd.Thicket(untpd.Literal(constant) :: _ :: Nil) =>
            StringContext.processEscapes(constant.stringValue)
          case untpd.Literal(constant) =>
            StringContext.processEscapes(constant.stringValue)
          case other =>
            fail(s"unexpected interpolation segment: $other")
        }
      case other =>
        fail(
          s"expected InterpolatedString, found ${other.getClass.getSimpleName}"
        )

  private def rawSummary(tree: untpd.Tree): String =
    tree match
      case untpd.InterpolatedString(prefix, segments) =>
        s"InterpolatedString(${prefix.toString},${segments.map(rawSummary).mkString("[", ",", "]")})"
      case untpd.Thicket(trees) =>
        s"Thicket(${trees.map(rawSummary).mkString("[", ",", "]")})"
      case untpd.Block(statements, expression) =>
        s"Block(${statements.map(rawSummary).mkString("[", ",", "]")},${rawSummary(expression)})"
      case untpd.Ident(name) =>
        s"Ident(${name.toString})"
      case untpd.Literal(constant) =>
        s"Literal(${escaped(String.valueOf(constant.value))})"
      case untpd.Number(digits, kind) =>
        s"Number($digits,$kind)"
      case untpd.Select(qualifier, name) =>
        s"Select(${rawSummary(qualifier)},${name.toString})"
      case untpd.Apply(function, arguments) =>
        s"Apply(${rawSummary(function)},${arguments.map(rawSummary).mkString("[", ",", "]")})"
      case untpd.InfixOp(left, operator, right) =>
        s"Infix(${rawSummary(left)},${rawSummary(operator)},${rawSummary(right)})"
      case untpd.PrefixOp(operator, operand) =>
        s"Prefix(${rawSummary(operator)},${rawSummary(operand)})"
      case untpd.Typed(expression, typeTree) =>
        s"Typed(${rawSummary(expression)},${rawSummary(typeTree)})"
      case untpd.Tuple(elements) =>
        s"Tuple(${elements.map(rawSummary).mkString("[", ",", "]")})"
      case untpd.If(condition, thenBranch, elseBranch) =>
        s"If(${rawSummary(condition)},${rawSummary(thenBranch)},${rawSummary(elseBranch)})"
      case untpd.Parens(expression) =>
        s"Parens(${rawSummary(expression)})"
      case untpd.AppliedTypeTree(constructor, arguments) =>
        s"AppliedType(${rawSummary(constructor)},${arguments.map(rawSummary).mkString("[", ",", "]")})"
      case other =>
        other.getClass.getSimpleName

  private def spanSummary(tree: untpd.Tree): Vector[(String, Int, Int, Int)] =
    allTrees(tree).map { current =>
      (
        current.getClass.getSimpleName,
        current.span.start,
        current.span.point,
        current.span.end
      )
    }

  private def assertSourceFree(tree: untpd.Tree)(using Context): Unit =
    allTrees(tree).foreach { current =>
      assert(!current.source.exists)
      assert(!current.span.exists)
      assertEquals(current.symbol, NoSymbol)
      assert(!current.isInstanceOf[untpd.TypedSplice])
    }

  private def assertPositioned(
      result: GeneratedOriginTermResult
  )(using Context): Unit =
    allTrees(result.tree).foreach { current =>
      assert(current.source.exists)
      assertEquals(current.source.path, result.virtualSourceName)
      assert(current.span.exists)
      assertEquals(current.symbol, NoSymbol)
      assert(!current.isInstanceOf[untpd.TypedSplice])
    }
    assertEquals(result.tree.span.start, 0)
    assertEquals(result.tree.span.end, result.generatedSource.length)

  private def allTrees(tree: untpd.Tree): Vector[untpd.Tree] =
    val children = tree match
      case untpd.InterpolatedString(_, segments) => segments.toVector
      case untpd.Thicket(trees) => trees.toVector
      case untpd.Block(statements, expression) =>
        statements.toVector :+ expression
      case untpd.Select(qualifier, _) => Vector(qualifier)
      case untpd.Apply(function, arguments) => function +: arguments.toVector
      case untpd.InfixOp(left, operator, right) =>
        Vector(left, operator, right)
      case untpd.PrefixOp(operator, operand) =>
        Vector(operator, operand)
      case untpd.Typed(expression, typeTree) =>
        Vector(expression, typeTree)
      case untpd.AppliedTypeTree(constructor, arguments) =>
        constructor +: arguments.toVector
      case untpd.Tuple(elements) => elements.toVector
      case untpd.If(condition, thenBranch, elseBranch) =>
        Vector(condition, thenBranch, elseBranch)
      case untpd.Parens(expression) => Vector(expression)
      case _ => Vector.empty
    tree +: children.flatMap(allTrees)

  private def escaped(value: String): String =
    val builder = new StringBuilder
    value.foreach {
      case '\\' => builder.append("\\\\")
      case '\n' => builder.append("\\n")
      case '\r' => builder.append("\\r")
      case '\t' => builder.append("\\t")
      case '\b' => builder.append("\\b")
      case '\f' => builder.append("\\f")
      case char if char < ' ' || char == '\u007f' =>
        builder.append(f"\\u${char.toInt}%04x")
      case char => builder.append(char)
    }
    builder.toString

  private def corrupt(
      root: TermShape,
      sidecars: Vector[TypeNormalForm]
  ): ConstructedTerm =
    val constructor =
      classOf[ConstructedTerm].getDeclaredConstructors.head
    constructor.setAccessible(true)
    constructor
      .newInstance(root, sidecars)
      .asInstanceOf[ConstructedTerm]
