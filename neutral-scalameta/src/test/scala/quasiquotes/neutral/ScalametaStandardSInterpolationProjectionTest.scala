package quasiquotes.neutral

import _root_.quasiquotes.parser.{BinderId, BlockStatement, TermShape}

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3
import scala.meta.tokens.Token

@nowarn("cat=deprecation")
final class ScalametaStandardSInterpolationProjectionTest extends munit.FunSuite:
  test("Scalameta exposes semantic string parts and direct versus braced argument topology"):
    val plain = interpolation("""s"plain"""")
    assertEquals(plain.prefix.value, "s")
    assertEquals(plain.prefix.tokens.map(_.text).toList, List("s"))
    assertEquals(stringPartValues(plain), List("plain"))
    assertEquals(plain.args, Nil)

    val direct = interpolation("""s"hello $name"""")
    assertEquals(stringPartValues(direct), List("hello ", ""))
    direct.args match
      case (name: Term.Name) :: Nil => assertEquals(name.value, "name")
      case other => fail(s"expected one direct Term.Name argument, got $other")

    val bracedName = interpolation("""s"hello ${name}"""")
    assertEquals(stringPartValues(bracedName), List("hello ", ""))
    bracedName.args match
      case Term.Block((name: Term.Name) :: Nil) :: Nil =>
        assertEquals(name.value, "name")
      case other => fail(s"expected one braced one-Term block, got $other")

    val bracedApply = interpolation("""s"value = ${foo(x)}"""")
    assertEquals(stringPartValues(bracedApply), List("value = ", ""))
    bracedApply.args match
      case Term.Block((application: Term.Apply) :: Nil) :: Nil =>
        assertEquals(application.fun.asInstanceOf[Term.Name].value, "foo")
        assertEquals(application.argClause.values.map(_.productPrefix), List("Term.Name"))
      case other => fail(s"expected one braced Apply block, got $other")

  test("Scalameta preserves argument order and semantic escape values"):
    val twoArguments = interpolation("""s"$a / $b"""")
    assertEquals(stringPartValues(twoArguments), List("", " / ", ""))
    assertEquals(
      twoArguments.args.map(_.asInstanceOf[Term.Name].value),
      List("a", "b")
    )

    val literalDollar = interpolation("""s"literal $$ dollar"""")
    assertEquals(stringPartValues(literalDollar), List("literal $ dollar"))
    assertEquals(literalDollar.args, Nil)

    val escaped = interpolation("""s"quote = \"$name\"; slash = \\; line = \n; tab = \t"""")
    assertEquals(
      stringPartValues(escaped),
      List("quote = \\\"", "\\\"; slash = \\\\; line = \\n; tab = \\t")
    )

  test("Scalameta tokens distinguish single-quoted and triple-quoted s interpolation"):
    val singlePlain = interpolation("""s"plain"""")
    val singleArgument = interpolation("""s"hello $name"""")
    val triplePlain = interpolation("s\"\"\"plain\"\"\"")
    val tripleArgument = interpolation("s\"\"\"hello $name\"\"\"")
    val tripleEscape = interpolation("s\"\"\"line = \\n\"\"\"")

    assertEquals(interpolationDelimiters(singlePlain), List("\"", "\""))
    assertEquals(interpolationDelimiters(singleArgument), List("\"", "\""))
    assertEquals(interpolationDelimiters(triplePlain), List("\"\"\"", "\"\"\""))
    assertEquals(interpolationDelimiters(tripleArgument), List("\"\"\"", "\"\"\""))
    assertEquals(interpolationDelimiters(tripleEscape), List("\"\"\"", "\"\"\""))
    assertEquals(stringPartValues(triplePlain), List("plain"))
    assertEquals(stringPartValues(tripleArgument), List("hello ", ""))
    assertEquals(stringPartValues(tripleEscape), List("line = \\n"))

  test("Scalameta distinguishes neighboring prefixes and retains prefix token spelling"):
    val standard = interpolation("""s"plain"""")
    val raw = interpolation("""raw"plain"""")
    val formatted = interpolation("""f"value = $x%d"""")
    val custom = interpolation("""custom"plain"""")
    val programmatic = Term.Interpolate(
      Term.Name("s"),
      List(Lit.String("plain")),
      Nil
    )

    assertEquals(
      List(standard, raw, formatted, custom, programmatic).map(_.prefix.value),
      List("s", "raw", "f", "custom", "s")
    )
    assertEquals(standard.prefix.tokens.map(_.text).toList, List("s"))
    assertEquals(programmatic.prefix.tokens.map(_.text).mkString, "s")
    assertEquals(interpolationDelimiters(programmatic), List("\"", "\""))
    assertEquals(programmatic.pos, Position.None)
    assert(Input.String("""`s`"plain"""").parse[Term].isInstanceOf[Parsed.Error])

  test("Scalameta interpolation arguments expose the admitted recursive child categories"):
    val fixtures = List(
      """s"${-x}"""" -> "Term.ApplyUnary",
      """s"${foo(x)}"""" -> "Term.Apply",
      """s"${(x, y)}"""" -> "Term.Tuple",
      """s"${if cond then x else y}"""" -> "Term.If"
    )

    fixtures.foreach { (source, expectedCategory) =>
      interpolation(source).args match
        case Term.Block((argument: Term) :: Nil) :: Nil =>
          assertEquals(argument.productPrefix, expectedCategory, clues(source))
        case other => fail(s"expected one braced one-Term block for $source, got $other")
    }

  test("projects semantic parts and ordered direct or braced arguments into the existing Core carrier"):
    val fixtures = List(
      """s"plain"""" ->
        TermShape.InterpolatedString("s", List("plain"), Nil),
      """s"hello $name"""" ->
        TermShape.InterpolatedString(
          "s",
          List("hello ", ""),
          List(TermShape.Identifier("name", isPlaceholder = false))
        ),
      """s"$a / $b"""" ->
        TermShape.InterpolatedString(
          "s",
          List("", " / ", ""),
          List(
            TermShape.Identifier("a", isPlaceholder = false),
            TermShape.Identifier("b", isPlaceholder = false)
          )
        ),
      """s"value = ${foo(x)}"""" ->
        TermShape.InterpolatedString(
          "s",
          List("value = ", ""),
          List(
            TermShape.Apply(
              TermShape.Identifier("foo", isPlaceholder = false),
              List(TermShape.Identifier("x", isPlaceholder = false))
            )
          )
        ),
      """s"prefix ${foo(x)} suffix"""" ->
        TermShape.InterpolatedString(
          "s",
          List("prefix ", " suffix"),
          List(
            TermShape.Apply(
              TermShape.Identifier("foo", isPlaceholder = false),
              List(TermShape.Identifier("x", isPlaceholder = false))
            )
          )
        )
    )

    fixtures.foreach { (source, expected) =>
      assertEquals(project(parsed(source)).shape, expected, clues(source))
    }
    assertEquals(
      project(parsed("""s"hello $name"""")).shape,
      project(parsed("""s"hello ${name}"""")).shape
    )

  test("projects literal dollars and decodes Scalameta escape spellings into semantic values"):
    assertEquals(
      project(parsed("""s"literal $$ dollar"""")).shape,
      TermShape.InterpolatedString("s", List("literal $ dollar"), Nil)
    )
    assertEquals(
      project(parsed("""s"quote = \"$name\"; slash = \\; line = \n; tab = \t"""")).shape,
      TermShape.InterpolatedString(
        "s",
        List("quote = \"", "\"; slash = \\; line = \n; tab = \t"),
        List(TermShape.Identifier("name", isPlaceholder = false))
      )
    )

  private val semanticEscapeFixtures = List(
    "plain" -> "plain",
    "\\\"" -> "\"",
    "\\\\" -> "\\",
    "\\n" -> "\n",
    "\\r" -> "\r",
    "\\t" -> "\t",
    "\\b" -> "\b",
    "\\f" -> "\f",
    "\\u0001" -> "\u0001"
  )

  semanticEscapeFixtures.foreach { (sourcePart, semanticPart) =>
    test(s"decodes standard s escape spelling $sourcePart into its Core semantic value"):
      val source = "s\"" + sourcePart + "\""
      assertEquals(
        project(parsed(source)).shape,
        TermShape.InterpolatedString("s", List(semanticPart), Nil),
        clues(source, sourcePart, semanticPart)
      )
  }

  test("decodes escaped parts around an argument while preserving argument identity and order"):
    val mixedSource = "s\"before " + "\\n" + "$name after " + "\\t" + "\""
    assertEquals(
      project(parsed(mixedSource)).shape,
      TermShape.InterpolatedString(
        "s",
        List("before \n", " after \t"),
        List(TermShape.Identifier("name", isPlaceholder = false))
      )
    )

  private val tripleQuotedSources = List(
    "s\"\"\"plain\"\"\"",
    "s\"\"\"hello $name\"\"\"",
    "s\"\"\"line = \\n\"\"\""
  )

  tripleQuotedSources.foreach { source =>
    test(s"rejects triple-quoted standard s surface ${source.take(12)}"):
      assertErrorCode(parsed(source), "NEUTRAL_INTERPOLATION_SURFACE_UNSUPPORTED")
  }

  test("converts malformed programmatic escape spelling into a controlled Left"):
    assertErrorCode(
      Term.Interpolate(Term.Name("s"), List(Lit.String("\\q")), Nil),
      "NEUTRAL_INTERPOLATION_STRUCTURE_UNSUPPORTED"
    )

  test("recursively projects the already admitted unary tuple if Apply New and P1 children"):
    val fixtures = List(
      """s"${-x}"""" -> TermShape.Unary(
        "-",
        TermShape.Identifier("x", isPlaceholder = false)
      ),
      """s"${(x, y)}"""" -> TermShape.Tuple(
        List(
          TermShape.Identifier("x", isPlaceholder = false),
          TermShape.Identifier("y", isPlaceholder = false)
        )
      ),
      """s"${if cond then x else y}"""" -> TermShape.If(
        TermShape.Identifier("cond", isPlaceholder = false),
        TermShape.Identifier("x", isPlaceholder = false),
        TermShape.Identifier("y", isPlaceholder = false)
      ),
      """s"${foo(x)}"""" -> TermShape.Apply(
        TermShape.Identifier("foo", isPlaceholder = false),
        List(TermShape.Identifier("x", isPlaceholder = false))
      ),
      """s"${new synthetic.unresolved.Widget(1)}"""" -> TermShape.New(
        "synthetic.unresolved.Widget",
        List(TermShape.Literal("1"))
      ),
      """s"${{ 1; 2 }}"""" -> TermShape.Block(
        List(TermShape.Literal("1")),
        TermShape.Literal("2")
      )
    )

    fixtures.foreach { (source, expectedArgument) =>
      assertEquals(
        project(parsed(source)).shape,
        TermShape.InterpolatedString("s", List("", ""), List(expectedArgument)),
        clues(source)
      )
    }

  test("preserves Lambda1 and P2 binder identity through direct and braced arguments"):
    val direct = project(parsed("""(x: Int) => s"$x"""")).shape
    val braced = project(parsed("""(x: Int) => s"${x}"""")).shape

    List(direct, braced).foreach {
      case TermShape.Lambda1(
            lambdaId,
            "x",
            "Int",
            TermShape.InterpolatedString(
              "s",
              List("", ""),
              List(TermShape.BoundReference(argumentId, "x"))
            )
          ) =>
        assertEquals(lambdaId, BinderId(0))
        assertEquals(argumentId, lambdaId)
      case other => fail(s"unexpected Lambda1 interpolation projection: ${other.render}")
    }

    project(parsed("""{ val x: Int = 1; s"$x" }""")).shape match
      case TermShape.Block(
            List(BlockStatement.LocalVal(localId, "x", "Int", TermShape.Literal("1"))),
            TermShape.InterpolatedString(
              "s",
              List("", ""),
              List(TermShape.BoundReference(argumentId, "x"))
            )
          ) =>
        assertEquals(localId, BinderId(0))
        assertEquals(argumentId, localId)
      case other => fail(s"unexpected P2 interpolation projection: ${other.render}")

  test("preserves truthful positioned and unpositioned interpolation root spans"):
    val source = """s"prefix ${foo(x)} suffix""""
    assertEquals(
      project(parsed(source)).sourceSpan,
      Some(NeutralSourceSpan(0, source.length))
    )

    val unpositioned = Term.Interpolate(
      Term.Name("s"),
      List(Lit.String("plain")),
      Nil
    )
    assertEquals(unpositioned.pos, Position.None)
    assertEquals(
      project(unpositioned),
      ProjectedTermShape(
        TermShape.InterpolatedString("s", List("plain"), Nil),
        None
      )
    )

  test("rejects unsupported prefixes and malformed interpolation structure with stable categories"):
    List(
      """raw"plain"""",
      """f"value = $x%d"""",
      """custom"plain""""
    ).foreach(source => assertErrorCode(parsed(source), "NEUTRAL_INTERPOLATION_PREFIX_UNSUPPORTED"))

    val nonStringPart = Term.Interpolate(
      Term.Name("s"),
      List(Lit.Int(1)),
      Nil
    )
    assertErrorCode(nonStringPart, "NEUTRAL_INTERPOLATION_STRUCTURE_UNSUPPORTED")
    intercept[org.scalameta.invariants.InvariantFailedException] {
      Term.Interpolate(
        Term.Name("s"),
        List(Lit.String("only")),
        List(Term.Name("x"))
      )
    }

  test("rejects unsupported interpolation wrapper topology without hiding ordinary child failures"):
    val unsupportedWrappers = List(
      Term.Block(Nil),
      Term.Block(List(Lit.Int(1), Lit.Int(2))),
      parsed("{ val x = 1 }")
    )
    unsupportedWrappers.foreach { wrapper =>
      assertErrorCode(
        Term.Interpolate(
          Term.Name("s"),
          List(Lit.String(""), Lit.String("")),
          List(wrapper)
        ),
        "NEUTRAL_INTERPOLATION_ARGUMENT_UNSUPPORTED"
      )
    }

    assertErrorCode(
      Term.Interpolate(
        Term.Name("s"),
        List(Lit.String(""), Lit.String("")),
        List(parsed("value match { case _ => 1 }"))
      ),
      "NEUTRAL_TERM_UNSUPPORTED"
    )
    assertErrorCode(
      parsed("""(x: Int) => s"${(y: Int) => y}""""),
      "NEUTRAL_LAMBDA_NESTED_UNSUPPORTED"
    )
    assertEquals(
      ScalametaTermProjection.project(null),
      Left(
        NeutralProjectionError(
          "NEUTRAL_TERM_MISSING",
          "the Scalameta term must be present."
        )
      )
    )

  private def interpolation(source: String): Term.Interpolate =
    parsed(source) match
      case value: Term.Interpolate => value
      case other => fail(s"expected Term.Interpolate for $source, got ${other.productPrefix}")

  private def parsed(source: String): Term =
    Input.String(source).parse[Term].get

  private def project(source: Term): ProjectedTermShape =
    ScalametaTermProjection.project(source) match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def assertErrorCode(source: Term, expected: String): Unit =
    assertEquals(
      ScalametaTermProjection.project(source).left.toOption.map(_.code),
      Some(expected),
      clues(source.structure)
    )

  private def stringPartValues(interpolation: Term.Interpolate): List[String] =
    interpolation.parts.map {
      case Lit.String(value) => value
      case other => fail(s"expected Lit.String interpolation part, got ${other.productPrefix}")
    }

  private def interpolationDelimiters(interpolation: Term.Interpolate): List[String] =
    interpolation.tokens.toList.collect {
      case token: Token.Interpolation.Start => token.text
      case token: Token.Interpolation.End => token.text
    }
