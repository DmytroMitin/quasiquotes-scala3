package quasiquotes.parser

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Constants.{
  BooleanTag,
  CharTag,
  DoubleTag,
  FloatTag,
  LongTag,
  NullTag,
  StringTag
}

import quasiquotes.terms.{ConstructedTerm, TermConstructionError}

class ParserOriginLiteralIntegrityTest extends munit.FunSuite:
  private val isScala338 =
    dotty.tools.dotc.config.Properties.versionNumberString == "3.3.8"

  private val snapshots = Vector(
    "0" -> Set("Number(0,Whole(10))"),
    "1_000" -> Set("Number(1000,Whole(10))"),
    "0x10" -> Set("Number(10,Whole(16))"),
    // Scala 3.3.8 rejects binary literal syntax before producing a raw tree.
    "0b10" ->
      (if isScala338 then Set("ParseError(SyntaxError)")
       else Set("Number(10,Whole(2))")),
    "10L" -> Set("Literal(Long)"),
    "'1'" -> Set("Literal(Char)"),
    "1.0" -> Set("Number(1.0,Decimal)"),
    "1e2" -> Set("Number(1e2,Floating)"),
    "1.0f" -> Set("Literal(Float)"),
    "null" -> Set("Literal(Null)"),
    "true" -> Set("Literal(Boolean)"),
    "\"text\"" -> Set("Literal(String)"),
    "-10" -> Set("Number(-10,Whole(10))"),
    "-0x10" -> Set("Number(-10,Whole(16))"),
    "-10L" -> Set("Literal(Long)"),
    "-1.0" -> Set("Number(-1.0,Decimal)")
  )

  private val inspectorRawSnapshots = Map(
    "0" -> "Number(0,Whole(10))",
    "1_000" -> "Number(1000,Whole(10))",
    "0x10" -> "Number(10,Whole(16))",
    "0b10" -> "Number(10,Whole(2))",
    "10L" -> "Literal(Long)",
    "'1'" -> "Literal(Character)",
    "1.0" -> "Number(1.0,Decimal)",
    "1e2" -> "Number(1e2,Floating)",
    "1.0f" -> "Literal(Float)",
    "null" -> "Literal(Null)",
    "true" -> "Literal(Boolean(true))",
    "\"text\"" -> "Literal(String(\"text\"))",
    "-10" -> "Number(-10,Whole(10))",
    "-0x10" -> "Number(-10,Whole(16))",
    "-10L" -> "Literal(Long)",
    "-1.0" -> "Number(-1.0,Decimal)"
  )

  snapshots.foreach { case (source, expected) =>
    test(s"parser-origin literal raw snapshot: $source") {
      val actual = TinyTermParser.parse(source) match
        case Right(parsed) =>
          assertEquals(parsed.rawStructure, inspectorRawSnapshots(source))
          describe(parsed.rawTree)
        case Left(error) => s"ParseError(${error.kind})"
      assert(expected(actual), clues(actual, expected))
    }
  }

  private val supportedParserOrigin = Vector(
    "0" -> "0",
    "10" -> "10",
    "-10" -> "-10",
    "1_000" -> "1000",
    "-1_000" -> "-1000",
    "true" -> "true",
    "false" -> "false",
    "\"\"" -> "\"\"",
    "\"text\"" -> "\"text\"",
    "\"a\\\"b\"" -> "\"a\"b\"",
    "\"first\\nsecond\"" -> "\"first\nsecond\"",
    "\"\\u03bb\"" -> "\"\u03bb\""
  )

  supportedParserOrigin.foreach { case (source, semanticValue) =>
    test(s"supported parser-origin literal round-trip: $source") {
      val parsed = TinyTermParser.parseOrThrow(source)
      val expected = TermShape.Literal(semanticValue)
      assertEquals(parsed.shape, expected)

      val constructed = ConstructedTerm.fromShape(parsed.shape).toOption.get
      assertEquals(constructed.root, expected)
    }
  }

  private val unsupportedParserOrigin = Vector(
    "0x10" ->
      TermShape.Unsupported("NonDecimalIntegerLiteral", "radix=16"),
    "0X10" ->
      TermShape.Unsupported("NonDecimalIntegerLiteral", "radix=16"),
    "-0x10" ->
      TermShape.Unsupported("NonDecimalIntegerLiteral", "radix=16"),
    "10L" ->
      unsupportedConstant("Long"),
    "-10L" ->
      unsupportedConstant("Long"),
    "'1'" ->
      unsupportedConstant("Character"),
    "'x'" ->
      unsupportedConstant("Character"),
    "1.0" ->
      TermShape.Unsupported("DecimalNumberLiteral", "numberKind=Decimal"),
    "-1.0" ->
      TermShape.Unsupported("DecimalNumberLiteral", "numberKind=Decimal"),
    "1e2" ->
      TermShape.Unsupported("FloatingNumberLiteral", "numberKind=Floating"),
    "1.0f" ->
      unsupportedConstant("Float"),
    "-1.0f" ->
      unsupportedConstant("Float"),
    "1.0d" ->
      unsupportedConstant("Double"),
    "null" ->
      unsupportedConstant("Null")
  )

  unsupportedParserOrigin.foreach { case (source, expected) =>
    test(s"unsupported parser-origin literal is rejected before construction: $source") {
      val parsed = TinyTermParser.parseOrThrow(source)
      assertEquals(parsed.shape, expected)
      assertEquals(
        ConstructedTerm.fromShape(parsed.shape),
        Left(TermConstructionError.UnsupportedTermShape())
      )
    }
  }

  Vector("0b10", "0B10").foreach { source =>
    test(s"binary parser-origin literal is never constructed: $source") {
      TinyTermParser.parse(source) match
        case Right(_) if isScala338 =>
          fail("Scala 3.3.8 must reject binary literal syntax")
        case Right(parsed) =>
          assertEquals(
            parsed.shape,
            TermShape.Unsupported("NonDecimalIntegerLiteral", "radix=2")
          )
          assertEquals(
            ConstructedTerm.fromShape(parsed.shape),
            Left(TermConstructionError.UnsupportedTermShape())
          )
        case Left(error) if isScala338 =>
          assertEquals(error.kind, ParseErrorKind.SyntaxError)
        case Left(error) =>
          fail(s"expected a raw binary number, got ${error.kind}")
    }
  }

  private def unsupportedConstant(kind: String): TermShape =
    TermShape.Unsupported(
      s"${kind}Literal",
      s"parser-origin constant kind $kind is not supported"
    )

  private def describe(tree: untpd.Tree): String =
    tree match
      case untpd.Number(digits, kind) =>
        s"Number($digits,$kind)"
      case untpd.Literal(constant) =>
        val kind = constant.tag match
          case BooleanTag => "Boolean"
          case CharTag => "Char"
          case LongTag => "Long"
          case FloatTag => "Float"
          case DoubleTag => "Double"
          case StringTag => "String"
          case NullTag => "Null"
          case other => s"OtherTag($other)"
        s"Literal($kind)"
      case other =>
        s"Unexpected(${other.getClass.getSimpleName})"
