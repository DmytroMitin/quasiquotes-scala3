package quasiquotes.construct

import dotty.tools.dotc.ast.untpd

import quasiquotes.parser.*
import quasiquotes.source.*
import quasiquotes.types.ConstructedType
import quasiquotes.types.TypeNormalForm

class SourceMappingTest extends munit.FunSuite:
  private val constructedInt = ConstructedType(TypeNormalForm.STypeIdent("Int"))

  test("categorized synthesis maps simple term and constructed-type splices") {
    val term = PlaceholderSource.synthesizeCategorized(
      Seq("f(", ")"),
      Seq(QuasiquoteHole.Term("term"))
    ).toOption.get
    val tpe = PlaceholderSource.synthesizeCategorized(
      Seq("x: ", ""),
      Seq(QuasiquoteHole.ConstructedTypeSplice(constructedInt))
    ).toOption.get

    assertEquals(
      term.originMap.originAt(term.source.indexOf("__qq_term_hole_0")),
      Some(SourceOrigin.InterpolationArgument(SourceId.TermConstructionTemplate, 0, InterpolationCategory.TermSplice))
    )
    assertEquals(
      tpe.originMap.originAt(tpe.source.indexOf("__qq_type_hole_0")),
      Some(SourceOrigin.InterpolationArgument(SourceId.TermConstructionTemplate, 0, InterpolationCategory.ConstructedTypeSplice))
    )
  }

  test("mixed synthesis maps literal parts and adjacent interpolations deterministically") {
    val parts = Seq("(", "", ": ", ")")
    val holes = Seq(
      QuasiquoteHole.Term("first"),
      QuasiquoteHole.Term("second"),
      QuasiquoteHole.ConstructedTypeSplice(constructedInt)
    )
    val first = PlaceholderSource.synthesizeCategorized(parts, holes).toOption.get
    val second = PlaceholderSource.synthesizeCategorized(parts, holes).toOption.get

    assertEquals(first, second)
    assertEquals(first.originMap.segments.size, 6)
    assertEquals(
      first.originMap.originAt(0),
      Some(SourceOrigin.LiteralPart(SourceId.TermConstructionTemplate, 0, SourceSpan(0, 1)))
    )
    assertEquals(
      first.originMap.segments.collect { case GeneratedSegment(_, origin: SourceOrigin.InterpolationArgument) => origin.category },
      Vector(InterpolationCategory.TermSplice, InterpolationCategory.TermSplice, InterpolationCategory.ConstructedTypeSplice)
    )
  }

  test("collision-suffixed placeholders retain their interpolation origin") {
    val synthesized = PlaceholderSource.synthesizeCategorized(
      Seq("(__qq_term_hole_0, ", ")"),
      Seq(QuasiquoteHole.Term("actual"))
    ).toOption.get
    val generatedName = synthesized.bindings.head.name

    assertEquals(generatedName, "__qq_term_hole_0_1")
    assertEquals(
      synthesized.originMap.originAt(synthesized.source.lastIndexOf(generatedName)),
      Some(SourceOrigin.InterpolationArgument(SourceId.TermConstructionTemplate, 0, InterpolationCategory.TermSplice))
    )
  }

  test("all-empty literal parts still produce adjacent mapped placeholders") {
    val synthesized = PlaceholderSource.synthesizeCategorized(
      Seq("", "", ""),
      Seq(QuasiquoteHole.Term("left"), QuasiquoteHole.Term("right"))
    ).toOption.get

    assertEquals(synthesized.originMap.segments.size, 2)
    assertEquals(synthesized.originMap.segments.head.generatedSpan.end, synthesized.originMap.segments(1).generatedSpan.start)
  }

  test("parsed term and type placeholder spans map back to arguments while punctuation maps to a literal part") {
    val synthesized = PlaceholderSource.synthesizeCategorized(
      Seq("(", ": ", ")"),
      Seq(QuasiquoteHole.Term("term"), QuasiquoteHole.ConstructedTypeSplice(constructedInt))
    ).toOption.get
    val raw = TinyTermParser.parseOrThrow(synthesized.source).rawTree
    val termSpan = identifierSpan(raw, synthesized.bindings(0).name).get
    val typeSpan = identifierSpan(raw, synthesized.bindings(1).name).get

    assertEquals(
      synthesized.originMap.originsFor(termSpan).map(_.origin),
      Vector(SourceOrigin.InterpolationArgument(SourceId.TermConstructionTemplate, 0, InterpolationCategory.TermSplice))
    )
    assertEquals(
      synthesized.originMap.originsFor(typeSpan).map(_.origin),
      Vector(SourceOrigin.InterpolationArgument(SourceId.TermConstructionTemplate, 1, InterpolationCategory.ConstructedTypeSplice))
    )
    assertEquals(
      synthesized.originMap.originAt(0),
      Some(SourceOrigin.LiteralPart(SourceId.TermConstructionTemplate, 0, SourceSpan(0, 1)))
    )
  }

  test("a term splice under a unary prefix retains its interpolation origin") {
    val synthesized = PlaceholderSource.synthesizeCategorized(
      Seq("-", ""),
      Seq(QuasiquoteHole.Term("operand"))
    ).toOption.get
    val raw = TinyTermParser.parseOrThrow(synthesized.source).rawTree
    val operandSpan = identifierSpan(raw, synthesized.bindings.head.name).get

    assertEquals(
      synthesized.originMap.originsFor(operandSpan).map(_.origin),
      Vector(SourceOrigin.InterpolationArgument(
        SourceId.TermConstructionTemplate,
        0,
        InterpolationCategory.TermSplice
      ))
    )
    assertEquals(
      synthesized.originMap.originAt(0),
      Some(SourceOrigin.LiteralPart(SourceId.TermConstructionTemplate, 0, SourceSpan(0, 1)))
    )
  }

  private def identifierSpan(tree: untpd.Tree, expected: String): Option[SourceSpan] =
    tree match
      case ident @ untpd.Ident(name) if name.toString == expected => DottySourceSpanAdapter.fromTree(ident)
      case untpd.Typed(expression, typeTree) => identifierSpan(expression, expected).orElse(identifierSpan(typeTree, expected))
      case untpd.PrefixOp(_, operand) => identifierSpan(operand, expected)
      case untpd.Parens(inner) => identifierSpan(inner, expected)
      case untpd.TypedSplice(inner) => identifierSpan(inner, expected)
      case _ => None
