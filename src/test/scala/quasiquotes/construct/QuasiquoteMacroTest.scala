package quasiquotes.construct

private object NamedInfixScope:
  private val foo = 2
  private val bar = 5
  val demo: QuasiquoteMacroExamples.DemoCase = QuasiquoteMacroExamples.namedInfixSummary

private object NamedSelectInfixScope:
  private object foo:
    val bar = 4
  val demo: QuasiquoteMacroExamples.DemoCase = QuasiquoteMacroExamples.namedSelectInfixSummary(3)

private object NestedNamedApplicationScope:
  private def foo(value: Int): Int = value + 10
  private def bar(value: Int): Int = value * 2
  private val baz = 3
  val demo: QuasiquoteMacroExamples.DemoCase = QuasiquoteMacroExamples.nestedNamedApplicationSummary

private object NestedSelectApplicationScope:
  private object foo:
    def bar(value: Int): Int = value + 4
  private def baz(value: Int): Int = value * 3
  val demo: QuasiquoteMacroExamples.DemoCase = QuasiquoteMacroExamples.nestedSelectApplicationSummary(2)

private object ParenthesizedNamedScope:
  private val foo = 11
  val demo: QuasiquoteMacroExamples.DemoCase = QuasiquoteMacroExamples.parenthesizedNamedSummary

private object ParenthesizedSelectedHoleScope:
  private object foo:
    def bar(value: Int): Int = value + 6
  val demo: QuasiquoteMacroExamples.DemoCase = QuasiquoteMacroExamples.parenthesizedSelectedHoleSummary(3)

private object NestedParenHoleScope:
  val demo: QuasiquoteMacroExamples.DemoCase = QuasiquoteMacroExamples.nestedParenHoleSummary(7)

private object TupleApplicationScope:
  private def foo(value: (Int, Int)): Int = value._1 + value._2
  val demo: QuasiquoteMacroExamples.DemoCase = QuasiquoteMacroExamples.tupleApplicationSummary(2, 3)

private object IfApplicationScope:
  private def foo(value: Int): Int = value + 10
  val demo: QuasiquoteMacroExamples.DemoCase = QuasiquoteMacroExamples.ifApplicationSummary(true, 2, 3)

private object TermPlaceholderCollisionScope:
  private val __qq_term_hole_0 = 7
  val result: Int = QuasiTypeSpliceExamples.literalTermCandidateCollision(3)

private object TypePlaceholderCollisionScope:
  private val __qq_type_hole_1 = 4
  val result: Int = QuasiTypeSpliceExamples.literalTypeCandidateCollision(3)

class QuasiquoteMacroTest extends munit.FunSuite:
  private val constructedInt = quasiquotes.types.ConstructedType(
    quasiquotes.types.TypeNormalForm.STypeIdent("Int")
  )

  test("qr can emit an integer literal as a Term") {
    assertEquals(QuasiquoteMacroExamples.emitIntLiteral, 1)
  }

  test("qr can emit a string literal as a Term") {
    assertEquals(QuasiquoteMacroExamples.emitStringLiteral, "abc")
  }

  test("qr can emit a boolean literal as a Term") {
    assertEquals(QuasiquoteMacroExamples.emitBooleanLiteral, true)
  }

  test("qr can lower selection plus application with a qualifier hole") {
    assertEquals(QuasiquoteMacroExamples.callSelectedMethodViaHole(2), 3)
  }

  test("qr can use a hole in function position") {
    assertEquals(QuasiquoteMacroExamples.callFunctionHole(2), 3)
  }

  test("qr can use a hole as the qualifier of a selection") {
    assertEquals(QuasiquoteMacroExamples.stringLength("abcd"), 4)
  }

  test("qr can construct an infix expression from two holes") {
    assertEquals(QuasiquoteMacroExamples.addHoles(2, 3), 5)
  }

  test("qr can construct nested applications with holes") {
    assertEquals(QuasiquoteMacroExamples.nestedFunctionHoles(2), 5)
  }

  test("qr accepts parenthesized infix expressions") {
    assertEquals(QuasiquoteMacroExamples.parenthesizedAdd(2, 3), 5)
  }

  test("qr can construct a typed hole expression") {
    assertEquals(QuasiquoteMacroExamples.typedHole(2), 2)
  }

  test("qr can construct an application with a typed hole argument") {
    assertEquals(QuasiquoteMacroExamples.typedHoleApplication(2), 3)
  }

  test("qr splices a constructed simple type into an expression ascription") {
    assertEquals(
      QuasiTypeSpliceExamples.spliceSummary("int", "$t", "t", "Int"),
      "typed=true constructed=STypeIdent(Int) inspected=STypeIdent(Int) matched=true"
    )
  }

  test("qr splices constructed applied types into expression ascriptions") {
    assertEquals(
      QuasiTypeSpliceExamples.spliceSummary("listInt", "List[$t]", "t", "Int"),
      "typed=true constructed=STypeApply(STypeIdent(List), [STypeIdent(Int)]) inspected=STypeApply(STypeIdent(List), [STypeIdent(Int)]) matched=true"
    )
    assertEquals(
      QuasiTypeSpliceExamples.spliceSummary("optionString", "Option[$t]", "t", "String"),
      "typed=true constructed=STypeApply(STypeIdent(Option), [STypeIdent(String)]) inspected=STypeApply(STypeIdent(Option), [STypeIdent(String)]) matched=true"
    )
  }

  test("qr splices constructed tuple and function types into expression ascriptions") {
    assertEquals(
      QuasiTypeSpliceExamples.spliceSummary("tupleIntString", "($a, $b)", "a", "Int", "b", "String"),
      "typed=true constructed=STypeTuple([STypeIdent(Int), STypeIdent(String)]) inspected=STypeTuple([STypeIdent(Int), STypeIdent(String)]) matched=true"
    )
    assertEquals(
      QuasiTypeSpliceExamples.spliceSummary("functionIntString", "$a => $b", "a", "Int", "b", "String"),
      "typed=true constructed=STypeFunction([STypeIdent(Int)], STypeIdent(String)) inspected=STypeFunction([STypeIdent(Int)], STypeIdent(String)) matched=true"
    )
  }

  test("qr splices a constructed type containing a repeated type-template hole") {
    assertEquals(
      QuasiTypeSpliceExamples.spliceSummary("tupleIntInt", "($t, $t)", "t", "Int"),
      "typed=true constructed=STypeTuple([STypeIdent(Int), STypeIdent(Int)]) inspected=STypeTuple([STypeIdent(Int), STypeIdent(Int)]) matched=true"
    )
  }

  test("qr supports a normal term splice and a constructed-type splice in a nested term context") {
    assertEquals(QuasiTypeSpliceExamples.nestedAppliedSplice(List(1, 2)), List(1, 2))
    assertEquals(
      QuasiTypeSpliceExamples.placeholderSourceSummary,
      "(__qq_term_hole_0: __qq_type_hole_1)"
    )
  }

  test("categorized term placeholder generation avoids literal source collisions") {
    val synthesized = PlaceholderSource.synthesizeCategorized(
      Seq("foo(__qq_term_hole_0, ", ")"),
      Seq(QuasiquoteHole.Term("actual-term"))
    ).toOption.get

    assertEquals(synthesized.source, "foo(__qq_term_hole_0, __qq_term_hole_0_1)")
    assertEquals(synthesized.bindings.map(_.name), Vector("__qq_term_hole_0_1"))
  }

  test("literal term-candidate identifiers remain resolvable beside a real term splice") {
    assertEquals(TermPlaceholderCollisionScope.result, 10)
  }

  test("categorized type placeholder generation avoids literal source collisions") {
    val synthesized = PlaceholderSource.synthesizeCategorized(
      Seq("(__qq_type_hole_1, ", ": ", ")"),
      Seq(
        QuasiquoteHole.Term("actual-term"),
        QuasiquoteHole.ConstructedTypeSplice(constructedInt)
      )
    ).toOption.get

    assertEquals(
      synthesized.source,
      "(__qq_type_hole_1, __qq_term_hole_0: __qq_type_hole_1_1)"
    )
    assertEquals(
      synthesized.bindings.map(_.name),
      Vector("__qq_term_hole_0", "__qq_type_hole_1_1")
    )
  }

  test("literal type-candidate identifiers remain resolvable beside real term and type splices") {
    assertEquals(TypePlaceholderCollisionScope.result, 7)
  }

  test("categorized placeholders stay unique and deterministic across mixed splices") {
    val parts = Seq("(", ", ", ", ", ": ", ")")
    val holes = Seq(
      QuasiquoteHole.Term("first-term"),
      QuasiquoteHole.ConstructedTypeSplice(constructedInt),
      QuasiquoteHole.Term("second-term"),
      QuasiquoteHole.ConstructedTypeSplice(constructedInt)
    )

    val first = PlaceholderSource.synthesizeCategorized(parts, holes).toOption.get
    val second = PlaceholderSource.synthesizeCategorized(parts, holes).toOption.get

    assertEquals(first, second)
    assertEquals(first.bindings.map(_.name).distinct.size, holes.size)
    assertEquals(
      first.bindings.map(_.name),
      Vector("__qq_term_hole_0", "__qq_type_hole_1", "__qq_term_hole_2", "__qq_type_hole_3")
    )
  }

  test("categorized placeholder lookup detects exact identifiers structurally") {
    val binding = PlaceholderBinding(
      "__qq_type_hole_1",
      QuasiquoteHole.ConstructedTypeSplice(constructedInt)
    )
    val index = new CategorizedPlaceholderIndex(Vector(binding))
    val exactTree = quasiquotes.parser.TinyTermParser
      .parse("identity[__qq_type_hole_1](1)")
      .toOption.get.rawTree
    val substringTree = quasiquotes.parser.TinyTermParser
      .parse("identity[prefix__qq_type_hole_1suffix](1)")
      .toOption.get.rawTree

    assertEquals(index.findIn(exactTree).map(_.name), List("__qq_type_hole_1"))
    assertEquals(index.findIn(substringTree), Nil)
  }

  test("qr controlled type-splice path agrees structurally with TypedTermConstruct.ascribe") {
    assertEquals(
      QuasiTypeSpliceExamples.equivalenceSummary(List(1)),
      "sameStructure=true sameNormalForm=true typed=true"
    )
  }

  test("qr rejects placeholder categories in the wrong syntactic position") {
    assertEquals(
      QuasiTypeSpliceExamples.markerInTermPositionMessage,
      "Constructed-type splice `__qq_type_hole_0` is not valid in term position."
    )
    assertEquals(
      QuasiTypeSpliceExamples.termInTypePositionMessage,
      "Term splice `__qq_term_hole_1` is not valid as the complete type of an expression ascription."
    )
  }

  test("qr rejects constructed-type splices outside the complete ascription type position") {
    assertEquals(
      QuasiTypeSpliceExamples.unsupportedTypePositionMessage,
      "Constructed-type splice `__qq_type_hole_0` is not supported inside method type arguments; only the complete type of an expression ascription is supported."
    )
  }

  test("qr propagates existing ConstructedType lowering failures") {
    assertEquals(
      QuasiTypeSpliceExamples.unsupportedNormalFormMessage,
      "Cannot lower unsupported constructed type normal form to TypeRepr: AnyVal"
    )
  }

  test("qr reports unknown categorized placeholders clearly") {
    assertEquals(
      QuasiTypeSpliceExamples.unknownPlaceholderMessage,
      "Unknown categorized quasiquote placeholder `__qq_type_hole_99`."
    )
  }

  test("constructed types can ascribe terms with simple type normal forms") {
    assertEquals(
      TypedTermConstructExamples.typedAscriptionSummary("int", "$t", "t", "Int"),
      "term=typed=true constructed=STypeIdent(Int) inspected=STypeIdent(Int) matched=true"
    )
  }

  test("constructed types can ascribe terms with applied type normal forms") {
    assertEquals(
      TypedTermConstructExamples.typedAscriptionSummary("listInt", "List[$t]", "t", "Int"),
      "term=typed=true constructed=STypeApply(STypeIdent(List), [STypeIdent(Int)]) inspected=STypeApply(STypeIdent(List), [STypeIdent(Int)]) matched=true"
    )
    assertEquals(
      TypedTermConstructExamples.typedAscriptionSummary("optionString", "Option[$t]", "t", "String"),
      "term=typed=true constructed=STypeApply(STypeIdent(Option), [STypeIdent(String)]) inspected=STypeApply(STypeIdent(Option), [STypeIdent(String)]) matched=true"
    )
  }

  test("constructed types can ascribe terms with tuple type normal forms") {
    assertEquals(
      TypedTermConstructExamples.typedAscriptionSummary("tupleIntString", "($a, $b)", "a", "Int", "b", "String"),
      "term=typed=true constructed=STypeTuple([STypeIdent(Int), STypeIdent(String)]) inspected=STypeTuple([STypeIdent(Int), STypeIdent(String)]) matched=true"
    )
  }

  test("constructed types can ascribe terms with function type normal forms") {
    assertEquals(
      TypedTermConstructExamples.typedAscriptionSummary("functionIntString", "$a => $b", "a", "Int", "b", "String"),
      "term=typed=true constructed=STypeFunction([STypeIdent(Int)], STypeIdent(String)) inspected=STypeFunction([STypeIdent(Int)], STypeIdent(String)) matched=true"
    )
  }

  test("constructed repeated type holes can ascribe terms") {
    assertEquals(
      TypedTermConstructExamples.typedAscriptionSummary("tupleIntInt", "($t, $t)", "t", "Int"),
      "term=typed=true constructed=STypeTuple([STypeIdent(Int), STypeIdent(Int)]) inspected=STypeTuple([STypeIdent(Int), STypeIdent(Int)]) matched=true"
    )
  }

  test("constructed typed/ascription integration preserves rejected type-construction boundaries") {
    assertEquals(
      TypedTermConstructExamples.typedAscriptionMissingBindingMessage("List[$t]"),
      "Missing type-construction binding `t`"
    )
    assertEquals(
      TypedTermConstructExamples.typedAscriptionMessage("List[$t]", "t", "Int", "extra", "String"),
      "Extra type-construction binding(s): extra"
    )
    assert(TypedTermConstructExamples.typedAscriptionMessage("scala.Int", "t", "Int").contains("Selected type syntax is not supported"))
    assert(TypedTermConstructExamples.typedAscriptionMessage("List[?]", "t", "Int").contains("Unsupported type construction template shape"))
    assertEquals(
      TypedTermConstructExamples.typedAscriptionUnsupportedNormalFormMessage("AnyVal"),
      "Cannot lower unsupported constructed type normal form to TypeRepr: AnyVal"
    )
  }

  test("constructed typed/ascription integration does not add selected-alias equality or direct interpolators") {
    assert(TypedTermConstructExamples.typedAscriptionMessage("scala.Int", "t", "Int").contains("Selected type syntax is not supported"))
    assert(!quasiquotes.types.QuasiTypeExamples.matches("Int", "scala.Int"))
  }

  test("qr can construct a tuple expression from holes") {
    assertEquals(QuasiquoteMacroExamples.tupleHoles(2, 3), (2, 3))
  }

  test("qr can construct a nested tuple expression") {
    assertEquals(QuasiquoteMacroExamples.nestedTupleHoles(2, 3, 4), (2, (3, 4)))
  }

  test("qr can construct an application with a tuple argument") {
    assertEquals(TupleApplicationScope.demo.input, "foo(($x, $y))")
    assertEquals(TupleApplicationScope.demo.placeholderSource, "foo((__hole0, __hole1))")
    assert(TupleApplicationScope.demo.treeStructure.contains("Apply"))
    assertEquals(TupleApplicationScope.demo.substitutedResult, "5")
  }

  test("qr can construct an if expression from holes") {
    assertEquals(QuasiquoteMacroExamples.ifHoles(true, 2, 3), 2)
    assertEquals(QuasiquoteMacroExamples.ifHoles(false, 2, 3), 3)
  }

  test("qr can construct an application with an if argument") {
    assertEquals(IfApplicationScope.demo.input, "foo(if $cond then $x else $y)")
    assertEquals(IfApplicationScope.demo.placeholderSource, "foo(if __hole0 then __hole1 else __hole2)")
    assert(IfApplicationScope.demo.treeStructure.contains("If"))
    assertEquals(IfApplicationScope.demo.substitutedResult, "12")
  }

  test("demo summary for hole infix expressions is usable") {
    val demo = QuasiquoteMacroExamples.holeInfixSummary(2, 3)
    assertEquals(demo.input, "$x + $y")
    assertEquals(demo.placeholderSource, "__hole0 + __hole1")
    assert(demo.treeStructure.contains("Apply"))
    assertEquals(demo.substitutedResult, "5")
  }

  test("demo summary for named infix expressions resolves caller scope identifiers") {
    val demo = NamedInfixScope.demo
    assertEquals(demo.input, "foo + bar")
    assertEquals(demo.placeholderSource, "foo + bar")
    assert(demo.treeStructure.contains("Apply"))
    assertEquals(demo.substitutedResult, "7")
  }

  test("demo summary for select plus infix expressions supports mixed named and hole input") {
    val demo = NamedSelectInfixScope.demo
    assertEquals(demo.input, "foo.bar + $x")
    assertEquals(demo.placeholderSource, "foo.bar + __hole0")
    assert(demo.treeStructure.contains("Select"))
    assertEquals(demo.substitutedResult, "7")
  }

  test("demo summary for nested named applications stays usable") {
    val demo = NestedNamedApplicationScope.demo
    assertEquals(demo.input, "foo(bar(baz))")
    assertEquals(demo.placeholderSource, "foo(bar(baz))")
    assert(demo.treeStructure.contains("Apply"))
    assertEquals(demo.substitutedResult, "16")
  }

  test("demo summary for nested select applications stays usable") {
    val demo = NestedSelectApplicationScope.demo
    assertEquals(demo.input, "foo.bar(baz($x))")
    assertEquals(demo.placeholderSource, "foo.bar(baz(__hole0))")
    assert(demo.treeStructure.contains("Apply"))
    assertEquals(demo.substitutedResult, "10")
  }

  test("demo summary for parenthesized named identifiers stays usable") {
    val demo = ParenthesizedNamedScope.demo
    assertEquals(demo.input, "(foo)")
    assertEquals(demo.placeholderSource, "(foo)")
    assert(demo.treeStructure.contains("Ident"))
    assertEquals(demo.substitutedResult, "11")
  }

  test("demo summary for parenthesized selected holes stays usable") {
    val demo = ParenthesizedSelectedHoleScope.demo
    assertEquals(demo.input, "(foo.bar($x))")
    assertEquals(demo.placeholderSource, "(foo.bar(__hole0))")
    assert(demo.treeStructure.contains("Apply"))
    assertEquals(demo.substitutedResult, "9")
  }

  test("demo summary for nested parenthesized hole expressions stays usable") {
    val demo = NestedParenHoleScope.demo
    assertEquals(demo.input, "$f(($x))")
    assertEquals(demo.placeholderSource, "__hole0((__hole1))")
    assert(demo.treeStructure.contains("Apply"))
    assertEquals(demo.substitutedResult, "8")
  }

  test("demo summary for parenthesized infix expressions stays usable") {
    val demo = QuasiquoteMacroExamples.parenthesizedInfixSummary(4, 5)
    assertEquals(demo.input, "($x + $y)")
    assertEquals(demo.placeholderSource, "(__hole0 + __hole1)")
    assert(demo.treeStructure.contains("Apply"))
    assertEquals(demo.substitutedResult, "9")
  }

  test("unsupported syntax fails clearly") {
    assert(QuasiquoteMacroExamples.unsupportedSyntaxMessage.trim.nonEmpty)
  }

  test("unsupported complex type ascriptions fail clearly") {
    assert(QuasiquoteMacroExamples.unsupportedComplexTypeAscriptionMessage.contains("Unsupported type ascription"))
  }
