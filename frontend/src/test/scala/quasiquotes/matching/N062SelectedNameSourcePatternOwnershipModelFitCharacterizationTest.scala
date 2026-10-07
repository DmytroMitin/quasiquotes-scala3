package quasiquotes.matching

import dotty.tools.dotc.ast.untpd
import quasiquotes.parser.{TermShape, TinyTermParser}
import quasiquotes.source.{HoleRole, SourceSpan}

class N062SelectedNameSourcePatternOwnershipModelFitCharacterizationTest
    extends munit.FunSuite:

  private final case class ExpectedSource(
      input: String,
      generated: String,
      originalSpans: Vector[SourceSpan],
      generatedSpans: Vector[SourceSpan]
  )

  private val sourceMatrix = Vector(
    ExpectedSource(
      "$receiver.$member",
      "__qqhole_receiver.__qqhole_member",
      Vector(SourceSpan(0, 9), SourceSpan(10, 17)),
      Vector(SourceSpan(0, 17), SourceSpan(18, 33))
    ),
    ExpectedSource(
      "$receiver.$member()",
      "__qqhole_receiver.__qqhole_member()",
      Vector(SourceSpan(0, 9), SourceSpan(10, 17)),
      Vector(SourceSpan(0, 17), SourceSpan(18, 33))
    ),
    ExpectedSource(
      "$receiver.$member($arg)",
      "__qqhole_receiver.__qqhole_member(__qqhole_arg)",
      Vector(SourceSpan(0, 9), SourceSpan(10, 17), SourceSpan(18, 22)),
      Vector(SourceSpan(0, 17), SourceSpan(18, 33), SourceSpan(34, 46))
    )
  )

  test("PatternSource preserves selected-position transport and exact occurrence metadata") {
    sourceMatrix.foreach { expected =>
      val mapped = PatternSource.synthesizeMapped(expected.input).toOption.get

      assertEquals(mapped.patternSource.source, expected.generated)
      assertEquals(
        mapped.patternSource.holes,
        expected.input match
          case "$receiver.$member($arg)" => Vector("receiver", "member", "arg")
          case _ => Vector("receiver", "member")
      )
      assertEquals(mapped.occurrences.map(_.name), mapped.patternSource.holes)
      assertEquals(mapped.occurrences.map(_.generatedName),
        mapped.patternSource.holes.map("__qqhole_" + _))
      assertEquals(mapped.occurrences.map(_.originalSpan), expected.originalSpans)
      assertEquals(mapped.occurrences.map(_.generatedSpan), expected.generatedSpans)
      assertEquals(mapped.occurrences.map(_.role).distinct, Vector(HoleRole.TermPattern))
    }
  }

  test("TinyTermParser stores the selected-name transport in Select.name rather than an Ident child") {
    sourceMatrix.foreach { expected =>
      val raw = TinyTermParser.parseOrThrow(expected.generated).rawTree
      expected.input match
        case "$receiver.$member" =>
          raw match
            case untpd.Select(untpd.Ident(receiver), member) =>
              assertEquals(receiver.toString, "__qqhole_receiver")
              assertEquals(member.toString, "__qqhole_member")
            case other => fail(s"unexpected direct raw tree: ${other.getClass.getSimpleName}")
        case "$receiver.$member()" =>
          raw match
            case untpd.Apply(untpd.Select(untpd.Ident(receiver), member), Nil) =>
              assertEquals(receiver.toString, "__qqhole_receiver")
              assertEquals(member.toString, "__qqhole_member")
            case other => fail(s"unexpected nullary raw tree: ${other.getClass.getSimpleName}")
        case "$receiver.$member($arg)" =>
          raw match
            case untpd.Apply(
                  untpd.Select(untpd.Ident(receiver), member),
                  untpd.Ident(argument) :: Nil
                ) =>
              assertEquals(receiver.toString, "__qqhole_receiver")
              assertEquals(member.toString, "__qqhole_member")
              assertEquals(argument.toString, "__qqhole_arg")
            case other => fail(s"unexpected unary raw tree: ${other.getClass.getSimpleName}")
      }
    }

  test("PatternCompiler consumes qualifier and argument metadata but leaves Select.name fixed") {
    val expectedPatterns = Vector(
      TermPattern.Select(TermPattern.Hole("receiver"), "__qqhole_member"),
      TermPattern.Apply(
        TermPattern.Select(TermPattern.Hole("receiver"), "__qqhole_member"),
        Nil
      ),
      TermPattern.Apply(
        TermPattern.Select(TermPattern.Hole("receiver"), "__qqhole_member"),
        List(TermPattern.Hole("arg"))
      )
    )

    sourceMatrix.zip(expectedPatterns).foreach { case (expected, expectedPattern) =>
      val mapped = PatternSource.synthesizeMapped(expected.input).toOption.get
      val raw = TinyTermParser.parseOrThrow(mapped.patternSource.source).rawTree
      val compiled = PatternCompiler.compileLocated(raw, mapped.generatedHoleIndex).toOption.get

      assertEquals(compiled, expectedPattern)
      assertEquals(mapped.generatedHoleIndex.semanticNameFor("__qqhole_member"), Some("member"))
    }
  }

  test("one selected-name occurrence can be role-recovered after compilation for N061") {
    sourceMatrix.foreach { expected =>
      val mapped = PatternSource.synthesizeMapped(expected.input).toOption.get
      val raw = TinyTermParser.parseOrThrow(mapped.patternSource.source).rawTree
      val compiled = PatternCompiler.compileLocated(raw, mapped.generatedHoleIndex).toOption.get
      val member = mapped.occurrences.find(_.name == "member").get
      val occurrence = SelectedNamePatternOccurrence(
        name = member.name,
        selectOrdinal = 0,
        transport = member.generatedName
      )
      val validated = ValidatedSelectedNamePattern.create(compiled, occurrence)

      assert(validated.isRight)
      assertEquals(ordinaryTermHoleNames(compiled),
        if expected.input.endsWith("($arg)") then Set("receiver", "arg")
        else Set("receiver"))
      assertEquals(member.originalSpan, SourceSpan(10, 17))
      assertEquals(member.generatedSpan, SourceSpan(18, 33))
    }
  }

  test("mapped occurrences retain exact spans for duplicate and invalid selected-role diagnostics") {
    val duplicate = PatternSource.synthesizeMapped("$receiver.$member.$member").toOption.get
    val duplicatePattern = compile(duplicate)
    val memberOccurrences = duplicate.occurrences.filter(_.name == "member")
    val duplicateResult = ValidatedSelectedNamePattern.create(
      duplicatePattern,
      SelectedNamePatternOccurrence("member", 0, memberOccurrences.head.generatedName)
    )

    assertEquals(memberOccurrences.map(_.originalSpan), Vector(SourceSpan(10, 17), SourceSpan(18, 25)))
    assert(duplicateResult.swap.toOption.get.isInstanceOf[
      NeutralSelectedNameMatchError.DuplicateSelectedNameCapture
    ])

    val invalidRole = PatternSource.synthesizeMapped("$receiver.fixed($member)").toOption.get
    val invalidMember = invalidRole.occurrences.find(_.name == "member").get
    val invalidRoleResult = ValidatedSelectedNamePattern.create(
      compile(invalidRole),
      SelectedNamePatternOccurrence("member", 0, invalidMember.generatedName)
    )

    assertEquals(invalidMember.originalSpan, SourceSpan(16, 23))
    assert(invalidRoleResult.swap.toOption.get.isInstanceOf[
      NeutralSelectedNameMatchError.InvalidSelectedNameOccurrence
    ])
  }

  test("unsupported target lexical diagnostics can be anchored to the recovered member occurrence") {
    val mapped = PatternSource.synthesizeMapped("$receiver.$member").toOption.get
    val member = mapped.occurrences.find(_.name == "member").get
    val validated = ValidatedSelectedNamePattern
      .create(
        compile(mapped),
        SelectedNamePatternOccurrence("member", 0, member.generatedName)
      )
      .toOption
      .get
    val result = SelectedNamePatternMatcher.matchTarget(
      validated,
      TermShape.Select(TermShape.Identifier("target", isPlaceholder = false), "+")
    )

    assert(result.swap.toOption.get.isInstanceOf[
      NeutralSelectedNameMatchError.SelectedNameLexicalUnsupported
    ])
    assertEquals(member.originalSpan, SourceSpan(10, 17))
    assertEquals(member.generatedSpan, SourceSpan(18, 33))
  }

  test("QuasiPattern public value remains truthful while the selected role stays private sidecar data") {
    val compiled = QuasiPattern.term("$receiver.$member($arg)").toOption.get

    assertEquals(compiled.input, "$receiver.$member($arg)")
    assertEquals(
      compiled.placeholderSource,
      "__qqhole_receiver.__qqhole_member(__qqhole_arg)"
    )
    assertEquals(
      compiled.pattern,
      TermPattern.Apply(
        TermPattern.Select(TermPattern.Hole("receiver"), "__qqhole_member"),
        List(TermPattern.Hole("arg"))
      )
    )
  }

  private def compile(mapped: MappedPatternSource): TermPattern =
    val raw = TinyTermParser.parseOrThrow(mapped.patternSource.source).rawTree
    PatternCompiler.compileLocated(raw, mapped.generatedHoleIndex).toOption.get

  private def ordinaryTermHoleNames(pattern: TermPattern): Set[String] =
    pattern match
      case TermPattern.Hole(name) => Set(name)
      case TermPattern.Select(qualifier, _) => ordinaryTermHoleNames(qualifier)
      case TermPattern.Apply(function, arguments) =>
        ordinaryTermHoleNames(function) ++ arguments.flatMap(ordinaryTermHoleNames)
      case _ => Set.empty
