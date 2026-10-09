package quasiquotes.matching

import scala.collection.mutable
import scala.compiletime.testing.typeCheckErrors

import dotty.tools.dotc.ast.untpd

import quasiquotes.construct.SelectedMemberName
import quasiquotes.parser.{TermShape, TinyTermParser}
import quasiquotes.source.{
  GeneratedHoleIndex,
  GeneratedSegment,
  GeneratedSourceMap,
  HoleOccurrence,
  HoleRole,
  SourceId,
  SourceOrigin,
  SourceSpan
}

final class N066MultipleSelectedNameSourceRoleModelFitCharacterizationTest
    extends munit.FunSuite:

  private final case class Trace(
      source: String,
      generated: String,
      names: Vector[String],
      generatedNames: Vector[String],
      originalSpans: Vector[SourceSpan],
      generatedSpans: Vector[SourceSpan],
      compiled: TermPattern
  )

  private final case class SelectedPlan(
      sourceOccurrenceIndex: Int,
      selectOrdinal: Int
  )

  private final case class PrivateNormalizedSource(
      generatedSource: String,
      occurrences: Vector[HoleOccurrence],
      sourceMap: GeneratedSourceMap,
      compiled: TermPattern,
      selectedOccurrences: Vector[SelectedNamePatternOccurrence]
  )

  private val directDistinct = Trace(
    source = "$receiver.$first.$second",
    generated = "__qqhole_receiver.__qqhole_first.__qqhole_second",
    names = Vector("receiver", "first", "second"),
    generatedNames =
      Vector("__qqhole_receiver", "__qqhole_first", "__qqhole_second"),
    originalSpans =
      Vector(SourceSpan(0, 9), SourceSpan(10, 16), SourceSpan(17, 24)),
    generatedSpans =
      Vector(SourceSpan(0, 17), SourceSpan(18, 32), SourceSpan(33, 48)),
    compiled = TermPattern.Select(
      TermPattern.Select(TermPattern.Hole("receiver"), "__qqhole_first"),
      "__qqhole_second"
    )
  )

  private val distinctMatrix = Vector(
    directDistinct,
    directDistinct.copy(
      source = "$receiver.$first.$second()",
      generated = "__qqhole_receiver.__qqhole_first.__qqhole_second()",
      compiled = TermPattern.Apply(directDistinct.compiled, Nil)
    ),
    Trace(
      source = "$receiver.$first.$second($arg)",
      generated =
        "__qqhole_receiver.__qqhole_first.__qqhole_second(__qqhole_arg)",
      names = Vector("receiver", "first", "second", "arg"),
      generatedNames = Vector(
        "__qqhole_receiver",
        "__qqhole_first",
        "__qqhole_second",
        "__qqhole_arg"
      ),
      originalSpans = Vector(
        SourceSpan(0, 9),
        SourceSpan(10, 16),
        SourceSpan(17, 24),
        SourceSpan(25, 29)
      ),
      generatedSpans = Vector(
        SourceSpan(0, 17),
        SourceSpan(18, 32),
        SourceSpan(33, 48),
        SourceSpan(49, 61)
      ),
      compiled = TermPattern.Apply(
        directDistinct.compiled,
        List(TermPattern.Hole("arg"))
      )
    )
  )

  test(
    "distinct selected sources retain exact text occurrences spans roles maps raw shape and root-first ordinals"
  ):
    distinctMatrix.foreach { expected =>
      val mapped = mappedSource(expected.source)
      val raw =
        TinyTermParser.parseOrThrow(mapped.patternSource.source).rawTree
      val compiled =
        PatternCompiler
          .compileLocated(raw, mapped.generatedHoleIndex)
          .fold(error => fail(error.error.message), identity)

      assertEquals(mapped.patternSource.source, expected.generated)
      assertEquals(mapped.patternSource.holes, expected.names)
      assertEquals(mapped.occurrences.map(_.name), expected.names)
      assertEquals(
        mapped.occurrences.map(_.generatedName),
        expected.generatedNames
      )
      assertEquals(
        mapped.occurrences.map(_.originalSpan),
        expected.originalSpans
      )
      assertEquals(
        mapped.occurrences.map(_.generatedSpan),
        expected.generatedSpans
      )
      assertEquals(
        mapped.occurrences.map(_.role).distinct,
        Vector(HoleRole.TermPattern)
      )
      assertEquals(rewrittenSegments(mapped.originMap), mapped.occurrences)
      assertEquals(rawSelectedNames(raw), Vector("__qqhole_second", "__qqhole_first"))
      assertEquals(compiled, expected.compiled)
      assertEquals(
        compiledSelectedFields(compiled),
        Vector(0 -> "__qqhole_second", 1 -> "__qqhole_first")
      )
    }

  test(
    "repeated logical selected sources share one transport while occurrences and spans remain distinct"
  ):
    val mapped = mappedSource("$receiver.$member.$member")
    val compiled = compile(mapped)
    val members = mapped.occurrences.filter(_.name == "member")

    assertEquals(
      mapped.patternSource.source,
      "__qqhole_receiver.__qqhole_member.__qqhole_member"
    )
    assertEquals(
      members.map(_.generatedName),
      Vector("__qqhole_member", "__qqhole_member")
    )
    assertEquals(
      members.map(_.originalSpan),
      Vector(SourceSpan(10, 17), SourceSpan(18, 25))
    )
    assertEquals(
      members.map(_.generatedSpan),
      Vector(SourceSpan(18, 33), SourceSpan(34, 49))
    )
    assertEquals(rewrittenSegments(mapped.originMap), mapped.occurrences)
    assertEquals(
      mapped.generatedHoleIndex.entries,
      Vector(
        "__qqhole_member" -> "member",
        "__qqhole_receiver" -> "receiver"
      )
    )
    assertEquals(
      compiled,
      TermPattern.Select(
        TermPattern.Select(TermPattern.Hole("receiver"), "__qqhole_member"),
        "__qqhole_member"
      )
    )
    assertEquals(
      compiledSelectedFields(compiled),
      Vector(0 -> "__qqhole_member", 1 -> "__qqhole_member")
    )
    assert(
      RecoveredSelectedNameSourcePattern
        .recover(mapped, compiled)
        .left
        .toOption
        .exists(
          _.isInstanceOf[
            SelectedNameSourceRecoveryError.SourcePatternSidecarMismatch
          ]
        )
    )
    assertEquals(
      ValidatedSelectedNamePattern.createTwo(
        compiled,
        SelectedNamePatternOccurrence(
          "member",
          selectOrdinal = 1,
          members.head.generatedName
        ),
        SelectedNamePatternOccurrence(
          "member",
          selectOrdinal = 0,
          members(1).generatedName
        )
      ),
      Left(
        NeutralSelectedNameMatchError.SelectedNameTransportConflict(
          "__qqhole_member"
        )
      )
    )

  test("N063 stays single-occurrence while mapped source exposes zero through three selected candidates"):
    val cases = Vector(
      "$receiver.fixed" -> 0,
      "$receiver.$member" -> 1,
      "$receiver.$first.$second" -> 2,
      "$receiver.$first.$second.$third" -> 3
    )

    cases.foreach { case (source, expectedCandidates) =>
      val mapped = mappedSource(source)
      val compiled = compile(mapped)
      assertEquals(selectedCandidateOccurrences(mapped, compiled).size, expectedCandidates)

      val result = RecoveredSelectedNameSourcePattern.recover(mapped, compiled)
      expectedCandidates match
        case 0 =>
          assert(
            result.left.toOption.exists(
              _.isInstanceOf[
                SelectedNameSourceRecoveryError.MissingSelectedNameSourceRole
              ]
            )
          )
        case 1 =>
          assert(result.isRight)
        case _ =>
          assert(
            result.left.toOption.exists(
              _.isInstanceOf[
                SelectedNameSourceRecoveryError.MultipleSelectedNameSourceRoles
              ]
            )
          )
    }

    assert(
      typeCheckErrors(
        """quasiquotes.matching.ValidatedSelectedNamePattern.createTwo(null, null, null, null)"""
      ).nonEmpty
    )

  test("distinct mapped selected sources drive N065 createTwo for direct nullary and unary matching"):
    distinctMatrix.foreach { expected =>
      val mapped = mappedSource(expected.source)
      val compiled = compile(mapped)
      val first = mapped.occurrences.find(_.name == "first").get
      val second = mapped.occurrences.find(_.name == "second").get
      val validated = ValidatedSelectedNamePattern
        .createTwo(
          compiled,
          SelectedNamePatternOccurrence("first", 1, first.generatedName),
          SelectedNamePatternOccurrence("second", 0, second.generatedName)
        )
        .fold(error => fail(error.message), identity)
      val selectedTarget = TermShape.Select(
        TermShape.Select(identifier("service"), "alpha"),
        "beta"
      )
      val target =
        if expected.source.endsWith("()") then
          TermShape.Apply(selectedTarget, Nil)
        else if expected.source.endsWith("($arg)") then
          TermShape.Apply(selectedTarget, List(TermShape.Literal("1")))
        else selectedTarget
      val result = SelectedNamePatternMatcher
        .matchTarget(validated, target)
        .fold(error => fail(error.message), identity)

      assertEquals(
        result.selectedNameBindings,
        Map("first" -> selected("alpha"), "second" -> selected("beta"))
      )
      assertEquals(
        result.termBindings,
        if expected.source.endsWith("($arg)") then
          Map(
            "receiver" -> identifier("service"),
            "arg" -> TermShape.Literal("1")
          )
        else Map("receiver" -> identifier("service"))
      )
      assertEquals(
        validated.occurrences.map(value => value.selectOrdinal -> value.name),
        Vector(0 -> "second", 1 -> "first")
      )
    }

  test(
    "private per-occurrence normalization makes repeated selected sources N065-compatible without changing semantic identity"
  ):
    val source = "$receiver.$member.$member($arg)"
    val plans = Vector(SelectedPlan(1, 1), SelectedPlan(2, 0))
    val normalized = normalize(source, plans, marker = "selected")
    val alternate = normalize(source, plans, marker = "alternate")
    val validated = validatedTwo(normalized)
    val alternateValidated = validatedTwo(alternate)
    val equalTarget = TermShape.Apply(
      TermShape.Select(
        TermShape.Select(identifier("service"), "same"),
        "same"
      ),
      List(TermShape.Literal("1"))
    )
    val unequalTarget = equalTarget.copy(
      function = TermShape.Select(
        TermShape.Select(identifier("service"), "inner"),
        "outer"
      )
    )

    assertEquals(
      normalized.selectedOccurrences.map(_.name),
      Vector("member", "member")
    )
    assertEquals(
      normalized.selectedOccurrences.map(_.transport).distinct.size,
      2
    )
    assertEquals(
      SelectedNamePatternMatcher
        .matchTarget(validated, equalTarget)
        .toOption
        .get,
      NeutralSelectedNameMatchResult(
        Map(
          "receiver" -> identifier("service"),
          "arg" -> TermShape.Literal("1")
        ),
        Map("member" -> selected("same"))
      )
    )
    assertEquals(
      SelectedNamePatternMatcher.matchTarget(validated, unequalTarget),
      Left(
        NeutralSelectedNameMatchError.RepeatedSelectedNameCaptureMismatch(
          "member"
        )
      )
    )
    assertEquals(validated, alternateValidated)
    assertEquals(validated.hashCode, alternateValidated.hashCode)
    assertSourceMapConsistent(normalized)
    assertSourceMapConsistent(alternate)

    val publicPattern = QuasiPattern.term(source).toOption.get
    assertEquals(
      publicPattern.placeholderSource,
      "__qqhole_receiver.__qqhole_member.__qqhole_member(__qqhole_arg)"
    )
    assertEquals(
      compiledSelectedFields(publicPattern.pattern),
      Vector(0 -> "__qqhole_member", 1 -> "__qqhole_member")
    )

  test("private normalization is collision-free and preserves ordinary versus selected category checks"):
    val collisionSource =
      "__qqhole_member__selected_1.$member.$member"
    val collisionNormalized = normalize(
      collisionSource,
      Vector(SelectedPlan(0, 1), SelectedPlan(1, 0)),
      marker = "selected"
    )

    assertEquals(
      collisionNormalized.selectedOccurrences.map(_.transport),
      Vector(
        "__qqhole_member__selected_1_1",
        "__qqhole_member__selected_0"
      )
    )
    assertEquals(
      collisionNormalized.occurrences.map(_.generatedName).distinct.size,
      collisionNormalized.occurrences.size
    )
    assertSourceMapConsistent(collisionNormalized)
    assert(validatedTwo(collisionNormalized) != null)

    val categorySource = "$member.$member.$second"
    val categoryNormalized = normalize(
      categorySource,
      Vector(SelectedPlan(1, 1), SelectedPlan(2, 0)),
      marker = "selected"
    )
    assertEquals(
      ValidatedSelectedNamePattern.createTwo(
        categoryNormalized.compiled,
        categoryNormalized.selectedOccurrences.head,
        categoryNormalized.selectedOccurrences(1)
      ),
      Left(NeutralSelectedNameMatchError.CaptureCategoryConflict("member"))
    )

  test("missing misplaced and stale mapped transports fail closed"):
    val mapped = mappedSource("$receiver.$first.$second")
    val compiled = compile(mapped)
    val first = mapped.occurrences.find(_.name == "first").get
    val second = mapped.occurrences.find(_.name == "second").get

    Vector(
      ValidatedSelectedNamePattern.createTwo(
        compiled,
        SelectedNamePatternOccurrence("first", 1, "__missing"),
        SelectedNamePatternOccurrence("second", 0, second.generatedName)
      ),
      ValidatedSelectedNamePattern.createTwo(
        compiled,
        SelectedNamePatternOccurrence("first", 0, first.generatedName),
        SelectedNamePatternOccurrence("second", 1, second.generatedName)
      )
    ).foreach { result =>
      assert(
        result.left.toOption.exists(
          _.isInstanceOf[
            NeutralSelectedNameMatchError.InvalidSelectedNameOccurrence
          ]
        )
      )
    }

    val single = mappedSource("$receiver.$member")
    val stale = single.copy(
      patternSource =
        single.patternSource.copy(holes = Vector("receiver", "stale"))
    )
    assert(
      RecoveredSelectedNameSourcePattern
        .recover(stale, compile(single))
        .left
        .toOption
        .exists(
          _.isInstanceOf[
            SelectedNameSourceRecoveryError.SourcePatternSidecarMismatch
          ]
        )
    )

  test("invalid target names retain exact selected-source span attribution"):
    val mapped = mappedSource("$receiver.$first.$second")
    val first = mapped.occurrences.find(_.name == "first").get
    val second = mapped.occurrences.find(_.name == "second").get
    val validated = ValidatedSelectedNamePattern
      .createTwo(
        compile(mapped),
        SelectedNamePatternOccurrence("first", 1, first.generatedName),
        SelectedNamePatternOccurrence("second", 0, second.generatedName)
      )
      .fold(error => fail(error.message), identity)

    val cases = Vector(
      (
        TermShape.Select(
          TermShape.Select(identifier("service"), "+"),
          "ordinary"
        ),
        first,
        "+"
      ),
      (
        TermShape.Select(
          TermShape.Select(identifier("service"), "ordinary"),
          "type"
        ),
        second,
        "type"
      )
    )
    cases.foreach { case (target, occurrence, invalidName) =>
      assertEquals(
        SelectedNamePatternMatcher.matchTarget(validated, target),
        Left(
          NeutralSelectedNameMatchError.SelectedNameLexicalUnsupported(
            invalidName
          )
        )
      )
      assertEquals(
        occurrence.originalSpan,
        if occurrence.name == "first" then SourceSpan(10, 16)
        else SourceSpan(17, 24)
      )
      assertEquals(
        mapped.originMap.originsFor(occurrence.generatedSpan).map(_.origin),
        Vector(
          SourceOrigin.RewrittenHole(
            SourceId.TermPattern,
            occurrence.originalSpan,
            occurrence.name,
            HoleRole.TermPattern
          )
        )
      )
    }

  private def mappedSource(source: String): MappedPatternSource =
    PatternSource
      .synthesizeMapped(source)
      .fold(error => fail(error.message), identity)

  private def compile(mapped: MappedPatternSource): TermPattern =
    val raw =
      TinyTermParser.parseOrThrow(mapped.patternSource.source).rawTree
    PatternCompiler
      .compileLocated(raw, mapped.generatedHoleIndex)
      .fold(error => fail(error.error.message), identity)

  private def validatedTwo(
      normalized: PrivateNormalizedSource
  ): ValidatedSelectedNamePattern =
    ValidatedSelectedNamePattern
      .createTwo(
        normalized.compiled,
        normalized.selectedOccurrences.head,
        normalized.selectedOccurrences(1)
      )
      .fold(error => fail(error.message), identity)

  private def normalize(
      source: String,
      plans: Vector[SelectedPlan],
      marker: String
  ): PrivateNormalizedSource =
    val publicMapped = mappedSource(source)
    val planByIndex = plans.map(plan => plan.sourceOccurrenceIndex -> plan).toMap
    require(planByIndex.size == plans.size)
    require(plans.size == 2)
    require(plans.forall(plan =>
      0 <= plan.sourceOccurrenceIndex &&
        plan.sourceOccurrenceIndex < publicMapped.occurrences.size
    ))

    val literalIdentifier =
      raw"[A-Za-z_][A-Za-z0-9_]*".r
    val used = mutable.Set.from(
      literalIdentifier.findAllIn(source).toSet ++
        publicMapped.occurrences.map(_.generatedName)
    )
    val replacementNames =
      publicMapped.occurrences.zipWithIndex.map { case (occurrence, index) =>
        planByIndex.get(index) match
          case None => occurrence.generatedName
          case Some(plan) =>
            fresh(
              s"${occurrence.generatedName}__${marker}_${plan.selectOrdinal}",
              used
            )
      }

    val builder = new StringBuilder
    val segments = mutable.ArrayBuffer.empty[GeneratedSegment]
    val normalizedOccurrences = mutable.ArrayBuffer.empty[HoleOccurrence]
    var originalCursor = 0

    def appendOriginal(start: Int, end: Int): Unit =
      if start < end then
        val generatedStart = builder.length
        builder.append(source.substring(start, end))
        segments += GeneratedSegment(
          SourceSpan(generatedStart, builder.length),
          SourceOrigin.OriginalText(SourceId.TermPattern, SourceSpan(start, end))
        )

    publicMapped.occurrences.zipWithIndex.foreach {
      case (occurrence, index) =>
        appendOriginal(originalCursor, occurrence.originalSpan.start)
        val generatedName = replacementNames(index)
        val generatedStart = builder.length
        builder.append(generatedName)
        val generatedSpan = SourceSpan(generatedStart, builder.length)
        segments += GeneratedSegment(
          generatedSpan,
          SourceOrigin.RewrittenHole(
            SourceId.TermPattern,
            occurrence.originalSpan,
            occurrence.name,
            occurrence.role
          )
        )
        normalizedOccurrences += occurrence.copy(
          generatedName = generatedName,
          generatedSpan = generatedSpan
        )
        originalCursor = occurrence.originalSpan.end
    }
    appendOriginal(originalCursor, source.length)

    val generatedSource = builder.toString
    val occurrences = normalizedOccurrences.toVector
    val sourceMap = GeneratedSourceMap(
      generatedSource,
      SourceId.VirtualTermPatternParserInput,
      segments.toVector
    )
    val ordinaryOccurrences = occurrences.zipWithIndex.collect {
      case (occurrence, index) if !planByIndex.contains(index) => occurrence
    }
    val ordinaryIndex =
      GeneratedHoleIndex.fromOccurrences(ordinaryOccurrences)
    val raw = TinyTermParser.parseOrThrow(generatedSource).rawTree
    val compiled = PatternCompiler
      .compileLocated(raw, ordinaryIndex)
      .fold(error => fail(error.error.message), identity)
    val selectedOccurrences = plans.map { plan =>
      val occurrence = occurrences(plan.sourceOccurrenceIndex)
      SelectedNamePatternOccurrence(
        occurrence.name,
        plan.selectOrdinal,
        occurrence.generatedName
      )
    }

    PrivateNormalizedSource(
      generatedSource,
      occurrences,
      sourceMap,
      compiled,
      selectedOccurrences
    )

  private def fresh(base: String, used: mutable.Set[String]): String =
    var candidate = base
    var suffix = 0
    while used(candidate) do
      suffix += 1
      candidate = s"${base}_$suffix"
    used += candidate
    candidate

  private def rewrittenSegments(
      sourceMap: GeneratedSourceMap
  ): Vector[HoleOccurrence] =
    sourceMap.segments.collect {
      case GeneratedSegment(
            generatedSpan,
            SourceOrigin.RewrittenHole(
              _,
              originalSpan,
              name,
              role
            )
          ) =>
        HoleOccurrence(
          name,
          sourceMap.generatedSource.substring(
            generatedSpan.start,
            generatedSpan.end
          ),
          originalSpan,
          generatedSpan,
          role
        )
    }

  private def assertSourceMapConsistent(
      normalized: PrivateNormalizedSource
  ): Unit =
    assertEquals(
      rewrittenSegments(normalized.sourceMap),
      normalized.occurrences
    )
    normalized.occurrences.foreach { occurrence =>
      assertEquals(
        normalized.generatedSource.substring(
          occurrence.generatedSpan.start,
          occurrence.generatedSpan.end
        ),
        occurrence.generatedName
      )
    }

  private def rawSelectedNames(tree: untpd.Tree): Vector[String] =
    tree match
      case untpd.Select(qualifier, name) =>
        name.toString +: rawSelectedNames(qualifier)
      case untpd.Apply(function, arguments) =>
        rawSelectedNames(function) ++ arguments.toVector.flatMap(rawSelectedNames)
      case _ => Vector.empty

  private def compiledSelectedFields(
      pattern: TermPattern
  ): Vector[(Int, String)] =
    var nextOrdinal = 0
    val fields = mutable.ArrayBuffer.empty[(Int, String)]

    def loop(current: TermPattern): Unit =
      current match
        case TermPattern.Select(qualifier, name) =>
          val ordinal = nextOrdinal
          nextOrdinal += 1
          fields += ordinal -> name
          loop(qualifier)
        case TermPattern.Apply(function, arguments) =>
          loop(function)
          arguments.foreach(loop)
        case _ => ()

    loop(pattern)
    fields.toVector

  private def selectedCandidateOccurrences(
      mapped: MappedPatternSource,
      compiled: TermPattern
  ): Vector[HoleOccurrence] =
    val selectedTransports = compiledSelectedFields(compiled).map(_._2).toSet
    mapped.occurrences.filter(occurrence =>
      selectedTransports(occurrence.generatedName)
    )

  private def identifier(name: String): TermShape =
    TermShape.Identifier(name, isPlaceholder = false)

  private def selected(name: String): SelectedMemberName =
    SelectedMemberName
      .from(name)
      .fold(error => fail(error.message), identity)
