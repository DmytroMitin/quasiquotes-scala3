package quasiquotes.matching.q062

import quasiquotes.matching.{BlockPatternStatement, QuasiPattern, TermPattern}
import quasiquotes.matching.q062.Q062P3LocalIdentityMatchingFeasibilityProbe.*

private object Q062P3LocalIdentityMatchingFixtures:
  private inline val intPattern =
    """{ def boundedIdentity(value: Int): Int = value; boundedIdentity($argument) }"""

  val exact = analyze(
    {
      def boundedIdentity(value: Int): Int = value
      boundedIdentity(41)
    },
    intPattern
  )

  val alphaRenamed = analyze(
    {
      def otherName(x: Int): Int = x
      otherName(41)
    },
    intPattern
  )

  val stringIdentity = analyze(
    {
      def otherName(x: String): String = x
      otherName("value")
    },
    """{ def id(value: String): String = value; id($argument) }"""
  )

  val booleanIdentity = analyze(
    {
      def otherName(x: Boolean): Boolean = x
      otherName(true)
    },
    """{ def id(value: Boolean): Boolean = value; id($argument) }"""
  )

  val stringAgainstInt = analyze(
    {
      def otherName(x: String): String = x
      otherName("value")
    },
    intPattern
  )

  val wrongBody = analyze(
    {
      def otherName(x: Int): Int = 1
      otherName(41)
    },
    intPattern
  )

  val recursiveBody = analyze(
    {
      def otherName(x: Int): Int = otherName(x)
      otherName(41)
    },
    intPattern
  )

  private def external(value: Int): Int = value

  val wrongFollowingReference = analyze(
    {
      def otherName(x: Int): Int = x
      external(41)
    },
    intPattern
  )

  val twoLocalDefs = analyze(
    {
      def first(x: Int): Int = x
      def second(x: Int): Int = x
      first(41)
    },
    intPattern
  )

  val localValAndDef = analyze(
    {
      val seed: Int = 41
      def otherName(x: Int): Int = x
      otherName(seed)
    },
    intPattern
  )

  val nestedDefBody = analyze(
    {
      def otherName(x: Int): Int =
        def nested(y: Int): Int = y
        nested(x)
      otherName(41)
    },
    intPattern
  )

  val noCall = analyze(
    {
      def otherName(x: Int): Int = x
      otherName
    },
    intPattern
  )

  val multipleClauses = analyze(
    {
      def otherName(x: Int)(y: Int): Int = x
      otherName(41)(1)
    },
    intPattern
  )

  val typeParameters = analyze(
    {
      def otherName[A](x: Int): Int = x
      otherName[Int](41)
    },
    intPattern
  )

  val inferredResult = analyze(
    {
      def otherName(x: Int) = x
      otherName(41)
    },
    intPattern
  )

  val inlineModifier = analyze(
    {
      inline def otherName(x: Int): Int = x
      otherName(41)
    },
    intPattern
  )

  val incompatibleFixedResult = analyze[AnyVal](
    {
      def otherName(x: Int): AnyVal = x
      otherName(41)
    },
    intPattern
  )

  private val ambient = 41

  val ambientArgument = analyze(
    {
      def otherName(x: Int): Int = x
      otherName(ambient)
    },
    intPattern
  )

  val repeatedAmbientArgument = analyze(
    {
      def otherName(x: Int): Int = x
      otherName((ambient, ambient)._1)
    },
    """{ def id(value: Int): Int = value; id(($same, $same)._1) }"""
  )

class Q062P3LocalIdentityMatchingFeasibilityTest extends munit.FunSuite:
  import Q062P3LocalIdentityMatchingFixtures.*

  test("Q063 production realizes the Q062-locked M1 node while the feasibility stand-in remains valid") {
    val source =
      """{ def boundedIdentity(value: Int): Int = value; boundedIdentity($argument) }"""
    val current = QuasiPattern.term(source)
    assert(current.isRight, current)
    current.toOption.get.pattern match
      case TermPattern.Block(
            List(_: BlockPatternStatement.LocalDef),
            TermPattern.Apply(_: TermPattern.BoundReference, List(TermPattern.Hole("argument")))
          ) => ()
      case other => fail(s"unexpected production P3 pattern: ${other.render}")

    val candidate = Q062P3LocalIdentityMatchingFeasibilityProbe.compile(source)
    assert(candidate.isRight, candidate)
    assertEquals(candidate.toOption.get.holeNames, Vector("argument"))
  }

  test("B1 alpha-equivalent method and parameter binders match") {
    List(exact, alphaRenamed).foreach { evidence =>
      assert(evidence.matched, evidence)
      assert(evidence.distinctBinderSymbols, evidence)
      assert(evidence.bodyUsesParameterBinder, evidence)
      assert(evidence.resultUsesMethodBinder, evidence)
      assertEquals(evidence.patternTypes, "Int/Int")
      assertEquals(evidence.targetTypes, "Int/Int")
    }
    assertEquals(alphaRenamed.patternMethodName, "boundedIdentity")
    assertEquals(alphaRenamed.targetMethodName, "otherName")
    assertEquals(alphaRenamed.patternParameterName, "value")
    assertEquals(alphaRenamed.targetParameterName, "x")
  }

  test("T1 fixed semantic Types admit Int, String, and Boolean and reject mismatches") {
    List(exact, stringIdentity, booleanIdentity).foreach(evidence => assert(evidence.matched, evidence))
    assert(!stringAgainstInt.matched, stringAgainstInt)
    assert(stringAgainstInt.detail.contains("Types differ"), stringAgainstInt)
    assert(!incompatibleFixedResult.matched, incompatibleFixedResult)
    assert(incompatibleFixedResult.detail.contains("Unsupported target type representation"), incompatibleFixedResult)
  }

  test("the direct scalar hole captures the exact original reflected argument") {
    List(exact, alphaRenamed, stringIdentity, booleanIdentity, ambientArgument).foreach { evidence =>
      assert(evidence.matched, evidence)
      assert(evidence.exactArgumentIdentity, evidence)
      assert(evidence.argumentAvoidsLocalBinders, evidence)
    }
  }

  test("body and following-reference disagreements fail atomically") {
    List(wrongBody, recursiveBody, wrongFollowingReference).foreach { evidence =>
      assert(!evidence.matched, evidence)
      assert(!evidence.exactArgumentIdentity, evidence)
    }
  }

  test("wrong topology remains outside the first P3 matching slice") {
    List(
      twoLocalDefs,
      localValAndDef,
      nestedDefBody,
      noCall,
      multipleClauses,
      typeParameters,
      inferredResult,
      inlineModifier
    ).foreach(evidence => assert(!evidence.matched, evidence))
  }

  test("existing repeated-hole normalization remains usable for an ambient symbol inside the P3 argument") {
    assert(repeatedAmbientArgument.matched, repeatedAmbientArgument)
    assert(!repeatedAmbientArgument.exactArgumentIdentity, repeatedAmbientArgument)
    assert(repeatedAmbientArgument.argumentAvoidsLocalBinders, repeatedAmbientArgument)
  }
