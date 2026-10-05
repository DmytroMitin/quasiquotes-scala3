package quasiquotes.types.q054

import scala.compiletime.testing.typeCheckErrors

class Q054DynamicConstructorSequenceCaptureTest extends munit.FunSuite:
  test("dynamic constructor plus whole argument sequence preserves exact objects"):
    val (constructor, arguments, exact) =
      Q054DynamicConstructorSequenceCaptureMacros.wholeArguments
    assertEquals(constructor, "Either")
    assertEquals(arguments, List("Int", "String"))
    assert(exact)

  test("all four ranked layouts and a fixed scalar constraint preserve types and identity"):
    assert(Q054DynamicConstructorSequenceCaptureMacros.layouts)

  test("D3 matching covers Q046 constructors and one stable broader AppliedType"):
    val result = Q054DynamicConstructorSequenceCaptureMacros.constructorDomains
    assert(result.list)
    assert(result.either)
    assert(result.box)
    assert(result.higherKinded)
    assert(result.instanceInner)
    assert(result.q046ProperRoundTrips)
    assert(result.higherKindedCapturedArgumentRejectedByQ046)
    assert(result.instanceInnerRejectedByQ046)

  test("nested, independently assembled, equal-distinct, and alias-expanded targets preserve identity"):
    assert(Q054DynamicConstructorSequenceCaptureMacros.identityCases)

  test("non-Applied targets and null fall through deliberately"):
    assert(Q054DynamicConstructorSequenceCaptureMacros.targetBoundaries)

  test("public carrier gives exact heterogeneous binder types without casts"):
    val positive = typeCheckErrors(
      """import scala.quoted.*
import quasiquotes.types.q054.Q054TypePatternProbe.*
def attempt(using q: Quotes)(target: q.reflect.TypeRepr) =
  target match
    case tqq"$constructor[$head, ..$tail]" =>
      val c: q.reflect.TypeRepr = constructor
      val h: q.reflect.TypeRepr = head
      val ts: Seq[q.reflect.TypeRepr] = tail
      (c, h, ts)
    case _ => throw new MatchError(target)
"""
    )
    val constructorAsSequence = typeCheckErrors(
      """import scala.quoted.*
import quasiquotes.types.q054.Q054TypePatternProbe.*
def attempt(using q: Quotes)(target: q.reflect.TypeRepr) =
  target match
    case tqq"$constructor[..$arguments]" =>
      val wrong: Seq[q.reflect.TypeRepr] = constructor
    case _ => ()
"""
    )
    val argumentsAsScalar = typeCheckErrors(
      """import scala.quoted.*
import quasiquotes.types.q054.Q054TypePatternProbe.*
def attempt(using q: Quotes)(target: q.reflect.TypeRepr) =
  target match
    case tqq"$constructor[..$arguments]" =>
      val wrong: q.reflect.TypeRepr = arguments
    case _ => ()
"""
    )
    assertEquals(positive, Nil)
    assert(constructorAsSequence.nonEmpty)
    assert(argumentsAsScalar.nonEmpty)

  test("unsupported source shapes fail with deliberate diagnostics"):
    val cases = List(
      "missing-rank" -> Q054DynamicConstructorSequenceCaptureMacros.diagnostic("missing-rank"),
      "rank3" -> Q054DynamicConstructorSequenceCaptureMacros.diagnostic("rank3"),
      "two-ranked" -> Q054DynamicConstructorSequenceCaptureMacros.diagnostic("two-ranked"),
      "constructor-outside-apply" -> Q054DynamicConstructorSequenceCaptureMacros.diagnostic("constructor-outside-apply"),
      "unsupported-fixed" -> Q054DynamicConstructorSequenceCaptureMacros.diagnostic("unsupported-fixed")
    )
    assert(cases.forall((_, message) => message != "<no-error>"), cases.mkString("\n"))
    val messages = cases.toMap
    assertEquals(messages("missing-rank").contains("rank-2"), true)
    assertEquals(messages("rank3").contains("rank-3"), true)
    assertEquals(messages("two-ranked").contains("only one rank-2"), true)
    assertEquals(messages("constructor-outside-apply").contains("applied Type position"), true)
    assertEquals(messages("unsupported-fixed").toLowerCase.contains("unsupported applied type constructor"), true)
