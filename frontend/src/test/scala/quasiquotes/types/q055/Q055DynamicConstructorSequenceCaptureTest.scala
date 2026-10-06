package quasiquotes.types.q055

import scala.compiletime.testing.typeCheckErrors

class Q055DynamicConstructorSequenceCaptureTest extends munit.FunSuite:
  private inline def errors(inline source: String): List[String] =
    typeCheckErrors(source).map(_.message)

  test("production tqq preserves the exact dynamic constructor and whole argument sequence"):
    val (constructor, arguments, exact) =
      Q055DynamicConstructorSequenceCaptureMacros.wholeArguments
    assertEquals(constructor, "Either")
    assertEquals(arguments, List("Int", "String"))
    assert(exact)

  test("all required dynamic ranked layouts preserve argument identity and order"):
    assert(Q055DynamicConstructorSequenceCaptureMacros.layouts)

  test("D3 matching is broader than Q046 construction admission"):
    val result = Q055DynamicConstructorSequenceCaptureMacros.constructorDomains
    assert(result.list)
    assert(result.either)
    assert(result.box)
    assert(result.higherKinded)
    assert(result.instanceInner)
    assert(result.q046ProperRoundTrips)
    assert(result.higherKindedCapturedArgumentRejectedByQ046)
    assert(result.instanceInnerRejectedByQ046)

  test("nested, assembled, equal-distinct, and alias-expanded targets preserve identity"):
    assert(Q055DynamicConstructorSequenceCaptureMacros.identityCases)

  test("non-Applied targets and null fall through without exceptions"):
    assert(Q055DynamicConstructorSequenceCaptureMacros.targetBoundaries)

  test("fixed-side mismatches and insufficient targets fail atomically"):
    assert(Q055DynamicConstructorSequenceCaptureMacros.fixedSideFailures)

  test("production carrier exposes exact heterogeneous binder types"):
    val positive = errors(
      """import scala.quoted.*
import quasiquotes.types.QuasiTypequotes.*
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
    val constructorAsSequence = errors(
      """import scala.quoted.*
import quasiquotes.types.QuasiTypequotes.*
def attempt(using q: Quotes)(target: q.reflect.TypeRepr) =
  target match
    case tqq"$constructor[..$arguments]" =>
      val wrong: Seq[q.reflect.TypeRepr] = constructor
    case _ => ()
"""
    )
    val argumentsAsScalar = errors(
      """import scala.quoted.*
import quasiquotes.types.QuasiTypequotes.*
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

  test("unsupported dynamic source layouts retain deliberate diagnostics"):
    val diagnostics = List(
      errors(
        """import scala.quoted.*
import quasiquotes.types.QuasiTypequotes.*
def attempt(using q: Quotes)(target: q.reflect.TypeRepr) =
  target match
    case tqq"$constructor[$arguments]" => ()
    case _ => ()
"""
      ),
      errors(
        """import scala.quoted.*
import quasiquotes.types.QuasiTypequotes.*
def attempt(using q: Quotes)(target: q.reflect.TypeRepr) =
  target match
    case tqq"$constructor[...$arguments]" => ()
    case _ => ()
"""
      ),
      errors(
        """import scala.quoted.*
import quasiquotes.types.QuasiTypequotes.*
def attempt(using q: Quotes)(target: q.reflect.TypeRepr) =
  target match
    case tqq"$constructor[..$left, ..$right]" => ()
    case _ => ()
"""
      ),
      errors(
        """import scala.quoted.*
import quasiquotes.types.QuasiTypequotes.*
def attempt(using q: Quotes)(target: q.reflect.TypeRepr) =
  target match
    case tqq"List[$constructor[..$arguments]]" => ()
    case _ => ()
"""
      ),
      errors(
        """import scala.quoted.*
import quasiquotes.types.QuasiTypequotes.*
def attempt(using q: Quotes)(target: q.reflect.TypeRepr) =
  target match
    case tqq"$constructor[.. $arguments]" => ()
    case _ => ()
"""
      ),
      errors(
        """import scala.quoted.*
import quasiquotes.types.QuasiTypequotes.*
def attempt(using q: Quotes)(target: q.reflect.TypeRepr) =
  target match
    case tqq"$constructor[Map[Int, String], ..$arguments]" => ()
    case _ => ()
"""
      )
    )
    val repeatedConstructor = errors(
      """import scala.quoted.*
import quasiquotes.types.QuasiTypequotes.*
def attempt(using q: Quotes)(target: q.reflect.TypeRepr) =
  target match
    case tqq"$constructor[$constructor, ..$arguments]" => ()
    case _ => ()
"""
    )
    assert(diagnostics.forall(_.nonEmpty), diagnostics.mkString("\n"))
    assert(
      diagnostics.flatten.forall(_.contains("Invalid tqq type-pattern template:")),
      diagnostics.mkString("\n")
    )
    assert(repeatedConstructor.exists(_.contains("duplicate pattern variable")))
