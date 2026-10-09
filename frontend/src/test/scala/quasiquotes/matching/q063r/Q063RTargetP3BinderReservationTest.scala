package quasiquotes.matching.q063r

import Q063RTargetP3BinderReservationMacros.*

private object Q063RTargetP3BinderReservationFixtures:
  val collision = collisionEvidence {
    def renamed(v: Int): Int = v
    renamed((((m: Int) => renamed(m)), 41)._2)
  }

  val mismatch = mismatchMatches {
    def renamed(v: Int): Int = v
    renamed((((m: Int) => identity(m)), 41)._2)
  }

  val directCaptureIdentity = directCaptureKeepsOriginal {
    def renamed(v: Int): Int = v
    renamed(41)
  }

class Q063RTargetP3BinderReservationTest extends munit.FunSuite:
  import Q063RTargetP3BinderReservationFixtures.*

  test("P3 admits and matches an argument Lambda1 that refers to the surrounding method"):
    assert(collision.sourceAdmitted, collision)
    assert(collision.matched, collision)
    assert(collision.exactCaptureIdentity, collision)

  test("P3 reserves distinct method, parameter, and descendant Lambda1 BinderIds"):
    assertEquals(
      Set(collision.methodBinderId, collision.parameterBinderId, collision.lambdaBinderId).size,
      3,
      collision
    )
    assertEquals(collision.lambdaParameterDepth, 0, collision)
    assertEquals(collision.enclosingMethodDepth, 1, collision)

  test("P3 rejects the wrong enclosing call and preserves direct original Term capture identity"):
    assertEquals(mismatch, false)
    assertEquals(directCaptureIdentity, true)
