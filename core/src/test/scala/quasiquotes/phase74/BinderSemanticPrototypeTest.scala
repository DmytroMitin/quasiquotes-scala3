package quasiquotes.phase74

import BinderSemanticPrototype.*
import BinderSemanticPrototype.Semantic
import BinderSemanticPrototype.Surface

class BinderSemanticPrototypeTest extends munit.FunSuite:
  private val sourceFree = FreeId("source:free")
  private val sourceX = FreeId("source:x")

  private def name(value: String): Surface = Surface.Name(value, FreeId(s"source:$value"))
  private def lambda(parameter: String, body: Surface): Surface =
    Surface.Lambda(parameter, "Int", body)
  private def add(left: Surface, right: Surface): Surface = Surface.Add(left, right)

  test("binder allocation is deterministic preorder and bound lookup is lexical") {
    val surface = lambda("x", lambda("y", add(name("x"), name("y"))))
    val expected = Semantic.Lambda(
      BinderId(0),
      "x",
      "Int",
      Semantic.Lambda(
        BinderId(1),
        "y",
        "Int",
        Semantic.Add(
          Semantic.Bound(BinderId(0), "x"),
          Semantic.Bound(BinderId(1), "y")
        )
      )
    )
    assertEquals(resolve(surface), expected)
    assertEquals(resolve(surface), resolve(surface))
  }

  test("nested same-name binders shadow by identity rather than text") {
    val semantic = resolve(lambda("x", lambda("x", name("x"))))
    assertEquals(
      semantic,
      Semantic.Lambda(
        BinderId(0),
        "x",
        "Int",
        Semantic.Lambda(
          BinderId(1),
          "x",
          "Int",
          Semantic.Bound(BinderId(1), "x")
        )
      )
    )
  }

  test("bound names are alpha-equivalent while free identities are preserved") {
    assert(alphaEquivalent(resolve(lambda("x", name("x"))), resolve(lambda("y", name("y")))))
    assert(
      alphaEquivalent(
        resolve(lambda("x", add(name("x"), Surface.Number(1)))),
        resolve(lambda("y", add(name("y"), Surface.Number(1))))
      )
    )
    assert(
      alphaEquivalent(
        resolve(lambda("x", add(Surface.Name("free", sourceFree), name("x")))),
        resolve(lambda("y", add(Surface.Name("free", sourceFree), name("y"))))
      )
    )
  }

  test("free identity and capture structure make alpha-equivalence fail") {
    val freePlusBound = resolve(
      lambda("x", add(Surface.Name("free", sourceFree), name("x")))
    )
    val capturedTwice = resolve(lambda("free", add(name("free"), name("free"))))
    assert(!alphaEquivalent(freePlusBound, capturedTwice))

    val differentFree = resolve(
      lambda("y", add(Surface.Name("free", FreeId("other:free")), name("y")))
    )
    assert(!alphaEquivalent(freePlusBound, differentFree))
  }

  test("nested lambdas are alpha-equivalent without beta or expression rewriting") {
    val left = resolve(lambda("x", lambda("y", add(name("x"), name("y")))))
    val right = resolve(lambda("a", lambda("b", add(name("a"), name("b")))))
    val shadowLeft = resolve(lambda("x", lambda("x", name("x"))))
    val shadowRight = resolve(lambda("a", lambda("b", name("b"))))
    assert(alphaEquivalent(left, right))
    assert(alphaEquivalent(shadowLeft, shadowRight))
    assert(!alphaEquivalent(resolve(lambda("x", add(name("x"), Surface.Number(1)))), resolve(lambda("y", add(Surface.Number(1), name("y"))))))
  }

  test("external splice keeps a same-text free identifier free") {
    val external = Semantic.Free(sourceX, "x")
    val semantic = resolve(lambda("x", add(name("x"), Surface.External(external))))
    assertEquals(
      semantic,
      Semantic.Lambda(
        BinderId(0),
        "x",
        "Int",
        Semantic.Add(
          Semantic.Bound(BinderId(0), "x"),
          Semantic.Free(sourceX, "x")
        )
      )
    )
  }

  test("repeated-hole equality is relative to lexical scope, not binder IDs or text") {
    val leftBinder = BinderId(10)
    val rightBinder = BinderId(20)
    val outerBinder = BinderId(30)
    val left = Semantic.Bound(leftBinder, "x")
    val right = Semantic.Bound(rightBinder, "y")
    val capturesOuter = Semantic.Bound(outerBinder, "x")

    assert(alphaEquivalentUnder(left, List(leftBinder), right, List(rightBinder)))
    assert(!alphaEquivalentUnder(left, List(outerBinder, leftBinder), capturesOuter, List(outerBinder, rightBinder)))
    assert(!alphaEquivalentUnder(Semantic.Free(sourceX, "x"), List(leftBinder), right, List(rightBinder)))
  }
