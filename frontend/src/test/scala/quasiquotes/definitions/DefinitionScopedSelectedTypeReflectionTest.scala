package quasiquotes.definitions

import scala.compiletime.testing.typeCheckErrors

private object Phase126SelectedTypeFixtures:
  trait HolderA:
    type Out

  trait HolderB:
    type Out

  trait Outer:
    val inner: HolderA

  object MutablePrefix:
    var current: HolderA = null

  object CallPrefix:
    def current(): HolderA = null

  val evidence = DefinitionScopedSelectedTypeProbe.inspect {
    val outside: HolderA = null

    def contextual(using inst: HolderA): inst.Out = ???
    def renamedContextual(using service: HolderA): service.Out = ???
    def ordinary(value: HolderA): value.Out = ???
    def firstPrefix(first: HolderA, second: HolderA): first.Out = ???
    def otherDeclaration(value: HolderB): value.Out = ???
    def externalPrefix(value: HolderA): outside.Out = ???
    def nestedPrefix(outer: Outer): outer.inner.Out = ???

    ()
  }

class DefinitionScopedSelectedTypeReflectionTest extends munit.FunSuite:
  private val evidence = Phase126SelectedTypeFixtures.evidence

  test("direct contextual and ordinary parameter prefixes use the same exact scoped route") {
    assert(evidence.contextualAccepted)
    assert(evidence.ordinaryAccepted)
    assert(evidence.alphaRenamed)
    assert(evidence.rebuiltContextualAccepted)
    assert(evidence.rebuiltOrdinaryAccepted)
  }

  test("exact target inspection distinguishes prefix and member declaration identity") {
    assertEquals(
      evidence.differentPrefixCode,
      Some("STABLE_SELECTED_TYPE_PREFIX_MISMATCH")
    )
    assertEquals(
      evidence.differentDeclarationCode,
      Some("STABLE_SELECTED_TYPE_MEMBER_MISMATCH")
    )
  }

  test("external and nested stable paths fail closed in the definition-scoped tranche") {
    assertEquals(
      evidence.externalPrefixCode,
      Some("STABLE_SELECTED_TYPE_PREFIX_UNBOUND")
    )
    assertEquals(
      evidence.nestedPrefixCode,
      Some("STABLE_SELECTED_TYPE_NESTED_PATH_UNSUPPORTED")
    )
  }

  test("mutable and call-shaped prefixes remain illegal Scala selected-Type inputs") {
    assert(
      typeCheckErrors(
        "type Bad = quasiquotes.definitions.Phase126SelectedTypeFixtures.MutablePrefix.current.Out"
      ).nonEmpty
    )
    assert(
      typeCheckErrors(
        "type Bad = quasiquotes.definitions.Phase126SelectedTypeFixtures.CallPrefix.current().Out"
      ).nonEmpty
    )
  }
