package quasiquotes.neutral

import scala.meta.*
import scala.meta.dialects.Scala3

final class C011InstanceFactoryScalametaCharacterizationTest extends munit.FunSuite:
  private val CanonicalSource =
    """def instance[A](emptyValue: => A, combineFunction: (A, A) => A): Monoid[A] = new Monoid[A] {
      |  override def empty: A = emptyValue
      |  override def combine(a: A, a1: A): A = combineFunction(a, a1)
      |}""".stripMargin

  test("records the complete canonical factory as one Scalameta definition"):
    val definition = parseDefinition(CanonicalSource)
    val structure = definition.structure

    assertEquals(definition.syntax, CanonicalSource)
    assertEquals(definition.paramClauseGroups.size, 1)
    assertEquals(
      definition.paramClauseGroups.head.tparamClause.values.map(_.name.value),
      List("A")
    )
    assertEquals(
      definition.paramClauseGroups.head.paramClauses.flatMap(_.values).map(_.name.value),
      List("emptyValue", "combineFunction")
    )
    assert(structure.contains("Type.ByName"), structure)
    assert(structure.contains("Type.Function"), structure)
    assert(structure.contains("Term.NewAnonymous"), structure)
    assertEquals(structure.sliding("Mod.Override".length).count(_ == "Mod.Override"), 2)
    assert(categorySignature(definition).contains("Term.Apply"))

  test("dynamic legal names preserve every structural category"):
    val renamed = parseDefinition(
      """def make[Element](fallbackValue: => Element, selection: (Element, Element) => Element): Choice[Element] = new Choice[Element] {
        |  override def fallback: Element = fallbackValue
        |  override def select(left: Element, right: Element): Element = selection(left, right)
        |}""".stripMargin
    )

    assertEquals(
      categorySignature(renamed),
      categorySignature(parseDefinition(CanonicalSource))
    )

  private def parseDefinition(source: String): Defn.Def =
    Input.String(source).parse[Stat].get match
      case definition: Defn.Def => definition
      case other => fail(s"expected Defn.Def, found ${other.productPrefix}")

  private def categorySignature(definition: Defn.Def): List[String] =
    definition.collect {
      case tree: Tree => tree.productPrefix
    }
