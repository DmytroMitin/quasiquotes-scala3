package quasiquotes.neutral

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaSimpleTypeAliasCharacterizationTest extends munit.FunSuite:
  test("pins simple Defn.Type fields names bodies and root positions"):
    val fixtures = List(
      "type Result = Int" -> "Type.Name",
      "type Result = Option[Int]" -> "Type.Apply",
      "type Result = (Int, String)" -> "Type.Tuple",
      "type Result = (Int, String) => Boolean" -> "Type.Function",
      "type `type` = Int" -> "Type.Name"
    )

    fixtures.foreach { (source, expectedBodyKind) =>
      val definition = parsed(source)

      assertEquals(definition.mods, Nil)
      assertEquals(definition.tparamClause.values, Nil)
      assertEmptyBounds(definition.bounds)
      assertEquals(definition.body.productPrefix, expectedBodyKind)
      assertEquals(
        definition.pos,
        Position.Range(definition.pos.input, 0, source.length)
      )
    }

    assertNameEvidence(parsed("type Result = Int").name, "Result", "Result")
    assertNameEvidence(parsed("type `type` = Int").name, "type", "`type`")

  test("pins generic bounded and opaque neighboring topologies"):
    val generic = parsed("type Result[A] = A")
    val bounded = parsed("type Result >: Int <: AnyVal = Int")
    val opaque = parsed("opaque type Result = Int")

    assertEquals(generic.tparamClause.values.size, 1)
    assertEmptyBounds(generic.bounds)

    assertEquals(bounded.tparamClause.values, Nil)
    assertEquals(bounded.bounds.lo.map(_.productPrefix), Some("Type.Name"))
    assertEquals(bounded.bounds.hi.map(_.productPrefix), Some("Type.Name"))
    assertEquals(bounded.bounds.context, Nil)
    assertEquals(bounded.bounds.view, Nil)

    assert(opaque.mods.exists(_.isInstanceOf[Mod.Opaque]))
    assertEquals(opaque.tparamClause.values, Nil)
    assertEmptyBounds(opaque.bounds)

  test("direct Defn.Type construction is unpositioned with structural Type.Name token evidence"):
    val ordinary = direct(Type.Name("Result"), Type.Name("Int"))
    val keyword = direct(Type.Name("type"), Type.Name("Int"))

    assertEquals(ordinary.pos, Position.None)
    assertEquals(keyword.pos, Position.None)
    assertNameEvidence(ordinary.name, "Result", "Result")
    assertNameEvidence(keyword.name, "type", "`type`")
    assertEmptyBounds(ordinary.bounds)
    assertEquals(ordinary.tparamClause.values, Nil)

  private def parsed(source: String): Defn.Type =
    Scala3(source).parse[Stat].get match
      case definition: Defn.Type => definition
      case other => fail(s"expected Defn.Type, found ${other.productPrefix}")

  private def direct(name: Type.Name, body: Type): Defn.Type =
    Defn.Type(
      Nil,
      name,
      Type.ParamClause(Nil),
      body,
      Type.Bounds.empty
    )

  private def assertNameEvidence(
      name: Type.Name,
      expectedValue: String,
      expectedTokenText: String
  ): Unit =
    assertEquals(name.value, expectedValue)
    assertEquals(name.tokens.map(_.text).mkString, expectedTokenText)

  private def assertEmptyBounds(bounds: Type.Bounds): Unit =
    assertEquals(bounds.lo, None)
    assertEquals(bounds.hi, None)
    assertEquals(bounds.context, Nil)
    assertEquals(bounds.view, Nil)
