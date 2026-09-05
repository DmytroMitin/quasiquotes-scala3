package quasiquotes.neutral

import _root_.quasiquotes.definitions.{DefinitionName, DefinitionShape}
import _root_.quasiquotes.parser.{BinderId, TermShape, TypeShape}

import scala.annotation.nowarn
import scala.meta.{Defn, Position}

@nowarn("cat=deprecation")
final class ScalametaDefinitionAuthoringCharacterizationTest extends munit.FunSuite:
  private val intType = TypeShape.Identifier("Int")

  test("the current sealed DefinitionShape hierarchy is exactly the five reusable families"):
    val fixtures = List[DefinitionShape](
      alias("Result", intType),
      immutable("answer", TermShape.Literal("42")),
      parameterless("answer", TermShape.Literal("42")),
      single("identity", BinderId(7), "value", TermShape.BoundReference(BinderId(7), "stale")),
      two(
        "pair",
        BinderId(9),
        "left",
        BinderId(2),
        "right",
        TermShape.Tuple(
          List(
            TermShape.BoundReference(BinderId(2), "stale-right"),
            TermShape.BoundReference(BinderId(9), "stale-left")
          )
        )
      )
    )

    assertEquals(fixtures.map(familyName), List("alias", "val", "parameterless", "single", "two"))

  test("the five accepted direct authorers produce their exact unpositioned Defn subtypes"):
    val authored = List[Defn](
      ScalametaSimpleTypeAliasAuthoring.author(alias("Result", intType)).toOption.get,
      ScalametaTypedImmutableValAuthoring
        .author(immutable("answer", TermShape.Literal("42")))
        .toOption
        .get,
      ScalametaTypedParameterlessDefAuthoring
        .author(parameterless("answer", TermShape.Literal("42")))
        .toOption
        .get,
      ScalametaTypedSingleParameterDefAuthoring
        .author(
          single(
            "identity",
            BinderId(7),
            "value",
            TermShape.BoundReference(BinderId(7), "value")
          )
        )
        .toOption
        .get,
      ScalametaTypedTwoParameterDefAuthoring
        .author(
          two(
            "pair",
            BinderId(9),
            "left",
            BinderId(2),
            "right",
            TermShape.Tuple(
              List(
                TermShape.BoundReference(BinderId(2), "right"),
                TermShape.BoundReference(BinderId(9), "left")
              )
            )
          )
        )
        .toOption
        .get
    )

    assert(authored(0).isInstanceOf[Defn.Type])
    assert(authored(1).isInstanceOf[Defn.Val])
    assert(authored.drop(2).forall(_.isInstanceOf[Defn.Def]))
    assert(authored.forall(_.pos == Position.None))

  private def familyName(shape: DefinitionShape): String =
    shape match
      case _: DefinitionShape.SimpleTypeAlias => "alias"
      case _: DefinitionShape.ImmutableVal => "val"
      case _: DefinitionShape.ParameterlessDef => "parameterless"
      case _: DefinitionShape.SingleParameterDef => "single"
      case _: DefinitionShape.TwoParameterDef => "two"

  private def alias(name: String, rhs: TypeShape): DefinitionShape.SimpleTypeAlias =
    DefinitionShape.simpleTypeAlias(plain(name), rhs).toOption.get

  private def immutable(name: String, rhs: TermShape): DefinitionShape.ImmutableVal =
    DefinitionShape.immutableVal(plain(name), intType, rhs).toOption.get

  private def parameterless(name: String, body: TermShape): DefinitionShape.ParameterlessDef =
    DefinitionShape.parameterlessDef(plain(name), intType, body).toOption.get

  private def single(
      methodName: String,
      binderId: BinderId,
      parameterName: String,
      body: TermShape
  ): DefinitionShape.SingleParameterDef =
    DefinitionShape
      .singleParameterDef(
        plain(methodName),
        binderId,
        plain(parameterName),
        intType,
        intType,
        body
      )
      .toOption
      .get

  private def two(
      methodName: String,
      firstBinderId: BinderId,
      firstName: String,
      secondBinderId: BinderId,
      secondName: String,
      body: TermShape
  ): DefinitionShape.TwoParameterDef =
    DefinitionShape
      .twoParameterDef(
        plain(methodName),
        firstBinderId,
        plain(firstName),
        intType,
        secondBinderId,
        plain(secondName),
        intType,
        intType,
        body
      )
      .toOption
      .get

  private def plain(value: String): DefinitionName =
    DefinitionName.plain(value).toOption.get
