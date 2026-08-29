package quasiquotes.neutral

import quasiquotes.definitions.DefinitionName

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3
import scala.meta.parsers.Parsed

@nowarn("cat=deprecation")
class Phase136Auxify046ScalametaProbeTest extends munit.FunSuite:
  private final case class ExpectedNames(
      member: String,
      selfAlias: String,
      upperBase: String
  )

  test("characterizes the exact canonical Decl.Type tree structurally") {
    val declaration = parseDeclaration(
      "type Self >: self.type <: Nat { type Self = self.Self }"
    )

    assertEquals(declaration.mods, Nil)
    assertEquals(declaration.name.value, "Self")
    assertEquals(declaration.tparamClause.values, Nil)
    assertEquals(declaration.bounds.context, Nil)
    assertEquals(declaration.bounds.view, Nil)
    (declaration.bounds.lo, declaration.bounds.hi) match
      case (
            Some(Type.Singleton(lower: Term.Name)),
            Some(Type.Refine(Some(base: Type.Name), List(alias: Defn.Type)))
          ) =>
        assertEquals(lower.value, "self")
        assertEquals(base.value, "Nat")
        assertEquals(alias.mods, Nil)
        assertEquals(alias.name.value, "Self")
        assertEquals(alias.tparamClause.values, Nil)
        assertEquals(alias.bounds.lo, None)
        assertEquals(alias.bounds.hi, None)
        assertEquals(alias.bounds.context, Nil)
        assertEquals(alias.bounds.view, Nil)
        alias.body match
          case Type.Select(prefix: Term.Name, selected) =>
            assertEquals(prefix.value, "self")
            assertEquals(selected.value, "Self")
          case other => fail(s"expected direct selected-Type alias RHS, found $other")
      case other => fail(s"expected exact bounded abstract Decl.Type, found $other")
  }

  test("fully renamed legal names preserve the same exact categories") {
    val declaration = parseDeclaration(
      "type Element >: owner$2.type <: Domain { type Element = owner$2.Element }"
    )

    assertEquals(
      classify(declaration, ExpectedNames("Element", "owner$2", "Domain")),
      Right(())
    )
  }

  test("classifies every independently parseable malformed near miss in stable order") {
    assertRejected(
      "type Self >: self.type <: Nat { type Self = self.Self }",
      ExpectedNames("Self", "type", "Nat"),
      "NEUTRAL_SELF_MEMBER_EXPECTED_SELF_ALIAS_INVALID"
    )
    assertRejected(
      "type Self >: self$0.type <: Nat { type Self = self$0.Self }",
      ExpectedNames("Self", "self$0", "Nat"),
      "NEUTRAL_SELF_MEMBER_EXPECTED_SELF_ALIAS_INVALID"
    )
    assertRejected(
      "type Self <: Nat { type Self = self.Self }",
      ExpectedNames("Self", "self", "Nat"),
      "NEUTRAL_SELF_MEMBER_LOWER_BOUND_MISSING"
    )
    assertRejected(
      "type Self >: String <: Nat { type Self = self.Self }",
      ExpectedNames("Self", "self", "Nat"),
      "NEUTRAL_SELF_MEMBER_LOWER_BOUND_NOT_SINGLETON"
    )
    assertRejected(
      "type Self >: other.type <: Nat { type Self = self.Self }",
      ExpectedNames("Self", "self", "Nat"),
      "NEUTRAL_SELF_MEMBER_LOWER_ALIAS_MISMATCH"
    )
    assertRejected(
      "type Self >: self.type <: Nat",
      ExpectedNames("Self", "self", "Nat"),
      "NEUTRAL_SELF_MEMBER_UPPER_REFINEMENT_MISSING"
    )
    assertRejected(
      "type Self >: self.type <: Nat { type Self = self.Self; type Other = self.Other }",
      ExpectedNames("Self", "self", "Nat"),
      "NEUTRAL_SELF_MEMBER_REFINEMENT_COUNT_UNSUPPORTED"
    )
    assertRejected(
      "type Self >: self.type <: Nat { type Self }",
      ExpectedNames("Self", "self", "Nat"),
      "NEUTRAL_SELF_MEMBER_REFINEMENT_MEMBER_UNSUPPORTED"
    )
    assertRejected(
      "type Self >: self.type <: Nat { type Self = other.Self }",
      ExpectedNames("Self", "self", "Nat"),
      "NEUTRAL_SELF_MEMBER_SELECTED_PREFIX_MISMATCH"
    )
    assertRejected(
      "type Self >: self.type <: Nat { type Other = self.Other }",
      ExpectedNames("Self", "self", "Nat"),
      "NEUTRAL_SELF_MEMBER_REFINEMENT_NAME_MISMATCH"
    )
    assertRejected(
      "type Self >: self.type <: Nat { type Self = self.Other }",
      ExpectedNames("Self", "self", "Nat"),
      "NEUTRAL_SELF_MEMBER_SELECTED_MEMBER_MISMATCH"
    )
    assertRejected(
      "type Self >: self.type <: Other { type Self = self.Self }",
      ExpectedNames("Self", "self", "Nat"),
      "NEUTRAL_SELF_MEMBER_UPPER_BASE_MISMATCH"
    )
    assertRejected(
      "private type Self >: self.type <: Nat { type Self = self.Self }",
      ExpectedNames("Self", "self", "Nat"),
      "NEUTRAL_SELF_MEMBER_MODIFIERS_UNSUPPORTED"
    )
    assertRejected(
      "type Self[A] >: self.type <: Nat { type Self = self.Self }",
      ExpectedNames("Self", "self", "Nat"),
      "NEUTRAL_SELF_MEMBER_TYPE_PARAMETERS_UNSUPPORTED"
    )
  }

  test("illegal names fail in the Scala 3 parser before structural projection") {
    val parsed = Scala3(
      "type 1Self >: self.type <: Nat { type 1Self = self.1Self }"
    ).parse[Stat]
    assert(parsed.isInstanceOf[Parsed.Error], clues(parsed))
  }

  private def parseDeclaration(source: String): Decl.Type =
    Scala3(source).parse[Stat].get match
      case declaration: Decl.Type => declaration
      case other => fail(s"expected Decl.Type, found ${other.getClass.getSimpleName}")

  private def assertRejected(
      source: String,
      expected: ExpectedNames,
      expectedCode: String
  ): Unit =
    val result = classify(parseDeclaration(source), expected)
    assertEquals(result.left.toOption, Some(expectedCode), clues(result))

  private def classify(
      declaration: Decl.Type,
      expected: ExpectedNames
  ): Either[String, Unit] =
    for
      _ <- require(
        legal(expected.member),
        "NEUTRAL_SELF_MEMBER_EXPECTED_MEMBER_INVALID"
      )
      _ <- require(
        legalStableTerm(expected.selfAlias),
        "NEUTRAL_SELF_MEMBER_EXPECTED_SELF_ALIAS_INVALID"
      )
      _ <- require(
        legal(expected.upperBase),
        "NEUTRAL_SELF_MEMBER_EXPECTED_UPPER_BASE_INVALID"
      )
      _ <- require(
        declaration.mods.isEmpty,
        "NEUTRAL_SELF_MEMBER_MODIFIERS_UNSUPPORTED"
      )
      _ <- require(
        declaration.tparamClause.values.isEmpty,
        "NEUTRAL_SELF_MEMBER_TYPE_PARAMETERS_UNSUPPORTED"
      )
      _ <- require(
        legal(declaration.name.value) && declaration.name.value == expected.member,
        "NEUTRAL_SELF_MEMBER_OUTER_NAME_MISMATCH"
      )
      lower <- declaration.bounds.lo.toRight(
        "NEUTRAL_SELF_MEMBER_LOWER_BOUND_MISSING"
      )
      lowerAlias <- lower match
        case Type.Singleton(name: Term.Name) => Right(name.value)
        case _ => Left("NEUTRAL_SELF_MEMBER_LOWER_BOUND_NOT_SINGLETON")
      _ <- require(
        legalStableTerm(lowerAlias) && lowerAlias == expected.selfAlias,
        "NEUTRAL_SELF_MEMBER_LOWER_ALIAS_MISMATCH"
      )
      upper <- declaration.bounds.hi.toRight(
        "NEUTRAL_SELF_MEMBER_UPPER_BOUND_MISSING"
      )
      refined <- upper match
        case value: Type.Refine => Right(value)
        case _ => Left("NEUTRAL_SELF_MEMBER_UPPER_REFINEMENT_MISSING")
      base <- refined.tpe match
        case Some(name: Type.Name) => Right(name.value)
        case _ => Left("NEUTRAL_SELF_MEMBER_UPPER_BASE_UNSUPPORTED")
      _ <- require(
        legal(base) && base == expected.upperBase,
        "NEUTRAL_SELF_MEMBER_UPPER_BASE_MISMATCH"
      )
      member <- refined.stats match
        case value :: Nil => Right(value)
        case _ => Left("NEUTRAL_SELF_MEMBER_REFINEMENT_COUNT_UNSUPPORTED")
      alias <- member match
        case value: Defn.Type => Right(value)
        case _ => Left("NEUTRAL_SELF_MEMBER_REFINEMENT_MEMBER_UNSUPPORTED")
      _ <- require(
        alias.mods.isEmpty && alias.tparamClause.values.isEmpty,
        "NEUTRAL_SELF_MEMBER_REFINEMENT_MEMBER_UNSUPPORTED"
      )
      _ <- require(
        alias.bounds.lo.isEmpty &&
          alias.bounds.hi.isEmpty &&
          alias.bounds.context.isEmpty &&
          alias.bounds.view.isEmpty,
        "NEUTRAL_SELF_MEMBER_REFINEMENT_ALIAS_BOUNDS_UNSUPPORTED"
      )
      _ <- require(
        alias.name.value == declaration.name.value,
        "NEUTRAL_SELF_MEMBER_REFINEMENT_NAME_MISMATCH"
      )
      selected <- alias.body match
        case value: Type.Select => Right(value)
        case _ => Left("NEUTRAL_SELF_MEMBER_REFINEMENT_RHS_UNSUPPORTED")
      prefix <- selected.qual match
        case value: Term.Name => Right(value.value)
        case _ => Left("NEUTRAL_SELF_MEMBER_REFINEMENT_RHS_UNSUPPORTED")
      _ <- require(
        prefix == lowerAlias && prefix == expected.selfAlias,
        "NEUTRAL_SELF_MEMBER_SELECTED_PREFIX_MISMATCH"
      )
      _ <- require(
        selected.name.value == alias.name.value && selected.name.value == expected.member,
        "NEUTRAL_SELF_MEMBER_SELECTED_MEMBER_MISMATCH"
      )
    yield ()

  private def legal(value: String): Boolean =
    value != null && DefinitionName.fromSource(value).isRight

  private def legalStableTerm(value: String): Boolean =
    value != null && (
      DefinitionName.fromSource(value).isRight ||
        isPeerCollisionAlias(value)
    )

  private def isPeerCollisionAlias(value: String): Boolean =
    val separator = value.lastIndexOf('$')
    if separator <= 0 || separator == value.length - 1 then false
    else
      val base = value.substring(0, separator)
      val suffix = value.substring(separator + 1)
      DefinitionName.plain(base).isRight &&
        suffix.forall(_.isDigit) &&
        suffix.head != '0'

  private def require(condition: Boolean, code: String): Either[String, Unit] =
    Either.cond(condition, (), code)
