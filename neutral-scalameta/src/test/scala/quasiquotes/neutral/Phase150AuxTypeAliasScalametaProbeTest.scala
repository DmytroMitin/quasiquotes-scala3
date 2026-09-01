package quasiquotes.neutral

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3
import scala.meta.parsers.Parsed

@nowarn("cat=deprecation")
class Phase150AuxTypeAliasScalametaProbeTest extends munit.FunSuite:
  private val Canonical =
    "type Aux[N <: Nat, M <: Nat, Out0 <: Nat] = Add[N, M] { type Out = Out0 }"

  test("Scalameta 4.17.3 preserves the exact canonical Defn.Type structure") {
    val definition = parseAlias(Canonical)

    assertEquals(definition.mods, Nil)
    assertEquals(definition.name.value, "Aux")
    assertEquals((definition.pos.start, definition.pos.end), (0, 73))
    assertEquals(definition.tparamClause.values.map(_.name.value), List("N", "M", "Out0"))
    assertEquals(
      definition.tparamClause.values.map(parameter => (parameter.pos.start, parameter.pos.end)),
      List((9, 17), (19, 27), (29, 40))
    )
    definition.tparamClause.values.foreach { parameter =>
      assertEquals(parameter.mods, Nil)
      assertEquals(parameter.tparamClause.values, Nil)
      assertEquals(parameter.bounds.lo, None)
      assertEquals(parameter.bounds.hi.map(_.syntax), Some("Nat"))
      assertEquals(parameter.bounds.context, Nil)
      assertEquals(parameter.bounds.view, Nil)
    }
    assertEquals(definition.bounds.lo, None)
    assertEquals(definition.bounds.hi, None)
    assertEquals(definition.bounds.context, Nil)
    assertEquals(definition.bounds.view, Nil)

    definition.body match
      case Type.Refine(Some(base: Type.Apply), List(member: Defn.Type)) =>
        assertEquals((definition.body.pos.start, definition.body.pos.end), (44, 73))
        assertEquals((base.pos.start, base.pos.end), (44, 53))
        assertEquals(base.tpe.syntax, "Add")
        assertEquals(base.argClause.values.map(_.syntax), List("N", "M"))
        assertEquals(member.mods, Nil)
        assertEquals(member.name.value, "Out")
        assertEquals(member.tparamClause.values, Nil)
        assertEquals(member.bounds.lo, None)
        assertEquals(member.bounds.hi, None)
        assertEquals(member.body.syntax, "Out0")
        assertEquals((member.pos.start, member.pos.end), (56, 71))
      case other => fail(s"expected applied base with one type-alias refinement, found $other")
  }

  test("fully renamed legal names retain the same Defn.Type categories") {
    val definition = parseAlias(
      "type Evidence[Left <: Domain, Right <: Domain, Result0 <: Domain] = Combine[Left, Right] { type Result = Result0 }"
    )

    assertEquals(definition.name.value, "Evidence")
    assertEquals(definition.tparamClause.values.map(_.name.value), List("Left", "Right", "Result0"))
    assertEquals(definition.tparamClause.values.flatMap(_.bounds.hi.map(_.syntax)), List.fill(3)("Domain"))
    definition.body match
      case Type.Refine(Some(base: Type.Apply), List(member: Defn.Type)) =>
        assertEquals(base.tpe.syntax, "Combine")
        assertEquals(base.argClause.values.map(_.syntax), List("Left", "Right"))
        assertEquals(member.name.value, "Result")
        assertEquals(member.body.syntax, "Result0")
      case other => fail(s"unexpected renamed structure: $other")
  }

  test("parseable near misses expose independent structural differences") {
    val cases = List(
      "type Aux[N <: Nat, M <: Nat] = Add[N, M] { type Out = N }" -> "TYPE_PARAMETER_ARITY_UNSUPPORTED",
      "type Aux[N <: Nat, M <: Nat, Out0 <: Nat, Extra <: Nat] = Add[N, M] { type Out = Out0 }" -> "TYPE_PARAMETER_ARITY_UNSUPPORTED",
      "type Aux[N >: Nothing <: Nat, M <: Nat, Out0 <: Nat] = Add[N, M] { type Out = Out0 }" -> "TYPE_PARAMETER_BOUNDS_UNSUPPORTED",
      "type Aux[N <: Nat, M <: Nat, Out0] = Add[N, M] { type Out = Out0 }" -> "TYPE_PARAMETER_BOUNDS_UNSUPPORTED",
      "type Aux[+N <: Nat, M <: Nat, Out0 <: Nat] = Add[N, M] { type Out = Out0 }" -> "TYPE_PARAMETER_MODIFIERS_UNSUPPORTED",
      "type Aux[N <: Nat : Ordering, M <: Nat, Out0 <: Nat] = Add[N, M] { type Out = Out0 }" -> "TYPE_PARAMETER_CONTEXT_BOUNDS_UNSUPPORTED",
      "type Aux[N[X] <: Nat, M <: Nat, Out0 <: Nat] = Add[N, M] { type Out = Out0 }" -> "TYPE_PARAMETER_TOPOLOGY_UNSUPPORTED",
      "type Aux[N <: Nat[String], M <: Nat, Out0 <: Nat] = Add[N, M] { type Out = Out0 }" -> "TYPE_PARAMETER_BOUND_SHAPE_UNSUPPORTED",
      "type Aux[N <: pkg.Nat, M <: Nat, Out0 <: Nat] = Add[N, M] { type Out = Out0 }" -> "TYPE_PARAMETER_BOUND_SHAPE_UNSUPPORTED",
      "type Aux[N <: Nat, M <: Nat, Out0 <: Nat] = Add[N] { type Out = Out0 }" -> "TARGET_ARITY_UNSUPPORTED",
      "type Aux[N <: Nat, M <: Nat, Out0 <: Nat] = Add[N, M, Out0] { type Out = Out0 }" -> "TARGET_ARITY_UNSUPPORTED",
      "type Aux[N <: Nat, M <: Nat, Out0 <: Nat] = pkg.Add[N, M] { type Out = Out0 }" -> "TARGET_CONSTRUCTOR_SHAPE_UNSUPPORTED",
      "type Aux[N <: Nat, M <: Nat, Out0 <: Nat] = Add[List[N], M] { type Out = Out0 }" -> "TARGET_ARGUMENT_SHAPE_UNSUPPORTED",
      "type Aux[N <: Nat, M <: Nat, Out0 <: Nat] = Add[M, N] { type Out = Out0 }" -> "APPLIED_ARGUMENT_ORDER_MISMATCH",
      "type Aux[N <: Nat, M <: Nat, Out0 <: Nat] = Add[N, N] { type Out = Out0 }" -> "APPLIED_ARGUMENT_ORDER_MISMATCH",
      "type Aux[N <: Nat, M <: Nat, Out0 <: Nat] = Add[N, M] {}" -> "REFINEMENT_MEMBER_COUNT_UNSUPPORTED",
      "type Aux[N <: Nat, M <: Nat, Out0 <: Nat] = Add[N, M] { type Out = Out0; type Other = Out0 }" -> "REFINEMENT_MEMBER_COUNT_UNSUPPORTED",
      "type Aux[N <: Nat, M <: Nat, Out0 <: Nat] = Add[N, M] { type Out }" -> "REFINEMENT_MEMBER_TOPOLOGY_UNSUPPORTED",
      "type Aux[N <: Nat, M <: Nat, Out0 <: Nat] = Add[N, M] { type Out[A] = Out0 }" -> "REFINEMENT_MEMBER_TOPOLOGY_UNSUPPORTED",
      "type Aux[N <: Nat, M <: Nat, Out0 <: Nat] = Add[N, M] { type Out >: Nothing = Out0 }" -> "REFINEMENT_MEMBER_TOPOLOGY_UNSUPPORTED",
      "type Aux[N <: Nat, M <: Nat, Out0 <: Nat] = Add[N, M] { type Result = Out0 }" -> "REFINEMENT_MEMBER_NAME_MISMATCH",
      "type Aux[N <: Nat, M <: Nat, Out0 <: Nat] = Add[N, M] { type Out = String }" -> "REFINEMENT_RHS_OUTPUT_MISMATCH",
      "private type Aux[N <: Nat, M <: Nat, Out0 <: Nat] = Add[N, M] { type Out = Out0 }" -> "ALIAS_MODIFIERS_UNSUPPORTED"
    )

    cases.foreach { case (source, expected) =>
      assertEquals(classify(parseAlias(source)), expected, clues(source))
    }
  }

  test("illegal refinement-member modifier and alias-bound combinations stop at the Scala 3 parser") {
    val illegal = List(
      "type Aux[N <: Nat, M <: Nat, Out0 <: Nat] = Add[N, M] { private type Out = Out0 }"
    )
    illegal.foreach { source =>
      assert(Scala3(source).parse[Stat].isInstanceOf[Parsed.Error], clues(source))
    }
  }

  private def parseAlias(source: String): Defn.Type =
    Scala3(source).parse[Stat].get match
      case value: Defn.Type => value
      case other => fail(s"expected Defn.Type, found ${other.getClass.getSimpleName}")

  private def classify(definition: Defn.Type): String =
    val parameters = definition.tparamClause.values
    if definition.mods.nonEmpty then "ALIAS_MODIFIERS_UNSUPPORTED"
    else if parameters.size != 3 then "TYPE_PARAMETER_ARITY_UNSUPPORTED"
    else if parameters.exists(_.mods.nonEmpty) then "TYPE_PARAMETER_MODIFIERS_UNSUPPORTED"
    else if parameters.exists(_.tparamClause.values.nonEmpty) then "TYPE_PARAMETER_TOPOLOGY_UNSUPPORTED"
    else if parameters.exists(_.bounds.context.nonEmpty) then "TYPE_PARAMETER_CONTEXT_BOUNDS_UNSUPPORTED"
    else if parameters.exists(parameter => parameter.bounds.lo.nonEmpty || parameter.bounds.hi.isEmpty)
    then "TYPE_PARAMETER_BOUNDS_UNSUPPORTED"
    else if parameters.exists(_.bounds.hi.exists(!_.isInstanceOf[Type.Name]))
    then "TYPE_PARAMETER_BOUND_SHAPE_UNSUPPORTED"
    else
      definition.body match
        case Type.Refine(Some(base: Type.Apply), members) =>
          if base.tpe.syntax != "Add" then "TARGET_CONSTRUCTOR_SHAPE_UNSUPPORTED"
          else if base.argClause.values.size != 2 then "TARGET_ARITY_UNSUPPORTED"
          else if base.argClause.values.exists(!_.isInstanceOf[Type.Name])
          then "TARGET_ARGUMENT_SHAPE_UNSUPPORTED"
          else if base.argClause.values.map(_.syntax) != List("N", "M")
          then "APPLIED_ARGUMENT_ORDER_MISMATCH"
          else if members.size != 1 then "REFINEMENT_MEMBER_COUNT_UNSUPPORTED"
          else
            members.head match
              case member: Defn.Type
                  if member.mods.nonEmpty || member.tparamClause.values.nonEmpty ||
                    member.bounds.lo.nonEmpty || member.bounds.hi.nonEmpty ||
                    member.bounds.context.nonEmpty || member.bounds.view.nonEmpty =>
                "REFINEMENT_MEMBER_TOPOLOGY_UNSUPPORTED"
              case member: Defn.Type if member.name.value != "Out" => "REFINEMENT_MEMBER_NAME_MISMATCH"
              case member: Defn.Type if member.body.syntax == "Out0" => "EXACT"
              case _: Defn.Type => "REFINEMENT_RHS_OUTPUT_MISMATCH"
              case _ => "REFINEMENT_MEMBER_TOPOLOGY_UNSUPPORTED"
        case _ => "RHS_REFINEMENT_REQUIRED"
