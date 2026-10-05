package quasiquotes.neutral

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaCurriedMethodInstanceFactoryCharacterizationTest extends munit.FunSuite:
  private val Canonical =
    """def instance[A](combineFunction: A => A => A): Curried[A] =
      |  new Curried[A]:
      |    override def combine(a: A)(b: A): A =
      |      combineFunction(a)(b)""".stripMargin

  test("pins the parsed nested unary carrier and two ordinary member clauses"):
    val definition = parse(Canonical)
    definition.paramClauseGroups match
      case List(group) =>
        assertEquals(group.tparamClause.values.size, 1)
        group.paramClauses match
          case List(clause) =>
            clause.values.head.decltpe match
              case Some(Type.Function(List(Type.Name("A")), Type.Function(List(Type.Name("A")), Type.Name("A")))) => ()
              case other => fail(s"unexpected nested carrier $other")
          case other => fail(s"unexpected outer clauses $other")
      case other => fail(s"unexpected groups $other")
    val member = anonymousTemplate(definition).stats.head.asInstanceOf[Defn.Def]
    assertEquals(member.mods.map(_.productPrefix), List("Mod.Override"))
    assertEquals(member.paramClauseGroups.size, 1)
    assertEquals(member.paramClauseGroups.head.tparamClause.values, Nil)
    assertEquals(member.paramClauseGroups.head.paramClauses.map(_.values.size), List(1, 1))

  test("pins the parsed nested two-step application body"):
    val member = anonymousTemplate(parse(Canonical)).stats.head.asInstanceOf[Defn.Def]
    member.body match
      case Term.Apply(Term.Apply(Term.Name("combineFunction"), List(Term.Name("a"))), List(Term.Name("b"))) => ()
      case other => fail(s"unexpected nested body ${other.structure}")

  private def parse(source: String): Defn.Def =
    Scala3(source).parse[Stat].get.asInstanceOf[Defn.Def]

  private def anonymousTemplate(definition: Defn.Def): Template =
    definition.body.asInstanceOf[Term.NewAnonymous].templ
