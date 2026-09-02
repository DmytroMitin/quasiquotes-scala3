package quasiquotes.neutral

import scala.annotation.nowarn
import scala.meta.*

@nowarn("cat=deprecation")
final class ScalametaTermShapeAuthoringCharacterizationTest extends munit.FunSuite:
  test("direct Scalameta 4.17.3 constructors expose the required N013 topology"):
    val identifier = Term.Name("value")
    val integer = Lit.Int(-1)
    val boolean = Lit.Boolean(true)
    val string = Lit.String("text")
    val selection = Term.Select(identifier, Term.Name("field"))
    val application = Term.Apply(
      selection,
      Term.ArgClause(List(integer, boolean, string))
    )
    val infix = Term.ApplyInfix(
      identifier,
      Term.Name("+"),
      Type.ArgClause(Nil),
      Term.ArgClause(List(integer))
    )
    val unary = Term.ApplyUnary(Term.Name("!"), identifier)
    val tuple = Term.Tuple(List(integer, boolean, string))
    val conditional = Term.If(boolean, application, tuple)

    assertEquals(identifier.value, "value")
    assertEquals(integer.value, -1)
    assertEquals(boolean.value, true)
    assertEquals(string.value, "text")
    assertEquals(selection.qual.asInstanceOf[Term.Name].value, "value")
    assertEquals(selection.name.value, "field")
    assertEquals(application.fun.productPrefix, "Term.Select")
    assertEquals(application.argClause.mod, None)
    assertEquals(application.argClause.values.map(_.productPrefix), List("Lit.Int", "Lit.Boolean", "Lit.String"))
    assertEquals(infix.lhs.asInstanceOf[Term.Name].value, "value")
    assertEquals(infix.op.value, "+")
    assertEquals(infix.targClause.values, Nil)
    assertEquals(infix.argClause.mod, None)
    assertEquals(infix.argClause.values.map(_.asInstanceOf[Lit.Int].value), List(-1))
    assertEquals(unary.op.value, "!")
    assertEquals(unary.arg.asInstanceOf[Term.Name].value, "value")
    assertEquals(tuple.args.map(_.productPrefix), List("Lit.Int", "Lit.Boolean", "Lit.String"))
    assertEquals(conditional.cond.asInstanceOf[Lit.Boolean].value, true)
    assertEquals(conditional.thenp.productPrefix, "Term.Apply")
    assertEquals(conditional.elsep.productPrefix, "Term.Tuple")

  test("fresh direct roots children and synthetic clauses are unpositioned"):
    val root = Term.If(
      Term.ApplyUnary(Term.Name("!"), Term.Name("flag")),
      Term.Apply(
        Term.Select(Term.Name("service"), Term.Name("call")),
        Term.ArgClause(List(Lit.Int(1), Lit.String("two")))
      ),
      Term.Tuple(
        List(
          Lit.Boolean(false),
          Term.ApplyInfix(
            Lit.Int(2),
            Term.Name("+"),
            Type.ArgClause(Nil),
            Term.ArgClause(List(Lit.Int(3)))
          )
        )
      )
    )

    assert(allTrees(root).forall(_.pos == Position.None))

    val application = root.thenp.asInstanceOf[Term.Apply]
    val infix = root.elsep.asInstanceOf[Term.Tuple].args(1).asInstanceOf[Term.ApplyInfix]
    assertEquals(application.argClause.pos, Position.None)
    assertEquals(infix.targClause.pos, Position.None)
    assertEquals(infix.argClause.pos, Position.None)

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
