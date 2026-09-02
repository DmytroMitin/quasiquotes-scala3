package quasiquotes.neutral

import scala.annotation.nowarn
import scala.meta.*

@nowarn("cat=deprecation")
final class ScalametaConstructorNewAuthoringCharacterizationTest extends munit.FunSuite:
  test("direct Scalameta 4.17.3 constructors expose the required N014 new topology"):
    val qualifier = Term.Select(Term.Name("java"), Term.Name("lang"))
    val constructorType = Type.Select(qualifier, Type.Name("StringBuilder"))
    val argumentClause = Term.ArgClause(List(Lit.Int(16), Term.Name("capacity")))
    val init = Init(constructorType, Name.Anonymous(), List(argumentClause))
    val fresh = Term.New(init)

    assertEquals(fresh.init.name.value, "")
    assertEquals(fresh.init.tpe.productPrefix, "Type.Select")
    assertEquals(constructorSegments(fresh.init.tpe), List("java", "lang", "StringBuilder"))
    assertEquals(fresh.init.argClauses.size, 1)
    assertEquals(fresh.init.argClauses.head.mod, None)
    assertEquals(
      fresh.init.argClauses.head.values.map(_.productPrefix).toList,
      List("Lit.Int", "Term.Name")
    )

  test("direct two-segment and nested constructors are fresh and wholly unpositioned"):
    val nested = Term.New(
      Init(
        Type.Select(Term.Name("other"), Type.Name("Value")),
        Name.Anonymous(),
        List(Term.ArgClause(List(Lit.Int(1))))
      )
    )
    val root = Term.New(
      Init(
        Type.Select(
          Term.Select(Term.Name("synthetic"), Term.Name("unresolved")),
          Type.Name("Widget")
        ),
        Name.Anonymous(),
        List(
          Term.ArgClause(
            List(
              nested,
              Term.If(
                Term.Name("cond"),
                Term.Apply(Term.Name("foo"), Term.ArgClause(List(Term.Name("x")))),
                Lit.Int(0)
              )
            )
          )
        )
      )
    )

    assertEquals(constructorSegments(nested.init.tpe), List("other", "Value"))
    assertEquals(constructorSegments(root.init.tpe), List("synthetic", "unresolved", "Widget"))
    assertEquals(root.init.argClauses.head.values.map(_.productPrefix).toList, List("Term.New", "Term.If"))
    assert(allTrees(root).forall(_.pos == Position.None))
    assertEquals(root.init.pos, Position.None)
    assertEquals(root.init.tpe.pos, Position.None)
    assertEquals(root.init.argClauses.head.pos, Position.None)

  private def constructorSegments(tpe: Type): List[String] =
    tpe match
      case name: Type.Name => name.value :: Nil
      case select: Type.Select => qualifierSegments(select.qual) :+ select.name.value
      case other => fail(s"expected Type.Name/Type.Select constructor path, got ${other.productPrefix}")

  private def qualifierSegments(term: Term): List[String] =
    term match
      case name: Term.Name => name.value :: Nil
      case select: Term.Select => qualifierSegments(select.qual) :+ select.name.value
      case other => fail(s"expected Term.Name/Term.Select qualifier path, got ${other.productPrefix}")

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
