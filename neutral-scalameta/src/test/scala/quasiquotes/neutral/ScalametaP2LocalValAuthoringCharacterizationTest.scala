package quasiquotes.neutral

import _root_.quasiquotes.parser.{BinderId, BlockStatement, TermShape}

import scala.annotation.nowarn
import scala.meta.*

@nowarn("cat=deprecation")
final class ScalametaP2LocalValAuthoringCharacterizationTest extends munit.FunSuite:
  test("fresh P2 Block keeps one typed immutable val before its result"):
    val name = Term.Name("x")
    val declaredType = Type.Name("Int")
    val initializer = Lit.Int(1)
    val result = Term.Name("x")
    val definition = Defn.Val(
      Nil,
      List(Pat.Var(name)),
      Some(declaredType),
      initializer
    )
    val block = Term.Block(List(definition, result))

    assertEquals(block.stats.map(_.productPrefix), List("Defn.Val", "Term.Name"))
    assert(block.stats.head eq definition)
    assert(block.stats.last eq result)
    assertEquals(definition.mods, Nil)
    definition.pats match
      case Pat.Var(authoredName) :: Nil =>
        assert(authoredName eq name)
        assertEquals(authoredName.value, "x")
      case other => fail(s"expected one Pat.Var, got $other")
    assertEquals(definition.decltpe, Some(declaredType))
    assert(definition.rhs eq initializer)
    assert(allTrees(block).forall(_.pos == Position.None))
    assertEquals(
      project(block),
      TermShape.Block(
        List(
          BlockStatement.LocalVal(
            BinderId(0),
            "x",
            "Int",
            TermShape.Literal("1")
          )
        ),
        TermShape.BoundReference(BinderId(0), "x")
      )
    )

  test("fresh AnyVal declared Type remains one direct accepted Type.Name"):
    val declaredType = Type.Name("AnyVal")
    val block = Term.Block(
      List(
        Defn.Val(
          Nil,
          List(Pat.Var(Term.Name("value"))),
          Some(declaredType),
          Lit.Int(1)
        ),
        Term.Name("value")
      )
    )

    assertEquals(block.stats.head.asInstanceOf[Defn.Val].decltpe, Some(declaredType))
    project(block) match
      case TermShape.Block(
            List(BlockStatement.LocalVal(_, "value", "AnyVal", TermShape.Literal("1"))),
            TermShape.BoundReference(_, "value")
          ) => ()
      case other => fail(s"unexpected fresh AnyVal P2 shape: ${other.render}")

  test("fresh keyword local names are structurally backticked but P2 projection rejects them"):
    val name = Term.Name("match")
    val block = Term.Block(
      List(
        Defn.Val(
          Nil,
          List(Pat.Var(name)),
          Some(Type.Name("Int")),
          Lit.Int(1)
        ),
        Term.Name("match")
      )
    )

    assertEquals(name.value, "match")
    assertEquals(name.tokens.map(_.text).mkString, "`match`")
    assertEquals(
      ScalametaTermProjection.project(block).left.toOption.map(_.code),
      Some("NEUTRAL_P2_BINDER_NAME_UNSUPPORTED")
    )

  private def project(term: Term): TermShape =
    ScalametaTermProjection.project(term) match
      case Right(projected) => projected.shape
      case Left(problem) => fail(problem.message)

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
