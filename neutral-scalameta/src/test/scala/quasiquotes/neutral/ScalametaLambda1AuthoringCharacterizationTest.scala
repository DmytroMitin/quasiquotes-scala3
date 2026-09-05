package quasiquotes.neutral

import _root_.quasiquotes.parser.{BinderId, TermShape}

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaLambda1AuthoringCharacterizationTest extends munit.FunSuite:
  test("fresh ordinary Function uses one unmodified explicitly typed parameter clause"):
    val parameterName = Term.Name("x")
    val parameterType = Type.Name("Int")
    val parameter = Term.Param(Nil, parameterName, Some(parameterType), None)
    val body = Term.Name("x")
    val function = Term.Function(Term.ParamClause(List(parameter)), body)

    assertEquals(function.productPrefix, "Term.Function")
    assertEquals(function.paramClause.values, List(parameter))
    assertEquals(function.paramClause.mod, None)
    assertEquals(parameter.mods, Nil)
    assert(parameter.name eq parameterName)
    assert(parameter.decltpe.contains(parameterType))
    assert(function.body eq body)
    assert(allTrees(function).forall(_.pos == Position.None))
    assertEquals(
      ScalametaTermProjection.project(function),
      Right(
        ProjectedTermShape(
          TermShape.Lambda1(
            BinderId(0),
            "x",
            "Int",
            TermShape.BoundReference(BinderId(0), "x")
          ),
          None
        )
      )
    )

  test("fresh keyword names carry structural backtick tokens without manual quoting"):
    val name = Term.Name("match")
    val parameter = Term.Param(Nil, name, Some(Type.Name("String")), None)
    val function = Term.Function(Term.ParamClause(List(parameter)), Term.Name("match"))

    assertEquals(name.value, "match")
    assertEquals(name.tokens.map(_.text).mkString, "`match`")
    assertEquals(function.body.asInstanceOf[Term.Name].tokens.map(_.text).mkString, "`match`")
    assertEquals(parameter.decltpe.get.asInstanceOf[Type.Name].value, "String")
    assert(allTrees(function).forall(_.pos == Position.None))

  test("fresh Boolean Lambda body retains direct If topology and projects exactly"):
    val body = Term.If(Term.Name("x"), Lit.Boolean(true), Lit.Boolean(false))
    val function = Term.Function(
      Term.ParamClause(
        List(Term.Param(Nil, Term.Name("x"), Some(Type.Name("Boolean")), None))
      ),
      body
    )

    assert(function.body eq body)
    assertEquals(
      ScalametaTermProjection.project(function),
      Right(
        ProjectedTermShape(
          TermShape.Lambda1(
            BinderId(0),
            "x",
            "Boolean",
            TermShape.If(
              TermShape.BoundReference(BinderId(0), "x"),
              TermShape.Literal("true"),
              TermShape.Literal("false")
            )
          ),
          None
        )
      )
    )
    assert(allTrees(function).forall(_.pos == Position.None))

  test("ordinary and contextual function syntax use distinct Scalameta roots"):
    val ordinary = Input.String("(x: Int) => x").parse[Term].get
    val contextual = Input.String("(x: Int) ?=> x").parse[Term].get

    assert(ordinary.isInstanceOf[Term.Function])
    assert(!ordinary.isInstanceOf[Term.ContextFunction])
    assert(contextual.isInstanceOf[Term.ContextFunction])
    assert(!contextual.isInstanceOf[Term.Function])

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
