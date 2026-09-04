package quasiquotes.neutral

import _root_.quasiquotes.parser.{BinderId, TermShape}

import scala.annotation.nowarn
import scala.meta.*

@nowarn("cat=deprecation")
final class ScalametaDefinitionBinderAwareTermAuthoringCharacterizationTest
    extends munit.FunSuite:
  test("a fresh one-parameter method body name resolves through the seeded projector"):
    val body = Term.Name("x")
    val method = directMethod("x", body)
    val seed = ScalametaTermProjection.DefinitionBinder("x", BinderId(0))

    assert(method.body eq body)
    assertEquals(
      ScalametaTermProjection.projectWithDefinitionBinders(method.body, Vector(seed)),
      Right(ProjectedTermShape(TermShape.BoundReference(BinderId(0), "x"), None))
    )
    assert(allTrees(method).forall(_.pos == Position.None))

  test("a fresh backticked-keyword parameter body preserves decoded binder lookup"):
    val body = Term.Name("match")
    val method = directMethod("match", body)
    val seed = ScalametaTermProjection.DefinitionBinder("match", BinderId(7))

    assertEquals(method.paramClauseGroups.head.paramClauses.head.values.head.name.value, "match")
    assertEquals(
      method.paramClauseGroups.head.paramClauses.head.values.head.name.tokens.map(_.text).mkString,
      "`match`"
    )
    assertEquals(body.tokens.map(_.text).mkString, "`match`")
    assertEquals(
      ScalametaTermProjection.projectWithDefinitionBinders(body, Vector(seed)),
      Right(ProjectedTermShape(TermShape.BoundReference(BinderId(7), "match"), None))
    )

  test("two seeded names map independently to their supplied BinderIds"):
    val body = Term.Tuple(List(Term.Name("left"), Term.Name("right")))
    val seeds = Vector(
      ScalametaTermProjection.DefinitionBinder("left", BinderId(0)),
      ScalametaTermProjection.DefinitionBinder("right", BinderId(1))
    )

    assertEquals(
      ScalametaTermProjection.projectWithDefinitionBinders(body, seeds),
      Right(
        ProjectedTermShape(
          TermShape.Tuple(
            List(
              TermShape.BoundReference(BinderId(0), "left"),
              TermShape.BoundReference(BinderId(1), "right")
            )
          ),
          None
        )
      )
    )
    assert(allTrees(body).forall(_.pos == Position.None))

  test("seeded capture affects direct identifiers but not selected-member fields"):
    val direct = Term.Name("x")
    val selected = Term.Select(Term.Name("service"), Term.Name("x"))
    val seeds = Vector(ScalametaTermProjection.DefinitionBinder("x", BinderId(0)))

    assertEquals(
      ScalametaTermProjection.project(direct),
      Right(ProjectedTermShape(TermShape.Identifier("x", false), None))
    )
    assertEquals(
      ScalametaTermProjection.projectWithDefinitionBinders(direct, seeds),
      Right(ProjectedTermShape(TermShape.BoundReference(BinderId(0), "x"), None))
    )
    assertEquals(
      ScalametaTermProjection.projectWithDefinitionBinders(selected, seeds),
      Right(
        ProjectedTermShape(
          TermShape.Select(TermShape.Identifier("service", false), "x"),
          None
        )
      )
    )
    assert(allTrees(selected).forall(_.pos == Position.None))

  private def directMethod(parameterName: String, body: Term): Defn.Def =
    Defn.Def(
      Nil,
      Term.Name("method"),
      List(
        Member.ParamClauseGroup(
          Type.ParamClause(Nil),
          List(
            Term.ParamClause(
              List(Term.Param(Nil, Term.Name(parameterName), Some(Type.Name("Int")), None))
            )
          )
        )
      ),
      Some(Type.Name("Int")),
      body
    )

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
