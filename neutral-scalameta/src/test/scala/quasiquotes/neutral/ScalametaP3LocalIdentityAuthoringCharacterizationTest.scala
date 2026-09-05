package quasiquotes.neutral

import _root_.quasiquotes.parser.{BinderId, BlockStatement, TermShape, TypeShape}

import scala.annotation.nowarn
import scala.meta.*

@nowarn("cat=deprecation")
final class ScalametaP3LocalIdentityAuthoringCharacterizationTest extends munit.FunSuite:
  test("fresh P3 Block keeps one explicit identity Def before its method result"):
    val block = direct("id", "x", "Int")

    assertEquals(block.stats.map(_.productPrefix), List("Defn.Def", "Term.Name"))
    block.stats match
      case (definition: Defn.Def) :: (result: Term.Name) :: Nil =>
        assertEquals(definition.mods, Nil)
        assertEquals(definition.name.value, "id")
        definition.paramClauseGroups match
          case group :: Nil =>
            assertEquals(group.tparamClause.values, Nil)
            group.paramClauses match
              case clause :: Nil =>
                assertEquals(clause.mod, None)
                clause.values match
                  case parameter :: Nil =>
                    assertEquals(parameter.mods, Nil)
                    assertEquals(parameter.name.value, "x")
                    assertEquals(parameter.default, None)
                    assertEquals(parameter.decltpe.map(_.productPrefix), Some("Type.Name"))
                    assertEquals(parameter.decltpe.map(_.asInstanceOf[Type.Name].value), Some("Int"))
                  case other => fail(s"expected one parameter, got $other")
              case other => fail(s"expected one ordinary parameter clause, got $other")
          case other => fail(s"expected one parameter-clause group, got $other")
        assertEquals(definition.decltpe.map(_.asInstanceOf[Type.Name].value), Some("Int"))
        assertEquals(definition.body.asInstanceOf[Term.Name].value, "x")
        assertEquals(result.value, "id")
        assert(!(definition.name eq result))
        assert(!(definition.paramClauseGroups.head.paramClauses.head.values.head.name eq definition.body))
      case other => fail(s"expected exact P3 block, got $other")

    assert(allTrees(block).forall(_.pos == Position.None))
    assertEquals(
      project(block),
      TermShape.Block(
        List(
          BlockStatement.LocalDef(
            BinderId(0),
            "id",
            BinderId(1),
            "x",
            TypeShape.Identifier("Int"),
            TypeShape.Identifier("Int"),
            TermShape.BoundReference(BinderId(1), "x")
          )
        ),
        TermShape.BoundReference(BinderId(0), "id")
      )
    )

  test("fresh same-spelled method and parameter preserve their separate binder roles"):
    project(direct("x", "x", "String")) match
      case TermShape.Block(
            List(
              BlockStatement.LocalDef(
                methodId,
                "x",
                parameterId,
                "x",
                TypeShape.Identifier("String"),
                TypeShape.Identifier("String"),
                TermShape.BoundReference(bodyId, "x")
              )
            ),
            TermShape.BoundReference(resultId, "x")
          ) =>
        assertNotEquals(methodId, parameterId)
        assertEquals(bodyId, parameterId)
        assertEquals(resultId, methodId)
      case other => fail(s"unexpected same-spelled P3 shape: ${other.render}")

  test("fresh keyword and malformed names remain structural but fail existing P3 policy"):
    val fixtures = List(
      direct("match", "x", "Int") -> "NEUTRAL_LOCAL_DEF_NAME_UNSUPPORTED",
      direct("id", "match", "Int") -> "NEUTRAL_LOCAL_DEF_NAME_UNSUPPORTED",
      direct("bad.name", "x", "Int") -> "NEUTRAL_LOCAL_DEF_NAME_UNSUPPORTED",
      direct("id", "bad.name", "Int") -> "NEUTRAL_LOCAL_DEF_NAME_UNSUPPORTED"
    )

    assertEquals(direct("match", "x", "Int").stats.head.asInstanceOf[Defn.Def].name.tokens.map(_.text).mkString, "`match`")
    fixtures.foreach { (block, expectedCode) =>
      assertEquals(
        ScalametaTermProjection.project(block).left.toOption.map(_.code),
        Some(expectedCode)
      )
    }

  private def direct(methodName: String, parameterName: String, typeName: String): Term.Block =
    val definition = Defn.Def(
      Nil,
      Term.Name(methodName),
      List(
        Member.ParamClauseGroup(
          Type.ParamClause(Nil),
          List(
            Term.ParamClause(
              List(
                Term.Param(
                  Nil,
                  Term.Name(parameterName),
                  Some(Type.Name(typeName)),
                  None
                )
              ),
              None
            )
          )
        )
      ),
      Some(Type.Name(typeName)),
      Term.Name(parameterName)
    )
    Term.Block(List(definition, Term.Name(methodName)))

  private def project(term: Term): TermShape =
    ScalametaTermProjection.project(term) match
      case Right(projected) => projected.shape
      case Left(problem) => fail(problem.message)

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
