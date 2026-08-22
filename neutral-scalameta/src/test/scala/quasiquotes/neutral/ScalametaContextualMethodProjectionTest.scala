package quasiquotes.neutral

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
class ScalametaContextualMethodProjectionTest extends munit.FunSuite:
  test("projects Show.apply structurally to the existing validated IR") {
    val source: Defn.Def =
      q"def apply[A](using inst: Show[A]): Show[A] = inst".asInstanceOf[Defn.Def]

    val projected = project(source)

    assertEquals(projected.result.name, "apply")
    assertEquals(projected.result.typeParameterName, "A")
    assertEquals(projected.result.contextualParameterName, "inst")
    assertEquals(projected.result.contextualParameterType.source, "Show[A]")
    assertEquals(projected.result.resultType.source, "Show[A]")
    assertEquals(projected.result.body.referenceName, "inst")
    assertEquals(
      projected.sourceSpan,
      Some(NeutralSourceSpan(0, 49))
    )
  }

  test("accepts structurally constructed generated definitions with no position") {
    val source = syntheticApply("inst")
    assertEquals(source.pos, Position.None)

    val projected = project(source)

    assertEquals(projected.result.contextualParameterName, "inst")
    assertEquals(projected.result.body.referenceName, "inst")
    assertEquals(projected.sourceSpan, None)
  }

  test("rejects unsupported shapes with stable anchors and no semantic invention") {
    val ordinaryClause: Defn.Def =
      q"def apply[A](inst: Show[A]): Show[A] = inst".asInstanceOf[Defn.Def]
    val applicationBody: Defn.Def =
      q"def apply[A](using inst: Show[A]): Show[A] = summon[Show[A]]"
        .asInstanceOf[Defn.Def]
    val multipleTypeParameters: Defn.Def =
      q"def apply[A, B](using inst: Show[A]): Show[A] = inst"
        .asInstanceOf[Defn.Def]

    assertEquals(
      ScalametaContextualMethodProjection.project(ordinaryClause).left.toOption.map(_.code),
      Some("NEUTRAL_CONTEXTUAL_CLAUSE_UNSUPPORTED")
    )
    assertEquals(
      ScalametaContextualMethodProjection.project(applicationBody).left.toOption.map(_.code),
      Some("NEUTRAL_BODY_UNSUPPORTED")
    )
    assertEquals(
      ScalametaContextualMethodProjection.project(multipleTypeParameters).left.toOption.map(_.code),
      Some("NEUTRAL_TYPE_PARAMETER_CLAUSE_UNSUPPORTED")
    )
    assertEquals(
      ScalametaContextualMethodProjection.project(null).left.toOption.map(_.code),
      Some("NEUTRAL_DEFINITION_MISSING")
    )
  }

  private def project(source: Defn.Def): ProjectedContextualMethod =
    ScalametaContextualMethodProjection.project(source) match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def syntheticApply(parameterName: String): Defn.Def =
    val typeParameter = Type.Param(
      Nil,
      Type.Name("A"),
      Type.ParamClause(Nil),
      Type.Bounds(None, None, Nil, Nil)
    )
    val showOfA =
      Type.Apply(Type.Name("Show"), Type.ArgClause(List(Type.Name("A"))))
    val contextualParameter =
      Term.Param(Nil, Term.Name(parameterName), Some(showOfA), None)
    val parameterGroup = Member.ParamClauseGroup(
      Type.ParamClause(List(typeParameter)),
      List(Term.ParamClause(List(contextualParameter), Some(Mod.Using())))
    )
    Defn.Def(
      Nil,
      Term.Name("apply"),
      List(parameterGroup),
      Some(showOfA),
      Term.Name(parameterName)
    )
