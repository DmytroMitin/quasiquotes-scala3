package quasiquotes.hybrid

import scala.quoted.staging.{Compiler, withQuotes}

import quasiquotes.parser.TypeShape
import quasiquotes.types.{TargetTypeReprInspector, TypeNormalForm}
import quasiquotes.types.TypeNormalForm.*
import quasiquotes.types.hybrid.ScalametaTypeFrontend

class C010ScalametaTupleFunctionRepresentationProbeTest extends munit.FunSuite:
  test("Scalameta source AST preserves syntax versus explicit generic applications before Core policy"):
    val tupleSyntax = ScalametaTypeFrontend.parseShape("(Int, String)").toOption.get
    val tupleExplicit = ScalametaTypeFrontend.parseShape("Tuple2[Int, String]").toOption.get
    val functionSyntax = ScalametaTypeFrontend.parseShape("Int => String").toOption.get
    val functionExplicit = ScalametaTypeFrontend.parseShape("Function1[Int, String]").toOption.get

    assert(tupleSyntax.isInstanceOf[TypeShape.Tuple])
    assert(tupleExplicit.isInstanceOf[TypeShape.Apply])
    assert(functionSyntax.isInstanceOf[TypeShape.Function])
    assert(functionExplicit.isInstanceOf[TypeShape.Apply])
    assertNotEquals(tupleSyntax, tupleExplicit)
    assertNotEquals(functionSyntax, functionExplicit)

    assertEquals(
      ScalametaTypeFrontend.normalForm("(Int, String)"),
      Right(STypeTuple(List(STypeIdent("Int"), STypeIdent("String"))))
    )
    assertEquals(
      ScalametaTypeFrontend.normalForm("Int => String"),
      Right(STypeFunction(List(STypeIdent("Int")), STypeIdent("String")))
    )
    assert(ScalametaTypeFrontend.normalForm("Tuple2[Int, String]").isLeft)
    assert(ScalametaTypeFrontend.normalForm("Function1[Int, String]").isLeft)

  test("typed-Scalameta tqr and tqq retain structured tuple function and nested behavior"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val evidence = withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*

      def inspect(value: TypeRepr): TypeNormalForm =
        TargetTypeReprInspector.inspect(using q)(value).fold(error => report.errorAndAbort(error.message), identity)

      val tuple = quasiquotes.scalameta.ScalametaQuasiquotes.tqr(
        StringContext("(Int, String)")
      )(using q)()
      val function = quasiquotes.scalameta.ScalametaQuasiquotes.tqr(
        StringContext("(Int, Boolean) => String")
      )(using q)()
      val nested = quasiquotes.scalameta.ScalametaQuasiquotes.tqr(
        StringContext("Either[(Int, String), Int => String]")
      )(using q)()

      val tuplePattern = quasiquotes.scalameta.ScalametaQuasiPattern.tqq(
        StringContext("(", ", ", ")")
      )(using q)
      val functionPattern = quasiquotes.scalameta.ScalametaQuasiPattern.tqq(
        StringContext("(", ", ", ") => ", "")
      )(using q)
      val nestedPattern = quasiquotes.scalameta.ScalametaQuasiPattern.tqq(
        StringContext("Either[(Int, String), Int => String]")
      )(using q)

      (
        inspect(tuple),
        inspect(function),
        inspect(nested),
        tuplePattern.unapplySeq(TypeRepr.of[(Int, String)]).map(_.size),
        functionPattern.unapplySeq(TypeRepr.of[(Int, Boolean) => String]).map(_.size),
        nestedPattern.unapplySeq(TypeRepr.of[Either[(Int, String), Int => String]]).map(_.size)
      )

    assertEquals(
      evidence._1,
      STypeTuple(List(STypeIdent("Int"), STypeIdent("String")))
    )
    assertEquals(
      evidence._2,
      STypeFunction(
        List(STypeIdent("Int"), STypeIdent("Boolean")),
        STypeIdent("String")
      )
    )
    assertEquals(
      evidence._3,
      STypeApply(
        STypeIdent("Either"),
        List(
          STypeTuple(List(STypeIdent("Int"), STypeIdent("String"))),
          STypeFunction(List(STypeIdent("Int")), STypeIdent("String"))
        )
      )
    )
    assertEquals(evidence._4, Some(2))
    assertEquals(evidence._5, Some(3))
    assertEquals(evidence._6, Some(0))
