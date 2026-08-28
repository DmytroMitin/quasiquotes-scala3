package quasiquotes.scalameta

import scala.quoted.staging.{Compiler, withQuotes}

final class ScalametaReflectedConstructorTypeBoundaryTest extends munit.FunSuite:
  test("Scalameta keeps reflected-Type lowering failures terminal and structurally bounded"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val evidence = withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*

      val reflected = TypeRepr.of[java.lang.StringBuilder]
      val complete = TermFrontend.build(using q)(
        Seq("new ", "(16)"),
        Seq(reflected)
      )
      val termPosition = TermFrontend.build(using q)(
        Seq("", ""),
        Seq(reflected)
      )
      val partialPath = TermFrontend.build(using q)(
        Seq("new java.lang.", "(16)"),
        Seq(reflected)
      )

      (
        complete.map(_.engine),
        termPosition.left.toOption.map(failure => failure.category -> failure.detail),
        partialPath.left.toOption.map(failure => failure.category -> failure.detail)
      )

    assertEquals(evidence._1, Right(TermFrontend.Engine.Scalameta))
    assert(evidence._2.exists(_._1 == "SCALAMETA_TYPED_LOWERING_FAILURE"), evidence._2)
    assert(evidence._2.exists(_._2.contains("not valid in term position")), evidence._2)
    assert(evidence._3.exists(_._1 == "SCALAMETA_TYPED_LOWERING_FAILURE"), evidence._3)
    assert(evidence._3.exists(_._2.contains("partial or applied constructor type syntax")), evidence._3)
