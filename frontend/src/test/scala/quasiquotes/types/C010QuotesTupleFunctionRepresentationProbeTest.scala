package quasiquotes.types

import scala.quoted.staging.{Compiler, withQuotes}

import quasiquotes.parser.{TinyTypeParser, TypeShape}

class C010QuotesTupleFunctionRepresentationProbeTest extends munit.FunSuite:
  import TypeNormalForm.*

  test("current parser preserves syntax versus explicit generic application shapes"):
    val tupleSyntax = parseShape("(Int, String)")
    val tupleExplicit = parseShape("Tuple2[Int, String]")
    val functionSyntax = parseShape("Int => String")
    val functionExplicit = parseShape("Function1[Int, String]")

    assert(tupleSyntax.isInstanceOf[TypeShape.Tuple])
    assert(tupleExplicit.isInstanceOf[TypeShape.Apply])
    assert(functionSyntax.isInstanceOf[TypeShape.Function])
    assert(functionExplicit.isInstanceOf[TypeShape.Apply])
    assertNotEquals(tupleSyntax, tupleExplicit)
    assertNotEquals(functionSyntax, functionExplicit)

  test("Quotes TypeRepr has already erased tuple and function source spelling distinctions"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val evidence = withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*

      def inspect(value: TypeRepr): TypeNormalForm =
        TargetTypeReprInspector.inspect(using q)(value).fold(error => report.errorAndAbort(error.message), identity)

      val pairs = List(
        (
          TypeRepr.of[(Int, String)],
          TypeRepr.of[Tuple2[Int, String]],
          STypeTuple(List(STypeIdent("Int"), STypeIdent("String")))
        ),
        (
          TypeRepr.of[Int => String],
          TypeRepr.of[Function1[Int, String]],
          STypeFunction(List(STypeIdent("Int")), STypeIdent("String"))
        ),
        (
          TypeRepr.of[(Int, Boolean) => String],
          TypeRepr.of[Function2[Int, Boolean, String]],
          STypeFunction(
            List(STypeIdent("Int"), STypeIdent("Boolean")),
            STypeIdent("String")
          )
        )
      )

      pairs.map { (syntax, explicit, expected) =>
        (
          syntax =:= explicit,
          inspect(syntax),
          inspect(explicit),
          expected
        )
      }

    evidence.foreach { (compilerEquivalent, syntax, explicit, expected) =>
      assert(compilerEquivalent)
      assertEquals(syntax, expected)
      assertEquals(explicit, expected)
    }

  test("current-Dotty tqr and tqq retain structured tuple function and nested behavior"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val evidence = withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*

      def inspect(value: TypeRepr): TypeNormalForm =
        TargetTypeReprInspector.inspect(using q)(value).fold(error => report.errorAndAbort(error.message), identity)

      val tuple = QuasiTypequotes.tqr(StringContext("(Int, String)"))(using q)()
      val function = QuasiTypequotes.tqr(StringContext("(Int, Boolean) => String"))(using q)()
      val nested = QuasiTypequotes.tqr(
        StringContext("Either[(Int, String), Int => String]")
      )(using q)()

      val tuplePattern = QuasiTypequotes.tqq(StringContext("(", ", ", ")"))(using q)
      val functionPattern = QuasiTypequotes.tqq(
        StringContext("(", ", ", ") => ", "")
      )(using q)
      val nestedPattern = QuasiTypequotes.tqq(
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

  private def parseShape(source: String): TypeShape =
    TinyTypeParser.parse(source).fold(error => fail(error.summary), _.shape)
