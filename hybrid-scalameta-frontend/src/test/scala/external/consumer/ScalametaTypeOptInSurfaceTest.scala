package external.consumer

import scala.quoted.staging.{Compiler, withQuotes}

import quasiquotes.scalameta.TypeFrontend

class ScalametaTypeOptInSurfaceTest extends munit.FunSuite:
  test("opt-in tqr supports zero and ordered reflected slots"):
    assert(ScalametaTypeOptInMacros.constructedTypesAgree)

  test("opt-in tqq preserves ordered original reflected subtree identity"):
    assert(ScalametaTypeOptInMacros.orderedCapturesAreOriginal)

  test("opt-in tqq supports a fixed zero-capture pattern"):
    assert(ScalametaTypeOptInMacros.zeroCapturePatternMatches)

  test("programmatic TypeFrontend exposes Scalameta construction metadata"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val evidence = withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*
      TypeFrontend
        .build(using q)(
          Seq("Either[List[", "], Option[", "]]"),
          Seq(TypeRepr.of[Int], TypeRepr.of[String])
        )
        .map(result => (result.engine, result.primaryFailure, result.typeRepr =:= TypeRepr.of[Either[List[Int], Option[String]]]))
    assertEquals(evidence, Right((TypeFrontend.Engine.Scalameta, None, true)))
    assert(Set("Scala3", "Scala38").contains(TypeFrontend.defaultDialectName))

  test("programmatic TypeFrontend compiles, matches, and preserves repeated-hole semantics"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val evidence = withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*
      val compiled = TypeFrontend.compilePattern("Either[$same, $same]").toOption.get
      val matched = TypeFrontend.matchPattern(using q)(compiled, TypeRepr.of[Either[Int, Int]])
      val mismatch = TypeFrontend.matchPattern(using q)(compiled, TypeRepr.of[Either[Int, String]])
      (
        compiled.engine,
        compiled.primaryFailure,
        matched.map(_.map(result => (result.captures.size, result.engine))),
        mismatch
      )
    assertEquals(
      evidence,
      (
        TypeFrontend.Engine.Scalameta,
        None,
        Right(Some((1, TypeFrontend.Engine.Scalameta))),
        Right(None)
      )
    )

  test("selected engine and terminal failures remain observable"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val selected = withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*
      TypeFrontend.build(using q)(Seq("Either[Int, String]"), Nil)
    assertEquals(selected.map(_.engine), Right(TypeFrontend.Engine.Scalameta))
    assertEquals(selected.toOption.flatMap(_.primaryFailure), None)

    val malformed = TypeFrontend.compilePattern("List[")
    assertEquals(malformed.swap.toOption.map(_.category), Some("SCALAMETA_PARSE_FAILURE"))
    assert(malformed.swap.toOption.exists(failure => failure.start >= 0 && failure.end >= failure.start))

    val terminal = withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*
      val construction =
        TypeFrontend.build(using q)(Seq("", ""), Seq(TypeRepr.of[Map[Int, String]]))
      val compiled = TypeFrontend.compile(Seq("", "")).toOption.get
      val matching =
        TypeFrontend.matchPattern(using q)(compiled, TypeRepr.of[Map[Int, String]])
      (
        construction.swap.toOption.map(_.category),
        matching.swap.toOption.map(_.category)
      )
    assertEquals(
      terminal,
      (Some("TYPE_SPLICE_INSPECTION_FAILURE"), Some("TYPE_TARGET_INSPECTION_FAILURE"))
    )

  test("ordinary current-Dotty tqr and tqq remain independently callable"):
    assert(ScalametaTypeOptInMacros.currentDottyDefaultStillWorks)
