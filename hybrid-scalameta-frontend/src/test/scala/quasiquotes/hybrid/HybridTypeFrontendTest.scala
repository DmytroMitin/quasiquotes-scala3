package quasiquotes.hybrid

import scala.quoted.staging.{Compiler, withQuotes}
import scala.meta.dialects

import quasiquotes.types.*
import quasiquotes.types.hybrid.{HybridTypeFrontend, ScalametaTypeFrontend}

class HybridTypeFrontendTest extends munit.FunSuite:
  test("selected type dialect follows the active supported compiler line"):
    val expected = if TypeQ3DialectPolicy.compilerVersion.startsWith("3.8") then "Scala38" else "Scala3"
    assertEquals(TypeQ3DialectPolicy.selectedName, expected)

  test("authoritative public type matrix is unique, exhaustive, and singly classified"):
    import TypeQ3ParityMatrix.Classification.*

    val rows = TypeQ3ParityMatrix.rows
    assertEquals(rows.map(_.id).distinct.size, rows.size)
    assertEquals(rows.map(_.id).toSet, TypeQ3ParityMatrix.requiredIds)
    assertEquals(rows.size, 30)
    assertEquals(rows.count(_.classification == CURRENT_PUBLIC_TYPE_CASE), 21)
    assertEquals(rows.count(_.classification == NOT_A_PUBLIC_TYPE_CASE), 3)
    assertEquals(rows.count(_.classification == DEFERRED_TYPE_FAMILY), 6)

  test("Scalameta maps the recursively supported current source matrix directly to the existing normal form"):
    val sources = List(
      "Int",
      "String",
      "Boolean",
      "List[Int]",
      "Option[List[String]]",
      "Either[List[Int], Option[(String, Boolean) => Int]]",
      "(Int, String)",
      "(Int, String, Boolean)",
      "Int => String",
      "(Int, String) => Boolean"
    )

    sources.foreach { source =>
      val candidate = ScalametaTypeFrontend.normalForm(source).map(_.render)
      val current = TypeNormalFormSource.fromSource(source).left.map(_.message).map(_.render)
      assertEquals(candidate.left.map(_.detail), current, source)
    }

  test("accepted but out-of-contract Scalameta type ASTs fail terminally without fallback"):
    val cases = List(
      "scala.Int",
      "List[?]",
      "Map[Int, String]"
    )

    cases.foreach { source =>
      val result = HybridTypeFrontend.normalForm(source)
      assertEquals(
        result.swap.toOption.map(_.category),
        Some("SCALAMETA_TYPE_LOWERING_UNSUPPORTED"),
        source
      )
    }

  test("synthetic primary parse failure proves exact-parser fallback policy"):
    val primary = ScalametaTypeFrontend.Failure.parse(0, 0, "synthetic restricted dialect")
    val result = HybridTypeFrontend.resolveNormalForm("Either[Int, String]", Left(primary))
    assertEquals(result.map(_.engine), Right(HybridTypeFrontend.Engine.CurrentDottyFallback))
    assert(result.toOption.flatMap(_.primaryFailure).exists(_.category == "SCALAMETA_PARSE_FAILURE"))

  test("a broader Scalameta dialect never overrides exact compiler syntax rejection"):
    val result = HybridTypeFrontend.normalForm("(name: Int, age: Int)", dialects.Scala3Future)
    if TypeQ3DialectPolicy.compilerVersion.startsWith("3.3") then
      assertEquals(result.swap.toOption.map(_.category), Some("EXACT_COMPILER_SYNTAX_REJECTED"))
    else
      assert(!result.exists(_.engine == HybridTypeFrontend.Engine.Scalameta), result.toString)

  test("bounded supported-line dialect search finds no real current-matrix syntax lag"):
    val searchMatrix = List(
      "Int",
      "String",
      "Boolean",
      "List[Int]",
      "Option[String]",
      "Either[Int, String]",
      "(Int, String)",
      "(Int, String, Boolean)",
      "Int => String",
      "(Int, String) => Boolean"
    )
    val realLags = searchMatrix.filter { source =>
      TypeNormalFormSource.fromSource(source).isRight &&
        ScalametaTypeFrontend.normalForm(source).isLeft
    }
    assertEquals(realLags, Nil, "NO_REAL_SUPPORTED_LINE_TYPE_LAG_FOUND_IN_SEARCH_MATRIX")

  test("Scalameta tqr construction is structurally equal to the current engine for zero and multiple slots"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val evidence = withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*

      val zero = HybridTypeFrontend.construct(using q)(Seq("Either[List[Int], Option[String]]"), Nil)
      val multiple = HybridTypeFrontend.construct(using q)(
        Seq("Either[List[", "], Option[", "]]"),
        Seq(TypeRepr.of[Int], TypeRepr.of[String])
      )
      val expected = TypeRepr.of[Either[List[Int], Option[String]]]
      (
        zero.flatMap(result => TargetTypeReprInspector.inspect(using q)(result.value).left.map(error =>
          ScalametaTypeFrontend.Failure.construction(error.message)
        )).map(_.render),
        multiple.flatMap(result => TargetTypeReprInspector.inspect(using q)(result.value).left.map(error =>
          ScalametaTypeFrontend.Failure.construction(error.message)
        )).map(_.render),
        TargetTypeReprInspector.inspect(using q)(expected).map(_.render)
      )

    assertEquals(evidence._1, evidence._3.left.map(error => ScalametaTypeFrontend.Failure.construction(error.message)))
    assertEquals(evidence._2, evidence._3.left.map(error => ScalametaTypeFrontend.Failure.construction(error.message)))

  test("Scalameta tqq matching returns ordered original reflected subtree identities"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val evidence = withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*

      val target = TypeRepr.of[Either[List[Int], Option[String]]]
      val expectedChildren = target match
        case AppliedType(_, left :: right :: Nil) =>
          val leftChild = left match
            case AppliedType(_, child :: Nil) => child
          val rightChild = right match
            case AppliedType(_, child :: Nil) => child
          (leftChild, rightChild)
      val compiled = HybridTypeFrontend
        .compile(Seq("Either[List[", "], Option[", "]]"))
        .toOption
        .get
      val captures = HybridTypeFrontend.matchPattern(using q)(compiled, target).toOption.flatten.get
      val fixed = HybridTypeFrontend
        .compile(Seq("Either[List[", "], Option[String]]"))
        .toOption
        .get
      val mismatch = HybridTypeFrontend.extract(using q)(fixed, TypeRepr.of[Either[List[Int], Option[Int]]])
      (
        captures.size,
        captures(0).asInstanceOf[AnyRef] eq expectedChildren._1.asInstanceOf[AnyRef],
        captures(1).asInstanceOf[AnyRef] eq expectedChildren._2.asInstanceOf[AnyRef],
        mismatch.isEmpty
      )

    assertEquals(evidence, (2, true, true, true))

  test("zero-capture, repeated programmatic holes, and mismatch behavior retain current semantics"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val evidence = withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*

      val zero = HybridTypeFrontend.compile(Seq("Int")).toOption.get
      val repeated = HybridTypeFrontend.compileProgrammatic("Either[$same, $same]").toOption.get
      (
        HybridTypeFrontend.matchPattern(using q)(zero, TypeRepr.of[Int]).map(_.map(_.size)),
        TypePattern.matchNormalForm(repeated.value, TypeNormalFormSource.fromSource("Either[Int, Int]").toOption.get).nonEmpty,
        TypePattern.matchNormalForm(repeated.value, TypeNormalFormSource.fromSource("Either[Int, String]").toOption.get).isEmpty
      )

    assertEquals(evidence, (Right(Some(0)), true, true))

  test("inspection and parser failures use stable categories and ordinary extractor fallthrough"):
    val malformed = HybridTypeFrontend.compileProgrammatic("List[")
    assertEquals(malformed.swap.toOption.map(_.category), Some("SCALAMETA_PARSE_FAILURE"))
    assert(malformed.swap.toOption.exists(failure => failure.start >= 0 && failure.end >= failure.start))

    given Compiler = Compiler.make(getClass.getClassLoader)
    val evidence = withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*
      val construction = HybridTypeFrontend.construct(using q)(Seq("", ""), Seq(TypeRepr.of[Map[Int, String]]))
      val pattern = HybridTypeFrontend.compile(Seq("", "")).toOption.get
      val detailed = HybridTypeFrontend.matchPattern(using q)(pattern, TypeRepr.of[Map[Int, String]])
      val extractor = HybridTypeFrontend.extract(using q)(pattern, TypeRepr.of[Map[Int, String]])
      (
        construction.swap.toOption.map(_.category),
        detailed.swap.toOption.map(_.category),
        extractor
      )

    assertEquals(
      evidence,
      (Some("TYPE_SPLICE_INSPECTION_FAILURE"), Some("TYPE_TARGET_INSPECTION_FAILURE"), None)
    )
