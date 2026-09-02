package quasiquotes.neutral

import _root_.quasiquotes.parser.{BinderId, TermShape}

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaConstructorNewProjectionTest extends munit.FunSuite:
  test("Scalameta exposes the exact selected constructor-new fields"):
    val fixtures = List(
      ("new java.lang.StringBuilder()", List("java", "lang", "StringBuilder"), 0),
      ("new java.lang.StringBuilder(16)", List("java", "lang", "StringBuilder"), 1),
      ("new synthetic.unresolved.Widget(1, x)", List("synthetic", "unresolved", "Widget"), 2)
    )

    fixtures.foreach { (source, expectedSegments, expectedArguments) =>
      parsed(source) match
        case fresh: Term.New =>
          assertEquals(fresh.init.name.value, "")
          assertEquals(constructorSegments(fresh.init.tpe), expectedSegments)
          fresh.init.argClauses.toList match
            case clause :: Nil =>
              assertEquals(clause.mod, None)
              assertEquals(clause.values.size, expectedArguments)
            case other => fail(s"expected one argument clause for $source, got $other")
        case other => fail(s"expected Term.New for $source, got ${other.productPrefix}")
    }

  test("Scalameta distinguishes nearby rejected constructor topologies"):
    val typeApplied = parsed("new java.lang.StringBuilder[Int](16)")
    typeApplied match
      case fresh: Term.New => assert(fresh.init.tpe.isInstanceOf[Type.Apply])
      case other => fail(s"expected Term.New type application, got ${other.productPrefix}")

    parsed("new java.lang.StringBuilder(16)(17)") match
      case fresh: Term.New => assertEquals(fresh.init.argClauses.size, 2)
      case other => fail(s"expected Term.New with two clauses, got ${other.productPrefix}")

    parsed("new java.lang.StringBuilder(capacity = 16)") match
      case fresh: Term.New =>
        assert(fresh.init.argClauses.head.values.head.isInstanceOf[Term.Assign])
      case other => fail(s"expected Term.New with named argument, got ${other.productPrefix}")

    parsed("new java.lang.StringBuilder(values*)") match
      case fresh: Term.New =>
        assert(fresh.init.argClauses.head.values.head.isInstanceOf[Term.Repeated])
      case other => fail(s"expected Term.New with repeated argument, got ${other.productPrefix}")

    assert(parsed("new java.lang.StringBuilder(16) { }").isInstanceOf[Term.NewAnonymous])

    parsed("new java.lang.`StringBuilder`()") match
      case fresh: Term.New =>
        val last = constructorTypeNames(fresh.init.tpe).last
        assertEquals(last.value, "StringBuilder")
        assertEquals(last.tokens.map(_.text).toList, List("`StringBuilder`"))
      case other => fail(s"expected Term.New with backticked segment, got ${other.productPrefix}")

  test("projects empty, literal, ordered, and unresolved constructors"):
    val fixtures = List(
      "new java.lang.StringBuilder()" ->
        TermShape.New("java.lang.StringBuilder", Nil),
      "new java.lang.StringBuilder(16)" ->
        TermShape.New(
          "java.lang.StringBuilder",
          List(TermShape.Literal("16"))
        ),
      "new synthetic.unresolved.Widget(1, x)" ->
        TermShape.New(
          "synthetic.unresolved.Widget",
          List(
            TermShape.Literal("1"),
            TermShape.Identifier("x", isPlaceholder = false)
          )
        )
    )

    fixtures.foreach { (source, expected) =>
      assertEquals(project(parsed(source)).shape, expected, clues(source))
    }

  test("recursively projects nested and composite constructor arguments"):
    val source =
      "new synthetic.unresolved.Widget(new other.missing.Value(1), if cond then foo(x) else 0)"
    assertEquals(
      project(parsed(source)).shape,
      TermShape.New(
        "synthetic.unresolved.Widget",
        List(
          TermShape.New(
            "other.missing.Value",
            List(TermShape.Literal("1"))
          ),
          TermShape.If(
            TermShape.Identifier("cond", isPlaceholder = false),
            TermShape.Apply(
              TermShape.Identifier("foo", isPlaceholder = false),
              List(TermShape.Identifier("x", isPlaceholder = false))
            ),
            TermShape.Literal("0")
          )
        )
      )
    )

  test("preserves an outer binder reference without allocating a constructor binder"):
    project(parsed("(x: Int) => new synthetic.unresolved.Widget(x)")).shape match
      case TermShape.Lambda1(
            lambdaId,
            "x",
            "Int",
            TermShape.New(
              "synthetic.unresolved.Widget",
              List(TermShape.BoundReference(argumentId, "x"))
            )
          ) =>
        assertEquals(lambdaId, BinderId(0))
        assertEquals(argumentId, lambdaId)
      case other => fail(s"unexpected Lambda/New projection: ${other.render}")

  test("preserves positioned and unpositioned constructor root spans"):
    val source = "new java.lang.StringBuilder(16)"
    val positioned = parsed(source)
    assertEquals(
      project(positioned).sourceSpan,
      Some(NeutralSourceSpan(0, source.length))
    )

    val unpositioned = positioned match
      case fresh: Term.New => fresh.copy()
      case other => fail(s"expected Term.New, got ${other.productPrefix}")
    assertEquals(unpositioned.pos, Position.None)
    assertEquals(project(unpositioned).sourceSpan, None)

  test("rejects malformed constructor names, Types, and argument lists precisely"):
    val cases = List(
      "new StringBuilder()" -> "NEUTRAL_NEW_CONSTRUCTOR_NAME_UNSUPPORTED",
      "new java.lang.`StringBuilder`()" -> "NEUTRAL_NEW_CONSTRUCTOR_NAME_UNSUPPORTED",
      "new java.lang.Outer$Inner()" -> "NEUTRAL_NEW_CONSTRUCTOR_NAME_UNSUPPORTED",
      "new __qq_ctor_type_hole__(16)" -> "NEUTRAL_NEW_CONSTRUCTOR_NAME_UNSUPPORTED",
      "new java.lang.StringBuilder[Int](16)" -> "NEUTRAL_NEW_CONSTRUCTOR_TYPE_UNSUPPORTED",
      "new java.lang.StringBuilder" -> "NEUTRAL_NEW_ARGUMENT_LIST_UNSUPPORTED",
      "new java.lang.StringBuilder(16)(17)" -> "NEUTRAL_NEW_ARGUMENT_LIST_UNSUPPORTED",
      "new java.lang.StringBuilder(capacity = 16)" -> "NEUTRAL_NEW_ARGUMENT_UNSUPPORTED",
      "new java.lang.StringBuilder(values*)" -> "NEUTRAL_NEW_ARGUMENT_UNSUPPORTED",
      "new java.lang.StringBuilder(16) { }" -> "NEUTRAL_NEW_ANONYMOUS_UNSUPPORTED"
    )

    cases.foreach { (source, expectedCode) =>
      assertErrorCode(parsed(source), expectedCode)
    }

  test("retains recursive child failures and the existing null-root failure"):
    assertErrorCode(
      parsed("new synthetic.unresolved.Widget(value match { case _ => 1 })"),
      "NEUTRAL_TERM_UNSUPPORTED"
    )
    assertEquals(
      ScalametaTermProjection.project(null),
      Left(
        NeutralProjectionError(
          "NEUTRAL_TERM_MISSING",
          "the Scalameta term must be present."
        )
      )
    )

  private def parsed(source: String): Term =
    Input.String(source).parse[Term].get

  private def project(source: Term): ProjectedTermShape =
    ScalametaTermProjection.project(source) match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def assertErrorCode(source: Term, expected: String): Unit =
    assertEquals(
      ScalametaTermProjection.project(source).left.toOption.map(_.code),
      Some(expected),
      clues(source.structure)
    )

  private def constructorSegments(tpe: Type): List[String] =
    constructorTypeNames(tpe).map(_.value)

  private def constructorTypeNames(tpe: Type): List[Type.Name] =
    tpe match
      case name: Type.Name => name :: Nil
      case select: Type.Select =>
        constructorQualifierNames(select.qual) :+ select.name
      case other => fail(s"expected constructor Type.Name/Type.Select, got ${other.productPrefix}")

  private def constructorQualifierNames(term: Term): List[Type.Name] =
    term match
      case name: Term.Name => Type.Name(name.value) :: Nil
      case select: Term.Select =>
        constructorQualifierNames(select.qual) :+ Type.Name(select.name.value)
      case other => fail(s"expected constructor qualifier path, got ${other.productPrefix}")
