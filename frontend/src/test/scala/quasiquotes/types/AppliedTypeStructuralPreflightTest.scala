package quasiquotes.types

import quasiquotes.parser.TinyTypeParser

class AppliedTypeStructuralPreflightTest extends munit.FunSuite:
  private val snapshots = List(
    (
      "List[Option[Int]]",
      AppliedTypeStructuralPreflight.typedSnapshot[List[Option[Int]]]
    ),
    (
      "Option[List[String]]",
      AppliedTypeStructuralPreflight.typedSnapshot[Option[List[String]]]
    ),
    (
      "Either[Int, String]",
      AppliedTypeStructuralPreflight.typedSnapshot[Either[Int, String]]
    ),
    (
      "Either[List[Int], Option[String]]",
      AppliedTypeStructuralPreflight
        .typedSnapshot[Either[List[Int], Option[String]]]
    ),
    (
      "List[Either[Int, String]]",
      AppliedTypeStructuralPreflight.typedSnapshot[List[Either[Int, String]]]
    ),
    (
      "Either[Option[Int], List[Either[String, Boolean]]]",
      AppliedTypeStructuralPreflight
        .typedSnapshot[Either[Option[Int], List[Either[String, Boolean]]]]
    )
  )

  test("record raw parser, TypeShape, and typed TypeRepr applied-type structure"):
    snapshots.foreach { case (source, typed) =>
      val parsed = TinyTypeParser.parseOrThrow(source)
      println(
        s"APPLIED_TYPE_PREFLIGHT source=$source raw=${parsed.rawStructure} shape=${parsed.shape.render} typed=$typed"
      )
      assert(parsed.rawStructure.startsWith("AppliedTypeTree(Ident("))
      assert(parsed.shape.render.startsWith("TypeApply(TypeIdent("))
      assert(typed.startsWith("AppliedType(TypeRef("))
    }

  test("Either is a direct TypeRef with ordered binary arguments"):
    val typed = AppliedTypeStructuralPreflight
      .typedSnapshot[Either[List[Int], Option[String]]]
    assertEquals(
      typed,
      "AppliedType(TypeRef(Either), [AppliedType(TypeRef(List), [TypeRef(Int)]), AppliedType(TypeRef(Option), [TypeRef(String)])])"
    )
