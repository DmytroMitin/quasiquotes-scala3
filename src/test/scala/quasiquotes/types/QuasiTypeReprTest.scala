package quasiquotes.types

class QuasiTypeReprTest extends munit.FunSuite:
  test("lowers the supported simple TypeRepr subset") {
    val supported = QuasiTypeExamples.supportedConstructionSummary

    assert(supported.exists(_.startsWith("Int -> ")))
    assert(supported.exists(_.startsWith("String -> ")))
    assert(supported.exists(_.startsWith("Boolean -> ")))
    assert(supported.exists(_.startsWith("List[Int] -> ")))
    assert(supported.exists(_.startsWith("Option[String] -> ")))
    assert(supported.exists(_.startsWith("(Int, String) -> ")))
    assert(supported.exists(_.startsWith("Int => String -> ")))
  }

  test("matches exact supported TypeRepr shapes") {
    assert(QuasiTypeExamples.matches("Int", "Int"))
    assert(QuasiTypeExamples.matches("String", "String"))
    assert(QuasiTypeExamples.matches("Boolean", "Boolean"))
    assert(QuasiTypeExamples.matches("List[Int]", "List[Int]"))
    assert(QuasiTypeExamples.matches("Option[String]", "Option[String]"))
    assert(QuasiTypeExamples.matches("(Int, String)", "(Int, String)"))
    assert(QuasiTypeExamples.matches("Int => String", "Int => String"))
  }

  test("rejects non-equal supported TypeRepr shapes") {
    assert(!QuasiTypeExamples.matches("Int", "String"))
    assert(!QuasiTypeExamples.matches("Int", "AnyVal"))
    assert(!QuasiTypeExamples.matches("List[Int]", "List[String]"))
    assert(!QuasiTypeExamples.matches("List[Int]", "Option[Int]"))
    assert(!QuasiTypeExamples.matches("(Int, String)", "(String, Int)"))
    assert(!QuasiTypeExamples.matches("Int => String", "String => Int"))
  }

  test("unsupported type syntax fails clearly") {
    assert(QuasiTypeExamples.unsupportedMessage("List[?]").contains("Unsupported type shape"))
    assert(QuasiTypeExamples.unsupportedMessage("{ type A = Int }").contains("Unsupported type shape"))
  }

  test("scala.Int remains an unresolved selected-alias boundary") {
    val message = QuasiTypeExamples.unsupportedMessage("scala.Int")

    assert(message.contains("Selected type syntax is not supported"))
    assert(!QuasiTypeExamples.matches("Int", "scala.Int"))
  }
