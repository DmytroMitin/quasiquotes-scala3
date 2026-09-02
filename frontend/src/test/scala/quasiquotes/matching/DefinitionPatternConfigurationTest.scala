package quasiquotes.matching

import quasiquotes.definitions.DefinitionName
import quasiquotes.types.TypeNormalForm

final class DefinitionPatternConfigurationTest extends munit.FunSuite:
  test("structured factory creates the existing matcher without source parsing"):
    val pattern = DefinitionPattern.singleParameterStructured(
      DefinitionName.plain("selected").toOption.get,
      DefinitionName.plain("value").toOption.get,
      TypeNormalForm.STypeIdent("Int"),
      TypeNormalForm.STypeIdent("Int")
    )

    assert(pattern.isInstanceOf[SingleParameterDefinitionPattern])

  test("singleParameter accepts the exact bounded whole-body-hole grammar"):
    assert(
      DefinitionPattern
        .singleParameter("def selected(value: Int): String = $body")
        .isRight
    )

  test("singleParameter admits nested supported fixed types and flexible whitespace"):
    val sources = Vector(
      "def selected(value: List[Int]): Option[String] = $body",
      " def selected ( value : (Int, String) ) : Boolean = $body ",
      "def selected(value: Int => String): (Int, String) => Boolean = $body",
      "def selected(value: Either[List[Int], Option[String]]): String = $body"
    )

    sources.foreach(source =>
      assert(DefinitionPattern.singleParameter(source).isRight, source)
    )

  test("singleParameter rejects near misses recoverably"):
    val invalid = Vector(
      null,
      "not a definition",
      "def selected: Int = $body",
      "def selected(left: Int, right: Int): Int = $body",
      "def selected(value: Int = 1): Int = $body",
      "def selected(value: Int*): Int = $body",
      "def selected(using value: Int): Int = $body",
      "def selected[A](value: Int): Int = $body",
      "def selected(value: Int): Int = value",
      "def selected(value: Int): Int = $body + 1",
      "def selected(value: $input): Int = $body",
      "def selected(value: Int): $output = $body",
      "def $method(value: Int): Int = $body",
      "val selected: Int = $body",
      "def `selected`(value: Int): Int = $body"
    )

    invalid.foreach(source => assert(DefinitionPattern.singleParameter(source).isLeft))
