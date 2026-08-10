package external.consumer

import scala.quoted.Quotes

import quasiquotes.types.ConstructedType
import quasiquotes.types.QuasiTypeConstruct
import quasiquotes.types.TypeNormalForm
import quasiquotes.types.TypeNormalFormSource
import quasiquotes.types.TypePatternSource
import quasiquotes.types.TypeTemplateSource
import quasiquotes.types.TypeQuasiquoteError
import quasiquotes.types.toTypeRepr

final class PublicApiExampleCompileTest extends munit.FunSuite:
  test("frontend source adapters are callable outside quasiquotes packages"):
    val intType = TypeNormalFormSource.fromSource("Int")
    val equalTypes = TypeNormalFormSource.equalSources("List[Int]", "List[Int]")
    val pattern = TypePatternSource.fromSource("List[$element]")
    val template = TypeTemplateSource.fromSource("List[$element]")

    assertEquals(intType, Right(TypeNormalForm.STypeIdent("Int")))
    assertEquals(equalTypes, Right(true))
    assert(pattern.isRight)
    assert(template.isRight)

  test("frontend construction reaches the compiler-free core through declared dependencies"):
    val constructed = QuasiTypeConstruct.fromTemplate(
      "List[$element]",
      "element" -> TypeNormalForm.STypeIdent("String")
    )

    assertEquals(constructed.map(_.source), Right("List[String]"))

  test("external frontend consumer parses matches and constructs nested Either types"):
    val normal = TypeNormalFormSource.fromSource(
      "Either[List[Int], Option[String]]"
    )
    val pattern = TypePatternSource.fromSource(
      "Either[List[$left], Option[$right]]"
    )
    val constructed = QuasiTypeConstruct.fromTemplate(
      "List[Either[$left, $right]]",
      "left" -> TypeNormalForm.STypeIdent("Int"),
      "right" -> TypeNormalForm.STypeIdent("String")
    )

    assertEquals(
      normal.map(_.render),
      Right(
        "STypeApply(STypeIdent(Either), [STypeApply(STypeIdent(List), [STypeIdent(Int)]), STypeApply(STypeIdent(Option), [STypeIdent(String)])])"
      )
    )
    assert(pattern.isRight)
    assertEquals(constructed.map(_.source), Right("List[Either[Int, String]]"))
    assert(quasiquotes.matching.QuasiPattern.term("foo($value)").isRight)
    assert(quasiquotes.matching.QuasiPattern.term("$value + $value").isRight)
    assertEquals(PublicUserSmokeMacros.add(2, 3), 5)
    assertEquals(PublicUserSmokeMacros.greeting("Ada"), "hello Ada")

  // Compiling this method proves that the Phase 56 lowering relocation needs
  // the explicit frontend extension import even though no macro is run here.
  private def lowerInsideMacro(
      constructed: ConstructedType
  )(using q: Quotes): Either[TypeQuasiquoteError, q.reflect.TypeRepr] =
    constructed.toTypeRepr
