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

  // Compiling this method proves that the Phase 56 lowering relocation needs
  // the explicit frontend extension import even though no macro is run here.
  private def lowerInsideMacro(
      constructed: ConstructedType
  )(using q: Quotes): Either[TypeQuasiquoteError, q.reflect.TypeRepr] =
    constructed.toTypeRepr
