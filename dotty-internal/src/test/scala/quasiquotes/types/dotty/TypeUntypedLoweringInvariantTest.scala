package quasiquotes.types.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Names.typeName
import dotty.tools.dotc.util.{NoSource, SourceFile}

import quasiquotes.types.TypeNormalForm

final class TypeUntypedLoweringInvariantTest extends munit.FunSuite:
  given SourceFile = NoSource

  private def validateRaw(
      semantic: TypeNormalForm,
      raw: untpd.Tree
  ): Either[TypeUntypedLowering.Failure, Unit] =
    val method = TypeUntypedLowering.getClass.getDeclaredMethods
      .find(_.getName == "validateRaw")
      .getOrElse(fail("private raw invariant validator was not found"))
    method.setAccessible(true)
    method
      .invoke(TypeUntypedLowering, semantic, raw, "type")
      .asInstanceOf[Either[TypeUntypedLowering.Failure, Unit]]

  test("raw postcondition rejects a missing raw result as an internal invariant failure"):
    val failure = validateRaw(TypeNormalForm.STypeIdent("Int"), null)
      .left
      .toOption
      .getOrElse(fail("missing raw result unexpectedly passed"))

    assertEquals(failure.code, "INTERNAL_INVARIANT_FAILED")
    assert(failure.detail.contains("raw node was null"), clues(failure))

  test("raw postcondition rejects wrong exact topology as an internal invariant failure"):
    val wrongName = untpd.Ident(typeName("String"))
    val wrongFamily = untpd.Tuple(
      List(untpd.Ident(typeName("Int")), untpd.Ident(typeName("String")))
    )

    List(wrongName, wrongFamily).foreach { raw =>
      val failure = validateRaw(TypeNormalForm.STypeIdent("Int"), raw)
        .left
        .toOption
        .getOrElse(fail(s"wrong raw topology unexpectedly passed: $raw"))
      assertEquals(failure.code, "INTERNAL_INVARIANT_FAILED", clues(raw, failure))
    }
