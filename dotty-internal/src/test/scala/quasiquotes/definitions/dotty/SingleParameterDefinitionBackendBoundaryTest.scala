package quasiquotes.definitions.dotty

import dotty.tools.dotc.core.Contexts.{Context, ContextBase}

import quasiquotes.definitions.{ConstructedDefinition, DefinitionName}
import quasiquotes.parser.{BinderId, TermShape}
import quasiquotes.terms.ConstructedTerm
import quasiquotes.types.TypeNormalForm

class SingleParameterDefinitionBackendBoundaryTest extends munit.FunSuite:
  test("exact untyped backend rejects the core-only single-parameter variant explicitly") {
    withContext {
      val result = ConstructedDefinitionUntypedBackend.lower(definition)

      assertEquals(
        result,
        Left(
          ConstructedDefinitionUntypedBackendError
            .UnsupportedConstructedDefinitionVariant("SingleParameterDef")
        )
      )
    }
  }

  test("generated-origin backend rejects the core-only single-parameter variant explicitly") {
    withContext {
      val result = ConstructedDefinitionGeneratedOriginAdapter
        .lower(definition, "<single-parameter-definition-boundary>")

      assertEquals(
        result,
        Left(
          ConstructedDefinitionGeneratedOriginError
            .UnsupportedConstructedDefinitionVariant("SingleParameterDef")
        )
      )
    }
  }

  private def definition: ConstructedDefinition =
    val binder = BinderId(0)
    val body = ConstructedTerm
      .fromShapeInScope(
        TermShape.BoundReference(binder, "x"),
        binder
      )
      .toOption
      .get
    ConstructedDefinition
      .singleParameterDef(
        DefinitionName.plain("id").toOption.get,
        binder,
        DefinitionName.plain("x").toOption.get,
        TypeNormalForm.STypeIdent("Int"),
        TypeNormalForm.STypeIdent("Int"),
        body
      )
      .toOption
      .get

  private def withContext(body: Context ?=> Unit): Unit =
    val base = new ContextBase
    given Context = base.initialCtx
    body
