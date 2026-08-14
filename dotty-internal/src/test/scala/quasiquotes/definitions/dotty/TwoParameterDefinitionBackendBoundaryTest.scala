package quasiquotes.definitions.dotty

import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.reporting.StoreReporter

import quasiquotes.definitions.{ConstructedDefinition, DefinitionName}
import quasiquotes.parser.{BinderId, TermShape}
import quasiquotes.terms.ConstructedTerm
import quasiquotes.types.TypeNormalForm

class TwoParameterDefinitionBackendBoundaryTest extends munit.FunSuite:
  test("source-free and generated-origin exact backends reject exact-two definitions deliberately") {
    withContext {
      val completed = definition()
      val sourceFree = ConstructedDefinitionUntypedBackend
        .lower(completed)
        .left
        .toOption
        .get
      val generated = ConstructedDefinitionGeneratedOriginAdapter
        .lower(completed, "<two-parameter-deferred>")
        .left
        .toOption
        .get

      assertEquals(
        sourceFree,
        ConstructedDefinitionUntypedBackendError.TwoParameterDefinitionExactBackendDeferred
      )
      assertEquals(
        generated,
        ConstructedDefinitionGeneratedOriginError.TwoParameterDefinitionExactBackendDeferred
      )
    }
  }

  private def definition(): ConstructedDefinition.TwoParameterDef =
    val binders = Vector(BinderId(1), BinderId(2))
    val body = ConstructedTerm
      .fromShapeInScope(
        TermShape.Tuple(
          List(
            TermShape.BoundReference(binders(0), "hostile-y"),
            TermShape.BoundReference(binders(1), "hostile-x")
          )
        ),
        binders
      )
      .toOption
      .get
    ConstructedDefinition
      .twoParameterDef(
        DefinitionName.plain("pair").toOption.get,
        binders(0),
        DefinitionName.plain("x").toOption.get,
        TypeNormalForm.STypeIdent("Int"),
        binders(1),
        DefinitionName.plain("y").toOption.get,
        TypeNormalForm.STypeIdent("String"),
        TypeNormalForm.STypeTuple(
          List(TypeNormalForm.STypeIdent("Int"), TypeNormalForm.STypeIdent("String"))
        ),
        body
      )
      .toOption
      .get

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    val reporter = new StoreReporter(null)
    given Context = base.initialCtx.fresh.setReporter(reporter)
    run
