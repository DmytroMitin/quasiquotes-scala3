package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags.{EmptyFlags, Method, Param}
import dotty.tools.dotc.core.Names.{termName, typeName}
import dotty.tools.dotc.core.Symbols.{NoSymbol, newSymbol}
import dotty.tools.dotc.core.Types.NoType
import dotty.tools.dotc.util.{NoSource, SourceFile}
import dotty.tools.dotc.util.Spans.Span

import quasiquotes.definitions.*
import quasiquotes.definitions.dotty.DefinitionShapeUntypedLowererError.*
import quasiquotes.parser.{BinderId, TermShape, TypeShape}
import quasiquotes.types.{
  ResolvedTypeNameId,
  ResolvedTypeOwnerKind,
  ResolvedTypeOwnerSegment,
  TypeNormalForm
}

final class DefinitionUntypedLoweringInternalTest extends munit.FunSuite:
  private val intType = TypeNormalForm.STypeIdent("Int")

  test("adapter-stage classifications remain distinct through the public facade"):
    withContext:
      val malformed = new SemanticDefinition(
        DefinitionKind.Value,
        name("broken"),
        DefinitionModifiers.empty,
        storage("ValueStorage", TypeNormalForm.STypeTuple(null), TermShape.Literal("0"))
      )
      assertEquals(lowerFailure(malformed).code, "MALFORMED_SEMANTIC_VALUE")

      val resolved = TypeNormalForm.STypeResolved(
        ResolvedTypeNameId(
          Vector(ResolvedTypeOwnerSegment(ResolvedTypeOwnerKind.Package, "scala")),
          "Int"
        )
      )
      val unsupported = definition(
        SemanticDefinition.typeAlias(name("Resolved"), resolved)
      )
      assertEquals(lowerFailure(unsupported).code, "UNSUPPORTED_SEMANTIC_VALUE")

      val first = identityMethod("first")
      val second = identityMethod("second")
      val firstView = first.asMethod.get
      val secondView = second.asMethod.get
      val firstReference = firstView.body.get.asInstanceOf[TermShape.BoundReference]
      val secondReference = secondView.body.get.asInstanceOf[TermShape.BoundReference]
      assertEquals(firstReference.binderId, secondReference.binderId)

      val crossGraph = new SemanticDefinition(
        DefinitionKind.Method,
        name("identity"),
        DefinitionModifiers.empty,
        storage(
          "MethodStorage",
          secondView.parameterClauses,
          secondView.parameterScope,
          secondView.resultType,
          firstView.body.get,
          firstView.body.get
        )
      )
      assertEquals(lowerFailure(crossGraph).code, "SEMANTIC_ADAPTER_FAILED")

  test("public facade rejects every required unsupported semantic boundary"):
    withContext:
      val resolved = TypeNormalForm.STypeResolved(
        ResolvedTypeNameId(
          Vector(ResolvedTypeOwnerSegment(ResolvedTypeOwnerKind.Package, "scala")),
          "Int"
        )
      )
      val resolvedCandidates = List(
        definition(
          SemanticDefinition.immutableValue(
            name("resolvedValue"),
            resolved,
            TermShape.Literal("0")
          )
        ),
        definition(
          SemanticDefinition.concreteMethod(name("resolvedResult"), Vector.empty, resolved)(
            _ => Right(TermShape.Literal("0"))
          )
        ),
        definition(
          SemanticDefinition.concreteMethod(
            name("resolvedParameter"),
            Vector(clause(parameter("x", resolved))),
            intType
          )(_ => Right(TermShape.Literal("0")))
        ),
        definition(SemanticDefinition.typeAlias(name("ResolvedAlias"), resolved))
      )
      resolvedCandidates.foreach { candidate =>
        assertEquals(lowerFailure(candidate).code, "UNSUPPORTED_SEMANTIC_VALUE")
      }

      val wrongView = new SemanticDefinition(
        DefinitionKind.Method,
        name("wrongView"),
        DefinitionModifiers.empty,
        storage("ValueStorage", intType, TermShape.Literal("0"))
      )
      assertEquals(lowerFailure(wrongView).code, "MALFORMED_SEMANTIC_VALUE")

      val sourceMethod = identityMethod("x")
      val sourceView = sourceMethod.asMethod.get
      val threeParameters = clause(
        parameter("a", intType),
        parameter("b", intType),
        parameter("c", intType)
      )
      val wideMethod = new SemanticDefinition(
        DefinitionKind.Method,
        name("wide"),
        DefinitionModifiers.empty,
        storage(
          "MethodStorage",
          Vector(threeParameters),
          sourceView.parameterScope,
          intType,
          TermShape.Literal("0"),
          TermShape.Literal("0")
        )
      )
      assertEquals(lowerFailure(wideMethod).code, "UNSUPPORTED_SEMANTIC_VALUE")

      val unbound = TermShape.BoundReference(BinderId(999), "x")
      val unboundMethod = forgedMethod(sourceView, unbound)
      assertEquals(lowerFailure(unboundMethod).code, "SEMANTIC_ADAPTER_FAILED")

      val colliding = TermShape.Lambda1(
        sourceView.body.get.asInstanceOf[TermShape.BoundReference].binderId,
        "nested",
        "Int",
        TermShape.Literal("0")
      )
      val collidingMethod = forgedMethod(sourceView, colliding)
      assertEquals(lowerFailure(collidingMethod).code, "SEMANTIC_ADAPTER_FAILED")

      val unsupportedBody = new SemanticDefinition(
        DefinitionKind.Value,
        name("unsupportedBody"),
        DefinitionModifiers.empty,
        storage(
          "ValueStorage",
          intType,
          TermShape.Unsupported("future", "not admitted")
        )
      )
      assertEquals(lowerFailure(unsupportedBody).code, "UNSUPPORTED_SEMANTIC_VALUE")

  test("adapter codes map exactly and unexpected private codes are internal failures"):
    val admitted = List(
      "MISSING_INPUT",
      "MALFORMED_SEMANTIC_VALUE",
      "UNSUPPORTED_SEMANTIC_VALUE",
      "SEMANTIC_ADAPTER_FAILED"
    )
    admitted.foreach { code =>
      assertEquals(mapAdapter(SemanticDefinitionShapeAdapter.Error(code, "detail")).code, code)
    }

    assertEquals(
      mapAdapter(SemanticDefinitionShapeAdapter.Error("FUTURE_ADAPTER_CODE", "detail")).code,
      "INTERNAL_INVARIANT_FAILED"
    )
    assertEquals(mapAdapter(null).code, "INTERNAL_INVARIANT_FAILED")

  test("adapter-success AnyVal values fail only at the exact stage"):
    withContext:
      val anyVal = TypeNormalForm.STypeIdent("AnyVal")
      val candidate = new SemanticDefinition(
        DefinitionKind.Value,
        name("boxed"),
        DefinitionModifiers.empty,
        storage("ValueStorage", anyVal, TermShape.Literal("1"))
      )

      assert(SemanticDefinitionShapeAdapter.adapt(candidate).isRight)
      assertEquals(lowerFailure(candidate).code, "EXACT_LOWERING_FAILED")

  test("typed private lowerer failures map exact-stage versus raw-invariant codes"):
    withContext:
      val shape = immutableShape
      val ordinary = OrdinaryDefinitionCompletionFailure(
        DefinitionConstructionError.UnsupportedParsedDefinitionBody("test failure")
      )

      assertEquals(finish(shape, Left(ordinary)).left.toOption.get.code, "EXACT_LOWERING_FAILED")
      assertEquals(
        finish(shape, Left(RawInvariantFailure("test", "raw contradiction"))).left.toOption.get.code,
        "INTERNAL_INVARIANT_FAILED"
      )
      assertEquals(
        finish(shape, Left(null)).left.toOption.get.code,
        "INTERNAL_INVARIANT_FAILED"
      )
      assertEquals(finish(shape, null).left.toOption.get.code, "INTERNAL_INVARIANT_FAILED")

  test("private raw seam rejects null wrong-category and contradictory family topology"):
    withContext:
      given SourceFile = NoSource
      val shape = immutableShape

      assertInternal(finish(shape, Right(null)))
      assertInternal(finish(shape, Right(untpd.Ident(termName("notMember")))))
      assertInternal(
        finish(
          shape,
          Right(untpd.TypeDef(typeName("answer"), untpd.Ident(typeName("Int"))))
        )
      )

      val wrongParameterCount = untpd.DefDef(
        termName("method"),
        Nil,
        untpd.Ident(typeName("Int")),
        untpd.Number("1", untpd.NumberKind.Whole(10))
      )
      assertInternal(finish(singleParameterShape, Right(wrongParameterCount)))

  test("private raw seam rejects source span symbol and TypedSplice contamination"):
    withContext:
      given SourceFile = NoSource
      val shape = immutableShape

      val sourced = untpd
        .Ident(typeName("Int"))
        .cloneIn(SourceFile.virtual("C033Sourced.scala", "Int"))
      assertInternal(
        finish(
          shape,
          Right(untpd.ValDef(termName("answer"), sourced, untpd.Number("1", untpd.NumberKind.Whole(10))))
        )
      )

      val spanned = untpd.Ident(typeName("Int")).withSpan(Span(0, 1, 0))
      assertInternal(
        finish(
          shape,
          Right(untpd.ValDef(termName("answer"), spanned, untpd.Number("1", untpd.NumberKind.Whole(10))))
        )
      )

      val symbol = newSymbol(NoSymbol, termName("c033Symbol"), EmptyFlags, NoType)
      val symbolBearing = untpd.Ident(termName("symbolBearing")).withType(symbol.termRef)
      assertInternal(
        finish(
          shape,
          Right(untpd.ValDef(termName("answer"), untpd.Ident(typeName("Int")), symbolBearing))
        )
      )
      assertInternal(
        finish(
          shape,
          Right(
            untpd.ValDef(
              termName("answer"),
              untpd.Ident(typeName("Int")),
              untpd.TypedSplice(symbolBearing)
            )
          )
        )
      )

  test("private raw seam rejects annotations and private qualifiers everywhere"):
    withContext:
      given SourceFile = NoSource
      val annotation = untpd.Ident(typeName("Annotation"))
      val ordinaryValue = untpd.ValDef(
        termName("answer"),
        untpd.Ident(typeName("Int")),
        untpd.Number("1", untpd.NumberKind.Whole(10))
      )
      assertInternal(
        finish(
          immutableShape,
          Right(ordinaryValue.withMods(ordinaryValue.mods.withAddedAnnotation(annotation)))
        )
      )
      assertInternal(
        finish(
          immutableShape,
          Right(ordinaryValue.withMods(ordinaryValue.mods.withPrivateWithin(typeName("scope"))))
        )
      )

      val annotatedParameter = untpd
        .ValDef(termName("x"), untpd.Ident(typeName("Int")), untpd.EmptyTree)
        .withMods(untpd.Modifiers(Param).withAddedAnnotation(annotation))
      val method = untpd
        .DefDef(
          termName("method"),
          List(List(annotatedParameter)),
          untpd.Ident(typeName("Int")),
          untpd.Ident(termName("x"))
        )
        .withMods(untpd.Modifiers(Method))
      assertInternal(finish(singleParameterShape, Right(method)))

  private def immutableShape: DefinitionShape.ImmutableVal =
    DefinitionShape
      .immutableVal(name("answer"), TypeShape.Identifier("Int"), TermShape.Literal("1"))
      .fold(problem => fail(problem.message), identity)

  private def singleParameterShape: DefinitionShape.SingleParameterDef =
    DefinitionShape
      .singleParameterDef(
        name("method"),
        BinderId(0),
        name("x"),
        TypeShape.Identifier("Int"),
        TypeShape.Identifier("Int"),
        TermShape.BoundReference(BinderId(0), "x")
      )
      .fold(problem => fail(problem.message), identity)

  private def identityMethod(parameterName: String): SemanticDefinition =
    definition(
      SemanticDefinition.concreteMethod(
        name("identity"),
        Vector(clause(parameter(parameterName, intType))),
        intType
      )(_.reference(0, 0))
    )

  private def forgedMethod(
      sourceView: MethodDefinitionView,
      body: TermShape
  ): SemanticDefinition =
    new SemanticDefinition(
      DefinitionKind.Method,
      name("identity"),
      DefinitionModifiers.empty,
      storage(
        "MethodStorage",
        sourceView.parameterClauses,
        sourceView.parameterScope,
        sourceView.resultType,
        body,
        body
      )
    )

  private def parameter(
      source: String,
      declaredType: TypeNormalForm
  ): DefinitionParameter = DefinitionParameter(name(source), declaredType)

  private def clause(
      parameters: DefinitionParameter*
  ): DefinitionParameterClause =
    DefinitionParameterClause
      .ordinary(parameters.toVector)
      .fold(problem => fail(problem.message), identity)

  private def definition(
      value: Either[DefinitionSemanticError, SemanticDefinition]
  ): SemanticDefinition = value.fold(problem => fail(problem.message), identity)

  private def name(source: String): DefinitionName =
    DefinitionName.fromSource(source).fold(problem => fail(problem.message), identity)

  private def lowerFailure(
      value: SemanticDefinition
  )(using Context): DefinitionUntypedLowering.Failure =
    DefinitionUntypedLowering
      .lower(value)
      .left
      .toOption
      .getOrElse(fail(s"semantic Definition unexpectedly lowered: $value"))

  private def mapAdapter(
      problem: SemanticDefinitionShapeAdapter.Error
  ): DefinitionUntypedLowering.Failure =
    val method = DefinitionUntypedLowering.getClass.getDeclaredMethods
      .find(candidate => candidate.getName == "classifyAdapterFailure")
      .getOrElse(fail("private adapter failure classifier was not found"))
    method.setAccessible(true)
    method
      .invoke(DefinitionUntypedLowering, problem)
      .asInstanceOf[DefinitionUntypedLowering.Failure]

  private def finish(
      shape: DefinitionShape,
      lowered: Either[DefinitionShapeUntypedLowererError, untpd.Tree]
  )(using Context): Either[DefinitionUntypedLowering.Failure, untpd.MemberDef] =
    val method = DefinitionUntypedLowering.getClass.getDeclaredMethods
      .find(candidate =>
        candidate.getName == "finishLowering" && candidate.getParameterCount == 3
      )
      .getOrElse(fail("private Definition finishing seam was not found"))
    method.setAccessible(true)
    method
      .invoke(DefinitionUntypedLowering, shape, lowered, summon[Context])
      .asInstanceOf[Either[DefinitionUntypedLowering.Failure, untpd.MemberDef]]

  private def assertInternal(
      result: Either[DefinitionUntypedLowering.Failure, untpd.MemberDef]
  ): Unit =
    assertEquals(result.left.toOption.map(_.code), Some("INTERNAL_INVARIANT_FAILED"))

  private def storage(suffix: String, arguments: AnyRef*): AnyRef =
    val storageClass = Class.forName(
      s"quasiquotes.definitions.SemanticDefinition$$$suffix"
    )
    val constructor = storageClass.getDeclaredConstructors.head
    constructor.setAccessible(true)
    constructor.newInstance(arguments*).asInstanceOf[AnyRef]

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
