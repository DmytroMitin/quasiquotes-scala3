package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.{termName, typeName}
import dotty.tools.dotc.core.Symbols.{NoSymbol, newSymbol}
import dotty.tools.dotc.core.Types.NoType
import dotty.tools.dotc.util.{NoSource, SourceFile}
import dotty.tools.dotc.util.Spans.Span

import quasiquotes.definitions.*
import quasiquotes.parser.{BinderId, TermShape}
import quasiquotes.terms.dotty.{CompletedTypeUntypedLoweringError, ConstructedTermGeneratedOriginError}
import quasiquotes.types.{ResolvedTypeNameId, ResolvedTypeOwnerKind, ResolvedTypeOwnerSegment, TypeNormalForm, TypeQuasiquoteError}

final class DefinitionGeneratedOriginLoweringTest extends munit.FunSuite:
  private val intType = TypeNormalForm.STypeIdent("Int")
  private val stringType = TypeNormalForm.STypeIdent("String")
  private val listInt = TypeNormalForm.STypeApply(TypeNormalForm.STypeIdent("List"), List(intType))
  private val sourceName = "SemanticGenerated.scala"

  test("canonical and renamed G5 families have exact source and source-free topology"):
    withContext:
      fixtures.foreach { case (semantic, expectedSource, expectedClass) =>
        val result = lower(semantic)
        assertEquals(result.generatedSource, expectedSource)
        assertEquals(result.tree.getClass, expectedClass)
        assertEquals(result.tree.name.toString, semantic.name.decoded)
        assertComplete(result)
        val raw = DefinitionUntypedLowering.lower(semantic).fold(p => fail(p.message), identity)
        assertEquals(snapshot(result.tree), snapshot(raw))
        assertEquals(result.tree.span.start, 0)
        assertEquals(result.tree.span.end, expectedSource.length)
      }

  test("each G5 call keeps bytes spans and topology deterministic but all carriers fresh"):
    withContext:
      fixtures.foreach { case (semantic, _, _) =>
        val first = lower(semantic)
        val second = lower(semantic)
        assert(!(first eq second))
        assert(!(first.sourceFile eq second.sourceFile))
        assertEquals(first.generatedSource, second.generatedSource)
        assertEquals(first.virtualSourceName, second.virtualSourceName)
        assertEquals(snapshot(first.tree), snapshot(second.tree))
        val firstTrees = allTrees(first.tree)
        val secondTrees = allTrees(second.tree)
        assertEquals(firstTrees.size, secondTrees.size)
        firstTrees.zip(secondTrees).foreach { case (left, right) =>
          assert(!(left eq right), clues(left, right))
          assertEquals(left.span, right.span)
        }
        assertComplete(first)
        assertComplete(second)
      }

  test("parameter references use semantic slots even when spellings suggest opposite order"):
    withContext:
      val first = method("firstSlot", Vector("second", "first"), intType)(_.reference(0, 0))
      val second = method("secondSlot", Vector("second", "first"), intType)(_.reference(0, 1))
      val swapped = method("swapped", Vector("second", "first"), TypeNormalForm.STypeTuple(List(intType, intType))) { scope =>
        for
          left <- scope.reference(0, 0)
          right <- scope.reference(0, 1)
        yield TermShape.Tuple(List(right, left))
      }
      assertEquals(lower(first).generatedSource, "def firstSlot(second: Int, first: Int): Int = second")
      assertEquals(lower(second).generatedSource, "def secondSlot(second: Int, first: Int): Int = first")
      assertEquals(lower(swapped).generatedSource, "def swapped(second: Int, first: Int): (Int, Int) = (first, second)")
      List(first, second, swapped).foreach { semantic =>
        assertEquals(snapshot(lower(semantic).tree), snapshot(DefinitionUntypedLowering.lower(semantic).toOption.get))
      }

  test("nested alias RHS is recursively positioned and remains a TypeDef"):
    withContext:
      val semantic = right(SemanticDefinition.typeAlias(name("Nested"),
        TypeNormalForm.STypeApply(TypeNormalForm.STypeIdent("Option"), List(
          TypeNormalForm.STypeTuple(List(intType, TypeNormalForm.STypeFunction(List(stringType), intType)))
        ))))
      val result = lower(semantic)
      assert(result.tree.isInstanceOf[untpd.TypeDef])
      assert(allTrees(result.tree).size > 5)
      assertComplete(result)
      assertEquals(snapshot(result.tree), snapshot(DefinitionUntypedLowering.lower(semantic).toOption.get))

  test("missing inputs and invalid virtual sources precede semantic failures"):
    withContext:
      val malformed = forgedValue(null, TermShape.Literal("0"))
      assertEquals(failure(null, null).code, "MISSING_INPUT")
      assert(failure(null, null).detail.contains("Definition"))
      assertEquals(failure(malformed, null).code, "MISSING_INPUT")
      assert(failure(malformed, null).detail.contains("source"))
      val invalidNames = List("", " ", " Leading.scala", "Trailing.scala ", "Nul\u0000.scala", "Cr\r.scala", "Lf\n.scala")
      invalidNames.foreach { invalid =>
        assertEquals(failure(malformed, invalid).code, "INVALID_VIRTUAL_SOURCE", clues(invalid))
        assertEquals(failure(fixtures.head._1, invalid).code, "INVALID_VIRTUAL_SOURCE", clues(invalid))
      }
      assertEquals(failure(malformed).code, "MALFORMED_SEMANTIC_VALUE")
      val accepted = lower(fixtures.head._1, "<semantic-generated>")
      assertEquals(accepted.virtualSourceName, "<semantic-generated>")
      List("dir//Normalized.scala", "./Relative.scala", "dir/../Parent.scala").foreach { requested =>
        val effective = SourceFile.virtual(requested, "").path
        if effective == requested then
          assertEquals(lower(fixtures.head._1, requested).virtualSourceName, requested)
        else assertEquals(failure(fixtures.head._1, requested).code, "INVALID_VIRTUAL_SOURCE")
      }

  test("malformed unsupported adapter and exact-stage semantic values remain distinct"):
    withContext:
      val malformed = List(
        forgedValue(null, TermShape.Literal("0")),
        forgedValue(TypeNormalForm.STypeTuple(null), TermShape.Literal("0")),
        forgedValue(intType, null),
        new SemanticDefinition(DefinitionKind.Method, name("wrongView"), DefinitionModifiers.empty,
          storage("ValueStorage", intType, TermShape.Literal("0")))
      )
      malformed.foreach(value => assertEquals(failure(value).code, "MALFORMED_SEMANTIC_VALUE"))
      val resolved = TypeNormalForm.STypeResolved(ResolvedTypeNameId(
        Vector(ResolvedTypeOwnerSegment(ResolvedTypeOwnerKind.Package, "scala")), "Int"))
      assertEquals(failure(right(SemanticDefinition.typeAlias(name("Resolved"), resolved))).code, "UNSUPPORTED_SEMANTIC_VALUE")
      assertEquals(failure(forgedValue(intType, TermShape.Unsupported("future", "outside grammar"))).code, "UNSUPPORTED_SEMANTIC_VALUE")
      val source = method("identity", Vector("x"), intType)(_.reference(0, 0))
      val view = source.asMethod.get
      val unbound = TermShape.BoundReference(BinderId(999), "x")
      val invalidBinding = new SemanticDefinition(DefinitionKind.Method, name("identity"), DefinitionModifiers.empty,
        storage("MethodStorage", view.parameterClauses, view.parameterScope, intType, unbound, unbound))
      assertEquals(failure(invalidBinding).code, "SEMANTIC_ADAPTER_FAILED")
      val anyVal = forgedValue(TypeNormalForm.STypeIdent("AnyVal"), TermShape.Literal("1"))
      assert(SemanticDefinitionShapeAdapter.adapt(anyVal).isRight)
      assertEquals(failure(anyVal).code, "EXACT_LOWERING_FAILED")

  test("cross-graph method bodies are rejected even when private numeric binder IDs coincide"):
    withContext:
      val first = method("first", Vector("x"), intType)(_.reference(0, 0)).asMethod.get
      val second = method("second", Vector("x"), intType)(_.reference(0, 0)).asMethod.get
      assertEquals(first.body.get.asInstanceOf[TermShape.BoundReference].binderId,
        second.body.get.asInstanceOf[TermShape.BoundReference].binderId)
      val crossed = new SemanticDefinition(DefinitionKind.Method, name("crossed"), DefinitionModifiers.empty,
        storage("MethodStorage", second.parameterClauses, second.parameterScope, intType, first.body.get, first.body.get))
      assertEquals(failure(crossed).code, "SEMANTIC_ADAPTER_FAILED")

  test("only reviewed semantic adapter codes cross the public failure boundary"):
    List("MISSING_INPUT", "MALFORMED_SEMANTIC_VALUE", "UNSUPPORTED_SEMANTIC_VALUE", "SEMANTIC_ADAPTER_FAILED").foreach { code =>
      val mapped = classify("classifyAdapterFailure", SemanticDefinitionShapeAdapter.Error(code, "diagnostic"))
      assertEquals(mapped.code, code)
      assertEquals(mapped.message, s"$code: diagnostic")
    }
    List(SemanticDefinitionShapeAdapter.Error("NEW_PRIVATE_CODE", "detail"),
      SemanticDefinitionShapeAdapter.Error(null, "detail"), null).foreach { problem =>
      assertEquals(classify("classifyAdapterFailure", problem).code, "INTERNAL_INVARIANT_FAILED")
    }

  test("constructed backend failures map by stage without exposing private cases as codes"):
    import ConstructedDefinitionGeneratedOriginError.*
    val cases = List(
      InvalidVirtualSourceName("invalid") -> "INVALID_VIRTUAL_SOURCE",
      RawDefinitionLoweringFailure("raw") -> "EXACT_LOWERING_FAILED",
      UnsupportedConstructedDefinitionVariant("future") -> "INTERNAL_INVARIANT_FAILED",
      DefinitionNameRenderingFailure("name") -> "GENERATED_ORIGIN_FAILED",
      DefinitionTypePlanningFailure("type") -> "GENERATED_ORIGIN_FAILED",
      DefinitionBodyPlanningFailure("body") -> "GENERATED_ORIGIN_FAILED",
      RawDefinitionPlanMismatch("topology") -> "GENERATED_ORIGIN_FAILED",
      InvalidDefinitionStructuralPlan("plan") -> "GENERATED_ORIGIN_FAILED",
      IncompleteDefinitionPositionMap("positions") -> "GENERATED_ORIGIN_FAILED"
    )
    cases.foreach { case (problem, expected) => assertEquals(classify("classifyConstructedFailure", problem).code, expected) }
    assertEquals(classify("classifyConstructedFailure", null).code, "INTERNAL_INVARIANT_FAILED")

  test("alias backend failures preserve exact versus generated stages and impossible dispatch"):
    import SimpleTypeAliasGeneratedOriginError.*
    val cases = List(
      MissingDefinitionShape -> "INTERNAL_INVARIANT_FAILED",
      WrongDefinitionShapeFamily("wrong") -> "INTERNAL_INVARIANT_FAILED",
      AliasCompletionFailure(TypeQuasiquoteError("completion")) -> "EXACT_LOWERING_FAILED",
      AliasNameFailure("name") -> "EXACT_LOWERING_FAILED",
      CompletedTypeExactLoweringFailure(CompletedTypeUntypedLoweringError.UnsupportedCompletedType("AnyVal")) -> "EXACT_LOWERING_FAILED",
      SourceFreeInvariantFailure("raw") -> "EXACT_LOWERING_FAILED",
      InvalidVirtualSourceName("invalid") -> "INVALID_VIRTUAL_SOURCE",
      GeneratedTypePlanningFailure(ConstructedTermGeneratedOriginError.InvalidVirtualSourceName("plan")) -> "GENERATED_ORIGIN_FAILED",
      GeneratedSourcePlanMismatch("source") -> "GENERATED_ORIGIN_FAILED",
      RawTopologyMismatch("topology") -> "GENERATED_ORIGIN_FAILED",
      GeneratedOriginPositioningFailure("position") -> "GENERATED_ORIGIN_FAILED",
      PositionedInvariantFailure("invariant") -> "GENERATED_ORIGIN_FAILED"
    )
    cases.foreach { case (problem, expected) => assertEquals(classify("classifyAliasFailure", problem).code, expected) }
    assertEquals(classify("classifyAliasFailure", null).code, "INTERNAL_INVARIANT_FAILED")

  test("alias failures with absent nested causes are contained as impossible private carriers"):
    import SimpleTypeAliasGeneratedOriginError.*
    List(AliasCompletionFailure(null), CompletedTypeExactLoweringFailure(null), GeneratedTypePlanningFailure(null)).foreach { problem =>
      assertEquals(classify("classifyAliasFailure", problem).code, "INTERNAL_INVARIANT_FAILED")
    }

  test("private finishing rejects absent carriers fields and contradictory root families"):
    withContext:
      val semantic = fixtures.head._1
      val shape = SemanticDefinitionShapeAdapter.adapt(semantic).toOption.get
      val good = lower(semantic)
      val cases = List(
        null,
        new GeneratedOriginDefinitionResult(null, good.generatedSource, good.sourceFile),
        new GeneratedOriginDefinitionResult(good.tree, null, good.sourceFile),
        new GeneratedOriginDefinitionResult(good.tree, good.generatedSource, null),
        new GeneratedOriginDefinitionResult(lower(fixtures.last._1).tree, good.generatedSource, good.sourceFile)
      )
      cases.foreach(result => assertEquals(finish(shape, Right(result)).left.toOption.get.code, "INTERNAL_INVARIANT_FAILED"))
      assertEquals(finish(shape, null).left.toOption.get.code, "INTERNAL_INVARIANT_FAILED")
      assertEquals(finish(shape, Left(null)).left.toOption.get.code, "INTERNAL_INVARIANT_FAILED")
      assertEquals(finish(null, Right(new GeneratedOriginDefinitionResult(good.tree, good.generatedSource, good.sourceFile))).left.toOption.get.code, "INTERNAL_INVARIANT_FAILED")
      given SourceFile = NoSource
      val wrongName = untpd.ValDef(termName("different"), good.tree.asInstanceOf[untpd.ValDef].tpt, good.tree.asInstanceOf[untpd.ValDef].rhs)
        .cloneIn(good.sourceFile).withSpan(good.tree.span)
      assertEquals(finish(shape, Right(new GeneratedOriginDefinitionResult(wrongName, good.generatedSource, good.sourceFile))).left.toOption.get.code, "INTERNAL_INVARIANT_FAILED")

  test("private finishing rejects mismatched source identities content paths and spans"):
    withContext:
      val semantic = fixtures.head._1
      val shape = SemanticDefinitionShapeAdapter.adapt(semantic).toOption.get
      val good = lower(semantic)
      val result = new GeneratedOriginDefinitionResult(good.tree, good.generatedSource, good.sourceFile)
      val wrongSources = List(
        SourceFile.virtual(sourceName, good.generatedSource),
        SourceFile.virtual("Other.scala", good.generatedSource),
        SourceFile.virtual(sourceName, "different content")
      )
      wrongSources.foreach { source =>
        assertEquals(finish(shape, Right(new GeneratedOriginDefinitionResult(good.tree, good.generatedSource, source))).left.toOption.get.code, "GENERATED_ORIGIN_FAILED")
      }
      val spanned = lower(semantic)
      val value = spanned.tree.asInstanceOf[untpd.ValDef]
      val changedValue = untpd.cpy.ValDef(value)(value.name,
        value.tpt.withSpan(Span(0, spanned.generatedSource.length + 1, 0)), value.rhs)
      assertEquals(finish(shape, Right(new GeneratedOriginDefinitionResult(changedValue, spanned.generatedSource, spanned.sourceFile))).left.toOption.get.code, "GENERATED_ORIGIN_FAILED")
      val alias = lower(fixtures.last._1)
      val aliasTree = alias.tree.asInstanceOf[untpd.TypeDef]
      val changedAlias = untpd.cpy.TypeDef(aliasTree)(aliasTree.name,
        aliasTree.rhs.withSpan(Span(0, alias.generatedSource.length + 1, 0)))
      val aliasShape = SemanticDefinitionShapeAdapter.adapt(fixtures.last._1).toOption.get
      assertEquals(finish(aliasShape, Right(new GeneratedOriginDefinitionResult(changedAlias, alias.generatedSource, alias.sourceFile))).left.toOption.get.code, "GENERATED_ORIGIN_FAILED")
      assert(finish(shape, Right(result)).isRight)

  test("private finishing rejects nested symbols and TypedSplice contamination"):
    withContext:
      given SourceFile = NoSource
      val semantic = fixtures.head._1
      val shape = SemanticDefinitionShapeAdapter.adapt(semantic).toOption.get
      val good = lower(semantic)
      val value = good.tree.asInstanceOf[untpd.ValDef]
      val symbol = newSymbol(NoSymbol, termName("injected"), Flags.EmptyFlags, NoType)
      val symbolTree = untpd.Ident(termName("injected")).withType(symbol.termRef)
        .cloneIn(good.sourceFile).withSpan(value.rhs.span)
      List(symbolTree, untpd.TypedSplice(symbolTree).cloneIn(good.sourceFile).withSpan(value.rhs.span)).foreach { body =>
        val changed = untpd.ValDef(value.name, value.tpt, body).cloneIn(good.sourceFile).withSpan(value.span)
        assertEquals(finish(shape, Right(new GeneratedOriginDefinitionResult(changed, good.generatedSource, good.sourceFile))).left.toOption.get.code, "GENERATED_ORIGIN_FAILED")
      }

  private def fixtures: List[(SemanticDefinition, String, Class[?])] = List(
    (right(SemanticDefinition.immutableValue(name("answer"), intType, TermShape.Literal("42"))), "val answer: Int = 42", classOf[untpd.ValDef]),
    (right(SemanticDefinition.immutableValue(name("renamedValue"), intType, TermShape.Literal("42"))), "val renamedValue: Int = 42", classOf[untpd.ValDef]),
    (method("answer", Vector.empty, intType)(_ => Right(TermShape.Literal("42"))), "def answer: Int = 42", classOf[untpd.DefDef]),
    (method("renamedMethod", Vector.empty, intType)(_ => Right(TermShape.Literal("42"))), "def renamedMethod: Int = 42", classOf[untpd.DefDef]),
    (method("foo", Vector("x"), stringType)(_.reference(0, 0).map(TermShape.Select(_, "toString"))), "def foo(x: Int): String = x.toString", classOf[untpd.DefDef]),
    (method("show", Vector("input"), stringType)(_.reference(0, 0).map(TermShape.Select(_, "toString"))), "def show(input: Int): String = input.toString", classOf[untpd.DefDef]),
    (method("choose", Vector("x", "y"), intType)(_.reference(0, 0)), "def choose(x: Int, y: Int): Int = x", classOf[untpd.DefDef]),
    (method("pick", Vector("left", "right"), intType)(_.reference(0, 1)), "def pick(left: Int, right: Int): Int = right", classOf[untpd.DefDef]),
    (right(SemanticDefinition.typeAlias(name("T"), listInt)), "type T = List[Int]", classOf[untpd.TypeDef]),
    (right(SemanticDefinition.typeAlias(name("Renamed"), listInt)), "type Renamed = List[Int]", classOf[untpd.TypeDef])
  )

  private def method(label: String, parameters: Vector[String], result: TypeNormalForm)(
      body: DefinitionParameterScope => Either[DefinitionSemanticError, TermShape]
  ): SemanticDefinition =
    val clauses = if parameters.isEmpty then Vector.empty else Vector(right(DefinitionParameterClause.ordinary(
      parameters.map(p => DefinitionParameter(name(p), intType)))))
    right(SemanticDefinition.concreteMethod(name(label), clauses, result)(body))

  private def forgedValue(declaredType: TypeNormalForm, body: TermShape): SemanticDefinition =
    new SemanticDefinition(DefinitionKind.Value, name("forged"), DefinitionModifiers.empty, storage("ValueStorage", declaredType, body))

  private def storage(suffix: String, arguments: AnyRef*): AnyRef =
    val constructor = Class.forName(s"quasiquotes.definitions.SemanticDefinition$$$suffix").getDeclaredConstructors.head
    constructor.setAccessible(true)
    constructor.newInstance(arguments*).asInstanceOf[AnyRef]

  private def right[A](value: Either[DefinitionSemanticError, A]): A = value.fold(p => fail(p.message), identity)
  private def name(value: String): DefinitionName = right(DefinitionName.fromSource(value))
  private def lower(value: SemanticDefinition, path: String = sourceName)(using Context): DefinitionGeneratedOriginLowering.Lowered =
    DefinitionGeneratedOriginLowering.lower(value, path).fold(p => fail(p.message), identity)
  private def failure(value: SemanticDefinition, path: String = sourceName)(using Context): DefinitionGeneratedOriginLowering.Failure =
    DefinitionGeneratedOriginLowering.lower(value, path).left.toOption.getOrElse(fail("unexpected success"))

  private def classify(methodName: String, error: AnyRef): DefinitionGeneratedOriginLowering.Failure =
    val method = DefinitionGeneratedOriginLowering.getClass.getDeclaredMethods.find(_.getName == methodName).get
    method.setAccessible(true)
    method.invoke(DefinitionGeneratedOriginLowering, error).asInstanceOf[DefinitionGeneratedOriginLowering.Failure]

  private def finish(shape: DefinitionShape, result: Either[DefinitionGeneratedOriginLowering.Failure, GeneratedOriginDefinitionResult])(
      using Context
  ): Either[DefinitionGeneratedOriginLowering.Failure, DefinitionGeneratedOriginLowering.Lowered] =
    val method = DefinitionGeneratedOriginLowering.getClass.getDeclaredMethods.find(_.getName == "finishLowering").get
    method.setAccessible(true)
    method.invoke(DefinitionGeneratedOriginLowering, shape, sourceName, result, summon[Context])
      .asInstanceOf[Either[DefinitionGeneratedOriginLowering.Failure, DefinitionGeneratedOriginLowering.Lowered]]

  private def assertComplete(result: DefinitionGeneratedOriginLowering.Lowered)(using Context): Unit =
    assert(result != null && result.tree != null && result.generatedSource != null && result.sourceFile != null)
    assertEquals(result.virtualSourceName, result.sourceFile.path)
    assertEquals(result.sourceFile.content.mkString, result.generatedSource)
    allTrees(result.tree).foreach { tree =>
      assert(tree.source eq result.sourceFile, clues(tree))
      assert(tree.span.exists, clues(tree))
      assert(tree.span.start >= 0 && tree.span.start <= tree.span.point && tree.span.point <= tree.span.end && tree.span.end <= result.generatedSource.length, clues(tree))
      assertEquals(tree.symbol, NoSymbol)
      assert(!tree.isInstanceOf[untpd.TypedSplice])
    }

  private def children(tree: untpd.Tree)(using Context): Vector[untpd.Tree] = tree match
    case value: untpd.TypeDef => Vector(value.rhs)
    case value: untpd.DefDef => value.paramss.flatten.toVector ++ Vector(value.tpt, value.rhs)
    case value: untpd.ValDef => Vector(value.tpt, value.rhs).filterNot(_.isEmpty)
    case value: untpd.Select => Vector(value.qualifier)
    case value: untpd.Apply => value.fun +: value.args.toVector
    case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
    case value: untpd.Tuple => value.trees.toVector
    case value: untpd.Function => value.args.toVector :+ value.body
    case _ => Vector.empty

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] = tree +: children(tree).flatMap(allTrees)

  private def snapshot(tree: untpd.Tree)(using Context): String =
    val atom = tree match
      case value: untpd.MemberDef => s"${value.name}:${value.name.isTypeName}:${value.mods.flags}:${value.mods.annotations}:${value.mods.privateWithin}"
      case value: untpd.Ident => s"${value.name}:${value.name.isTypeName}"
      case value: untpd.Select => s"${value.name}:${value.name.isTypeName}"
      case value if children(value).isEmpty => value.toString
      case _ => ""
    s"${tree.getClass.getName}($atom)[${children(tree).map(snapshot).mkString(",")}]"

  private def withContext[A](run: Context ?=> A): A = run(using (new ContextBase).initialCtx)
