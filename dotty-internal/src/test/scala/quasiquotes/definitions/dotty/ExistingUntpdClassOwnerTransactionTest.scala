package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags.{EmptyFlags, Mutable}
import dotty.tools.dotc.core.Names.{termName, typeName}
import dotty.tools.dotc.core.Symbols.{NoSymbol, newSymbol}
import dotty.tools.dotc.core.Types.NoType
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.{NoSource, SourceFile, Spans}
import dotty.tools.dotc.util.Spans.NoSpan

import quasiquotes.definitions.*
import quasiquotes.parser.TermShape
import quasiquotes.types.TypeNormalForm

class ExistingUntpdClassOwnerTransactionTest extends munit.FunSuite:
  import ExistingUntpdClassOwnerTransaction.*

  test("no-op returns the exact original owner and body on every call") {
    withContext {
      val captured = capture(parseClass(
        "class Noop:\n  val first: Int = 1\n  def keep(x: Int): Int = x\n"
      ))
      val before = snapshot(captured.originalRoot)
      val first = transact(captured)
      val second = transact(captured)

      Vector(first, second).foreach { result =>
        assert(!result.changed)
        assert(result.root.eq(captured.originalRoot))
        assert(result.template.eq(captured.originalTemplate))
        assertIdentityVector(result.finalMembers, captured.members.map(_.tree))
        assertEquals(validateResult(result, captured), Right(()))
      }
      assertSnapshotUnchanged(captured.originalRoot, before)
    }
  }

  test("append-only admits all C037 G5 families including exact TypeDef alias in caller order") {
    withContext {
      val captured = capture(parseClass("class GeneratedFamilies:\n  def existing: Int = 1\n"))
      val original = captured.members.head.tree
      val prepared = Vector(
        prepareGenerated(valueDefinition("addedValue", "2"), "<generated:u044-value>"),
        prepareGenerated(methodDefinition("addedZero", Vector.empty)(_ => Right(TermShape.Literal("3"))), "<generated:u044-zero>"),
        prepareGenerated(methodDefinition("addedOne", Vector("x"))(_.reference(0, 0)), "<generated:u044-one>"),
        prepareGenerated(methodDefinition("addedTwo", Vector("x", "y"))(_.reference(0, 1)), "<generated:u044-two>"),
        prepareGenerated(aliasDefinition("AddedAlias"), "<generated:u044-alias>")
      )
      val generatedBefore = prepared.map(value => snapshot(value.member))

      val result = transact(captured, generated = prepared)

      assert(result.changed)
      assert(result.finalMembers.head.eq(original))
      assertIdentityVector(result.finalMembers.drop(1), prepared.map(_.member))
      assert(result.finalMembers.last.isInstanceOf[untpd.TypeDef])
      assert(result.template.body.last.eq(prepared.last.member))
      prepared.zip(generatedBefore).foreach { case (value, before) =>
        assertSnapshotUnchanged(value.member, before)
        assert(value.member.source.eq(value.sourceFile))
        assertEquals(value.member.source.path, value.requestedVirtualSourceName)
      }
      assertEquals(validateResult(result, captured), Right(()))
    }
  }

  test("U2 and U3 insert the exact U042 method and exact C037 sibling in one owner reconstruction") {
    withContext {
      val captured = capture(parseClass(
        "class NorthStar:\n  def bump(x: AnyVal): AnyVal = x\n  val keep: Int = 9\n"
      ))
      val edited = editMethod(captured, 0)
      val generated = prepareGenerated(methodDefinition("bonus", Vector.empty)(_ => Right(TermShape.Literal("5"))), "<generated:u044-bonus>")

      val u2 = transact(captured, edits = Vector(edited))
      assert(u2.finalMembers.head.eq(edited.positionedMethod))
      assert(u2.finalMembers(1).eq(captured.members(1).tree))
      assert(!u2.finalMembers.exists(_.eq(edited.descriptor.method)))

      val u3 = transact(captured, edits = Vector(edited), generated = Vector(generated))
      assertIdentityVector(
        u3.finalMembers,
        Vector(edited.positionedMethod, captured.members(1).tree, generated.member)
      )
      assert(u3.template.body.head.eq(edited.positionedMethod))
      assert(u3.template.body.last.eq(generated.member))
      assertEquals(validateResult(u2, captured), Right(()))
      assertEquals(validateResult(u3, captured), Right(()))
    }
  }

  test("omission composes with a different method edit and generated append in deterministic order") {
    withContext {
      val captured = capture(parseClass(
        "class Composition:\n  val removeMe: Int = 1\n  def change(x: AnyVal): AnyVal = x\n  val keepMe: Int = 2\n"
      ))
      val edited = editMethod(captured, 1)
      val generated = prepareGenerated(valueDefinition("added", "4"), "<generated:u044-composition>")

      val omissionOnly = transact(captured, omitted = Vector(0))
      assertIdentityVector(omissionOnly.finalMembers, Vector(captured.members(1).tree, captured.members(2).tree))

      val omissionAndAppend = transact(captured, omitted = Vector(0), generated = Vector(generated))
      assertIdentityVector(omissionAndAppend.finalMembers, Vector(captured.members(1).tree, captured.members(2).tree, generated.member))

      val omissionAndEdit = transact(captured, omitted = Vector(0), edits = Vector(edited))
      assertIdentityVector(omissionAndEdit.finalMembers, Vector(edited.positionedMethod, captured.members(2).tree))

      val full = transact(captured, omitted = Vector(0), edits = Vector(edited), generated = Vector(generated))
      assertIdentityVector(full.finalMembers, Vector(edited.positionedMethod, captured.members(2).tree, generated.member))
      assert(!full.finalMembers.exists(_.eq(captured.members(0).tree)))
      assertCode(apply(captured, Vector(1), Vector(edited), Vector.empty), "OMISSION_METHOD_EDIT_CONFLICT")
    }
  }

  test("fails before reconstruction for null duplicate foreign stale and incompatible operations") {
    withContext {
      val captured = capture(parseClass(
        "class Boundary:\n  def change(x: AnyVal): AnyVal = x\n  val keep: Int = 2\n"
      ))
      val edited = editMethod(captured, 0)
      val generated = prepareGenerated(valueDefinition("added", "4"), "<generated:u044-boundary>")
      val foreignCapture = capture(parseClass(
        "class Boundary:\n  def change(x: AnyVal): AnyVal = x\n  val keep: Int = 2\n"
      ))
      val foreignEdit = editMethod(foreignCapture, 0)
      val otherGenerated = prepareGenerated(valueDefinition("other", "5"), "<generated:u044-other>")
      val sameRawDescriptor = ExistingUntpdOrdinaryMethodDescriptor.capture(captured, 0).toOption.get
      val sameRawDescriptorEdit = edited.copy(descriptor = sameRawDescriptor)

      assertCode(apply(null, Vector.empty, Vector.empty, Vector.empty), "CAPTURE_REQUIRED")
      assertCode(apply(captured, null, Vector.empty, Vector.empty), "OMISSIONS_REQUIRED")
      assertCode(apply(captured, Vector.empty, null, Vector.empty), "METHOD_EDITS_REQUIRED")
      assertCode(apply(captured, Vector.empty, Vector.empty, null), "GENERATED_APPENDS_REQUIRED")
      assertCode(apply(captured, Vector(0, 0), Vector.empty, Vector.empty), "DUPLICATE_OMITTED_INDEX")
      assertCode(apply(captured, Vector(-1), Vector.empty, Vector.empty), "OMITTED_INDEX_NOT_CAPTURED")
      assertCode(apply(captured, Vector(2), Vector.empty, Vector.empty), "OMITTED_INDEX_NOT_CAPTURED")
      assertCode(apply(captured, Vector.empty, Vector(null), Vector.empty), "METHOD_EDIT_REQUIRED")
      assertCode(apply(captured, Vector.empty, Vector(edited, edited), Vector.empty), "DUPLICATE_METHOD_EDIT")
      assertCode(apply(captured, Vector.empty, Vector(foreignEdit), Vector.empty), "FOREIGN_METHOD_EDIT")
      assertCode(apply(captured, Vector.empty, Vector(sameRawDescriptorEdit), Vector.empty), "INVALID_METHOD_EDIT")
      assertCode(apply(captured, Vector.empty, Vector(edited.copy(positionedMethod = edited.descriptor.method)), Vector.empty), "INVALID_METHOD_EDIT")
      assertCode(apply(captured, Vector.empty, Vector.empty, Vector(null)), "GENERATED_PREPARATION_REQUIRED")
      assertCode(apply(captured, Vector.empty, Vector.empty, Vector(generated, generated)), "DUPLICATE_GENERATED_APPEND")
      assertCode(apply(captured, Vector.empty, Vector.empty, Vector(generated.copy(member = otherGenerated.member))), "INVALID_GENERATED_PREPARATION")
      assertCode(apply(captured, Vector.empty, Vector.empty, Vector(generated.copy(lowered = otherGenerated.lowered))), "INVALID_GENERATED_PREPARATION")
      assertCode(apply(captured, Vector.empty, Vector.empty, Vector(generated.copy(requestedVirtualSourceName = "<generated:wrong>"))), "INVALID_GENERATED_PREPARATION")
      assertCode(ExistingUntpdClassOwnerTransaction.prepareGenerated(null, "<generated:null>"), "DEFINITION_REQUIRED")
      assertCode(ExistingUntpdClassOwnerTransaction.prepareGenerated(valueDefinition("invalidPath", "1"), null), "VIRTUAL_SOURCE_NAME_REQUIRED")
      assertCode(ExistingUntpdClassOwnerTransaction.prepareGenerated(valueDefinition("invalidPath", "1"), " invalid.scala"), "GENERATED_MEMBER_PREPARATION_FAILED")

      val generatedValue = generated.member.asInstanceOf[untpd.ValDef]
      val malformedMember = untpd.cpy.ValDef(generatedValue)(
        generatedValue.name,
        generatedValue.tpt,
        null
      ).cloneIn(generated.sourceFile).withSpan(generatedValue.span)
      val malformedLowered = new DefinitionGeneratedOriginLowering.Lowered(
        malformedMember, generated.generatedSource, generated.sourceFile
      )
      val malformedPrepared = generated.copy(lowered = malformedLowered, member = malformedMember)
      assertCode(apply(captured, Vector.empty, Vector.empty, Vector(malformedPrepared)), "INVALID_GENERATED_PREPARATION")

      val alias = prepareGenerated(aliasDefinition("OriginalAlias"), "<generated:u044-forged-alias>")
      val aliasTree = alias.member.asInstanceOf[untpd.TypeDef]
      val renamedAlias = untpd.cpy.TypeDef(aliasTree)(typeName("ForgedAlias"), aliasTree.rhs)
      val renamedLowered = new DefinitionGeneratedOriginLowering.Lowered(
        renamedAlias, alias.generatedSource, alias.sourceFile
      )
      assertCode(
        apply(captured, Vector.empty, Vector.empty, Vector(alias.copy(
          lowered = renamedLowered,
          member = renamedAlias
        ))),
        "INVALID_GENERATED_PREPARATION"
      )

      val alreadyPlaced = ExistingUntpdClassMemberFilter.reconstruct(
        captured,
        captured.members.map(_.tree) :+ generated.member
      ).toOption.get
      val recaptured = capture(alreadyPlaced.root)
      assertCode(apply(recaptured, Vector.empty, Vector.empty, Vector(generated)), "GENERATED_MEMBER_ALREADY_PRESENT")
    }
  }

  test("rejects topology-consistent captures contaminated by symbols or TypedSplice even on no-op") {
    withContext {
      given SourceFile = NoSource
      val captured = capture(parseClass("class Contaminated:\n  val keep: Int = 1\n"))
      val symbol = newSymbol(NoSymbol, termName("u044Injected"), EmptyFlags, NoType)
      val symbolic = untpd.Ident(termName("u044Injected")).withType(symbol.termRef)
      Vector[untpd.Tree](symbolic, untpd.TypedSplice(symbolic)).foreach { contaminant =>
        val template = untpd.cpy.Template(captured.originalTemplate)(
          captured.originalTemplate.constr,
          captured.originalTemplate.parentsOrDerived,
          captured.originalTemplate.derived,
          captured.originalTemplate.self,
          captured.originalTemplate.body :+ contaminant
        )
        val root = untpd.cpy.TypeDef(captured.originalRoot)(captured.originalRoot.name, template)
        val forged = captured.copy(
          originalRoot = root,
          originalTemplate = template,
          members = captured.members :+ ExistingUntpdClassMemberFilter.Member(
            captured.members.size,
            contaminant
          )
        )
        assertEquals(ExistingUntpdClassMemberFilter.validateCaptured(forged), Right(()))
        assertCode(apply(forged, Vector.empty, Vector.empty, Vector.empty), "INVALID_CAPTURE")
      }
    }
  }

  test("rejects symbol and TypedSplice contamination hidden in modifier annotations") {
    withContext {
      given SourceFile = NoSource
      val symbol = newSymbol(NoSymbol, termName("u044Annotation"), EmptyFlags, NoType)

      val captured = capture(parseClass("class AnnotatedCapture:\n  val keep: Int = 1\n"))
      val existing = captured.members.head.tree.asInstanceOf[untpd.MemberDef]
      val existingSymbol = untpd.Ident(termName("u044Annotation")).withType(symbol.termRef)
      existing.setMods(existing.mods.copy(annotations = List(untpd.TypedSplice(existingSymbol))))
      assertEquals(ExistingUntpdClassMemberFilter.validateCaptured(captured), Right(()))
      assertCode(apply(captured, Vector.empty, Vector.empty, Vector.empty), "INVALID_CAPTURE")

      val owner = capture(parseClass("class GeneratedAnnotation:\n  val keep: Int = 1\n"))
      val prepared = prepareGenerated(valueDefinition("added", "2"), "<generated:u044-annotation>")
      val generatedSymbol = untpd.Ident(termName("u044Annotation")).withType(symbol.termRef)
        .cloneIn(prepared.sourceFile).withSpan(prepared.member.span)
      val generatedSplice = untpd.TypedSplice(generatedSymbol)
        .cloneIn(prepared.sourceFile).withSpan(prepared.member.span)
      prepared.member.setMods(prepared.member.mods.copy(annotations = List(generatedSplice)))
      assertCode(
        apply(owner, Vector.empty, Vector.empty, Vector(prepared)),
        "INVALID_GENERATED_PREPARATION"
      )
    }
  }

  test("rejects deferred and unavailable capture shells without forcing input") {
    withContext {
      val captured = capture(parseClass("class DeferredCapture:\n  val keep: Int = 1\n"))
      val originalTemplate = captured.originalTemplate
      var completions = 0
      val deferred = new _root_.dotty.tools.dotc.ast.Trees.Lazy[List[untpd.Tree]]:
        def complete(using Context): List[untpd.Tree] =
          completions += 1
          originalTemplate.body
      val deferredTemplate = untpd.cpy.Template(originalTemplate)(
        originalTemplate.constr,
        originalTemplate.parentsOrDerived,
        originalTemplate.derived,
        originalTemplate.self,
        deferred
      )
      val deferredRoot = untpd.cpy.TypeDef(captured.originalRoot)(
        captured.originalRoot.name,
        deferredTemplate
      )
      val deferredCapture = captured.copy(
        originalRoot = deferredRoot,
        originalTemplate = deferredTemplate
      )
      assertCode(apply(deferredCapture, Vector.empty, Vector.empty, Vector.empty), "INVALID_CAPTURE")
      assertEquals(completions, 0)
      assert(deferredTemplate.unforcedBody.eq(deferred))

      val sourceFreeRoot = captured.originalRoot.cloneIn(null).asInstanceOf[untpd.TypeDef]
      val sourceFreeCapture = captured.copy(originalRoot = sourceFreeRoot)
      assertEquals(ExistingUntpdClassMemberFilter.validateCaptured(sourceFreeCapture), Right(()))
      assertCode(apply(sourceFreeCapture, Vector.empty, Vector.empty, Vector.empty), "INVALID_CAPTURE")

      val noSpanRoot = captured.originalRoot.cloneIn(captured.originalRoot.source)
        .withSpan(NoSpan).asInstanceOf[untpd.TypeDef]
      val noSpanCapture = captured.copy(originalRoot = noSpanRoot)
      assertEquals(ExistingUntpdClassMemberFilter.validateCaptured(noSpanCapture), Right(()))
      assertCode(apply(noSpanCapture, Vector.empty, Vector.empty, Vector.empty), "INVALID_CAPTURE")
    }
  }

  test("rejects generated graph mutation after preparation even when it remains clean and contained") {
    withContext {
      val captured = capture(parseClass("class GeneratedMutation:\n  val keep: Int = 1\n"))

      val mutable = prepareGenerated(valueDefinition("mutableAfterPreparation", "2"), "<generated:u044-mutable>")
      mutable.member.setMods(mutable.member.mods.copy(flags = mutable.member.mods.flags | Mutable))
      assertCode(
        apply(captured, Vector.empty, Vector.empty, Vector(mutable)),
        "INVALID_GENERATED_PREPARATION"
      )

      val respanned = prepareGenerated(valueDefinition("respannedAfterPreparation", "3"), "<generated:u044-respanned>")
      val rhs = respanned.member.asInstanceOf[untpd.ValDef].rhs
      rhs.span = Spans.Span(rhs.span.start, rhs.span.end, rhs.span.end)
      assertCode(
        apply(captured, Vector.empty, Vector.empty, Vector(respanned)),
        "INVALID_GENERATED_PREPARATION"
      )
    }
  }

  test("rejects a deferred reconstructed-method field without forcing the edit carrier") {
    withContext {
      val captured = capture(parseClass(
        "class DeferredMethod:\n  def change(x: AnyVal): AnyVal = x\n"
      ))
      val edited = editMethod(captured, 0)
      var completions = 0
      val deferred = new _root_.dotty.tools.dotc.ast.Trees.Lazy[untpd.Tree]:
        def complete(using Context): untpd.Tree =
          completions += 1
          edited.positionedMethod.rhs
      val method = untpd.cpy.DefDef(edited.positionedMethod)(
        edited.positionedMethod.name,
        edited.positionedMethod.paramss,
        edited.positionedMethod.tpt,
        deferred
      )
      assertCode(
        apply(
          captured,
          Vector.empty,
          Vector(edited.copy(positionedMethod = method)),
          Vector.empty
        ),
        "INVALID_METHOD_EDIT"
      )
      assertEquals(completions, 0)
      assert(method.unforcedRhs.eq(deferred))
    }
  }

  test("direct-member limit accounts for omission replacement and ordered appends before reconstruction") {
    withContext {
      val captured = capture(parseClass(classWithMembers(64)))
      val first = prepareGenerated(valueDefinition("extraOne", "1"), "<generated:u044-limit-1>")
      val second = prepareGenerated(valueDefinition("extraTwo", "2"), "<generated:u044-limit-2>")
      assertCode(apply(captured, Vector.empty, Vector.empty, Vector(first)), "DIRECT_MEMBER_LIMIT_OVERFLOW")
      assertCode(apply(captured, Vector(0), Vector.empty, Vector(first, second)), "DIRECT_MEMBER_LIMIT_OVERFLOW")
      val atLimit = transact(captured, omitted = Vector(0), generated = Vector(first))
      assertEquals(atLimit.finalMembers.size, 64)
      assert(atLimit.finalMembers.last.eq(first.member))
    }
  }

  test("changed calls allocate fresh owner shells while reusing exact selected children and preserving every input graph") {
    withContext {
      val captured = capture(parseClass(
        "class Freshness:\n  def change(x: AnyVal): AnyVal = x\n  val keep: Int = 2\n"
      ))
      val edited = editMethod(captured, 0)
      val generated = prepareGenerated(valueDefinition("added", "4"), "<generated:u044-freshness>")
      val ownerBefore = snapshot(captured.originalRoot)
      val editBefore = snapshot(edited.positionedMethod)
      val preparedEditBefore = snapshotGraphs(preparedEditTrees(edited))
      val generatedBefore = snapshot(generated.member)

      val first = transact(captured, edits = Vector(edited), generated = Vector(generated))
      val second = transact(captured, edits = Vector(edited), generated = Vector(generated))
      assert(!first.root.eq(second.root))
      assert(!first.template.eq(second.template))
      assertIdentityVector(first.finalMembers, second.finalMembers)
      assert(first.finalMembers.head.eq(edited.positionedMethod))
      assert(first.finalMembers.last.eq(generated.member))
      assertSnapshotUnchanged(captured.originalRoot, ownerBefore)
      assertSnapshotUnchanged(edited.positionedMethod, editBefore)
      assertGraphsUnchanged(preparedEditBefore)
      assertSnapshotUnchanged(generated.member, generatedBefore)

      assertCode(apply(captured, Vector.empty, Vector(edited, edited), Vector(generated)), "DUPLICATE_METHOD_EDIT")
      assertSnapshotUnchanged(captured.originalRoot, ownerBefore)
      assertSnapshotUnchanged(edited.positionedMethod, editBefore)
      assertGraphsUnchanged(preparedEditBefore)
      assertSnapshotUnchanged(generated.member, generatedBefore)
    }
  }

  test("final validator rejects substituted capture operations members shells topology and provenance") {
    withContext {
      val captured = capture(parseClass(
        "class FinalCheck:\n  def change(x: AnyVal): AnyVal = x\n  val keep: Int = 2\n"
      ))
      val edited = editMethod(captured, 0)
      val generated = prepareGenerated(valueDefinition("added", "4"), "<generated:u044-final>")
      val valid = transact(captured, edits = Vector(edited), generated = Vector(generated))
      val other = capture(parseClass(
        "class FinalCheck:\n  def change(x: AnyVal): AnyVal = x\n  val keep: Int = 2\n"
      ))

      assertCode(validateResult(null, captured), "FINAL_TRANSACTION_INVARIANT_FAILED")
      assertCode(validateResult(valid, null), "FINAL_TRANSACTION_INVARIANT_FAILED")
      assertCode(validateResult(valid.copy(captured = other), captured), "FINAL_TRANSACTION_INVARIANT_FAILED")
      assertCode(validateResult(valid.copy(finalMembers = valid.finalMembers.reverse), captured), "FINAL_TRANSACTION_INVARIANT_FAILED")
      assertCode(validateResult(valid.copy(root = captured.originalRoot), captured), "FINAL_TRANSACTION_INVARIANT_FAILED")
      val renamedRoot = untpd.cpy.TypeDef(valid.root)(typeName("ForgedFinalOwner"), valid.template)
      assertCode(validateResult(valid.copy(root = renamedRoot), captured), "FINAL_TRANSACTION_INVARIANT_FAILED")
      assertCode(validateResult(valid.copy(template = captured.originalTemplate), captured), "FINAL_TRANSACTION_INVARIANT_FAILED")
      assertCode(validateResult(valid.copy(changed = false), captured), "FINAL_TRANSACTION_INVARIANT_FAILED")
      assertCode(validateResult(valid.copy(generatedAppends = Vector(generated, generated)), captured), "FINAL_TRANSACTION_INVARIANT_FAILED")
      assertCode(validateResult(valid.copy(methodEdits = Vector(edited.copy(positionedMethod = edited.descriptor.method))), captured), "FINAL_TRANSACTION_INVARIANT_FAILED")
    }
  }

  private def transact(
      captured: ExistingUntpdClassMemberFilter.Capture,
      omitted: Vector[Int] = Vector.empty,
      edits: Vector[ExistingUntpdOrdinaryMethodReconstruction.ReconstructedMethod] = Vector.empty,
      generated: Vector[PreparedGeneratedMember] = Vector.empty
  )(using Context): Result =
    apply(captured, omitted, edits, generated).fold(problem => fail(problem.message), identity)

  private def editMethod(
      captured: ExistingUntpdClassMemberFilter.Capture,
      memberIndex: Int
  )(using Context): ExistingUntpdOrdinaryMethodReconstruction.ReconstructedMethod =
    val descriptor = ExistingUntpdOrdinaryMethodDescriptor.capture(captured, memberIndex)
      .fold(problem => fail(problem.message), identity)
    val parameter = ExistingUntpdOrdinaryMethodTypeEditPreparation
      .prepareParameterType(descriptor, descriptor.parameterClauses.head.head, TypeNormalForm.STypeIdent("Int"))
      .fold(problem => fail(problem.message), identity)
    val result = ExistingUntpdOrdinaryMethodTypeEditPreparation
      .prepareResultType(descriptor, TypeNormalForm.STypeIdent("Int"))
      .fold(problem => fail(problem.message), identity)
    val body = ExistingUntpdOrdinaryMethodRhsEditPreparation.prepareBody(descriptor) { scope =>
      scope.reference(descriptor.parameterClauses.head.head).map(reference =>
        TermShape.Apply(
          TermShape.Select(TermShape.Identifier("Math", false), "abs"),
          List(reference)
        )
      )
    }.fold(problem => fail(problem.message), identity)
    ExistingUntpdOrdinaryMethodReconstruction
      .reconstructMethod(descriptor, Vector(parameter), Some(result), Some(body))
      .fold(problem => fail(problem.message), identity)

  private def prepareGenerated(
      definition: SemanticDefinition,
      path: String
  )(using Context): PreparedGeneratedMember =
    ExistingUntpdClassOwnerTransaction.prepareGenerated(definition, path)
      .fold(problem => fail(problem.message), identity)

  private def valueDefinition(label: String, literal: String): SemanticDefinition =
    right(SemanticDefinition.immutableValue(name(label), intType, TermShape.Literal(literal)))

  private def aliasDefinition(label: String): SemanticDefinition =
    right(SemanticDefinition.typeAlias(name(label), intType))

  private def methodDefinition(
      label: String,
      parameters: Vector[String]
  )(
      body: DefinitionParameterScope => Either[DefinitionSemanticError, TermShape]
  ): SemanticDefinition =
    val clauses =
      if parameters.isEmpty then Vector.empty
      else Vector(right(DefinitionParameterClause.ordinary(
        parameters.map(value => DefinitionParameter(name(value), intType))
      )))
    right(SemanticDefinition.concreteMethod(name(label), clauses, intType)(body))

  private def name(value: String): DefinitionName =
    right(DefinitionName.fromSource(value))

  private val intType = TypeNormalForm.STypeIdent("Int")

  private def right[A](result: Either[DefinitionSemanticError, A]): A =
    result.fold(problem => fail(problem.message), identity)

  private def capture(root: untpd.TypeDef)(using Context): ExistingUntpdClassMemberFilter.Capture =
    ExistingUntpdClassMemberFilter.capture(root).fold(problem => fail(problem.message), identity)

  private def assertCode[A](result: Either[Error, A], expected: String): Unit = result match
    case Left(problem) =>
      assertEquals(problem.code, expected, clues(problem))
      assert(problem.detail.nonEmpty)
      assert(problem.message.nonEmpty)
    case Right(value) => fail(s"expected $expected, found success $value")

  private def assertIdentityVector(actual: Vector[untpd.Tree], expected: Vector[untpd.Tree]): Unit =
    assertEquals(actual.size, expected.size)
    actual.zip(expected).foreach { case (left, right) => assert(left.eq(right)) }

  private final case class TreeSnapshot(
      tree: untpd.Tree,
      source: SourceFile,
      span: Spans.Span,
      symbol: _root_.dotty.tools.dotc.core.Symbols.Symbol
  )
  private final case class GraphSnapshot(root: untpd.Tree, graph: Vector[TreeSnapshot])

  private def snapshot(tree: untpd.Tree)(using Context): Vector[TreeSnapshot] =
    ExistingUntpdClassMemberFilter.allTrees(tree).map(node =>
      TreeSnapshot(node, node.source, node.span, node.symbol)
    )

  private def assertSnapshotUnchanged(
      tree: untpd.Tree,
      before: Vector[TreeSnapshot]
  )(using Context): Unit =
    val after = snapshot(tree)
    assertEquals(after.size, before.size)
    after.zip(before).foreach { case (actual, expected) =>
      assert(actual.tree.eq(expected.tree))
      assert(actual.source.eq(expected.source))
      assertEquals(actual.span, expected.span)
      assertEquals(actual.symbol, expected.symbol)
    }

  private def preparedEditTrees(
      edited: ExistingUntpdOrdinaryMethodReconstruction.ReconstructedMethod
  ): Vector[untpd.Tree] =
    edited.parameterTypes.flatMap(value => Vector(
      value.loweredType,
      value.positionedType,
      value.positionedParameter
    )) ++
      edited.resultType.toVector.flatMap(value => Vector(value.loweredType, value.positionedType)) ++
      edited.body.toVector.flatMap(value => Vector(value.loweredReplacement, value.positionedReplacement))

  private def snapshotGraphs(
      roots: Vector[untpd.Tree]
  )(using Context): Vector[GraphSnapshot] =
    roots.map(root => GraphSnapshot(root, snapshot(root)))

  private def assertGraphsUnchanged(
      before: Vector[GraphSnapshot]
  )(using Context): Unit =
    before.foreach(value => assertSnapshotUnchanged(value.root, value.graph))

  private def classWithMembers(count: Int): String =
    val members = (0.until(count)).map(index => s"  val member$index: Int = $index").mkString("\n")
    s"class AtLimit:\n$members\n"

  private def parseClass(source: String)(using outer: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U044OwnerTransaction.scala", source)
    given Context = outer.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed match
      case packageDef: untpd.PackageDef =>
        packageDef.stats match
          case (root: untpd.TypeDef) :: Nil => root
          case other => fail(s"expected one TypeDef, found $other")
      case other => fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")

  private def withContext[A](run: Context ?=> A): A =
    run(using (new ContextBase).initialCtx)
