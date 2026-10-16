package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Flags.EmptyFlags
import dotty.tools.dotc.core.Names.typeName
import dotty.tools.dotc.core.Symbols.{NoSymbol, newSymbol}
import dotty.tools.dotc.core.Types.NoType
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter

class ExistingGenericG1OpaqueTypeClausePreservationTest extends munit.FunSuite:
  test("public G1 capture exposes only its ordinary term clause") {
    withContext {
      val root = parseClass(
        "class GenericIndependent:\n  def keep[A](x: AnyVal): AnyVal = x\n  val untouched: Int = 7\n"
      )
      val captured = ExistingClassUntypedRewrite.capture(root).fold(
        problem => fail(problem.message),
        identity
      )
      val method = captured.method(captured.members.head.ref).fold(
        problem => fail(problem.message),
        identity
      )

      assertEquals(method.diagnosticName, "keep")
      assertEquals(method.parameterClauses.size, 1)
      assertEquals(
        method.parameterClauses.head.kind,
        ExistingClassUntypedRewrite.ParameterClauseKind.Ordinary
      )
      assertEquals(method.parameterClauses.head.parameters.size, 1)
      val parameter = method.parameterClauses.head.parameters.head
      assertEquals(parameter.clauseIndex, 0)
      assertEquals(parameter.parameterIndex, 0)
      assertEquals(parameter.diagnosticName, "x")
    }
  }

  test("G1 public edits preserve the original opaque Type clause by identity") {
    withContext {
      val root = parseClass(
        "class GenericEdited:\n  def keep[A](x: AnyVal): AnyVal = x\n  val untouched: Int = 7\n"
      )
      val originalTemplate = root.rhs.asInstanceOf[untpd.Template]
      val originalMethod = originalTemplate.body.head.asInstanceOf[untpd.DefDef]
      val originalTypeClause = originalMethod.paramss.head
      val originalTypeParameter = originalTypeClause.head.asInstanceOf[untpd.TypeDef]
      val originalBounds = originalTypeParameter.rhs
      val originalTermParameter = originalMethod.paramss(1).head
      val originalSibling = originalTemplate.body(1)

      val captured = ExistingClassUntypedRewrite.capture(root).fold(
        problem => fail(problem.message),
        identity
      )
      val method = captured.method(captured.members.head.ref).fold(
        problem => fail(problem.message),
        identity
      )
      val parameter = method.parameterClauses.head.parameters.head
      val body = method.parameterScope.reference(parameter.ref).fold(
        problem => fail(problem.message),
        identity
      )
      val plan = for
        parameterEdited <- captured.emptyPlan.replaceParameterType(
          parameter.ref,
          quasiquotes.types.TypeNormalForm.STypeIdent("Int")
        )
        resultEdited <- parameterEdited.replaceResultType(
          method.ref,
          quasiquotes.types.TypeNormalForm.STypeIdent("Int")
        )
        bodyEdited <- resultEdited.replaceBody(method.ref, body)
      yield bodyEdited
      val rewritten = ExistingClassUntypedRewrite(captured, plan.fold(
        problem => fail(problem.message),
        identity
      )).fold(
        problem => fail(problem.message),
        identity
      )
      val rebuiltTemplate = rewritten.tree.rhs.asInstanceOf[untpd.Template]
      val rebuiltMethod = rebuiltTemplate.body.head.asInstanceOf[untpd.DefDef]

      assertEquals(rebuiltMethod.paramss.map(_.size), List(1, 1))
      assert(rebuiltMethod.paramss.head.eq(originalTypeClause))
      assert(rebuiltMethod.paramss.head.head.eq(originalTypeParameter))
      assert(rebuiltMethod.paramss.head.head.asInstanceOf[untpd.TypeDef].rhs.eq(originalBounds))
      assert(!rebuiltMethod.paramss(1).head.eq(originalTermParameter))
      assert(rebuiltMethod.mods.eq(originalMethod.mods))
      assertEquals(rebuiltMethod.source, originalMethod.source)
      assertEquals(rebuiltMethod.span, originalMethod.span)
      val trees = ExistingUntpdClassMemberFilter.allTrees(rebuiltMethod)
      assert(trees.forall(_.symbol == NoSymbol))
      assert(!trees.exists(_.isInstanceOf[untpd.TypedSplice]))
      assert(rebuiltTemplate.body(1).eq(originalSibling))
    }
  }

  test("G1 parameter result and body edits are independently bounded") {
    withContext {
      def fixture() =
        val root = parseClass(
          "class GenericIndependentEdits:\n  def keep[A](x: AnyVal): AnyVal = x\n"
        )
        val before = root.rhs.asInstanceOf[untpd.Template]
          .body.head.asInstanceOf[untpd.DefDef]
        val captured = ExistingClassUntypedRewrite.capture(root).fold(
          problem => fail(problem.message),
          identity
        )
        val method = captured.method(captured.members.head.ref).fold(
          problem => fail(problem.message),
          identity
        )
        val parameter = method.parameterClauses.head.parameters.head
        (before, captured, method, parameter)

      def raw(result: ExistingClassUntypedRewrite.Result): untpd.DefDef =
        result.tree.rhs.asInstanceOf[untpd.Template]
          .body.head.asInstanceOf[untpd.DefDef]

      val (parameterBefore, parameterCapture, _, parameter) = fixture()
      val parameterOnly = raw(ExistingClassUntypedRewrite(
        parameterCapture,
        parameterCapture.emptyPlan.replaceParameterType(
          parameter.ref,
          quasiquotes.types.TypeNormalForm.STypeIdent("Int")
        ).fold(problem => fail(problem.message), identity)
      ).fold(problem => fail(problem.message), identity))
      assert(parameterOnly.paramss.head.eq(parameterBefore.paramss.head))
      assert(!parameterOnly.paramss(1).head.eq(parameterBefore.paramss(1).head))
      assert(parameterOnly.tpt.eq(parameterBefore.tpt))
      assert(parameterOnly.rhs.eq(parameterBefore.rhs))

      val (resultBefore, resultCapture, resultMethod, _) = fixture()
      val resultOnly = raw(ExistingClassUntypedRewrite(
        resultCapture,
        resultCapture.emptyPlan.replaceResultType(
          resultMethod.ref,
          quasiquotes.types.TypeNormalForm.STypeIdent("Int")
        ).fold(problem => fail(problem.message), identity)
      ).fold(problem => fail(problem.message), identity))
      assert(resultOnly.paramss.head.eq(resultBefore.paramss.head))
      assert(resultOnly.paramss(1).head.eq(resultBefore.paramss(1).head))
      assert(!resultOnly.tpt.eq(resultBefore.tpt))
      assert(resultOnly.rhs.eq(resultBefore.rhs))

      val (bodyBefore, bodyCapture, bodyMethod, bodyParameter) = fixture()
      val reference = bodyMethod.parameterScope.reference(bodyParameter.ref).fold(
        problem => fail(problem.message),
        identity
      )
      val bodyOnly = raw(ExistingClassUntypedRewrite(
        bodyCapture,
        bodyCapture.emptyPlan.replaceBody(bodyMethod.ref, reference)
          .fold(problem => fail(problem.message), identity)
      ).fold(problem => fail(problem.message), identity))
      assert(bodyOnly.paramss.head.eq(bodyBefore.paramss.head))
      assert(bodyOnly.paramss(1).head.eq(bodyBefore.paramss(1).head))
      assert(bodyOnly.tpt.eq(bodyBefore.tpt))
      assert(!bodyOnly.rhs.eq(bodyBefore.rhs))
    }
  }

  test("opaque Type-clause evidence fails closed when null or identity-tampered") {
    withContext {
      val root = parseClass(
        "class GenericEvidence:\n  def keep[A](x: Int): Int = x\n"
      )
      val owner = ExistingUntpdClassMemberFilter.capture(root).fold(
        problem => fail(problem.message),
        identity
      )
      val descriptor = ExistingUntpdOrdinaryMethodDescriptor.capture(owner, 0).fold(
        problem => fail(problem.message),
        identity
      )
      val opaque = descriptor.opaqueTypeClause.getOrElse(fail("opaque Type clause missing"))
      val originalMethod = owner.members.head.tree.asInstanceOf[untpd.DefDef]
      val originalTypeParameter = originalMethod.paramss.head.head.asInstanceOf[untpd.TypeDef]

      assert(opaque.rawClause.eq(originalMethod.paramss.head))
      assert(opaque.typeParameter.eq(originalTypeParameter))
      assert(opaque.bounds.eq(originalTypeParameter.rhs))
      assertEquals(opaque.rawClauseOrdinal, 0)
      assertEquals(opaque.rawTermClauseOrdinal, 1)
      Vector[untpd.Tree](opaque.typeParameter, opaque.bounds).foreach { tree =>
        assert(tree.source.eq(originalMethod.source))
        assert(tree.source.exists)
        assert(tree.span.exists)
      }

      val foreignRoot = parseClass(
        "class ForeignEvidence:\n  def keep[Element](value: Int): Int = value\n"
      )
      val foreignMethod = foreignRoot.rhs.asInstanceOf[untpd.Template]
        .body.head.asInstanceOf[untpd.DefDef]
      val foreignTypeParameter = foreignMethod.paramss.head.head.asInstanceOf[untpd.TypeDef]
      val tampered = Vector(
        descriptor.copy(opaqueTypeClause = null),
        descriptor.copy(opaqueTypeClause = Some(opaque.copy(rawClause = null))),
        descriptor.copy(opaqueTypeClause = Some(opaque.copy(rawClauseOrdinal = 1))),
        descriptor.copy(opaqueTypeClause = Some(opaque.copy(rawTermClauseOrdinal = 0))),
        descriptor.copy(opaqueTypeClause = Some(opaque.copy(typeParameter = foreignTypeParameter))),
        descriptor.copy(opaqueTypeClause = Some(opaque.copy(bounds = foreignTypeParameter.rhs.asInstanceOf[untpd.TypeBoundsTree])))
      )
      tampered.foreach { value =>
        assertEquals(
          ExistingUntpdOrdinaryMethodDescriptor.validate(value).left.toOption.map(_.code),
          Some("DESCRIPTOR_IDENTITY_INVARIANT_FAILED")
        )
      }
    }
  }

  test("reconstruction rejects copied reordered missing foreign and corrupt opaque Type clauses") {
    withContext {
      val root = parseClass(
        "class GenericReconstructionEvidence:\n  def keep[A](x: AnyVal): AnyVal = x\n"
      )
      val owner = ExistingUntpdClassMemberFilter.capture(root).fold(
        problem => fail(problem.message),
        identity
      )
      val descriptor = ExistingUntpdOrdinaryMethodDescriptor.capture(owner, 0).fold(
        problem => fail(problem.message),
        identity
      )
      val parameter = descriptor.parameterClauses.head.head
      val prepared = ExistingUntpdOrdinaryMethodTypeEditPreparation.prepareParameterType(
        descriptor,
        parameter,
        quasiquotes.types.TypeNormalForm.STypeIdent("Int")
      ).fold(problem => fail(problem.message), identity)
      val reconstructed = ExistingUntpdOrdinaryMethodReconstruction.reconstructMethod(
        descriptor,
        Vector(prepared),
        None,
        None
      ).fold(problem => fail(problem.message), identity)
      val validMethod = reconstructed.positionedMethod
      val originalTypeParameter = descriptor.method.paramss.head.head.asInstanceOf[untpd.TypeDef]
      val originalBounds = originalTypeParameter.rhs.asInstanceOf[untpd.TypeBoundsTree]
      val copiedTypeParameter = untpd.cpy.TypeDef(originalTypeParameter)()
      val corruptBounds = untpd.TypeBoundsTree(
        originalBounds.lo,
        originalBounds.hi,
        originalBounds.alias
      )
      val corruptTypeParameter = untpd.cpy.TypeDef(originalTypeParameter)(rhs = corruptBounds)
      val foreignRoot = parseClass(
        "class ForeignReconstructionEvidence:\n  def keep[Element](value: AnyVal): AnyVal = value\n"
      )
      val foreignMethod = foreignRoot.rhs.asInstanceOf[untpd.Template]
        .body.head.asInstanceOf[untpd.DefDef]

      def withClauses(clauses: List[untpd.ParamClause]): untpd.DefDef =
        untpd.cpy.DefDef(validMethod)(
          validMethod.name,
          clauses,
          validMethod.tpt,
          validMethod.rhs
        )

      val malformed = Vector(
        withClauses(List(List(originalTypeParameter), validMethod.paramss(1))),
        withClauses(List(List(copiedTypeParameter), validMethod.paramss(1))),
        withClauses(validMethod.paramss.reverse),
        withClauses(validMethod.paramss.tail),
        withClauses(List(foreignMethod.paramss.head, validMethod.paramss(1))),
        withClauses(List(List(corruptTypeParameter), validMethod.paramss(1)))
      )
      malformed.foreach { method =>
        assertEquals(
          ExistingUntpdOrdinaryMethodReconstruction
            .validateReconstructedMethod(
              reconstructed.copy(positionedMethod = method),
              descriptor
            ).left.toOption.map(_.code),
          Some("FINAL_METHOD_INVARIANT_FAILED")
        )
      }
    }
  }

  test("symbol-bearing opaque TypeDef components fail before descriptor capture") {
    withContext {
      val root = parseClass(
        "class GenericSymbolEvidence:\n  def keep[A](x: Int): Int = x\n"
      )
      val template = root.rhs.asInstanceOf[untpd.Template]
      val method = template.body.head.asInstanceOf[untpd.DefDef]
      val typeParameter = method.paramss.head.head.asInstanceOf[untpd.TypeDef]
      val symbol = newSymbol(NoSymbol, typeName("OpaqueSymbol"), EmptyFlags, NoType)
      val symbolBearingTypeParameter = typeParameter.withType(symbol.typeRef)
        .asInstanceOf[untpd.TypeDef]
      val symbolBearingMethod = untpd.cpy.DefDef(method)(
        method.name,
        List(List(symbolBearingTypeParameter), method.paramss(1)),
        method.tpt,
        method.rhs
      )
      val symbolBearingTemplate = untpd.cpy.Template(template)(
        template.constr,
        template.parentsOrDerived,
        template.derived,
        template.self,
        symbolBearingMethod :: template.body.tail
      )
      val symbolBearingRoot = untpd.cpy.TypeDef(root)(root.name, symbolBearingTemplate)
      val rejected = ExistingUntpdClassMemberFilter.capture(symbolBearingRoot) match
        case Left(_) => true
        case Right(owner) => ExistingUntpdOrdinaryMethodDescriptor.capture(owner, 0).isLeft
      assert(rejected)
    }
  }

  test("generic G1 rejects forged method roles") {
    withContext {
      val root = parseClass(
        "class GenericRoles:\n  def keep[A](x: Int): Int = x\n"
      )
      val template = root.rhs.asInstanceOf[untpd.Template]
      val method = template.body.head.asInstanceOf[untpd.DefDef]

      def captureForged(forged: untpd.DefDef) =
        val changedTemplate = untpd.cpy.Template(template)(
          template.constr,
          template.parentsOrDerived,
          template.derived,
          template.self,
          forged :: template.body.tail
        )
        val changedRoot = untpd.cpy.TypeDef(root)(root.name, changedTemplate)
        ExistingUntpdClassMemberFilter.capture(changedRoot).flatMap { owner =>
          ExistingUntpdOrdinaryMethodDescriptor.capture(owner, 0)
            .left.map(problem => ExistingUntpdClassMemberFilterError(problem.code, problem.detail))
        }

      Vector(
        Flags.Synthetic,
        Flags.Artifact,
        Flags.Accessor,
        Flags.ExtensionMethod,
        Flags.Given,
        Flags.Implicit
      ).foreach { flag =>
        assert(captureForged(method.withMods(untpd.Modifiers(method.mods.flags | flag))).isLeft)
      }
      assert(captureForged(method.withMods(null)).isLeft)
    }
  }

  test("G1 no-op and capture-local references preserve public transaction semantics") {
    withContext {
      val source = "class GenericLocal:\n  def keep[A](x: Int): Int = x\n  val untouched: Int = 7\n"
      val firstRoot = parseClass(source)
      val secondRoot = parseClass(source)
      val first = ExistingClassUntypedRewrite.capture(firstRoot).fold(
        problem => fail(problem.message),
        identity
      )
      val second = ExistingClassUntypedRewrite.capture(secondRoot).fold(
        problem => fail(problem.message),
        identity
      )
      val firstMethod = first.method(first.members.head.ref).fold(
        problem => fail(problem.message),
        identity
      )
      val secondMethod = second.method(second.members.head.ref).fold(
        problem => fail(problem.message),
        identity
      )

      val noOp = ExistingClassUntypedRewrite(first, first.emptyPlan).fold(
        problem => fail(problem.message),
        identity
      )
      assert(!noOp.changed)
      assert(noOp.tree.eq(firstRoot))
      assert(noOp.directMemberIdentities.head.sameObjectAs(first.members.head.identity))
      assert(noOp.directMemberIdentities(1).sameObjectAs(first.members(1).identity))

      val foreignParameter = secondMethod.parameterClauses.head.parameters.head.ref
      val failure = first.emptyPlan.replaceParameterType(
        foreignParameter,
        quasiquotes.types.TypeNormalForm.STypeIdent("Int")
      ).left.toOption.getOrElse(fail("foreign generic ParameterRef unexpectedly succeeded"))
      assertEquals(failure.code, "SELECTION_FAILED")

      val firstParameter = firstMethod.parameterClauses.head.parameters.head
      assert(firstMethod.parameterScope.binder(firstParameter.ref).isRight)
      assert(firstMethod.parameterScope.reference(firstParameter.ref).isRight)
      assertEquals(firstParameter.clauseIndex, 0)
      assertEquals(firstParameter.parameterIndex, 0)
    }
  }

  test("G2 method capture succeeds while dependent parameter and result Type semantics remain opaque") {
    withContext {
      val root = parseClass(
        "class GenericDependent:\n  def keep[A](x: A): A = x\n"
      )
      val captured = ExistingClassUntypedRewrite.capture(root).fold(
        problem => fail(problem.message),
        identity
      )
      val method = captured.method(captured.members.head.ref).fold(
        problem => fail(problem.message),
        identity
      )
      assertEquals(method.parameterClauses.head.parameters.head.declaredType.semantic.left.toOption.map(_.code), Some("UNSUPPORTED_STRUCTURE"))
      assertEquals(method.resultType.semantic.left.toOption.map(_.code), Some("UNSUPPORTED_STRUCTURE"))
    }
  }

  private def parseClass(source: String)(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U068ExistingGenericG1.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed.asInstanceOf[untpd.PackageDef].stats.head.asInstanceOf[untpd.TypeDef]

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
