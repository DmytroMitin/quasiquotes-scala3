package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags.EmptyFlags
import dotty.tools.dotc.core.Names.typeName
import dotty.tools.dotc.core.Symbols.{NoSymbol, newSymbol}
import dotty.tools.dotc.core.Types.NoType
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.{NoSource, SourceFile}
import quasiquotes.types.{ResolvedTypeNameId, ResolvedTypeOwnerKind, ResolvedTypeOwnerSegment, TypeNormalForm}

/** Negative-only access to private evidence; positives live in external.consumer. */
final class ExistingDualCanonicalOptionTamperTest extends munit.FunSuite:
  import ExistingUntpdOrdinaryMethodTypeEditPreparation.*

  test("second method cannot consume copied foreign generated handles spans nodes or symbol-bearing type splices"):
    withContext:
      val c = capture()
      val first = right(ExistingUntpdOrdinaryMethodDescriptor.capture(c, 0))
      val second = right(ExistingUntpdOrdinaryMethodDescriptor.capture(c, 1))
      val p = right(prepareParameterType(second, second.parameterClauses.head.head, canonical))
      val r = right(prepareResultType(second, canonical))
      val other = right(prepareResultType(first, canonical))
      val applied = r.positionedType.asInstanceOf[untpd.AppliedTypeTree]
      val constructor = applied.tpt.asInstanceOf[untpd.Select]
      given SourceFile = NoSource
      val copiedConstructor = constructor.cloneIn(constructor.source).withSpan(constructor.span)
      val copied = untpd.AppliedTypeTree(copiedConstructor, applied.args)
        .cloneIn(applied.source).withSpan(applied.span)
      val wrongSource = applied.cloneIn(SourceFile.virtual("forged.scala", "Option[String]"))
      val wrongSpan = applied.cloneIn(applied.source).withSpan(dotty.tools.dotc.util.Spans.NoSpan)
      val foreignConstructor = other.positionedType.asInstanceOf[untpd.AppliedTypeTree].tpt
      val shared = untpd.AppliedTypeTree(foreignConstructor, applied.args)
        .cloneIn(applied.source).withSpan(applied.span)
      val symbol = newSymbol(NoSymbol, typeName("Contaminated"), EmptyFlags, NoType)
      val typed = untpd.TypedSplice(untpd.Ident(typeName("Contaminated")).withType(symbol.typeRef))
        .cloneIn(applied.source).withSpan(applied.span)
      Vector(copied, wrongSource, wrongSpan, shared, typed).foreach { tree =>
        // Updating the selected node alone cannot change the retained exact evidence.
        code(validatePreparedResultType(r.copy(positionedType = tree), second), "FINAL_RESULT_PREPARATION_INVARIANT_FAILED")
        code(validatePreparedParameterType(p.copy(positionedType = tree), second), "FINAL_PARAMETER_PREPARATION_INVARIANT_FAILED")
      }
      // Even a synchronized evidence field cannot excuse bad source/span/TypedSplice.
      Vector(wrongSource, wrongSpan, typed).foreach { tree =>
        val forged = r.copy(positionedType = tree, evidence = r.evidence.copy(positionedType = tree))
        code(validatePreparedResultType(forged, second), "FINAL_RESULT_PREPARATION_INVARIANT_FAILED")
      }
      code(validatePreparedResultType(r, first), "FINAL_RESULT_PREPARATION_INVARIANT_FAILED")
      code(validatePreparedParameterType(p, first), "FINAL_PARAMETER_PREPARATION_INVARIANT_FAILED")

  test("method identity swapped ref and malformed owner reject alongside an otherwise valid canonical edit"):
    withContext:
      val exact = capture()
      val c = ExistingClassUntypedRewrite.capture(exact.originalRoot).toOption.get
      val ms = c.members.map(m => c.method(m.ref).toOption.get)
      val first = ms(0)
      val second = ms(1)
      val swapped = new ExistingClassUntypedRewrite.MethodRef(
        c.exact, 1, first.ref.methodIdentity, second.ref.scopeIdentity
      )
      val p = c.emptyPlan.replaceResultType(first.ref, canonical).toOption.get
      assertEquals(p.replaceResultType(swapped, canonical).left.toOption.map(_.code), Some("SELECTION_FAILED"))
      val before = exact.originalRoot.rhs
      val malformed = untpd.cpy.TypeDef(exact.originalRoot)(exact.originalRoot.name, untpd.EmptyTree)
      assert(ExistingClassUntypedRewrite.capture(malformed).isLeft)
      assert(exact.originalRoot.rhs.eq(before))

  private val canonical =
    def pkg(s: String) = ResolvedTypeOwnerSegment(ResolvedTypeOwnerKind.Package, s)
    TypeNormalForm.STypeApply(
      TypeNormalForm.STypeResolved(ResolvedTypeNameId(Vector(pkg("scala")), "Option")),
      List(TypeNormalForm.STypeResolved(ResolvedTypeNameId(Vector(pkg("java"), pkg("lang")), "String")))
    )
  private def capture()(using outer: Context): ExistingUntpdClassMemberFilter.Capture =
    val unit = CompilationUnit("DualCanonicalTamper.scala", "class C { def first(value: String): String = value; def second(other: String): String = other }")
    val reporter = new StoreReporter(null)
    given Context = outer.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse().asInstanceOf[untpd.PackageDef]
    assert(reporter.pendingMessages.isEmpty)
    right(ExistingUntpdClassMemberFilter.capture(parsed.stats.head))
  private def right[E, A](result: Either[E, A]): A = result.fold(p => fail(p.toString), identity)
  private def code[A](result: Either[Error, A], expected: String): Unit = result match
    case Left(p) => assertEquals(p.code, expected, clues(p))
    case Right(v) => fail(s"expected $expected, got $v")
  private def withContext[A](run: Context ?=> A): A = run(using (new ContextBase).initialCtx)
