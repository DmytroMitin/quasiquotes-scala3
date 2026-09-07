package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags.{Accessor, Artifact, EmptyFlags, Erased, ExtensionMethod, Given, Implicit, Synthetic}
import dotty.tools.dotc.core.Names.{termName, typeName}
import dotty.tools.dotc.core.Symbols.{NoSymbol, newSymbol}
import dotty.tools.dotc.core.Types.NoType
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.NoSource
import dotty.tools.dotc.util.Spans.Span

class ExistingUntpdOrdinaryMethodDescriptorTest extends munit.FunSuite:
  import ExistingUntpdOrdinaryMethodDescriptor.*

  test("differential exact handles ordered clauses provenance and immutable opaque islands") {
    withContext {
      val sources = Vector(
        "class One:\n  def f(x: Int): Int = x\n",
        "class Two:\n  def f(x: Int, y: String): String = y\n",
        "class RenamedOne:\n  def transform(entries: Map[String, List[Int]]): (Int, String) = entries.toList match { case (k, v) :: _ => (v.sum, k); case Nil => (0, \"\") }\n",
        "class RenamedTwo:\n  def merge(entries: Map[String, List[Int]], fallback: Either[Int, String]): (Int, String) = entries.toList match { case (k, v) :: _ => (v.sum, k); case Nil => (0, fallback.toString) }\n"
      )
      sources.foreach { source =>
        val owner = capture(parseSingleTypeDef(source))
        val before = snapshot(owner.originalRoot)
        val method = owner.members(0).tree.asInstanceOf[untpd.DefDef]
        val expected = method.paramss.head.map(_.asInstanceOf[untpd.ValDef]).toVector
        val first = descriptor(owner, 0)
        val second = descriptor(owner, 0)
        assert(!first.eq(second))
        Vector(first, second).foreach { value =>
          assert(value.captured.eq(owner))
          assertEquals(value.memberIndex, 0)
          assert(value.method.eq(method))
          assertEquals(value.parameterClauses.size, 1)
          assertEquals(value.parameterClauses.head.size, expected.size)
          value.parameterClauses.head.zip(expected).foreach { (actual, original) =>
            assert(actual.tree.eq(original))
            assert(actual.tpt.eq(original.tpt))
          }
          val sites = Vector(value.method, value.resultType, value.rhs,
            value.captured.originalRoot, value.captured.originalTemplate) ++
            value.parameterClauses.head.flatMap(p => Vector(p.tree, p.tpt))
          sites.foreach { site =>
            val original = before.find(_.tree.eq(site)).get
            assert(site.source.eq(original.source))
            assertEquals(site.span, original.span)
            assert(site.source.exists && site.span.exists)
          }
          assertEquals(validate(value), Right(()))
        }
        if expected.size == 1 then
          val v = ExistingUntpdSingleParameterMethodView.capture(owner, 0).toOption.get
          assert(first.captured.eq(v.captured)); assertEquals(first.memberIndex, v.memberIndex)
          assert(first.method.eq(v.method)); assert(first.parameterClauses.head.head.tree.eq(v.parameter))
          assert(first.parameterClauses.head.head.tpt.eq(v.parameterType))
          assert(first.resultType.eq(v.resultType)); assert(first.rhs.eq(v.rhs))
        else
          val v = ExistingUntpdTwoParameterMethodView.capture(owner, 0).toOption.get
          assert(first.captured.eq(v.captured)); assertEquals(first.memberIndex, v.memberIndex)
          assert(first.method.eq(v.method)); assert(first.parameterClauses.head(0).tree.eq(v.firstParameter))
          assert(first.parameterClauses.head(0).tpt.eq(v.firstParameterType))
          assert(first.parameterClauses.head(1).tree.eq(v.secondParameter))
          assert(first.parameterClauses.head(1).tpt.eq(v.secondParameterType))
          assert(first.resultType.eq(v.resultType)); assert(first.rhs.eq(v.rhs))
        assertSnapshotUnchanged(before)
      }
    }
  }

  test("same named members use captured index and diagnostic names have no authority") {
    withContext {
      val owner = capture(parseSingleTypeDef("class Overloads:\n  def same(x: Int): Int = x\n  def same(x: String, y: Int): String = x\n"))
      Vector(0, 1).foreach { index =>
        val value = descriptor(owner, index)
        assert(value.method.eq(owner.members(index).tree))
        assertEquals(value.parameterClauses.head.size, index + 1)
        assertEquals(validate(value.copy(diagnosticMethodName = null,
          parameterClauses = value.parameterClauses.map(_.map(_.copy(diagnosticName = "ignored"))))), Right(()))
      }
    }
  }

  test("forged descriptors cannot change raw identity topology or capture linkage") {
    withContext {
      Vector("x: Int", "x: Int, y: String").foreach { params =>
        val owner = capture(parseSingleTypeDef(s"class A:\n  def f($params): Int = 1\n"))
        val value = descriptor(owner, 0)
        val p = value.parameterClauses.head.head
        val foreign = capture(parseSingleTypeDef(s"class A:\n  def f($params): Int = 1\n"))
        val forgeries = Vector(
          null, value.copy(captured = null), value.copy(captured = foreign),
          value.copy(memberIndex = -1), value.copy(method = descriptor(foreign, 0).method),
          value.copy(method = null), value.copy(resultType = p.tpt), value.copy(resultType = null),
          value.copy(rhs = value.resultType), value.copy(rhs = null),
          value.copy(parameterClauses = null), value.copy(parameterClauses = Vector(null)),
          value.copy(parameterClauses = Vector(Vector(null))), value.copy(parameterClauses = Vector.empty),
          value.copy(parameterClauses = Vector(Vector.empty)),
          value.copy(parameterClauses = Vector(value.parameterClauses.head, value.parameterClauses.head)),
          value.copy(parameterClauses = Vector(Vector(p.copy(tree = null)))),
          value.copy(parameterClauses = Vector(value.parameterClauses.head.updated(0, p.copy(tpt = value.resultType))))
        )
        forgeries.foreach(v => assertCode(validate(v), "DESCRIPTOR_IDENTITY_INVARIANT_FAILED"))
        if value.parameterClauses.head.size == 2 then
          assertCode(validate(value.copy(parameterClauses = Vector(value.parameterClauses.head.reverse))), "DESCRIPTOR_IDENTITY_INVARIANT_FAILED")
      }
    }
  }

  test("missing stale and malformed owner captures reject structurally") {
    withContext {
      assertCode(ExistingUntpdOrdinaryMethodDescriptor.capture(null, 0), "CAPTURE_REQUIRED")
      val owner = capture(parseSingleTypeDef("class A:\n  val a: Int = 1\n  def f(x: Int): Int = x\n"))
      Vector(-1, 2, Int.MaxValue).foreach(i => assertCode(ExistingUntpdOrdinaryMethodDescriptor.capture(owner, i), "MEMBER_INDEX_NOT_CAPTURED"))
      assertCode(ExistingUntpdOrdinaryMethodDescriptor.capture(owner, 0), "SELECTED_MEMBER_NOT_METHOD")
      val malformed = Vector(owner.copy(originalRoot = null), owner.copy(originalTemplate = null),
        owner.copy(members = null), owner.copy(members = owner.members.reverse),
        owner.copy(members = owner.members.updated(1, null)),
        owner.copy(members = owner.members.updated(1, ExistingUntpdClassMemberFilter.Member(1, null))),
        owner.copy(originalRoot = untpd.cpy.TypeDef(owner.originalRoot)(owner.originalRoot.name, null)))
      malformed.foreach(c => assertRejected(ExistingUntpdOrdinaryMethodDescriptor.capture(c, 1)))
    }
  }

  test("accepted authority union rejects unsupported parsed method families") {
    withContext {
      Vector("def f: Int = 1", "def f(): Int = 1", "def f(x: Int, y: Int, z: Int): Int = x",
        "def f(x: Int)(y: Int): Int = x", "def f[A](x: A): A = x",
        "def f[A](x: A, y: A): A = x", "def f(using x: Int): Int = x",
        "def f(using x: Int, y: Int): Int = x", "implicit def f(x: Int): Int = x",
        "def f(x: Int = 1): Int = x", "def f(x: Int, y: Int = 1): Int = x").foreach { declaration =>
        val owner = capture(parseSingleTypeDef(s"class A:\n  $declaration\n"))
        assert(ExistingUntpdSingleParameterMethodView.capture(owner, 0).isLeft)
        assert(ExistingUntpdTwoParameterMethodView.capture(owner, 0).isLeft)
        assertRejected(ExistingUntpdOrdinaryMethodDescriptor.capture(owner, 0))
      }
    }
  }

  test("both arities reject forged roles clauses defaults missing fields and raw null seams") {
    withContext {
      given dotty.tools.dotc.util.SourceFile = NoSource
      Vector("x: Int", "x: Int, y: String").foreach { params =>
        val m = methodFrom(s"class A:\n  def f($params): Int = 1\n")
        val ps = m.paramss.head.map(_.asInstanceOf[untpd.ValDef])
        def reject(malformed: untpd.DefDef): Unit =
          assertRejected(ExistingUntpdOrdinaryMethodDescriptor.capture(captureAround(malformed), 0))
        Vector(Synthetic, Artifact, Accessor, ExtensionMethod, Given, Implicit).foreach { flag =>
          reject(m.withMods(untpd.Modifiers(m.mods.flags | flag)))
        }
        reject(m.withMods(null))
        reject(copyMethod(m, null, m.tpt, m.rhs))
        reject(copyMethod(m, List(null), m.tpt, m.rhs))
        ps.indices.foreach { index =>
          val p = ps(index)
          def withP(p: untpd.ValDef): Unit = reject(copyMethod(m, List(ps.updated(index, p)), m.tpt, m.rhs))
          withP(null)
          Vector(Implicit, Given, Erased).foreach(flag => withP(p.withMods(untpd.Modifiers(p.mods.flags | flag))))
          Vector(null, untpd.TypeTree()).foreach(t => withP(untpd.cpy.ValDef(p)(p.name, t, p.rhs)))
          withP(untpd.cpy.ValDef(p)(p.name, p.tpt, m.rhs))
          withP(untpd.cpy.ValDef(p)(p.name, p.tpt, null))
          withP(untpd.cpy.ValDef(p)(p.name, untpd.AppliedTypeTree(p.tpt, List(null)), p.rhs))
        }
        Vector(null, untpd.TypeTree()).foreach(t => reject(copyMethod(m, m.paramss, t, m.rhs)))
        Vector(null, untpd.EmptyTree, untpd.Apply(m.rhs, null.asInstanceOf[List[untpd.Tree]]), untpd.Apply(m.rhs, List(null))).foreach(t => reject(copyMethod(m, m.paramss, m.tpt, t)))
      }
      val constructor = parseSingleTypeDef("class Constructor\n").rhs.asInstanceOf[untpd.Template].constr
      assertRejected(ExistingUntpdOrdinaryMethodDescriptor.capture(captureAround(constructor), 0))
    }
  }

  test("both arities reject owner and selected slot symbols TypedSplice and null children") {
    withContext {
      given dotty.tools.dotc.util.SourceFile = NoSource
      val symbol = newSymbol(NoSymbol, termName("descriptorSymbol"), EmptyFlags, NoType)
      val symbolic = untpd.Ident(termName("symbolic")).withType(symbol.termRef)
      Vector("x: Int", "x: Int, y: String").foreach { params =>
        val m = methodFrom(s"class A:\n  def f($params): Int = 1\n")
        Vector(symbolic, untpd.TypedSplice(symbolic), untpd.Apply(m.rhs, null.asInstanceOf[List[untpd.Tree]])).foreach { bad =>
          assertRejected(ExistingUntpdOrdinaryMethodDescriptor.capture(captureAround(copyMethod(m, m.paramss, m.tpt, bad)), 0))
          val owner = captureAround(m)
          val t = owner.originalTemplate
          val changed = untpd.cpy.Template(t)(t.constr, t.parentsOrDerived, t.derived, t.self, List(m, bad))
          val root = untpd.cpy.TypeDef(owner.originalRoot)(owner.originalRoot.name, changed)
          val forged = owner.copy(originalRoot = root, originalTemplate = changed,
            members = Vector(ExistingUntpdClassMemberFilter.Member(0, m), ExistingUntpdClassMemberFilter.Member(1, bad)))
          assertRejected(ExistingUntpdOrdinaryMethodDescriptor.capture(forged, 0))
        }
        val owner = captureAround(m)
        val t = owner.originalTemplate
        val changed = untpd.cpy.Template(t)(t.constr, t.parentsOrDerived, t.derived, t.self, null)
        assertRejected(ExistingUntpdOrdinaryMethodDescriptor.capture(owner.copy(
          originalRoot = untpd.cpy.TypeDef(owner.originalRoot)(owner.originalRoot.name, changed), originalTemplate = changed), 0))
      }
    }
  }

  test("null sources and malformed annotation graphs reject before delegated traversal") {
    withContext {
      given dotty.tools.dotc.util.SourceFile = NoSource
      val symbol = newSymbol(NoSymbol, termName("annotationSymbol"), EmptyFlags, NoType)
      val symbolic = untpd.Ident(termName("symbolic")).withType(symbol.termRef)
      Vector("x: Int", "x: Int, y: String").foreach { params =>
        val m = methodFrom(s"class A:\n  def f($params): Int = 1\n")
        val ps = m.paramss.head.map(_.asInstanceOf[untpd.ValDef])
        def reject(method: untpd.DefDef): Unit =
          assertRejected(ExistingUntpdOrdinaryMethodDescriptor.capture(captureAround(method), 0))
        reject(copyMethod(m, m.paramss, m.tpt, m.rhs.cloneIn(null)))
        reject(copyMethod(m, m.paramss, m.tpt.cloneIn(null), m.rhs))
        reject(m.cloneIn(null))
        ps.indices.foreach { i =>
          val p = ps(i)
          reject(copyMethod(m, List(ps.updated(i, p.cloneIn(null))), m.tpt, m.rhs))
          reject(copyMethod(m, List(ps.updated(i, untpd.cpy.ValDef(p)(p.name, p.tpt.cloneIn(null), p.rhs))), m.tpt, m.rhs))
        }
        Vector(null, List(null), List(symbolic), List(untpd.TypedSplice(symbolic)),
          List(m.rhs.cloneIn(null))).foreach { annotations =>
          reject(m.withMods(m.mods.copy(annotations = annotations)))
          ps.indices.foreach { i =>
            val p = ps(i)
            reject(copyMethod(m, List(ps.updated(i, p.withMods(p.mods.copy(annotations = annotations)))), m.tpt, m.rhs))
          }
          val owner = captureAround(m)
          val root = owner.originalRoot.withMods(owner.originalRoot.mods.copy(annotations = annotations))
          assertRejected(ExistingUntpdOrdinaryMethodDescriptor.capture(owner.copy(originalRoot = root), 0))
        }
        val owner = captureAround(m)
        assertRejected(ExistingUntpdOrdinaryMethodDescriptor.capture(owner.copy(originalRoot = owner.originalRoot.cloneIn(null)), 0))
      }
    }
  }

  test("ordinary annotations remain exact while erased forged single parameters are excluded") {
    withContext {
      val owner = capture(parseSingleTypeDef("class Annotated:\n  @deprecated(\"old\", \"1\") def f(x: Int): Int = x\n"))
      val value = descriptor(owner, 0)
      val annotations = value.method.mods.annotations
      val again = descriptor(owner, 0)
      assert(again.method.eq(value.method))
      assert(again.method.mods.annotations.eq(annotations))
      val m = value.method
      val p = m.paramss.head.head.asInstanceOf[untpd.ValDef]
      val erased = p.withMods(p.mods.copy(flags = p.mods.flags | Erased))
      val forged = captureAround(copyMethod(m, List(List(erased)), m.tpt, m.rhs))
      assert(ExistingUntpdSingleParameterMethodView.capture(forged, 0).isRight)
      assertCode(ExistingUntpdOrdinaryMethodDescriptor.capture(forged, 0), "CONTEXTUAL_PARAMETER_UNSUPPORTED")
    }
  }

  test("deferred owner fields reject without forcing or mutating the original graph") {
    withContext {
      Vector("x: Int", "x: Int, y: String").foreach { params =>
        val m = methodFrom(s"class A:\n  def f($params): Int = 1\n")
        val owner = captureAround(m)
        val t = owner.originalTemplate
        var forced = false
        val deferred = new dotty.tools.dotc.ast.Trees.Lazy[untpd.Tree]:
          def complete(using Context): untpd.Tree =
            forced = true
            null
        val constr = untpd.cpy.DefDef(t.constr)(t.constr.name, t.constr.paramss, t.constr.tpt, deferred)
        val changed = untpd.cpy.Template(t)(constr, t.parentsOrDerived, t.derived, t.self, t.body)
        val forged = owner.copy(originalRoot = untpd.cpy.TypeDef(owner.originalRoot)(owner.originalRoot.name, changed),
          originalTemplate = changed)
        assertRejected(ExistingUntpdOrdinaryMethodDescriptor.capture(forged, 0))
        assert(!forced)
        assert(constr.unforcedRhs.eq(deferred))
      }
      val owner = capture(parseSingleTypeDef("class NullLiteral:\n  def f(x: Int): Any = null\n"))
      assertEquals(validate(descriptor(owner, 0)), Right(()))
    }
  }

  test("foreign template linkage rejects without reading its deferred body") {
    withContext {
      val m = methodFrom("class A:\n  def f(x: Int): Int = 1\n")
      val owner = captureAround(m)
      val t = owner.originalTemplate
      var forced = false
      val deferred = new dotty.tools.dotc.ast.Trees.Lazy[List[untpd.Tree]]:
        def complete(using Context): List[untpd.Tree] =
          forced = true
          List(m)
      val foreign = untpd.cpy.Template(t)(t.constr, t.parentsOrDerived, t.derived, t.self, deferred)
      assertRejected(ExistingUntpdOrdinaryMethodDescriptor.capture(owner.copy(originalTemplate = foreign), 0))
      assert(!forced)
      assert(foreign.unforcedBody.eq(deferred))
    }
  }

  private def descriptor(c: ExistingUntpdClassMemberFilter.Capture, i: Int)(using Context): Descriptor =
    ExistingUntpdOrdinaryMethodDescriptor.capture(c, i).fold(e => fail(e.message), identity)

  private def assertRejected[A](value: Either[Error, A]): Unit = value match
    case Left(e) => assert(e.code.nonEmpty && e.detail.nonEmpty)
    case Right(_) => fail("expected structured rejection")

  private def assertCode[A](value: Either[Error, A], code: String): Unit = value match
    case Left(e) => assertEquals(e.code, code)
    case Right(_) => fail(s"expected $code")

  private def copyMethod(
      method: untpd.DefDef,
      paramss: List[untpd.ParamClause],
      resultType: untpd.Tree,
      rhs: untpd.Tree
  )(using Context): untpd.DefDef =
    untpd.cpy.DefDef(method)(method.name, paramss, resultType, rhs)

  private def methodFrom(source: String)(using Context): untpd.DefDef =
    parseSingleTypeDef(source).rhs.asInstanceOf[untpd.Template].body.head.asInstanceOf[untpd.DefDef]

  private def capture(root: untpd.TypeDef)(using Context): ExistingUntpdClassMemberFilter.Capture =
    ExistingUntpdClassMemberFilter.capture(root).fold(problem => fail(problem.message), identity)

  private def captureAround(member: untpd.Tree)(using Context): ExistingUntpdClassMemberFilter.Capture =
    val parsed = parseSingleTypeDef("class CaptureShell\n")
    val oldTemplate = parsed.rhs.asInstanceOf[untpd.Template]
    val template = untpd.cpy.Template(oldTemplate)(
      oldTemplate.constr,
      oldTemplate.parentsOrDerived,
      oldTemplate.derived,
      oldTemplate.self,
      List(member)
    )
    val root = untpd.cpy.TypeDef(parsed)(parsed.name, template)
    ExistingUntpdClassMemberFilter.Capture(
      root,
      template,
      Vector(ExistingUntpdClassMemberFilter.Member(0, member))
    )

  private def parseSingleTypeDef(source: String)(using outer: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("OrdinaryMethodDescriptor.scala", source)
    given Context = outer.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed match
      case packageDef: untpd.PackageDef =>
        packageDef.stats match
          case (root: untpd.TypeDef) :: Nil => root
          case other => fail(s"expected one TypeDef, found $other")
      case other => fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")

  private final case class TreeSnapshot(
      tree: untpd.Tree,
      source: dotty.tools.dotc.util.SourceFile,
      span: Span,
      symbol: dotty.tools.dotc.core.Symbols.Symbol,
      modifiers: Option[dotty.tools.dotc.core.Flags.FlagSet]
  )

  private def snapshot(tree: untpd.Tree)(using Context): Vector[TreeSnapshot] =
    ExistingUntpdClassMemberFilter.allTrees(tree).map { node =>
      val modifiers = node match
        case member: untpd.MemberDef => Some(member.mods.flags)
        case _ => None
      TreeSnapshot(node, node.source, node.span, node.symbol, modifiers)
    }

  private def assertSnapshotUnchanged(before: Vector[TreeSnapshot])(using Context): Unit =
    val after = ExistingUntpdClassMemberFilter.allTrees(before.head.tree)
    assertEquals(after.size, before.size)
    after.zip(before).foreach { case (actual, expected) =>
      assert(actual.eq(expected.tree))
      assertEquals(actual.source, expected.source)
      assertEquals(actual.span, expected.span)
      assertEquals(actual.symbol, expected.symbol)
      expected.modifiers.foreach(flags =>
        assertEquals(actual.asInstanceOf[untpd.MemberDef].mods.flags, flags)
      )
    }

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
