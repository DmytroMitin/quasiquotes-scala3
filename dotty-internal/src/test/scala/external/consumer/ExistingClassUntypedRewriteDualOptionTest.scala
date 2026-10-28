package external.consumer

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.Spans.Span

import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite
import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite.*
import quasiquotes.parser.TermShape
import quasiquotes.types.{ResolvedTypeNameId, ResolvedTypeOwnerKind, ResolvedTypeOwnerSegment, TypeNormalForm}

/** Public-only foreign consumer; no private descriptor, oracle or certificate adapter. */
private[consumer] object DualOptionPublicFixture:
  def resolved(owners: String*)(terminal: String): TypeNormalForm =
    TypeNormalForm.STypeResolved(ResolvedTypeNameId(
      owners.toVector.map(ResolvedTypeOwnerSegment(ResolvedTypeOwnerKind.Package, _)), terminal
    ))
  val canonical: TypeNormalForm = TypeNormalForm.STypeApply(
    resolved("scala")("Option"), List(resolved("java", "lang")("String"))
  )
  val orders: Vector[Vector[Int]] = Vector(
    Vector(0, 1, 2, 3, 4, 5), Vector(5, 4, 3, 2, 1, 0),
    Vector(0, 3, 1, 4, 2, 5), Vector(2, 5, 1, 4, 0, 3),
    Vector(3, 4, 5, 0, 1, 2), Vector(4, 0, 5, 1, 3, 2)
  )
  val source: String = """class DualOptionService:
    |  val untouchedBefore: Int = 1
    |  def first(value: String): String = value
    |  def second(other: String): String = other
    |  val untouchedAfter: Int = 2
    |""".stripMargin

  def right[A](value: Either[Failure, A]): A =
    value.fold(p => throw new AssertionError(p.message), identity)
  def methods(c: Capture): Vector[MethodView] =
    c.members.filter(_.kind == MemberKind.Method).map(m => right(c.method(m.ref)))
  def parameter(m: MethodView): ParameterView = m.parameterClauses.head.parameters.head
  def body(m: MethodView): TermShape = right(m.parameterScope.reference(parameter(m).ref))
  def plan(c: Capture, order: Vector[Int], secondType: TypeNormalForm = canonical): EditPlan =
    val ms = methods(c)
    order.foldLeft(c.emptyPlan) { (p, intent) =>
      val index = intent / 3
      val m = ms(index)
      val tpe = if index == 0 then canonical else secondType
      right(intent % 3 match
        case 0 => p.replaceParameterType(parameter(m).ref, tpe)
        case 1 => p.replaceResultType(m.ref, tpe)
        case 2 => p.replaceBody(m.ref, body(m)))
    }
  def members(root: untpd.TypeDef)(using Context): List[untpd.Tree] =
    root.rhs.asInstanceOf[untpd.Template].body
  def rawMethods(root: untpd.TypeDef)(using Context): Vector[untpd.DefDef] =
    members(root).collect { case m: untpd.DefDef => m }.toVector
  def param(m: untpd.DefDef)(using Context): untpd.ValDef = m.paramss.head.head.asInstanceOf[untpd.ValDef]
  def nodes(tree: untpd.Tree): Vector[untpd.Tree] =
    def visit(value: Any): Vector[untpd.Tree] = value match
      case t: untpd.Tree => Vector(t) ++ t.productIterator.toVector.flatMap(visit)
      case xs: List[?] => xs.toVector.flatMap(visit)
      case _ => Vector.empty
    visit(tree)
  def canonicalShape(tree: untpd.Tree): Boolean = tree match
    case untpd.AppliedTypeTree(
        untpd.Select(untpd.Select(untpd.Ident(ro), sc), opt),
        List(untpd.Select(untpd.Select(untpd.Select(untpd.Ident(rs), ja), la), str))) =>
      Vector(ro, sc, opt, rs, ja, la, str).map(_.toString) ==
        Vector("_root_", "scala", "Option", "_root_", "java", "lang", "String") &&
        ro.isTermName && sc.isTermName && opt.isTypeName && rs.isTermName &&
        ja.isTermName && la.isTermName && str.isTypeName
    case _ => false
  def snapshot(root: untpd.Tree): Vector[(untpd.Tree, Any, Any)] =
    nodes(root).map(t => (t, t.source, t.span))
  def parse(source: String)(using outer: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("DualOptionService.scala", source)
    given Context = outer.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    require(reporter.pendingMessages.isEmpty)
    parsed.asInstanceOf[untpd.PackageDef].stats.collect { case c: untpd.TypeDef => c }.head

final class ExistingClassUntypedRewriteDualOptionTest extends munit.FunSuite:
  import DualOptionPublicFixture.*

  test("no-op retains exact owner members and two independent public binder authorities"):
    withContext:
      val root = parse(source)
      val c = right(ExistingClassUntypedRewrite.capture(root))
      val ms = methods(c)
      assertEquals(ms.map(_.diagnosticName), Vector("first", "second"))
      assertEquals(ms.map(m => parameter(m).diagnosticName), Vector("value", "other"))
      assert(ms(0).ref ne ms(1).ref)
      assert(parameter(ms(0)).ref ne parameter(ms(1)).ref)
      assert(right(ms(0).parameterScope.binder(parameter(ms(0)).ref)) !=
        right(ms(1).parameterScope.binder(parameter(ms(1)).ref)))
      ms.foreach { m =>
        assertEquals(parameter(m).declaredType.semantic, Right(TypeNormalForm.STypeIdent("String")))
        assertEquals(m.resultType.semantic, Right(TypeNormalForm.STypeIdent("String")))
      }
      val result = right(ExistingClassUntypedRewrite(c, c.emptyPlan))
      assert(!result.changed)
      assert(result.tree eq root)
      c.members.indices.foreach(i => assert(result.directMemberIdentities(i).sameObjectAs(c.members(i).identity)))

  test("each single selection preserves the other exact method and untouched values"):
    withContext:
      Vector(Vector(0, 1, 2), Vector(3, 4, 5)).foreach { order =>
        val root = parse(source)
        val c = right(ExistingClassUntypedRewrite.capture(root))
        val result = right(ExistingClassUntypedRewrite(c, plan(c, order)))
        verifyOwner(root, c, result, Set(order.head / 3 + 1))
        val index = order.head / 3
        verifyGraph(param(rawMethods(result.tree)(index)).tpt, param(rawMethods(root)(index)).tpt)
        verifyGraph(rawMethods(result.tree)(index).tpt, rawMethods(root)(index).tpt)
      }

  test("six public intents commute across two methods with four disjoint eight-node rooted graphs"):
    withContext:
      val structures = orders.map { order =>
        val root = parse(source)
        val before = snapshot(root)
        val c = right(ExistingClassUntypedRewrite.capture(root))
        val p = plan(c, order)
        assertEquals(p.editCount, 6)
        assert(c.emptyPlan.isEmpty)
        val result = right(ExistingClassUntypedRewrite(c, p)) // Exactly one owner apply.
        verifyOwner(root, c, result, Set(1, 2))
        val old = rawMethods(root)
        val fresh = rawMethods(result.tree)
        val graphs = fresh.zip(old).flatMap { (m, original) =>
          assert(!param(m).eq(param(original)))
          assert(!m.rhs.eq(original.rhs))
          assertEquals(m.rhs.asInstanceOf[untpd.Ident].name.toString, param(m).name.toString)
          assertEquals(m.rhs.source, original.rhs.source)
          assertEquals(m.rhs.span, original.rhs.span)
          Vector(param(m).tpt -> param(original).tpt, m.tpt -> original.tpt)
        }.map { (tree, oldSite) => verifyGraph(tree, oldSite); nodes(tree) }
        graphs.indices.foreach { i => graphs.indices.filter(_ != i).foreach { j =>
          assert(graphs(i).forall(t => graphs(j).forall(u => !t.eq(u))))
          assert(!graphs(i)(1).source.eq(graphs(j)(1).source))
        }}
        assertEquals(snapshot(root), before)
        val recapture = right(ExistingClassUntypedRewrite.capture(result.tree))
        methods(recapture).foreach { m =>
          Vector(parameter(m).declaredType.semantic, m.resultType.semantic).foreach {
            case Left(p) =>
              assertEquals(p.code, "UNSUPPORTED_STRUCTURE")
              assert(p.detail.contains("U038 UNSUPPORTED_SELECTED_TYPE"), clues(p))
            case Right(v) => fail(s"unexpected rooted semantic READ: $v")
          }
        }
        result.tree.toString
      }
      assertEquals(structures.distinct.size, 1)

  test("canonical first and primitive second compose without widening applied grammar"):
    withContext:
      val root = parse(source)
      val c = right(ExistingClassUntypedRewrite.capture(root))
      val result = right(ExistingClassUntypedRewrite(c, plan(c, orders.head, TypeNormalForm.STypeIdent("Int"))))
      verifyOwner(root, c, result, Set(1, 2))
      assertEquals(param(rawMethods(result.tree)(1)).tpt.asInstanceOf[untpd.Ident].name.toString, "Int")
      assertEquals(rawMethods(result.tree)(1).tpt.asInstanceOf[untpd.Ident].name.toString, "Int")

  test("one valid method plus every invalid applied family rejects in both method orders without mutation"):
    withContext:
      val invalid = Vector(
        TypeNormalForm.STypeApply(TypeNormalForm.STypeIdent("Option"), List(TypeNormalForm.STypeIdent("String"))),
        TypeNormalForm.STypeApply(resolved("other")("Option"), List(resolved("java", "lang")("String"))),
        TypeNormalForm.STypeApply(resolved("scala")("Option"), Nil),
        TypeNormalForm.STypeApply(resolved("scala")("Option"), List(canonical)),
        TypeNormalForm.STypeApply(resolved("scala")("Option"), List(resolved("java", "lang")("String"), resolved("java", "lang")("String"))),
        TypeNormalForm.STypeApply(resolved("scala")("Option"), List(resolved("scala")("String")))
      )
      for bad <- invalid; badIndex <- Vector(0, 1); reverse <- Vector(false, true) do
        val root = parse(source)
        val before = snapshot(root)
        val c = right(ExistingClassUntypedRewrite.capture(root))
        val ms = methods(c)
        val indices = if reverse then Vector(1, 0) else Vector(0, 1)
        val p = indices.foldLeft(c.emptyPlan) { (p, i) =>
          val tpe = if i == badIndex then bad else canonical
          val p1 = right(p.replaceParameterType(parameter(ms(i)).ref, tpe))
          right(p1.replaceResultType(ms(i).ref, tpe))
        }
        code(ExistingClassUntypedRewrite(c, p), "UNSUPPORTED_STRUCTURE")
        assertEquals(snapshot(root), before)
        val noOp = right(ExistingClassUntypedRewrite(c, c.emptyPlan))
        assert(noOp.tree eq root)

  test("cross-method scopes reject refs bound bodies and same-spelling impostors; stale captures reject"):
    withContext:
      Vector(source, source.replace("other", "value")).foreach { text =>
        val root = parse(text)
        val c = right(ExistingClassUntypedRewrite.capture(root))
        val ms = methods(c)
        for i <- Vector(0, 1) do
          val other = ms(1 - i)
          code(ms(i).parameterScope.reference(parameter(other).ref), "SELECTION_FAILED")
          code(ms(i).parameterScope.binder(parameter(other).ref), "SELECTION_FAILED")
          code(c.emptyPlan.replaceBody(ms(i).ref, body(other)), "SEMANTIC_FRAGMENT_INVALID")
          val impostor = right(c.emptyPlan.replaceBody(ms(i).ref, TermShape.Identifier(parameter(ms(i)).diagnosticName, false)))
          code(ExistingClassUntypedRewrite(c, impostor), "SEMANTIC_FRAGMENT_INVALID")
        val fresh = right(ExistingClassUntypedRewrite.capture(root))
        val foreign = right(ExistingClassUntypedRewrite.capture(parse(text)))
        Vector(fresh, foreign).foreach { different =>
          val m = methods(different).head
          code(c.emptyPlan.replaceParameterType(parameter(m).ref, canonical), "SELECTION_FAILED")
          code(c.emptyPlan.replaceResultType(m.ref, canonical), "SELECTION_FAILED")
          code(c.emptyPlan.replaceBody(ms.head.ref, body(m)), "SEMANTIC_FRAGMENT_INVALID")
          code(ExistingClassUntypedRewrite(c, different.emptyPlan), "SELECTION_FAILED")
        }
        val output = right(ExistingClassUntypedRewrite(c, plan(c, orders.head)))
        val next = right(ExistingClassUntypedRewrite.capture(output.tree))
        code(next.emptyPlan.replaceBody(ms.head.ref, body(methods(next).head)), "SELECTION_FAILED")
        code(next.emptyPlan.replaceBody(methods(next).head.ref, body(ms.head)), "SEMANTIC_FRAGMENT_INVALID")
      }

  test("duplicates edit-omit conflicts and malformed bodies fail at their actual public stage"):
    withContext:
      val root = parse(source)
      val c = right(ExistingClassUntypedRewrite.capture(root))
      val m = methods(c).head
      val p = plan(c, Vector(0, 1, 2))
      code(p.replaceParameterType(parameter(m).ref, canonical), "EDIT_CONFLICT")
      code(p.replaceResultType(m.ref, canonical), "EDIT_CONFLICT")
      code(p.replaceBody(m.ref, body(m)), "EDIT_CONFLICT")
      code(p.omit(m.member), "EDIT_CONFLICT")
      val omitted = right(c.emptyPlan.omit(m.member))
      code(omitted.replaceResultType(m.ref, canonical), "EDIT_CONFLICT")
      code(omitted.replaceParameterType(parameter(m).ref, canonical), "EDIT_CONFLICT")
      code(omitted.replaceBody(m.ref, body(m)), "EDIT_CONFLICT")
      code(c.emptyPlan.replaceBody(m.ref, null), "SEMANTIC_FRAGMENT_INVALID")
      val invalidBody = right(p.replaceBody(methods(c)(1).ref, TermShape.Literal("not-a-literal")))
      code(ExistingClassUntypedRewrite(c, invalidBody), "UNSUPPORTED_STRUCTURE")
      code(ExistingClassUntypedRewrite.capture(untpd.EmptyTree), "CAPTURE_FAILED")

  test("valid first canonical edits do not admit G2 dependent or unsupported second method types"):
    withContext:
      val unsupported = Vector(
        "def second[A](other: A): A = other",
        "def second[A](other: String): String = other",
        "def second(other: => String): String = other",
        "def second(other: String*): String = other.head",
        "def second(using other: String): String = other",
        "def second(): String = \"b\""
      )
      unsupported.foreach { replacement =>
        val root = parse(source.replace("def second(other: String): String = other", replacement))
        val before = snapshot(root)
        val c = right(ExistingClassUntypedRewrite.capture(root))
        val ms = methods(c)
        val first = plan(c, Vector(0, 1, 2))
        Vector(canonical, TypeNormalForm.STypeIdent("Int")).foreach { tpe =>
          // Primitive edits to wrappers/G1 remain accepted; G2 alone rejects all Type edits.
          if tpe == canonical || replacement.contains("(other: A)") then
            val p = right(first.replaceResultType(ms(1).ref, tpe))
            code(ExistingClassUntypedRewrite(c, p), "UNSUPPORTED_STRUCTURE")
            assertEquals(snapshot(root), before)
        }
      }

  test("independent G1 primitive and G2 body-only siblings retain opaque Type clauses alongside first Option edits"):
    withContext:
      Vector(false, true).foreach { dependent =>
        val signature = if dependent then "def second[A](other: A): A = other" else "def second[A](other: String): String = other"
        val root = parse(source.replace("def second(other: String): String = other", signature))
        val c = right(ExistingClassUntypedRewrite.capture(root))
        val second = methods(c)(1)
        val initial = plan(c, Vector(0, 1, 2))
        val p = if dependent then right(initial.replaceBody(second.ref, body(second)))
          else
            val p1 = right(initial.replaceParameterType(parameter(second).ref, TypeNormalForm.STypeIdent("Int")))
            val p2 = right(p1.replaceResultType(second.ref, TypeNormalForm.STypeIdent("Int")))
            right(p2.replaceBody(second.ref, body(second)))
        val result = right(ExistingClassUntypedRewrite(c, p))
        val old = rawMethods(root)(1)
        val fresh = rawMethods(result.tree)(1)
        assert(fresh.paramss.head.eq(old.paramss.head))
        if dependent then
          assert(fresh.tpt.eq(old.tpt))
          assert(fresh.paramss.flatten.zip(old.paramss.flatten).forall((a, b) => a.eq(b)))
        assert(result.directMemberIdentities(0).sameObjectAs(c.members(0).identity))
        assert(result.directMemberIdentities(3).sameObjectAs(c.members(3).identity))
      }

  private def verifyOwner(root: untpd.TypeDef, c: Capture, result: Result, changed: Set[Int])(using Context): Unit =
    assert(result.changed)
    assert(!result.tree.eq(root))
    assert(!result.tree.rhs.eq(root.rhs))
    assertEquals(result.tree.source, root.source)
    assertEquals(result.tree.span, root.span)
    assertEquals(result.tree.rhs.source, root.rhs.source)
    assertEquals(result.tree.rhs.span, root.rhs.span)
    val old = members(root)
    val fresh = members(result.tree)
    assertEquals(fresh.map(_.asInstanceOf[untpd.MemberDef].name.toString), List("untouchedBefore", "first", "second", "untouchedAfter"))
    assertEquals(fresh.size, 4)
    old.indices.foreach { i =>
      assertEquals(fresh(i).eq(old(i)), !changed(i))
      assertEquals(result.directMemberIdentities(i).sameObjectAs(c.members(i).identity), !changed(i))
    }
    assert(nodes(result.tree).forall(t => t.symbol == NoSymbol && !t.isInstanceOf[untpd.TypedSplice]))

  private def verifyGraph(tree: untpd.Tree, oldSite: untpd.Tree)(using Context): Unit =
    val graph = nodes(tree)
    assertEquals(graph.size, 8)
    assert(canonicalShape(tree), clues(tree))
    assert(!tree.eq(oldSite))
    assert(tree.source.eq(oldSite.source))
    assertEquals(tree.span, oldSite.span)
    val expected = Vector(Span(0, 19, 13), Span(0, 12, 7), Span(0, 6, 0),
      Span(20, 43, 37), Span(20, 36, 32), Span(20, 31, 27), Span(20, 26, 20))
    graph.tail.zip(expected).foreach { (t, span) =>
      assert(t.source.eq(graph(1).source))
      assertEquals(t.source.path, "generated/CanonicalResolvedOptionStringMethodType.scala")
      assertEquals(t.source.content.mkString, "_root_.scala.Option[_root_.java.lang.String]")
      assertEquals(t.span, span)
    }
    graph.indices.foreach(i => graph.indices.filter(_ != i).foreach(j => assert(!graph(i).eq(graph(j)))))

  private def code[A](result: Either[Failure, A], expected: String): Unit = result match
    case Left(p) => assertEquals(p.code, expected, clues(p))
    case Right(v) => fail(s"expected $expected, got $v")
  private def withContext[A](run: Context ?=> A): A = run(using (new ContextBase).initialCtx)
