package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.core.Symbols.NoSymbol
import quasiquotes.neutral.ScalametaDefinitionProjection
import quasiquotes.parser.TermShape
import quasiquotes.types.TypeNormalForm
import scala.meta.*

final class QuotedRootedAtomicOwnerTest extends munit.FunSuite:
  import ExistingClassUntypedRewrite.*
  import QuotedRootedAtomicOwnerModel.*

  test("two quoted methods prepared together emit one fresh owner retaining original siblings"):
    given Context = (new ContextBase).initialCtx
    val root = parse("class C { val before: Int = 1; def echo(value: AnyVal): AnyVal = value; val middle: Int = 2; def magnitude(number: AnyVal): AnyVal = number; val after: Int = 7 }")
    val capture = right(ExistingClassUntypedRewrite.capture(root))
    val edits = Vector("echo", "magnitude").map { name =>
      val selected = right(QuotedUnaryProjection.capture(capture,name))
      Edit(selected,right(QuotedRootedQualifierAuthorityModel.quote(selected)))
    }
    val prepared = right(prepare(capture,edits))
    val result = right(prepared.emit())
    val old = root.rhs.asInstanceOf[untpd.Template].body
    val fresh = result.rhs.asInstanceOf[untpd.Template].body
    assert(!result.eq(root))
    assertEquals(fresh.size,5)
    Vector(0,2,4).foreach(i => assert(fresh(i).eq(old(i))))
    Vector(1,3).foreach { i =>
      assert(!fresh(i).eq(old(i)))
      assertEquals(fresh(i).asInstanceOf[untpd.DefDef].tpt.asInstanceOf[untpd.Ident].name.toString,"Int")
      assertEquals(old(i).asInstanceOf[untpd.DefDef].tpt.asInstanceOf[untpd.Ident].name.toString,"AnyVal")
    }

  test("one or two edits in either order preserve sites, independent scopes and parser-equivalent fresh descendants"):
    given Context = (new ContextBase).initialCtx
    for
      sameName <- Vector(false,true)
      order <- Vector(Vector("echo"),Vector("magnitude"),Vector("echo","magnitude"),Vector("magnitude","echo"))
    do
      val parameter = if sameName then "value" else "number"
      val root = parse(s"class C { val before: Int = 1; def echo(value: AnyVal): AnyVal = value; val middle: Int = 2; def magnitude($parameter: AnyVal): AnyVal = $parameter; val after: Int = 7 }")
      val capture = right(ExistingClassUntypedRewrite.capture(root))
      val before = snapshot(root)
      val edits = order.map(name => edit(capture,name))
      if edits.size == 2 then
        val a = edits(0).selection; val b = edits(1).selection
        assert(a.method.parameterScope.reference(b.parameter.ref).isLeft)
        assert(b.method.parameterScope.reference(a.parameter.ref).isLeft)
        assert(right(a.method.parameterScope.binder(a.parameter.ref)) != right(b.method.parameterScope.binder(b.parameter.ref)))
      val prepared = right(prepare(capture,edits))
      assertEquals(snapshot(root),before)
      val result = right(prepared.emit())
      assertEquals(snapshot(root),before)
      val old = root.rhs.asInstanceOf[untpd.Template]
      val fresh = result.rhs.asInstanceOf[untpd.Template]
      assertEquals((result.source,result.span),(root.source,root.span))
      assertEquals((fresh.source,fresh.span),(old.source,old.span))
      assertEquals(fresh.body.map(_.asInstanceOf[untpd.MemberDef].name.toString),List("before","echo","middle","magnitude","after"))
      old.body.indices.foreach { i =>
        val chosen = edits.find(_.selection.member.index == i)
        assertEquals(fresh.body(i).eq(old.body(i)),chosen.isEmpty)
        chosen.foreach { e =>
          val m = fresh.body(i).asInstanceOf[untpd.DefDef]; val original = old.body(i).asInstanceOf[untpd.DefDef]
          assertEquals((m.source,m.span),(original.source,original.span))
          assertEquals((m.rhs.source,m.rhs.span),(original.rhs.source,original.rhs.span))
          assertEquals((m.paramss.head.head.source,m.paramss.head.head.span),(original.paramss.head.head.source,original.paramss.head.head.span))
          assert(!m.paramss.head.head.eq(original.paramss.head.head))
          assert(!m.tpt.eq(m.paramss.head.head.asInstanceOf[untpd.ValDef].tpt))
          val target = if e.selection.method.diagnosticName == "echo" then "Math" else "StrictMath"
          val argument = e.selection.parameter.diagnosticName
          val parsed = parse(s"class ParserControl { def f($argument: Int): Int = _root_.java.lang.$target.abs($argument) }").rhs.asInstanceOf[untpd.Template].body.head.asInstanceOf[untpd.DefDef].rhs
          val graph = QuotedRootedQualifierAuthorityModel.nodes(m.rhs)
          val expected = QuotedRootedQualifierAuthorityModel.nodes(parsed)
          assertEquals(graph.map(_.getClass.getSimpleName),Vector("Apply","Select","Select","Select","Select","Ident","Ident"))
          graph.tail.zip(expected.tail).foreach { (a,b) =>
            assertEquals((a.span.start,a.span.end,a.span.point),(b.span.start-parsed.span.start,b.span.end-parsed.span.start,b.span.point-parsed.span.start))
            assertEquals(new String(a.source.content),s"_root_.java.lang.$target.abs($argument)")
          }
          assert(graph.forall(t => before.forall((old,_,_,_) => !t.eq(old))))
        }
      }
      assert(ExistingUntpdClassMemberFilter.allTrees(result).forall(t => t.symbol == NoSymbol && !t.isInstanceOf[untpd.TypedSplice]))
      if edits.size == 2 then
        val first = QuotedRootedQualifierAuthorityModel.nodes(prepared.bodyFragment(0))
        val second = QuotedRootedQualifierAuthorityModel.nodes(prepared.bodyFragment(1))
        assert(first.forall(a => second.forall(b => !a.eq(b))))
        assert(!prepared.generatedSource(0).eq(prepared.generatedSource(1)))

  test("later invalid quote yields no prepared transaction in either order while first alone succeeds"):
    given Context = (new ContextBase).initialCtx
    val root = fixture()
    val capture = right(ExistingClassUntypedRewrite.capture(root))
    val first = edit(capture,"echo"); val second = edit(capture,"magnitude")
    val before = snapshot(root)
    assert(right(prepare(capture,Vector(first))).emit().isRight)
    val bad = Vector[Defn.Def](
      q"def other(number: Int): Int = _root_.java.lang.StrictMath.abs(number)",
      q"def magnitude(other: Int): Int = _root_.java.lang.StrictMath.abs(other)",
      q"def magnitude(number: Int): Int = _root_.java.lang.StrictMath.abs(free)",
      q"def magnitude(number: Int): Int = _root_.java.lang.StrictMath.abs(1)",
      q"def magnitude(number: Int): Int = java.lang.StrictMath.abs(number)",
      q"def magnitude(number: Int): Int = _root_.java.lang.System.abs(number)",
      q"def magnitude(number: Int): Int = _root_.java.lang.StrictMath.max(number)",
      q"def magnitude(number: Long): Long = _root_.java.lang.StrictMath.abs(number)",
      q"def magnitude[A](number: Int): Int = _root_.java.lang.StrictMath.abs(number)",
      q"def magnitude(number: Int): Int = { val number: Int = 1; _root_.java.lang.StrictMath.abs(number) }")
    bad.foreach { replacement =>
      val invalid = Edit(second.selection,replacement)
      Vector(Vector(first,invalid),Vector(invalid,first)).foreach { requests =>
        assert(prepare(capture,requests).isLeft,clues(replacement.syntax))
        assertEquals(snapshot(root),before)
        assert(right(ExistingClassUntypedRewrite(capture,capture.emptyPlan)).tree.eq(root))
      }
    }
    assert(prepare(capture,Vector(first,first)).isLeft)
    assert(prepare(capture,Vector(first,second),Vector(first.selection.member.ref)).isLeft)
    assert(prepare(capture,Vector.empty).isLeft)
    val foreign = edit(right(ExistingClassUntypedRewrite.capture(fixture())),"magnitude")
    val recaptured = edit(right(ExistingClassUntypedRewrite.capture(root)),"magnitude")
    Vector(foreign,recaptured).foreach(e => Vector(Vector(first,e),Vector(e,first)).foreach(v => assert(prepare(capture,v).isLeft)))
    val result = right(right(prepare(capture,Vector(first,second))).emit())
    val freshCapture = right(ExistingClassUntypedRewrite.capture(result))
    assert(prepare(freshCapture,Vector(first,second)).isLeft)
    assertEquals(snapshot(root),before)

  test("own binder transport rejects same-text foreign, swapped, free and cross-capture original references"):
    given Context = (new ContextBase).initialCtx
    val capture = right(ExistingClassUntypedRewrite.capture(fixture()))
    val first = edit(capture,"echo"); val second = edit(capture,"magnitude")
    def projected(e: Edit) = ScalametaDefinitionProjection.project(e.replacement).toOption.get.definition.asMethod.get
    val a = projected(first); val b = projected(first); val c = projected(second)
    val own = a.parameterScope.binder(0,0).toOption.get
    val reference = first.selection.parameter.ref
    assert(transport(first.selection,a.body.get,own,reference).isRight)
    assert(transport(first.selection,a.body.get,b.parameterScope.binder(0,0).toOption.get,reference).isLeft)
    assert(transport(first.selection,c.body.get,own,reference).isLeft)
    val function = a.body.get.asInstanceOf[TermShape.Apply].function
    assert(transport(first.selection,TermShape.Apply(function,List(TermShape.Identifier("value",false))),own,reference).isLeft)
    val other = second.selection.parameter.ref
    assert(transport(first.selection,a.body.get,own,other).isLeft)
    val foreign = edit(right(ExistingClassUntypedRewrite.capture(fixture())),"echo").selection
    assert(transport(first.selection,a.body.get,own,foreign.parameter.ref).isLeft)
    val fake = TermShape.Apply(TermShape.Select(TermShape.Identifier("_root_.java.lang.Math",false),"abs"),List(a.body.get.asInstanceOf[TermShape.Apply].arguments.head))
    assert(transport(first.selection,fake,own,reference).isLeft)

  test("in-place generated descendant span and source-content tamper reach snapshot guards without changing root identities"):
    given Context = (new ContextBase).initialCtx
    for mode <- Vector("span","content") do
      val root = fixture(); val before = snapshot(root)
      val capture = right(ExistingClassUntypedRewrite.capture(root))
      val prepared = right(prepare(capture,Vector(edit(capture,"echo"),edit(capture,"magnitude"))))
      val body = prepared.bodyFragment(1)
      val graph = QuotedRootedQualifierAuthorityModel.nodes(body)
      assert(prepared.emit().isRight)
      if mode == "span" then
        val descendant = graph(1)
        val old = descendant.span
        descendant.span = old.withPoint(old.point + 1)
        assert(prepared.bodyFragment(1).eq(body))
        assert(QuotedRootedQualifierAuthorityModel.nodes(body).zip(graph).forall((a,b) => a.eq(b)))
        assertEquals(prepared.emit().left.toOption.map(_.code),Some("STALE_FRAGMENT"))
        descendant.span = old
      else
        val source = prepared.generatedSource(1)
        val bytes = source.content
        val old = bytes(0)
        bytes(0) = 'X'
        assert(prepared.bodyFragment(1).eq(body))
        assert(QuotedRootedQualifierAuthorityModel.nodes(body).zip(graph).forall((a,b) => a.eq(b)))
        assertEquals(prepared.emit().left.toOption.map(_.code),Some("STALE_FRAGMENT"))
        bytes(0) = old
      assert(prepared.emit().isRight)
      assertEquals(snapshot(root),before)

  test("rooted predecessor snapshot guards also reject isolated in-place descendant span and generated bytes"):
    given Context = (new ContextBase).initialCtx
    for mode <- Vector("span","content") do
      val root = fixture(); val capture = right(ExistingClassUntypedRewrite.capture(root))
      val selected = edit(capture,"echo").selection
      val f = right(QuotedRootedQualifierAuthorityModel.prepare(selected))
      val graph = QuotedRootedQualifierAuthorityModel.nodes(f.tree)
      assert(QuotedRootedQualifierAuthorityModel.validate(selected,f,f.tree).isRight)
      if mode == "span" then
        val old = graph(1).span
        graph(1).span = old.withPoint(old.point+1)
        assert(QuotedRootedQualifierAuthorityModel.nodes(f.tree).zip(graph).forall((a,b) => a.eq(b)))
        assertEquals(QuotedRootedQualifierAuthorityModel.validate(selected,f,f.tree).left.toOption.map(_.code),Some("PRIVATE_ROOTED_EVIDENCE_MISMATCH"))
        graph(1).span = old
      else
        val bytes = f.source.content
        val old = bytes(0)
        bytes(0) = 'X'
        assert(QuotedRootedQualifierAuthorityModel.nodes(f.tree).zip(graph).forall((a,b) => a.eq(b)))
        assertEquals(QuotedRootedQualifierAuthorityModel.validate(selected,f,f.tree).left.toOption.map(_.code),Some("PRIVATE_ROOTED_EVIDENCE_MISMATCH"))
        bytes(0) = old
      assert(QuotedRootedQualifierAuthorityModel.validate(selected,f,f.tree).isRight)

  test("stale second original site is rejected after both methods prepare and after first could emit alone"):
    given Context = (new ContextBase).initialCtx
    val root = fixture(); val capture = right(ExistingClassUntypedRewrite.capture(root))
    val first = edit(capture,"echo"); val second = edit(capture,"magnitude")
    assert(right(prepare(capture,Vector(first))).emit().isRight)
    val prepared = right(prepare(capture,Vector(first,second)))
    val parameterType = second.selection.parameter.ref.typeIdentity
    val old = parameterType.span
    parameterType.span = old.withPoint(old.point + 1)
    val tampered = snapshot(root)
    assertEquals(prepared.emit().left.toOption.map(_.code),Some("STALE_ORIGINAL"))
    assertEquals(snapshot(root),tampered) // rejection itself changes nothing
    parameterType.span = old
    assert(prepared.emit().isRight)

  test("G1 G2 and nonstrict or multi-clause methods remain quote negatives"):
    given Context = (new ContextBase).initialCtx
    Vector("def echo[A](value: AnyVal): AnyVal = value", "def echo[A](value: A): A = value",
      "def echo(value: => AnyVal): AnyVal = value", "def echo(value: AnyVal*): AnyVal = value.head",
      "def echo(using value: AnyVal): AnyVal = value", "def echo: AnyVal = 1", "def echo(): AnyVal = 1",
      "def echo(value: AnyVal)(other: AnyVal): AnyVal = value").foreach { method =>
      val c = right(ExistingClassUntypedRewrite.capture(parse(s"class C { $method }")))
      assert(QuotedUnaryProjection.capture(c,"echo").isLeft,clues(method))
    }

  private def fixture()(using Context): untpd.TypeDef = parse("class C { val before: Int = 1; def echo(value: AnyVal): AnyVal = value; val middle: Int = 2; def magnitude(number: AnyVal): AnyVal = number; val after: Int = 7 }")
  private def edit(capture: Capture,name: String)(using Context): Edit =
    val s = right(QuotedUnaryProjection.capture(capture,name))
    Edit(s,right(QuotedRootedQualifierAuthorityModel.quote(s)))
  private def snapshot(root: untpd.Tree)(using Context) = ExistingUntpdClassMemberFilter.allTrees(root).map(t => (t,t.source,t.span,new String(t.source.content)))

  private def right[E,A](value: Either[E,A]): A = value.fold(p => fail(p.toString),identity)
  private def parse(source: String)(using outer: Context): untpd.TypeDef =
    val unit = CompilationUnit("AtomicQuoted.scala",source)
    val reporter = new StoreReporter(null)
    given Context = outer.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse().asInstanceOf[untpd.PackageDef]
    assert(reporter.pendingMessages.isEmpty)
    parsed.stats.collectFirst { case c: untpd.TypeDef => c }.get
