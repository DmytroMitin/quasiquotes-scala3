package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import quasiquotes.neutral.ScalametaDefinitionProjection
import quasiquotes.parser.TermShape
import quasiquotes.types.TypeNormalForm
import scala.meta.*

final class ExistingMethodQuotedRewriteAuthorityTest extends munit.FunSuite:
  import ExistingClassUntypedRewrite.*
  private val replacement = q"def echo(value: Int): Int = _root_.java.lang.Math.abs(value)"

  test("own output binder cannot be substituted by a same-text foreign binder or free reference"):
    given Context = (new ContextBase).initialCtx
    val capture = right(ExistingClassUntypedRewrite.capture(fixture()))
    val selected = right(ExistingMethodQuotedRewrite.select(capture,capture.members(1).ref))
    val a = right(ScalametaDefinitionProjection.project(replacement)).definition.asMethod.get
    val b = right(ScalametaDefinitionProjection.project(replacement)).definition.asMethod.get
    val own = right(a.parameterScope.binder(0,0))
    val reference = right(selected.method.parameterScope.reference(selected.parameter.ref))
    assert(ExistingMethodQuotedRewrite.transport(a.body.get,own,reference).isRight)
    assert(ExistingMethodQuotedRewrite.transport(a.body.get,right(b.parameterScope.binder(0,0)),reference).isLeft)
    val other = right(ExistingClassUntypedRewrite.capture(fixture()))
    val foreign = right(ExistingMethodQuotedRewrite.select(other,other.members(1).ref))
    val foreignRef = right(foreign.method.parameterScope.reference(foreign.parameter.ref))
    val wrongTarget = right(ExistingMethodQuotedRewrite.transport(a.body.get,own,foreignRef))
    assert(capture.emptyPlan.replaceQuotedBody(selected,wrongTarget).isLeft)
    val call = a.body.get.asInstanceOf[TermShape.Apply]
    assert(ExistingMethodQuotedRewrite.transport(TermShape.Apply(call.function,List(TermShape.Identifier("value",false))),own,reference).isLeft)
    assert(ExistingMethodQuotedRewrite.transport(TermShape.Apply(TermShape.Select(TermShape.Identifier("_root_.java.lang.Math",false),"abs"),call.arguments),own,reference).isLeft)

  test("selection and saved intent reject source and site tampering and different Contexts"):
    for mode <- Vector("source","span","context") do
      given Context = (new ContextBase).initialCtx
      val root = fixture()
      val capture = right(ExistingClassUntypedRewrite.capture(root))
      val selected = right(ExistingMethodQuotedRewrite.select(capture,capture.members(1).ref))
      val plan = right(selected.replace(capture.emptyPlan,replacement))
      if mode == "source" then root.source.content(0) = 'X'
      else if mode == "span" then root.span = root.span.withEnd(root.span.end - 1)
      else
        val foreign = (new ContextBase).initialCtx
        assert(ExistingMethodQuotedRewrite.select(capture,capture.members(1).ref)(using foreign).isLeft)
        assert(selected.replace(capture.emptyPlan,replacement)(using foreign).isLeft)
        assert(ExistingClassUntypedRewrite(capture,plan)(using foreign).isLeft)
      if mode != "context" then
        assert(selected.replace(capture.emptyPlan,replacement).isLeft,clues(mode))
        assert(ExistingClassUntypedRewrite(capture,plan).isLeft,clues(mode))

  test("selection cannot recertify source mutated after Capture"):
    given Context = (new ContextBase).initialCtx
    val root = fixture()
    val capture = right(ExistingClassUntypedRewrite.capture(root))
    root.source.content(0) = 'X'
    assert(ExistingMethodQuotedRewrite.select(capture,capture.members(1).ref).isLeft)

  test("selection rejects a duplicated exact method occurrence"):
    given Context = (new ContextBase).initialCtx
    val original = fixture()
    val template = original.rhs.asInstanceOf[untpd.Template]
    val duplicateTemplate = untpd.cpy.Template(template)(template.constr,template.parentsOrDerived,template.derived,template.self,template.body :+ template.body(1))
    val duplicate = untpd.cpy.TypeDef(original)(original.name,duplicateTemplate)
    val capturedDuplicate = right(ExistingClassUntypedRewrite.capture(duplicate))
    assert(ExistingMethodQuotedRewrite.select(capturedDuplicate,capturedDuplicate.members(1).ref).isLeft)

  test("selection rejects duplicated or overloaded direct method names"):
    given Context = (new ContextBase).initialCtx
    Vector("AnyVal","Int").foreach { otherType =>
      val root = parse(s"class C { def echo(value: AnyVal): AnyVal = value; def echo(other: $otherType): $otherType = other }")
      val capture = right(ExistingClassUntypedRewrite.capture(root))
      capture.members.foreach(member => assert(ExistingMethodQuotedRewrite.select(capture,member.ref).isLeft))
    }

  test("disjoint intents remain immutable while conflicting or second quoted operations fail"):
    given Context = (new ContextBase).initialCtx
    val capture = right(ExistingClassUntypedRewrite.capture(fixture()))
    val selected = right(ExistingMethodQuotedRewrite.select(capture,capture.members(1).ref))
    val sibling = right(capture.method(capture.members(3).ref))
    val ordinary = right(capture.emptyPlan.replaceBody(sibling.ref,TermShape.Literal("12")))
    val combined = right(selected.replace(ordinary,replacement))
    assertEquals(ordinary.editCount,1); assertEquals(combined.editCount,4)
    val output = right(ExistingClassUntypedRewrite(capture,combined)).tree.rhs.asInstanceOf[untpd.Template]
    assertEquals(output.body(3).asInstanceOf[untpd.DefDef].rhs.asInstanceOf[untpd.Number].digits,"12")
    val existingBody = right(capture.emptyPlan.replaceBody(selected.method.ref,right(selected.method.parameterScope.reference(selected.parameter.ref))))
    assert(selected.replace(existingBody,replacement).isLeft)
    assert(combined.replaceBody(selected.method.ref,TermShape.Literal("0")).isLeft)
    val second = right(ExistingMethodQuotedRewrite.select(capture,capture.members(4).ref))
    val secondQuote = q"def magnitude(number: Int): Int = _root_.java.lang.StrictMath.abs(number)"
    assert(second.replace(combined,secondQuote).isLeft)
    assertEquals(combined.editCount,4)
    val quotedOnly = right(selected.replace(capture.emptyPlan,replacement))
    val omittedSibling = right(quotedOnly.omit(capture.members(0).ref))
    assertEquals(quotedOnly.editCount,3); assertEquals(omittedSibling.editCount,4)
    assertEquals(right(ExistingClassUntypedRewrite(capture,omittedSibling)).directMemberIdentities.size,4)

  test("original private and final modifiers survive the quoted rewrite by exact identity"):
    given Context = (new ContextBase).initialCtx
    val root = parse("class C { private final def echo(value: AnyVal): AnyVal = value }")
    val original = root.rhs.asInstanceOf[untpd.Template].body.head.asInstanceOf[untpd.DefDef]
    val capture = right(ExistingClassUntypedRewrite.capture(root))
    val selected = right(ExistingMethodQuotedRewrite.select(capture,capture.members.head.ref))
    val result = right(ExistingClassUntypedRewrite(capture,right(selected.replace(capture.emptyPlan,replacement))))
    val changed = result.tree.rhs.asInstanceOf[untpd.Template].body.head.asInstanceOf[untpd.DefDef]
    assert(changed.mods.eq(original.mods))

  test("erased raw parameter cannot enter the quote view"):
    given Context = (new ContextBase).initialCtx
    val root = fixture()
    val template = root.rhs.asInstanceOf[untpd.Template]
    val method = template.body(1).asInstanceOf[untpd.DefDef]
    val parameter = method.paramss.head.head.asInstanceOf[untpd.ValDef]
    val erased = parameter.withMods(parameter.mods.copy(flags = parameter.mods.flags | dotty.tools.dotc.core.Flags.Erased))
    val forged = untpd.cpy.DefDef(method)(method.name,List(List(erased)),method.tpt,method.rhs)
    val owner = untpd.cpy.TypeDef(root)(root.name,untpd.cpy.Template(template)(template.constr,template.parentsOrDerived,template.derived,template.self,template.body.updated(1,forged)))
    val capture = right(ExistingClassUntypedRewrite.capture(owner))
    assert(ExistingMethodQuotedRewrite.select(capture,capture.members(1).ref).isLeft)

  test("rooted body preparation retains exact parser positions and rejects copied evidence and mutated origins"):
    given Context = (new ContextBase).initialCtx
    for mode <- Vector("copy","evidence","source","span","graph") do
      val capture = right(ExistingClassUntypedRewrite.capture(fixture()))
      val selected = right(ExistingMethodQuotedRewrite.select(capture,capture.members(1).ref))
      val plan = right(selected.replace(capture.emptyPlan,replacement))
      val intent = plan.quotedBodies.head
      val descriptor = right(ExistingUntpdOrdinaryMethodDescriptor.capture(capture.exact,1))
      val prepared = right(ExistingUntpdOrdinaryMethodRhsEditPreparation.prepareRootedQuotedBody(descriptor) { scope =>
        selected.method.ref.scopeIdentity.adaptForPreparation(intent.replacement,descriptor,scope)
      })
      val graph = ExistingQuotedRootedBody.nodes(prepared.positionedReplacement)
      assertEquals(graph.map(_.getClass.getSimpleName),Vector("Apply","Select","Select","Select","Select","Ident","Ident"))
      assert(graph.tail.forall(t => t.source.eq(graph(1).source) && t.span.exists))
      assert(!graph(1).source.eq(descriptor.rhs.source))
      assertEquals(new String(graph(1).source.content),"_root_.java.lang.Math.abs(value)")
      val parserBody = parse("class ParserControl { def echo(value: Int): Int = _root_.java.lang.Math.abs(value) }")
        .rhs.asInstanceOf[untpd.Template].body.head.asInstanceOf[untpd.DefDef].rhs
      val parserGraph = ExistingQuotedRootedBody.nodes(parserBody)
      graph.tail.zip(parserGraph.tail).foreach { (actual,expected) =>
        assertEquals((actual.span.start,actual.span.end,actual.span.point),
          (expected.span.start-parserBody.span.start,expected.span.end-parserBody.span.start,expected.span.point-parserBody.span.start))
      }
      assert(ExistingUntpdOrdinaryMethodRhsEditPreparation.validatePreparedBody(prepared,descriptor).isRight)
      val invalid = mode match
        case "copy" => prepared.copy(positionedReplacement = prepared.positionedReplacement.cloneIn(prepared.positionedReplacement.source))
        case "evidence" => prepared.copy(evidence = prepared.evidence.copy())
        case "source" => graph(1).source.content(0) = 'x'; prepared
        case "span" => graph(1).span = graph(1).span.withEnd(graph(1).span.end - 1); prepared
        case _ =>
          val rhs = prepared.positionedReplacement.asInstanceOf[untpd.Apply]
          val copied = untpd.cpy.Apply(rhs)(rhs.fun.cloneIn(rhs.fun.source),rhs.args)
          prepared.copy(positionedReplacement = copied)
      assert(ExistingUntpdOrdinaryMethodRhsEditPreparation.validatePreparedBody(invalid,descriptor).isLeft,clues(mode))
      assert(ExistingUntpdOrdinaryMethodReconstruction.reconstructMethod(descriptor,Vector.empty,None,Some(invalid)).isLeft,clues(mode))

  private def fixture()(using Context): untpd.TypeDef = parse("class C { val before: Int = 1; def echo(value: AnyVal): AnyVal = value; val after: Int = 7; def sibling(): Int = 10; def magnitude(number: AnyVal): AnyVal = number }")
  private def right[E,A](value: Either[E,A]): A = value.fold(p => fail(p.toString),identity)
  private def parse(source: String)(using Context): untpd.TypeDef =
    val unit = CompilationUnit("u080-authority.scala",source)
    val reporter = new StoreReporter(null)
    val ctx = summon[Context].fresh.setCompilationUnit(unit).setReporter(reporter)
    val tree = new Parsers.Parser(unit.source)(using ctx).parse()
    assert(!reporter.hasErrors)
    tree.asInstanceOf[untpd.PackageDef].stats.collectFirst { case t: untpd.TypeDef => t }.get
