package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.SourceFile
import quasiquotes.neutral.ScalametaDefinitionProjection
import quasiquotes.parser.TermShape

final class QuotedRootedQualifierAuthorityTest extends munit.FunSuite:
  import QuotedRootedQualifierAuthorityModel.*
  import ExistingClassUntypedRewrite.*

  test("actual rooted quote and parser have nested selections while public apply rejects the deeper qualifier"):
    given Context = (new ContextBase).initialCtx
    Vector(("echo", "value", "Math"), ("magnitude", "number", "StrictMath")).foreach { (name, param, target) =>
      val root = parse(s"class C { val before: Int = 1; def $name($param: AnyVal): AnyVal = $param; val after: Int = 2 }")
      val selected = right(QuotedExistingMethodAuthorityModel.select(right(ExistingClassUntypedRewrite.capture(root)), name))
      val replacement = right(quote(selected))
      val projected = ScalametaDefinitionProjection.project(replacement).toOption.get.definition.asMethod.get
      assert(projected.body.get.isInstanceOf[TermShape.Apply])
      val rejected = QuotedExistingMethodAuthorityModel.replace(selected, replacement).left.toOption.get
      assert(rejected.detail.contains("SEMANTIC_APPLY_FUNCTION_UNSUPPORTED"), clues(rejected))
      val parsed = parse(s"class Parsed { def $name($param: Int): Int = _root_.java.lang.$target.abs($param) }")
        .rhs.asInstanceOf[untpd.Template].body.head.asInstanceOf[untpd.DefDef].rhs
      val expected = Vector("Apply", "Select", "Select", "Select", "Select", "Ident", "Ident")
      assertEquals(nodes(parsed).map(_.getClass.getSimpleName), expected)
      assertEquals(path(parsed), Vector("_root_", "java", "lang", target, "abs", param))
      val f = right(prepare(selected))
      assertEquals(new String(f.source.content), s"_root_.java.lang.$target.abs($param)")
      assertEquals(nodes(f.tree).map(_.getClass.getSimpleName), expected)
      assertEquals(path(f.tree), path(parsed))
      val generated = nodes(f.tree).tail
      generated.foreach { t =>
        assert(t.source.eq(f.source)); assert(t.span.exists)
        val text = new String(f.source.content).substring(t.span.start, t.span.end)
        assert(text.nonEmpty)
      }
      // Concrete parser spans (start/end/point) agree with the independently
      // rendered/lowered fragment after subtracting the source RHS offset.
      nodes(f.tree).tail.zip(nodes(parsed).tail).foreach { (a,b) =>
        assertEquals((a.span.start,a.span.end,a.span.point),
          (b.span.start-parsed.span.start,b.span.end-parsed.span.start,b.span.point-parsed.span.start))
      }
      assert(right(validate(selected, f, f.tree)) == ())
      val copy = f.tree.cloneIn(f.tree.source).withSpan(f.tree.span)
      assert(validate(selected, f, copy).isLeft)
      val foreignSource = SourceFile.virtual(f.source.path, new String(f.source.content))
      assert(!foreignSource.eq(f.source))
      val foreign = f.tree.asInstanceOf[untpd.Apply]
      val foreignArg = foreign.args.head.cloneIn(foreignSource).withSpan(foreign.args.head.span)
      val foreignTree = untpd.cpy.Apply(foreign)(foreign.fun, List(foreignArg))
      assert(validate(selected, f, foreignTree).isLeft)
      val second = right(QuotedExistingMethodAuthorityModel.select(right(ExistingClassUntypedRewrite.capture(root)), name))
      // Same exact raw owner but a distinct fragment/source cannot replace the certificate graph.
      assert(validate(second, f, f.tree).isLeft)
      val otherFragment = right(prepare(second))
      assert(validate(selected, f, otherFragment.tree).isLeft)
      val otherOwner = parse(s"class C { def $name($param: AnyVal): AnyVal = $param }")
      val other = right(QuotedExistingMethodAuthorityModel.select(right(ExistingClassUntypedRewrite.capture(otherOwner)), name))
      assert(validate(other, f, f.tree).isLeft)
      assert(nodes(f.tree).forall(_.symbol == NoSymbol))
      val result = right(rewrite(selected, f))
      val old = root.rhs.asInstanceOf[untpd.Template].body
      val fresh = result.rhs.asInstanceOf[untpd.Template].body
      assert(fresh(0).eq(old(0))); assert(fresh(2).eq(old(2)))
      assert(!fresh(1).eq(old(1)))
      assertEquals((result.source,result.span),(root.source,root.span))
    }

  test("a dotted printed root identifier cannot substitute for the actual selection graph"):
    given Context = (new ContextBase).initialCtx
    val capture = right(ExistingClassUntypedRewrite.capture(parse("class C { def echo(value: AnyVal): AnyVal = value }")))
    val selected = right(QuotedExistingMethodAuthorityModel.select(capture,"echo"))
    val ref = right(selected.method.parameterScope.reference(selected.parameter.ref))
    val fake = TermShape.Apply(TermShape.Select(TermShape.Identifier("_root_.java.lang.Math",false),"abs"),List(ref))
    val plan = right(capture.emptyPlan.replaceBody(selected.method.ref,fake))
    val rejected = ExistingClassUntypedRewrite(capture,plan).left.toOption.get
    assert(rejected.detail.contains("SEMANTIC_IDENTIFIER_INVALID"),clues(rejected))

  test("direct owner member scan rejects collisions but cannot certify an unshadowed name"):
    given Context = (new ContextBase).initialCtx
    Vector(false,true).foreach { shadow =>
      val member = if shadow then "val Math: Int = 99;" else ""
      val selected = right(QuotedExistingMethodAuthorityModel.select(right(ExistingClassUntypedRewrite.capture(
        parse(s"class C { $member def echo(value: AnyVal): AnyVal = value }"))), "echo"))
      assertEquals(canonicalContextCheck(selected,"Math").left.toOption.get.code,
        if shadow then "DIRECT_QUALIFIER_SHADOW" else "ORIGINAL_CONTEXT_QUALIFIER_UNPROVEN")
    }

  private def path(tree: untpd.Tree): Vector[String] = tree match
    case a: untpd.Apply => path(a.fun) ++ a.args.toVector.flatMap(path)
    case s: untpd.Select => path(s.qualifier) :+ s.name.toString
    case i: untpd.Ident => Vector(i.name.toString)
    case _ => Vector.empty
  private def right[E,A](v: Either[E,A]): A = v.fold(p => fail(p.toString),identity)
  private def parse(source: String)(using outer: Context): untpd.TypeDef =
    val unit = CompilationUnit("RootedQualifier.scala", source)
    val reporter = new StoreReporter(null)
    given Context = outer.fresh.setCompilationUnit(unit).setReporter(reporter)
    val p = new Parsers.Parser(unit.source).parse().asInstanceOf[untpd.PackageDef]
    assert(reporter.pendingMessages.isEmpty)
    p.stats.collectFirst { case c: untpd.TypeDef => c }.get
