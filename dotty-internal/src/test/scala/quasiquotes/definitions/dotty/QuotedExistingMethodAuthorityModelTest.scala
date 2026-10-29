package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite.*
import quasiquotes.neutral.{ScalametaDefinitionProjection, ScalametaTermProjection}
import quasiquotes.parser.TermShape
import quasiquotes.types.TypeNormalForm
import scala.meta.*

final class QuotedExistingMethodAuthorityModelTest extends munit.FunSuite:
  import QuotedExistingMethodAuthorityModel.*

  test("two real q matches preserve original authority separately from neutral quote identity"):
    withContext:
      Vector(("echo", "value", "Math"), ("magnitude", "number", "StrictMath")).foreach { (name, argument, qualifier) =>
        val root = parse(s"class C { val before: Int = 1; def $name($argument: AnyVal): AnyVal = $argument; val after: Int = 2 }")
        val c = right(ExistingClassUntypedRewrite.capture(root))
        val selected = right(select(c, name))
        val raw = root.rhs.asInstanceOf[untpd.Template].body
        val old = raw(1).asInstanceOf[untpd.DefDef]
        val param = old.paramss.head.head.asInstanceOf[untpd.ValDef]
        assert(selected.parameter.identity.sameObjectAs(new ExactIdentity(param)))
        assert(selected.method.body.identity.sameObjectAs(new ExactIdentity(old.rhs)))
        assert(selected.tree.isInstanceOf[Defn.Def])
        assertEquals(selected.tree.pos, Position.None)
        val replacement = right(quotedReplacement(selected))
        assert(replacement.syntax.contains(s"$qualifier.abs($argument)"), clues(replacement.syntax))
        val output = ScalametaDefinitionProjection.project(replacement).toOption.get.definition.asMethod.get
        val binder = output.parameterScope.binder(0, 0).toOption.get
        val shape = output.body.get.asInstanceOf[TermShape.Apply]
        val own = _root_.quasiquotes.terms.TermShapeBindingView.inspect(shape.arguments.head).toOption.get.boundReference.get.binder
        assertEquals(own, binder)
        val original = selected.method.parameterScope.binder(selected.parameter.ref).toOption.get
        assert(original != binder)
        val result = right(replace(selected, replacement))
        val fresh = result.tree.rhs.asInstanceOf[untpd.Template].body
        assert(result.changed)
        assert(!result.tree.eq(root))
        assert(fresh(0).eq(raw(0)))
        assert(fresh(2).eq(raw(2)))
        assert(!fresh(1).eq(raw(1)))
        val m = fresh(1).asInstanceOf[untpd.DefDef]
        assertEquals((m.source, m.span), (old.source, old.span))
        assertEquals((m.rhs.source, m.rhs.span), (old.rhs.source, old.rhs.span))
        assertEquals((m.paramss.head.head.source, m.paramss.head.head.span), (param.source, param.span))
        assert(raw(1).eq(old))
        assert(ExistingUntpdClassMemberFilter.allTrees(result.tree).forall(t => t.symbol == NoSymbol && !t.isInstanceOf[untpd.TypedSplice]))
      }

  test("G1 and G2 public term views cannot authorize a generic-free literal method quote"):
    withContext:
      Vector("def echo[A](value: AnyVal): AnyVal = value", "def echo[A](value: A): A = value").foreach { declaration =>
        val c = right(ExistingClassUntypedRewrite.capture(parse(s"class C { $declaration }")))
        val m = right(c.method(c.members.head.ref))
        assertEquals(m.parameterClauses.size, 1)
        assertEquals(m.parameterClauses.head.parameters.size, 1)
        assertEquals(select(c, "echo").left.toOption.map(_.code), Some("QUOTED_INPUT_HIDDEN_TYPE_CLAUSE"))
      }

  test("unsupported original method roles and clause shapes reject before quote transport"):
    withContext:
      val declarations = Vector(
        "def echo(value: => AnyVal): AnyVal = value",
        "def echo(value: AnyVal*): AnyVal = value.head",
        "def echo(using value: AnyVal): AnyVal = value",
        "def echo(value: AnyVal)(other: AnyVal): AnyVal = value",
        "def echo: AnyVal = 1", "def echo(): AnyVal = 1",
        "def echo(value: AnyVal): AnyVal", "val echo: Int = 1",
        "def echo(value: AnyVal): AnyVal = 7",
        "def echo[A, B](value: AnyVal): AnyVal = value",
        "def echo(value: AnyVal): AnyVal = value; def echo(other: Int): Int = other"
      )
      declarations.foreach { declaration =>
        val root = parse(s"class C { $declaration }")
        val c = right(ExistingClassUntypedRewrite.capture(root))
        assert(select(c, "echo").isLeft, clues(declaration))
      }
      val c = right(ExistingClassUntypedRewrite.capture(parse("class C { def other(value: AnyVal): AnyVal = value }")))
      assert(select(c, "echo").isLeft)

  test("wrong replacement method parameter free argument shadowing and hidden output clauses reject atomically"):
    withContext:
      val root = parse("class C { val before: Int = 1; def echo(value: AnyVal): AnyVal = value; val after: Int = 2 }")
      val c = right(ExistingClassUntypedRewrite.capture(root))
      val s = right(select(c, "echo"))
      val original = root.rhs.asInstanceOf[untpd.Template].body
      val bad = Vector[Defn.Def](
        q"def other(value: Int): Int = Math.abs(value)",
        q"def echo(other: Int): Int = Math.abs(other)",
        q"def echo(value: Int): Int = Math.abs(UnknownTerm)",
        q"def echo(value: Int): Int = Math.abs(other)",
        q"def echo(value: Int): Int = Math.abs(1)",
        q"def echo(value: Int): Int = value.abs(value)",
        q"def echo(value: Int): Int = { val value: Int = 1; Math.abs(value) }",
        q"def echo(value: Int): Int = Math.abs(((value: Int) => value)(1))",
        q"def echo(value: Int): Int = Math.abs(value.toInt)",
        q"def echo[A](value: Int): Int = Math.abs(value)",
        q"def echo(value: Int, other: Int): Int = Math.abs(other)",
        q"def echo(value: Int)(other: Int): Int = Math.abs(other)"
      )
      bad.foreach { replacement =>
        assert(replace(s, replacement).isLeft, clues(replacement.syntax))
        assert(root.rhs.asInstanceOf[untpd.Template].body.zip(original).forall((a, b) => a.eq(b)))
      }

  test("same spelling different semantic output binder is not an authority substitute"):
    withContext:
      val first = ScalametaDefinitionProjection.project(q"def echo(value: Int): Int = Math.abs(value)").toOption.get.definition.asMethod.get
      val second = ScalametaDefinitionProjection.project(q"def echo(value: Int): Int = Math.abs(value)").toOption.get.definition.asMethod.get
      val original = right(select(right(ExistingClassUntypedRewrite.capture(parse("class C { def echo(value: AnyVal): AnyVal = value }"))), "echo"))
      val reference = right(original.method.parameterScope.reference(original.parameter.ref))
      assert(CaptureLocalQuoteRewrite.mapQualifiedUnaryCall(first.body.get, second.parameterScope.binder(0, 0).toOption.get, reference).isLeft)
      val foreign = right(select(right(ExistingClassUntypedRewrite.capture(parse("class C { def echo(value: AnyVal): AnyVal = value }"))), "echo"))
      val foreignRef = right(foreign.method.parameterScope.reference(foreign.parameter.ref))
      val plan = original.capture.emptyPlan.replaceBody(original.method.ref, foreignRef)
      assert(plan.isLeft)
      assert(original.capture.method(foreign.member.ref).isLeft)
      assert(original.capture.emptyPlan.replaceResultType(foreign.method.ref, TypeNormalForm.STypeIdent("Int")).isLeft)
      assert(original.capture.emptyPlan.replaceParameterType(foreign.parameter.ref, TypeNormalForm.STypeIdent("Int")).isLeft)
      assert(ExistingClassUntypedRewrite(original.capture, foreign.capture.emptyPlan).isLeft)
      val recaptured = right(ExistingClassUntypedRewrite.capture(original.capture.exact.originalRoot))
      assert(recaptured.emptyPlan.replaceResultType(original.method.ref, TypeNormalForm.STypeIdent("Int")).isLeft)
      assert(recaptured.method(original.member.ref).isLeft)
      assert(recaptured.emptyPlan.replaceParameterType(original.parameter.ref, TypeNormalForm.STypeIdent("Int")).isLeft)
      val rawOwner = original.capture.exact.originalRoot
      val copiedOwner = rawOwner.cloneIn(rawOwner.source)
      assert(!copiedOwner.eq(rawOwner))
      val copiedCapture = right(ExistingClassUntypedRewrite.capture(copiedOwner))
      assert(copiedCapture.method(original.member.ref).isLeft)
      assert(copiedCapture.emptyPlan.replaceResultType(original.method.ref, TypeNormalForm.STypeIdent("Int")).isLeft)
      assert(copiedCapture.emptyPlan.replaceParameterType(original.parameter.ref, TypeNormalForm.STypeIdent("Int")).isLeft)


  test("standalone neutral free name projection cannot evade U041 existing-parameter capture rejection"):
    withContext:
      val projected = ScalametaTermProjection.project(q"Math.abs(value)").toOption.get
      val c = right(ExistingClassUntypedRewrite.capture(parse("class C { def echo(value: AnyVal): AnyVal = value }")))
      val s = right(select(c, "echo"))
      val plan = right(c.emptyPlan.replaceBody(s.method.ref, projected.shape))
      val error = ExistingClassUntypedRewrite(c, plan).left.toOption.get
      assert(error.detail.contains("FREE_IDENTIFIER_PARAMETER_CAPTURE"), clues(error))

  test("mixed invalid Type/body plan and edit-omit conflicts do not mutate original owner"):
    withContext:
      val root = parse("class C { def echo(value: AnyVal): AnyVal = value; val untouched: Int = 7 }")
      val c = right(ExistingClassUntypedRewrite.capture(root))
      val s = right(select(c, "echo"))
      val original = c.exact.originalTemplate.body
      val valid = right(c.emptyPlan.replaceParameterType(s.parameter.ref, TypeNormalForm.STypeIdent("Int")))
      val invalid = right(valid.replaceResultType(s.method.ref, TypeNormalForm.STypeApply(TypeNormalForm.STypeIdent("Option"), List(TypeNormalForm.STypeIdent("String")))))
      assert(ExistingClassUntypedRewrite(c, invalid).isLeft)
      assert(valid.replaceParameterType(s.parameter.ref, TypeNormalForm.STypeIdent("Int")).isLeft)
      assert(valid.omit(s.member.ref).isLeft)
      assert(right(c.emptyPlan.omit(s.member.ref)).replaceBody(s.method.ref, right(s.method.parameterScope.reference(s.parameter.ref))).isLeft)
      assert(c.exact.originalTemplate.body.zip(original).forall((a, b) => a.eq(b)))

  private def right[E, A](v: Either[E, A]): A = v.fold(p => fail(p.toString), identity)
  private def parse(source: String)(using outer: Context): untpd.TypeDef =
    val unit = CompilationUnit("QuotedAuthority.scala", source)
    val reporter = new StoreReporter(null)
    given Context = outer.fresh.setCompilationUnit(unit).setReporter(reporter)
    val tree = new Parsers.Parser(unit.source).parse().asInstanceOf[untpd.PackageDef]
    assertEquals(reporter.pendingMessages.toList, Nil)
    tree.stats.collectFirst { case c: untpd.TypeDef => c }.get
  private def withContext[A](run: Context ?=> A): A = run(using (new ContextBase).initialCtx)
