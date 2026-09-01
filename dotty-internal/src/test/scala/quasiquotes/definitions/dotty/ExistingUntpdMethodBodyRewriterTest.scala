package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags.EmptyFlags
import dotty.tools.dotc.core.Names.{termName, typeName}
import dotty.tools.dotc.core.Symbols.{NoSymbol, newSymbol}
import dotty.tools.dotc.core.Types.NoType
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.{NoSource, SourceFile}
import dotty.tools.dotc.util.Spans.Span

class ExistingUntpdMethodBodyRewriterTest extends munit.FunSuite:
  private val CanonicalSource =
    """@marker
      |class C:
      |  def keep: Int = 1
      |  def change: Int = 2
      |  val opaque: String = "opaque"
      |  def call: Int = service.invoke(oldArg, keptArg)
      |""".stripMargin

  test("rebuilds only the admitted containers and preserves every declared raw hole by identity") {
    withContext {
      val originalRoot = parseClass(CanonicalSource)
      val originalTemplate = templateOf(originalRoot)
      val originalTarget = methodNamed(originalTemplate.body, "change")
      val originalAnnotation = originalRoot.mods.annotations.head
      val originalKeep = methodNamed(originalTemplate.body, "keep")
      val originalOpaque = originalTemplate.body.collectFirst {
        case value: untpd.ValDef if value.name.toString == "opaque" => value
      }.getOrElse(fail("missing opaque member"))
      val originalCall = methodNamed(originalTemplate.body, "call")

      given SourceFile = NoSource
      val replacementBody = untpd.Number("20", untpd.NumberKind.Whole(10))
      val result = ExistingUntpdMethodBodyRewriter
        .rewrite(originalRoot, originalTarget, replacementBody)
        .fold(problem => fail(problem.message), identity)

      assert(!result.rebuiltRoot.eq(originalRoot))
      assert(!result.rebuiltTemplate.eq(originalTemplate))
      assert(!result.rebuiltTarget.eq(originalTarget))
      assert(result.replacementBody.eq(replacementBody))
      assert(result.rebuiltTarget.rhs.eq(replacementBody))

      assert(result.rebuiltRoot.mods.eq(originalRoot.mods))
      assert(result.rebuiltRoot.mods.annotations.head.eq(originalAnnotation))
      assert(result.rebuiltTemplate.constr.eq(originalTemplate.constr))
      assert(result.rebuiltTemplate.parentsOrDerived.eq(originalTemplate.parentsOrDerived))
      assert(result.rebuiltTemplate.derived.eq(originalTemplate.derived))
      assert(result.rebuiltTemplate.self.eq(originalTemplate.self))
      assert(result.rebuiltTarget.mods.eq(originalTarget.mods))
      assert(result.rebuiltTarget.tpt.eq(originalTarget.tpt))

      result.rebuiltTemplate.body match
        case keep :: changed :: opaque :: call :: Nil =>
          assert(keep.eq(originalKeep))
          assert(changed.eq(result.rebuiltTarget))
          assert(opaque.eq(originalOpaque))
          assert(call.eq(originalCall))
        case other => fail(s"unexpected rebuilt body: $other")

      Vector[untpd.Tree](
        result.rebuiltRoot,
        result.rebuiltTemplate,
        result.rebuiltTarget,
        replacementBody
      ).foreach { tree =>
        assert(!tree.source.exists, clues(tree.getClass.getSimpleName))
        assert(!tree.span.exists, clues(tree.getClass.getSimpleName))
        assertEquals(tree.symbol, NoSymbol)
        assert(!tree.isInstanceOf[untpd.TypedSplice])
      }

      assertEquals(result.prefix.size, 1)
      assert(result.prefix.head.eq(originalKeep))
      assertEquals(result.suffix.size, 2)
      assert(result.suffix.head.eq(originalOpaque))
      assert(result.suffix(1).eq(originalCall))
    }
  }

  test("uses exact target identity when same-named direct methods are present") {
    withContext {
      val root = parseClass(
        """class SameNames:
          |  def change: Int = 1
          |  def change: Int = 2
          |  def keep: Int = 3
          |""".stripMargin
      )
      val template = templateOf(root)
      val sameNamed = template.body.collect { case value: untpd.DefDef => value }
      assertEquals(sameNamed.map(_.name.toString), List("change", "change", "keep"))
      val selected = sameNamed(1)
      given SourceFile = NoSource
      val replacement = untpd.Number("20", untpd.NumberKind.Whole(10))

      val result = ExistingUntpdMethodBodyRewriter
        .rewrite(root, selected, replacement)
        .fold(problem => fail(problem.message), identity)

      assert(result.rebuiltTemplate.body.head.eq(sameNamed.head))
      assert(result.rebuiltTemplate.body(1).eq(result.rebuiltTarget))
      assert(!result.rebuiltTemplate.body(1).eq(selected))
      assert(result.rebuiltTemplate.body(2).eq(sameNamed(2)))
    }
  }

  test("fails closed for invalid roots, targets, and target topology") {
    withContext {
      val root = parseClass(CanonicalSource)
      val template = templateOf(root)
      val target = methodNamed(template.body, "change")
      given SourceFile = NoSource
      val replacement = untpd.Number("20", untpd.NumberKind.Whole(10))
      val nonTemplateRoot = untpd.TypeDef(typeName("NotAClass"), untpd.EmptyTree)
      val absentTarget = methodNamed(templateOf(parseClass(CanonicalSource)).body, "change")
      val duplicateTemplate = untpd.Template(
        template.constr,
        template.parentsOrDerived,
        template.derived,
        template.self,
        target :: target :: template.body.filterNot(_.eq(target))
      )
      val duplicateRoot = untpd.TypeDef(root.name, duplicateTemplate).withMods(root.mods)
      val parameterizedRoot = parseClass(
        """class Parameterized:
          |  def change(value: Int): Int = value
          |""".stripMargin
      )
      val parameterizedTarget = methodNamed(templateOf(parameterizedRoot).body, "change")
      val abstractRoot = parseClass(
        """abstract class AbstractTarget:
          |  def change: Int
          |""".stripMargin
      )
      val abstractTarget = methodNamed(templateOf(abstractRoot).body, "change")

      assertError("ROOT_REQUIRED")(
        ExistingUntpdMethodBodyRewriter.rewrite(
          null.asInstanceOf[untpd.TypeDef],
          target,
          replacement
        )
      )
      assertError("ROOT_TEMPLATE_REQUIRED")(
        ExistingUntpdMethodBodyRewriter.rewrite(nonTemplateRoot, target, replacement)
      )
      assertError("TARGET_REQUIRED")(
        ExistingUntpdMethodBodyRewriter.rewrite(
          root,
          null.asInstanceOf[untpd.DefDef],
          replacement
        )
      )
      assertError("TARGET_NOT_DIRECT_MEMBER")(
        ExistingUntpdMethodBodyRewriter.rewrite(root, absentTarget, replacement)
      )
      assertError("TARGET_IDENTITY_NOT_UNIQUE")(
        ExistingUntpdMethodBodyRewriter.rewrite(duplicateRoot, target, replacement)
      )
      assertError("TARGET_PARAMETER_CLAUSES_UNSUPPORTED")(
        ExistingUntpdMethodBodyRewriter.rewrite(
          parameterizedRoot,
          parameterizedTarget,
          replacement
        )
      )
      assertError("TARGET_BODY_REQUIRED")(
        ExistingUntpdMethodBodyRewriter.rewrite(abstractRoot, abstractTarget, replacement)
      )
    }
  }

  test("fails closed for null or mixed-provenance replacement trees") {
    withContext {
      val root = parseClass(CanonicalSource)
      val target = methodNamed(templateOf(root).body, "change")

      assertError("REPLACEMENT_BODY_REQUIRED")(
        ExistingUntpdMethodBodyRewriter.rewrite(
          root,
          target,
          null.asInstanceOf[untpd.Tree]
        )
      )

      val sourceful = parseTerm("sourceValue")
      assertError("REPLACEMENT_SOURCE_PROVENANCE")(
        ExistingUntpdMethodBodyRewriter.rewrite(root, target, sourceful)
      )

      given SourceFile = NoSource
      val spanned =
        untpd.Number("20", untpd.NumberKind.Whole(10)).withSpan(Span(0, 1, 0))
      assertError("REPLACEMENT_SPAN_PROVENANCE")(
        ExistingUntpdMethodBodyRewriter.rewrite(root, target, spanned)
      )

      val testSymbol =
        newSymbol(NoSymbol, termName("replacementSymbol"), EmptyFlags, NoType)
      val symbolBearing =
        untpd.Ident(termName("value")).withType(testSymbol.termRef)
      assert(symbolBearing.symbol != NoSymbol)
      assertError("REPLACEMENT_SYMBOL_PROVENANCE")(
        ExistingUntpdMethodBodyRewriter.rewrite(root, target, symbolBearing)
      )

      val typedSplice = untpd.TypedSplice(
        untpd.Ident(termName("value")).withType(testSymbol.termRef)
      )
      assertError("REPLACEMENT_TYPED_SPLICE_UNSUPPORTED")(
        ExistingUntpdMethodBodyRewriter.rewrite(root, target, typedSplice)
      )

      val nestedSourceful = untpd.Apply(
        untpd.Ident(termName("identity")),
        parseTerm("nestedSourceValue") :: Nil
      )
      assertError("REPLACEMENT_SOURCE_PROVENANCE")(
        ExistingUntpdMethodBodyRewriter.rewrite(root, target, nestedSourceful)
      )

      val sourcefulStatement = parseTerm("sourcefulStatement")
      val nestedBlock = untpd.Block(
        sourcefulStatement :: Nil,
        untpd.Ident(termName("sourceFreeResult"))
      )
      assert(!nestedBlock.source.exists)
      assert(!nestedBlock.span.exists)
      assertError("REPLACEMENT_SOURCE_PROVENANCE")(
        ExistingUntpdMethodBodyRewriter.rewrite(root, target, nestedBlock)
      )

      val nestedTypeApply = untpd.TypeApply(
        untpd.Ident(termName("sourceFreeFunction")),
        parseTerm("SourcefulType") :: Nil
      )
      assert(!nestedTypeApply.source.exists)
      assert(!nestedTypeApply.span.exists)
      assertError("REPLACEMENT_SOURCE_PROVENANCE")(
        ExistingUntpdMethodBodyRewriter.rewrite(root, target, nestedTypeApply)
      )
    }
  }

  private def parseClass(source: String)(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U002ExistingRewrite.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    val messages = reporter.pendingMessages.map(_.message).toList
    assertEquals(messages, Nil)
    parsed match
      case packageDef: untpd.PackageDef =>
        packageDef.stats match
          case (value: untpd.TypeDef) :: Nil => value
          case other => fail(s"expected one TypeDef, found $other")
      case other => fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")

  private def parseTerm(source: String)(using outerContext: Context): untpd.Tree =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U002Replacement.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).expr()
    val messages = reporter.pendingMessages.map(_.message).toList
    assertEquals(messages, Nil)
    parsed

  private def templateOf(root: untpd.TypeDef): untpd.Template =
    root.rhs match
      case value: untpd.Template => value
      case other => fail(s"expected Template, found ${other.getClass.getSimpleName}")

  private def methodNamed(body: List[untpd.Tree], name: String): untpd.DefDef =
    body.collectFirst {
      case value: untpd.DefDef if value.name.toString == name => value
    }.getOrElse(fail(s"missing method $name"))

  private def assertError(expectedCode: String)(
      result: Either[ExistingUntpdMethodBodyRewriteError, ?]
  ): Unit =
    result match
      case Left(problem) => assertEquals(problem.code, expectedCode)
      case Right(value) => fail(s"expected $expectedCode, found success $value")

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
