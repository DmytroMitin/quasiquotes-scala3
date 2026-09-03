package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags.EmptyFlags
import dotty.tools.dotc.core.Names.{termName, typeName}
import dotty.tools.dotc.core.Symbols.{NoSymbol, newSymbol}
import dotty.tools.dotc.core.Types.NoType
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.{NoSource, SourceFile}
import dotty.tools.dotc.util.Spans.Span

class ExistingUntpdSelectedApplyArgumentRewriterTest extends munit.FunSuite:
  test("replaces one exact existing argument while preserving the function and sibling by identity") {
    withContext {
      val root = parseClass(
        """class U014Structural:
          |  object service:
          |    def invoke(a: Int, b: Int): Int = a + b
          |  val oldArg: Int = 1
          |  val keptArg: Int = 20
          |  def keep: Int = 1
          |  def change: Int = service.invoke(oldArg, keptArg)
          |  val opaque: String = "opaque"
          |""".stripMargin
      )
      val template = root.rhs.asInstanceOf[untpd.Template]
      val target = methodNamed(template.body, "change")
      val originalApply = target.rhs.asInstanceOf[untpd.Apply]
      val originalFunction = originalApply.fun
      val originalArgument = originalApply.args.head
      val untouchedArgument = originalApply.args(1)
      val untouchedMembers = template.body.filterNot(_.eq(target))

      given SourceFile = NoSource
      val replacement = untpd.Number("2", untpd.NumberKind.Whole(10))
      val result = ExistingUntpdSelectedApplyArgumentRewriter
        .rewrite(root, target, originalArgument, replacement)
        .fold(problem => fail(problem.message), identity)

      assertEquals(result.argumentIndex, 0)
      assert(!result.rebuiltRoot.eq(root))
      assert(!result.rebuiltTemplate.eq(template))
      assert(!result.rebuiltTarget.eq(target))
      assert(!result.rebuiltApply.eq(originalApply))
      assert(result.rebuiltApply.fun.eq(originalFunction))
      assert(result.rebuiltApply.args.head.eq(replacement))
      assert(result.rebuiltApply.args(1).eq(untouchedArgument))
      assert(!replacement.eq(originalArgument))
      val rebuiltUntouched = result.rebuiltTemplate.body.filterNot(_.eq(result.rebuiltTarget))
      assert(untouchedMembers.zip(rebuiltUntouched).forall((left, right) => left.eq(right)))
      Vector[untpd.Tree](
        result.rebuiltRoot,
        result.rebuiltTemplate,
        result.rebuiltTarget,
        result.rebuiltApply,
        replacement
      ).foreach { tree =>
        assert(!tree.source.exists, clues(tree))
        assert(!tree.span.exists, clues(tree))
        assertEquals(tree.symbol, NoSymbol)
        assert(!tree.isInstanceOf[untpd.TypedSplice])
      }
      assert(originalFunction.source.exists)
      assert(untouchedArgument.source.exists)
    }
  }

  test("distinguishes same-spelling existing arguments by exact identity") {
    withContext {
      val root = parseClass(
        """class U014SameSpelling:
          |  def change: Int = service.invoke(oldArg, oldArg)
          |""".stripMargin
      )
      val target = methodNamed(root.rhs.asInstanceOf[untpd.Template].body, "change")
      val originalApply = target.rhs.asInstanceOf[untpd.Apply]
      val first = originalApply.args.head
      val second = originalApply.args(1)
      assertEquals(first.toString, second.toString)
      assert(!first.eq(second))
      given SourceFile = NoSource
      val replacement = untpd.Number("2", untpd.NumberKind.Whole(10))

      val result = ExistingUntpdSelectedApplyArgumentRewriter
        .rewrite(root, target, second, replacement)
        .fold(problem => fail(problem.message), identity)

      assertEquals(result.argumentIndex, 1)
      assert(result.rebuiltApply.args.head.eq(first))
      assert(result.rebuiltApply.args(1).eq(replacement))
    }
  }

  test("fails closed for roots, target identity, selected Apply topology, and argument identity") {
    withContext {
      val root = parseClass(
        """class U014Failures:
          |  def change: Int = service.invoke(oldArg, keptArg)
          |""".stripMargin
      )
      val template = root.rhs.asInstanceOf[untpd.Template]
      val target = methodNamed(template.body, "change")
      val apply = target.rhs.asInstanceOf[untpd.Apply]
      val argument = apply.args.head
      given SourceFile = NoSource
      val replacement = untpd.Number("2", untpd.NumberKind.Whole(10))

      assertError("ROOT_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentRewriter.rewrite(
          null.asInstanceOf[untpd.TypeDef], target, argument, replacement
        )
      )
      assertError("TARGET_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentRewriter.rewrite(
          root, null.asInstanceOf[untpd.DefDef], argument, replacement
        )
      )
      assertError("ARGUMENT_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentRewriter.rewrite(
          root, target, null.asInstanceOf[untpd.Tree], replacement
        )
      )
      assertError("REPLACEMENT_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentRewriter.rewrite(
          root, target, argument, null.asInstanceOf[untpd.Tree]
        )
      )
      val foreign = parseClass("class Foreign:\n  def change: Int = service.invoke(oldArg, keptArg)\n")
      val foreignTarget = methodNamed(foreign.rhs.asInstanceOf[untpd.Template].body, "change")
      val foreignArgument = foreignTarget.rhs.asInstanceOf[untpd.Apply].args.head
      assertError("TARGET_NOT_DIRECT_MEMBER")(
        ExistingUntpdSelectedApplyArgumentRewriter.rewrite(
          root, foreignTarget, argument, replacement
        )
      )
      assertError("TARGET_ARGUMENT_NOT_DIRECT_MEMBER")(
        ExistingUntpdSelectedApplyArgumentRewriter.rewrite(
          root, target, foreignArgument, replacement
        )
      )
      assertError("TARGET_ARGUMENT_NOT_DIRECT_MEMBER")(
        ExistingUntpdSelectedApplyArgumentRewriter.rewrite(
          root, target, apply.fun, replacement
        )
      )
      assertError("TARGET_ARGUMENT_NOT_DIRECT_MEMBER")(
        ExistingUntpdSelectedApplyArgumentRewriter.rewrite(
          root,
          target,
          apply.fun.asInstanceOf[untpd.Select].qualifier,
          replacement
        )
      )

      val nonTemplateRoot = untpd.TypeDef(root.name, untpd.Ident(termName("notATemplate")))
      assertError("ROOT_TEMPLATE_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentRewriter.rewrite(
          nonTemplateRoot, target, argument, replacement
        )
      )
      val duplicateTargetRoot = replaceTemplateBody(root, target :: target :: Nil)
      assertError("TARGET_IDENTITY_NOT_UNIQUE")(
        ExistingUntpdSelectedApplyArgumentRewriter.rewrite(
          duplicateTargetRoot, target, argument, replacement
        )
      )

      val parameterizedRoot = parseClass(
        "class Parameterized:\n  def change(value: Int): Int = service.invoke(oldArg, keptArg)\n"
      )
      val parameterizedTarget = methodNamed(
        parameterizedRoot.rhs.asInstanceOf[untpd.Template].body,
        "change"
      )
      val parameterizedArgument = parameterizedTarget.rhs.asInstanceOf[untpd.Apply].args.head
      assertError("TARGET_PARAMETER_CLAUSES_UNSUPPORTED")(
        ExistingUntpdSelectedApplyArgumentRewriter.rewrite(
          parameterizedRoot, parameterizedTarget, parameterizedArgument, replacement
        )
      )

      val nonApplyRoot = parseClass("class NonApply:\n  def change: Int = oldArg\n")
      val nonApplyTarget = methodNamed(
        nonApplyRoot.rhs.asInstanceOf[untpd.Template].body,
        "change"
      )
      assertError("RHS_SELECTED_APPLY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentRewriter.rewrite(
          nonApplyRoot, nonApplyTarget, argument, replacement
        )
      )

      val (directApplyRoot, directApplyTarget) = replaceTargetRhs(
        root,
        target,
        untpd.Apply(untpd.Ident(termName("f")), apply.args)
      )
      assertError("APPLY_FUNCTION_SELECT_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentRewriter.rewrite(
          directApplyRoot, directApplyTarget, argument, replacement
        )
      )
      val (nestedQualifierRoot, nestedQualifierTarget) = replaceTargetRhs(
        root,
        target,
        untpd.Apply(
          untpd.Select(
            untpd.Select(untpd.Ident(termName("outer")), termName("service")),
            termName("invoke")
          ),
          apply.args
        )
      )
      assertError("SELECT_QUALIFIER_IDENT_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentRewriter.rewrite(
          nestedQualifierRoot, nestedQualifierTarget, argument, replacement
        )
      )
      val (typeNameRoot, typeNameTarget) = replaceTargetRhs(
        root,
        target,
        untpd.Apply(
          untpd.Select(untpd.Ident(termName("service")), typeName("Invoke")),
          apply.args
        )
      )
      assertError("SELECT_NAME_TERM_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentRewriter.rewrite(
          typeNameRoot, typeNameTarget, argument, replacement
        )
      )
      List(
        apply.args.take(1),
        apply.args ::: untpd.Number("3", untpd.NumberKind.Whole(10)) ::
          untpd.Number("4", untpd.NumberKind.Whole(10)) :: Nil
      ).foreach { arguments =>
        val (invalidRoot, invalidTarget) = replaceTargetRhs(
          root,
          target,
          untpd.Apply(apply.fun, arguments)
        )
        assertError("APPLY_ARGUMENT_COUNT_REQUIRED")(
          ExistingUntpdSelectedApplyArgumentRewriter.rewrite(
            invalidRoot, invalidTarget, argument, replacement
          )
        )
      }

      val (nullArgumentsRoot, nullArgumentsTarget) = replaceTargetRhs(
        root,
        target,
        untpd.Apply(apply.fun, null.asInstanceOf[List[untpd.Tree]])
      )
      assertError("APPLY_ARGUMENT_LIST_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentRewriter.rewrite(
          nullArgumentsRoot, nullArgumentsTarget, argument, replacement
        )
      )
      val (nullEntryRoot, nullEntryTarget) = replaceTargetRhs(
        root,
        target,
        untpd.Apply(apply.fun, argument :: null.asInstanceOf[untpd.Tree] :: Nil)
      )
      assertError("APPLY_ARGUMENT_ENTRY_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentRewriter.rewrite(
          nullEntryRoot, nullEntryTarget, argument, replacement
        )
      )

      val (duplicateRoot, duplicateTarget) = replaceTargetRhs(
        root,
        target,
        untpd.Apply(apply.fun, argument :: argument :: Nil)
      )
      assertError("TARGET_ARGUMENT_IDENTITY_NOT_UNIQUE")(
        ExistingUntpdSelectedApplyArgumentRewriter.rewrite(
          duplicateRoot, duplicateTarget, argument, replacement
        )
      )
    }
  }

  test("fails closed for unsupported existing/replacement leaves and replacement provenance") {
    withContext {
      val childRoot = parseClass(
        """class U014Child:
          |  def change: Int = service.invoke(other(oldArg), keptArg)
          |""".stripMargin
      )
      val childTarget = methodNamed(
        childRoot.rhs.asInstanceOf[untpd.Template].body,
        "change"
      )
      val childArgument = childTarget.rhs.asInstanceOf[untpd.Apply].args.head
      given SourceFile = NoSource
      val validReplacement = untpd.Number("2", untpd.NumberKind.Whole(10))
      assertError("TARGET_ARGUMENT_LEAF_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentRewriter.rewrite(
          childRoot, childTarget, childArgument, validReplacement
        )
      )

      val siblingRoot = parseClass(
        """class U014ChildSibling:
          |  def change: Int = service.invoke(oldArg, other(keptArg))
          |""".stripMargin
      )
      val siblingTarget = methodNamed(
        siblingRoot.rhs.asInstanceOf[untpd.Template].body,
        "change"
      )
      val siblingArgument = siblingTarget.rhs.asInstanceOf[untpd.Apply].args.head
      assertError("APPLY_ARGUMENT_LEAF_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentRewriter.rewrite(
          siblingRoot, siblingTarget, siblingArgument, validReplacement
        )
      )

      val root = parseClass("class U014Replacement:\n  def change: Int = service.invoke(oldArg, keptArg)\n")
      val target = methodNamed(root.rhs.asInstanceOf[untpd.Template].body, "change")
      val argument = target.rhs.asInstanceOf[untpd.Apply].args.head
      assertError("REPLACEMENT_LEAF_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentRewriter.rewrite(
          root,
          target,
          argument,
          untpd.Apply(untpd.Ident(termName("f")), validReplacement :: Nil)
        )
      )
      assertError("REPLACEMENT_SOURCE_PROVENANCE")(
        ExistingUntpdSelectedApplyArgumentRewriter.rewrite(
          root, target, argument, parseTerm("sourceful")
        )
      )
      assertError("REPLACEMENT_SPAN_PROVENANCE")(
        ExistingUntpdSelectedApplyArgumentRewriter.rewrite(
          root,
          target,
          argument,
          untpd.Number("2", untpd.NumberKind.Whole(10)).withSpan(Span(0, 1, 0))
        )
      )
      val symbol = newSymbol(NoSymbol, termName("u014Symbol"), EmptyFlags, NoType)
      val symbolBearing = untpd.Ident(termName("symbolBearing")).withType(symbol.termRef)
      assertError("REPLACEMENT_SYMBOL_PROVENANCE")(
        ExistingUntpdSelectedApplyArgumentRewriter.rewrite(
          root, target, argument, symbolBearing
        )
      )
      assertError("REPLACEMENT_TYPED_SPLICE_UNSUPPORTED")(
        ExistingUntpdSelectedApplyArgumentRewriter.rewrite(
          root, target, argument, untpd.TypedSplice(symbolBearing)
        )
      )
      val sourceFreeArgument = untpd.Ident(termName("oldArg"))
      val (sourceFreeRoot, sourceFreeTarget) = replaceTargetRhs(
        root,
        target,
        untpd.Apply(
          target.rhs.asInstanceOf[untpd.Apply].fun,
          sourceFreeArgument :: target.rhs.asInstanceOf[untpd.Apply].args(1) :: Nil
        )
      )
      assertError("TARGET_ARGUMENT_SITE_REQUIRED")(
        ExistingUntpdSelectedApplyArgumentRewriter.rewrite(
          sourceFreeRoot, sourceFreeTarget, sourceFreeArgument, validReplacement
        )
      )
    }
  }

  test("matches a parser-created equivalent on bounded structural facts") {
    withContext {
      val original = parseClass(
        "class U014Differential:\n  def change: Int = service.invoke(oldArg, keptArg)\n"
      )
      val originalTarget = methodNamed(
        original.rhs.asInstanceOf[untpd.Template].body,
        "change"
      )
      val originalApply = originalTarget.rhs.asInstanceOf[untpd.Apply]
      given SourceFile = NoSource
      val replacement = untpd.Number("2", untpd.NumberKind.Whole(10))
      val rebuilt = ExistingUntpdSelectedApplyArgumentRewriter
        .rewrite(original, originalTarget, originalApply.args.head, replacement)
        .fold(problem => fail(problem.message), identity)

      val equivalent = parseClass(
        "class U014Differential:\n  def change: Int = service.invoke(2, keptArg)\n"
      )
      val equivalentTarget = methodNamed(
        equivalent.rhs.asInstanceOf[untpd.Template].body,
        "change"
      )
      val equivalentApply = equivalentTarget.rhs.asInstanceOf[untpd.Apply]
      val rebuiltSelection = rebuilt.rebuiltApply.fun.asInstanceOf[untpd.Select]
      val equivalentSelection = equivalentApply.fun.asInstanceOf[untpd.Select]

      assertEquals(rebuilt.rebuiltApply.getClass, equivalentApply.getClass)
      assertEquals(rebuiltSelection.getClass, equivalentSelection.getClass)
      assertEquals(rebuiltSelection.qualifier.getClass, equivalentSelection.qualifier.getClass)
      assertEquals(rebuiltSelection.qualifier.toString, equivalentSelection.qualifier.toString)
      assertEquals(rebuiltSelection.name, equivalentSelection.name)
      assertEquals(
        rebuilt.rebuiltApply.args.map(_.getClass.getSimpleName),
        equivalentApply.args.map(_.getClass.getSimpleName)
      )
      assertEquals(rebuilt.rebuiltApply.args.map(_.toString), equivalentApply.args.map(_.toString))
      assert(!rebuilt.rebuiltApply.eq(equivalentApply))
      assert(rebuilt.rebuiltApply.fun.eq(originalApply.fun))
    }
  }

  private def parseClass(source: String)(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U014Structural.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.map(_.message).toList, Nil)
    parsed.asInstanceOf[untpd.PackageDef].stats.head.asInstanceOf[untpd.TypeDef]

  private def parseTerm(source: String)(using outerContext: Context): untpd.Tree =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U014Replacement.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).expr()
    assertEquals(reporter.pendingMessages.map(_.message).toList, Nil)
    parsed

  private def replaceTargetRhs(
      root: untpd.TypeDef,
      target: untpd.DefDef,
      rhs: untpd.Tree
  )(using Context): (untpd.TypeDef, untpd.DefDef) =
    val template = root.rhs.asInstanceOf[untpd.Template]
    val replacementTarget = untpd
      .cpy
      .DefDef(target)(target.name, target.paramss, target.tpt, rhs)
      .cloneIn(target.source)
      .withSpan(target.span)
    val replacementTemplate = untpd
      .cpy
      .Template(template)(
        template.constr,
        template.parentsOrDerived,
        template.derived,
        template.self,
        template.body.map(tree => if tree.eq(target) then replacementTarget else tree)
      )
      .cloneIn(template.source)
      .withSpan(template.span)
    val replacementRoot = untpd
      .cpy
      .TypeDef(root)(root.name, replacementTemplate)
      .cloneIn(root.source)
      .withSpan(root.span)
    replacementRoot -> replacementTarget

  private def replaceTemplateBody(
      root: untpd.TypeDef,
      body: List[untpd.Tree]
  )(using Context): untpd.TypeDef =
    val template = root.rhs.asInstanceOf[untpd.Template]
    val replacementTemplate = untpd
      .cpy
      .Template(template)(
        template.constr,
        template.parentsOrDerived,
        template.derived,
        template.self,
        body
      )
      .cloneIn(template.source)
      .withSpan(template.span)
    untpd
      .cpy
      .TypeDef(root)(root.name, replacementTemplate)
      .cloneIn(root.source)
      .withSpan(root.span)

  private def methodNamed(body: List[untpd.Tree], name: String): untpd.DefDef =
    body.collectFirst {
      case value: untpd.DefDef if value.name.toString == name => value
    }.getOrElse(fail(s"missing method $name"))

  private def assertError(expectedCode: String)(
      result: Either[ExistingUntpdSelectedApplyArgumentRewriteError, ?]
  ): Unit =
    result match
      case Left(problem) => assertEquals(problem.code, expectedCode)
      case Right(value) => fail(s"expected $expectedCode, found success $value")

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
