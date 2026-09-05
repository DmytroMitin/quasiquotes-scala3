package external.consumer

import scala.meta.*
import scala.meta.dialects.Scala3

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags.EmptyFlags
import dotty.tools.dotc.core.Names.termName
import dotty.tools.dotc.core.Symbols.{NoSymbol, newSymbol}
import dotty.tools.dotc.core.Types.NoType
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.{NoSource, SourceFile}
import dotty.tools.dotc.util.Spans.{NoSpan, Span}

import _root_.quasiquotes.definitions.dotty.ScalametaDefinitionClassMemberAppendBridge

final class ScalametaDefinitionClassMemberAppendBridgeTest extends munit.FunSuite:
  test("foreign consumer appends method value and backticked members with exact identity"):
    withContext:
      val fixtures = Vector(
        (
          "def foo(x: Int): String = x.toString",
          "<generated:c023-method>",
          "foo"
        ),
        ("val foo: Int = 42", "<generated:c023-value>", "foo"),
        ("val `match`: Int = 42", "<generated:c023-backticked>", "match")
      )

      fixtures.foreach: (definitionSource, virtualName, expectedName) =>
        val root = parseSingleTypeDef(existingClassSource)
        val originalTemplate = root.rhs.asInstanceOf[untpd.Template]
        val originalBody = originalTemplate.body.toVector
        val originalSnapshots = originalBody.flatMap(snapshot)

        val result = append(root, definitionSource, virtualName)
        val rebuiltTemplate = result.tree.rhs.asInstanceOf[untpd.Template]

        assert(!result.tree.eq(root))
        assert(!rebuiltTemplate.eq(originalTemplate))
        assertEquals(rebuiltTemplate.body.size, originalBody.size + 1)
        originalBody.indices.foreach: index =>
          assert(rebuiltTemplate.body(index).eq(originalBody(index)))
        assert(rebuiltTemplate.body.last.eq(result.appendedMember))
        assertEquals(rebuiltTemplate.body.count(_.eq(result.appendedMember)), 1)
        assertEquals(result.appendedMember.name.toString, expectedName)
        assertEquals(result.generatedSource, definitionSource)
        assertEquals(result.virtualSourceName, virtualName)
        assertEquals(result.generatedSourceFile.path, virtualName)
        assertEquals(result.generatedSourceFile.content.mkString, definitionSource)
        assertEquals(result.tree.source, root.source)
        assertEquals(result.tree.span, root.span)
        assertEquals(rebuiltTemplate.source, originalTemplate.source)
        assertEquals(rebuiltTemplate.span, originalTemplate.span)
        assert(!result.generatedSourceFile.equals(root.source))
        assertSnapshotsUnchanged(originalSnapshots)
        assertGeneratedGraph(result.appendedMember, result.generatedSourceFile, definitionSource.length)

  test("public failure boundary preserves generated and append stage diagnostics"):
    withContext:
      val root = parseSingleTypeDef(existingClassSource)

      assertFailure(
        ScalametaDefinitionClassMemberAppendBridge.append(root, null, "<generated:c023-missing>"),
        "GENERATED_DEFINITION_FAILED",
        "MISSING_INPUT:"
      )
      assertFailure(
        ScalametaDefinitionClassMemberAppendBridge.append(
          root,
          parsed("def unsupported(): Int = 1"),
          "<generated:c023-unsupported>"
        ),
        "GENERATED_DEFINITION_FAILED",
        "NEUTRAL_PROJECTION_FAILED:"
      )
      assertFailure(
        ScalametaDefinitionClassMemberAppendBridge.append(
          root,
          parsed("type T = Int"),
          "<generated:c023-alias>"
        ),
        "GENERATED_DEFINITION_FAILED",
        "GENERATED_ORIGIN_FAMILY_UNSUPPORTED:"
      )
      assertFailure(
        ScalametaDefinitionClassMemberAppendBridge.append(
          root,
          parsed("val foo: Int = 42"),
          "bad\nname"
        ),
        "GENERATED_DEFINITION_FAILED",
        "INVALID_VIRTUAL_SOURCE:"
      )
      assertFailure(
        ScalametaDefinitionClassMemberAppendBridge.append(
          null,
          parsed("val foo: Int = 42"),
          "<generated:c023-null-class>"
        ),
        "EXISTING_CLASS_APPEND_FAILED",
        "MISSING_CAPTURE_OR_CONTAINER:"
      )
      assertFailure(
        ScalametaDefinitionClassMemberAppendBridge.append(
          parseSingleTypeDef("trait Unsupported"),
          parsed("val foo: Int = 42"),
          "<generated:c023-trait>"
        ),
        "EXISTING_CLASS_APPEND_FAILED",
        "UNSUPPORTED_OUTER_TOPOLOGY:"
      )

      val sourceFree = root.cloneIn(NoSource).withSpan(NoSpan)
      assertFailure(
        ScalametaDefinitionClassMemberAppendBridge.append(
          sourceFree,
          parsed("val foo: Int = 42"),
          "<generated:c023-source-free>"
        ),
        "EXISTING_CLASS_APPEND_FAILED",
        "UNSUPPORTED_TEMPLATE_TOPOLOGY:"
      )

      val template = root.rhs.asInstanceOf[untpd.Template]
      val method = template.body.head.asInstanceOf[untpd.DefDef]
      val symbol = newSymbol(NoSymbol, termName("c023Owned"), EmptyFlags, NoType)
      val symbolMethod = untpd.cpy.DefDef(method)(
        method.name,
        method.paramss,
        method.tpt,
        method.rhs.withType(symbol.termRef)
      )
      val symbolTemplate = untpd.cpy.Template(template)(
        template.constr,
        template.parentsOrDerived,
        template.derived,
        template.self,
        symbolMethod :: template.body.tail
      )
      val symbolRoot = untpd.cpy.TypeDef(root)(root.name, symbolTemplate)
      assertFailure(
        ScalametaDefinitionClassMemberAppendBridge.append(
          symbolRoot,
          parsed("val foo: Int = 42"),
          "<generated:c023-symbol-class>"
        ),
        "EXISTING_CLASS_APPEND_FAILED",
        "OPERATION_REQUIRES_OWNER_OR_POST_TYPER_REPAIR:"
      )

  test("total body size 64 succeeds and append overflow fails atomically"):
    withContext:
      val atBoundary = parseSingleTypeDef(classWithMembers(63))
      val atBoundaryBody = atBoundary.rhs.asInstanceOf[untpd.Template].body.toVector
      val success = append(
        atBoundary,
        "val appended: Int = 42",
        "<generated:c023-limit-success>"
      )
      val successfulBody = success.tree.rhs.asInstanceOf[untpd.Template].body
      assertEquals(successfulBody.size, 64)
      atBoundaryBody.indices.foreach: index =>
        assert(successfulBody(index).eq(atBoundaryBody(index)))
      assert(successfulBody.last.eq(success.appendedMember))

      val overflow = parseSingleTypeDef(classWithMembers(64))
      val originalTemplate = overflow.rhs.asInstanceOf[untpd.Template]
      val originalBody = originalTemplate.body.toVector
      assertFailure(
        ScalametaDefinitionClassMemberAppendBridge.append(
          overflow,
          parsed("val appended: Int = 42"),
          "<generated:c023-limit-failure>"
        ),
        "EXISTING_CLASS_APPEND_FAILED",
        "DIRECT_MEMBER_LIMIT_OVERFLOW:"
      )
      assertEquals(originalTemplate.body.toVector, originalBody)
      originalBody.indices.foreach: index =>
        assert(originalTemplate.body(index).eq(originalBody(index)))

  private def existingClassSource: String =
    """@classMarker
      |final class Existing:
      |  def old(value: Int): Int = value
      |  val stable: Int = 7
      |""".stripMargin

  private def append(
      root: untpd.Tree,
      definitionSource: String,
      virtualName: String
  )(using Context): ScalametaDefinitionClassMemberAppendBridge.Lowered =
    ScalametaDefinitionClassMemberAppendBridge
      .append(root, parsed(definitionSource), virtualName)
      .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)

  private def assertFailure[A](
      result: Either[ScalametaDefinitionClassMemberAppendBridge.Failure, A],
      expectedCode: String,
      detailPrefix: String
  ): Unit =
    result match
      case Left(problem) =>
        assertEquals(problem.code, expectedCode)
        assert(problem.detail.startsWith(detailPrefix), clues(problem))
      case Right(value) => fail(s"expected $expectedCode, found success $value")

  private def parsed(source: String): Defn =
    Scala3(source).parse[Stat].get match
      case definition: Defn => definition
      case other => fail(s"expected Defn, found ${other.productPrefix}")

  private def parseSingleTypeDef(source: String)(using outer: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("C023Bridge.scala", source)
    given Context = outer.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsedTree = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsedTree match
      case packageDef: untpd.PackageDef =>
        packageDef.stats match
          case (root: untpd.TypeDef) :: Nil => root
          case other => fail(s"expected one TypeDef, found $other")
      case other => fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")

  private def classWithMembers(count: Int): String =
    val body =
      (0 until count)
        .map(index => s"  val member$index: Int = $index")
        .mkString("\n")
    s"final class Existing:\n$body\n"

  private final case class Snapshot(tree: untpd.Tree, source: SourceFile, span: Span)

  private def snapshot(tree: untpd.Tree)(using Context): Vector[Snapshot] =
    allTrees(tree).map(node => Snapshot(node, node.source, node.span))

  private def assertSnapshotsUnchanged(before: Vector[Snapshot]): Unit =
    before.foreach: value =>
      assertEquals(value.tree.source, value.source)
      assertEquals(value.tree.span, value.span)

  private def assertGeneratedGraph(
      member: untpd.MemberDef,
      expectedSource: SourceFile,
      sourceLength: Int
  )(using Context): Unit =
    allTrees(member).foreach: node =>
      assertEquals(node.symbol, NoSymbol, clues(node))
      assert(!node.isInstanceOf[untpd.TypedSplice], clues(node))
      if !node.isEmpty then
        assertEquals(node.source, expectedSource, clues(node))
        assert(node.span.exists, clues(node))
        assert(node.span.start >= 0, clues(node))
        assert(node.span.start <= node.span.point, clues(node))
        assert(node.span.point <= node.span.end, clues(node))
        assert(node.span.end <= sourceLength, clues(node))

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    val builder = Vector.newBuilder[untpd.Tree]
    val traverser = new untpd.UntypedTreeTraverser:
      override def traverse(current: untpd.Tree)(using Context): Unit =
        builder += current
        traverseChildren(current)
    traverser.traverse(tree)
    builder.result()

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
