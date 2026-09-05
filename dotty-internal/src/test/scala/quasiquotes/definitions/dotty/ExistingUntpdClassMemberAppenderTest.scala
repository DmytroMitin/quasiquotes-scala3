package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags.EmptyFlags
import dotty.tools.dotc.core.Names.termName
import dotty.tools.dotc.core.Symbols.{NoSymbol, newSymbol}
import dotty.tools.dotc.core.Types.NoType
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.Spans.Span

import scala.meta.*
import scala.meta.dialects.Scala3

class ExistingUntpdClassMemberAppenderTest extends munit.FunSuite:
  test("appends the exact C020 generated method to an empty existing class") {
    withContext {
      val root = parseSingleTypeDef("final class GeneratedClass")
      val originalTemplate = root.rhs.asInstanceOf[untpd.Template]
      val generated = generatedMember(
        "def foo(x: Int): String = x.toString",
        "<generated:u025-empty-method>"
      )
      val generatedGraph = snapshot(generated)

      val result = appendOrFail(root, generated)

      assertEquals(originalTemplate.body.size, 0)
      assertEquals(result.rebuiltTemplate.body.size, 1)
      assert(result.rebuiltTemplate.body.head.eq(generated))
      assert(result.appendedMember.eq(generated))
      assert(!result.rebuiltTemplate.eq(originalTemplate))
      assert(!result.rebuiltRoot.eq(root))
      assertShellContract(root, originalTemplate, result)
      assertSnapshotUnchanged(generatedGraph)
    }
  }

  test("preserves multiple opaque existing members in exact order before a generated method") {
    withContext {
      val root = parseSingleTypeDef(
        """@classMarker
          |final class GeneratedClass:
          |  @memberMarker def existing(value: Int): Int = value
          |  val opaque: Int = 7
          |  type Alias = Int
          |  object Nested
          |""".stripMargin
      )
      val originalTemplate = root.rhs.asInstanceOf[untpd.Template]
      val originalBody = originalTemplate.body.toVector
      val originalGraph = snapshot(root)
      val generated = generatedMember(
        "def foo(x: Int): String = x.toString",
        "<generated:u025-nonempty-method>"
      )

      val result = appendOrFail(root, generated)

      assertEquals(result.rebuiltTemplate.body.size, 5)
      originalBody.indices.foreach(index =>
        assert(result.rebuiltTemplate.body(index).eq(originalBody(index)))
      )
      assert(result.rebuiltTemplate.body.last.eq(generated))
      assertEquals(result.rebuiltTemplate.body.count(_.eq(generated)), 1)
      assertEquals(originalTemplate.body.toVector, originalBody)
      originalTemplate.body.zip(originalBody).foreach((actual, expected) =>
        assert(actual.eq(expected))
      )
      assertSnapshotUnchanged(originalGraph)
      assertShellContract(root, originalTemplate, result)
    }
  }

  test("appends the exact C020 generated val without changing its generated origin") {
    withContext {
      val root = parseSingleTypeDef(
        """final class GeneratedClass:
          |  def existing(): Int = 7
          |""".stripMargin
      )
      val originalTemplate = root.rhs.asInstanceOf[untpd.Template]
      val original = originalTemplate.body.head
      val generated = generatedMember(
        "val foo: Int = 42",
        "<generated:u025-value>"
      )
      val generatedGraph = snapshot(generated)

      val result = appendOrFail(root, generated)

      assert(result.rebuiltTemplate.body.head.eq(original))
      assert(result.rebuiltTemplate.body.last.eq(generated))
      assert(result.appendedMember.eq(generated))
      assertSnapshotUnchanged(generatedGraph)
      assertShellContract(root, originalTemplate, result)
    }
  }

  test("fails closed for missing, unsupported, malformed, aliased, and repair-requiring inputs") {
    withContext {
      val root = parseSingleTypeDef(
        """final class GeneratedClass:
          |  def existing(): Int = 7
          |""".stripMargin
      )
      val template = root.rhs.asInstanceOf[untpd.Template]
      val generated = generatedMember(
        "def foo(x: Int): String = x.toString",
        "<generated:u025-negative>"
      )
      val originalBody = template.body.toVector

      assertCode(
        ExistingUntpdClassMemberAppender.append(null, generated),
        "MISSING_CAPTURE_OR_CONTAINER"
      )
      assertCode(
        ExistingUntpdClassMemberAppender.append(
          parseSingleTypeDef("trait Unsupported"),
          generated
        ),
        "UNSUPPORTED_OUTER_TOPOLOGY"
      )
      assertCode(
        ExistingUntpdClassMemberAppender.append(
          parseSingleTypeDef("type Unsupported = Int"),
          generated
        ),
        "UNSUPPORTED_TEMPLATE_TOPOLOGY"
      )
      val malformedTemplate = untpd.cpy.Template(template)(
        template.constr,
        template.parentsOrDerived,
        template.derived,
        template.self,
        List(null)
      )
      val malformedRoot = untpd.cpy.TypeDef(root)(root.name, malformedTemplate)
      assertCode(
        ExistingUntpdClassMemberAppender.append(malformedRoot, generated),
        "MALFORMED_EXISTING_BODY"
      )
      assertCode(
        ExistingUntpdClassMemberAppender.append(root, null),
        "MISSING_APPENDED_MEMBER"
      )
      assertCode(
        ExistingUntpdClassMemberAppender.append(root, untpd.EmptyTree),
        "MISSING_APPENDED_MEMBER"
      )
      assertCode(
        ExistingUntpdClassMemberAppender.append(root, untpd.Ident(termName("value"))),
        "UNSUPPORTED_APPENDED_MEMBER_ROLE"
      )
      val alias = sourceFreeMember("type Alias = Int")
      assertCode(
        ExistingUntpdClassMemberAppender.append(root, alias),
        "UNSUPPORTED_APPENDED_MEMBER_ROLE"
      )
      assertCode(
        ExistingUntpdClassMemberAppender.append(root, template.body.head),
        "APPENDED_MEMBER_ALREADY_PRESENT"
      )
      val sourceFree = sourceFreeMember("def sourceFree: Int = 1")
      assertCode(
        ExistingUntpdClassMemberAppender.append(root, sourceFree),
        "APPENDED_MEMBER_PROVENANCE_FAILURE"
      )
      val malformedSpanBase = generatedMember(
        "val malformed: Int = 1",
        "<generated:u025-malformed-span>"
      )
      val malformedSpan = malformedSpanBase
        .withSpan(
          Span(
            0,
            malformedSpanBase.source.content.length + 1,
            malformedSpanBase.span.point
          )
        )
        .asInstanceOf[untpd.MemberDef]
      assertCode(
        ExistingUntpdClassMemberAppender.append(root, malformedSpan),
        "APPENDED_MEMBER_PROVENANCE_FAILURE"
      )

      val symbol = newSymbol(NoSymbol, termName("u025Symbol"), EmptyFlags, NoType)
      val method = generatedMember(
        "def symbolBearing: Int = value",
        "<generated:u025-symbol>"
      ).asInstanceOf[untpd.DefDef]
      val symbolRhs = method.rhs.withType(symbol.termRef)
      val symbolBearing = untpd.cpy.DefDef(method)(
        method.name,
        method.paramss,
        method.tpt,
        symbolRhs
      )
      assertCode(
        ExistingUntpdClassMemberAppender.append(root, symbolBearing),
        "APPENDED_MEMBER_SYMBOL_BEARING"
      )
      val typedSplice = untpd.cpy.DefDef(method)(
        method.name,
        method.paramss,
        method.tpt,
        untpd.TypedSplice(symbolRhs)
      )
      assertCode(
        ExistingUntpdClassMemberAppender.append(root, typedSplice),
        "APPENDED_MEMBER_TYPED_SPLICE"
      )
      val ownerRepairRoot = parseSingleTypeDef(
        "final class OwnerRepairRequired:\n  def existing(): Int = 7\n"
      )
      val ownerRepairTemplate = ownerRepairRoot.rhs.asInstanceOf[untpd.Template]
      val ownerRepairLeaf =
        untpd.Ident(termName("ownerRepair")).withType(symbol.termRef)
      val ownerRepairChangedTemplate = untpd.cpy.Template(ownerRepairTemplate)(
        ownerRepairTemplate.constr,
        ownerRepairTemplate.parentsOrDerived,
        ownerRepairTemplate.derived,
        ownerRepairTemplate.self,
        ownerRepairLeaf :: ownerRepairTemplate.body.tail
      )
      val ownerRepairChangedRoot = untpd.cpy.TypeDef(ownerRepairRoot)(
        ownerRepairRoot.name,
        ownerRepairChangedTemplate
      )
      assertCode(
        ExistingUntpdClassMemberAppender.append(ownerRepairChangedRoot, generated),
        "OPERATION_REQUIRES_OWNER_OR_POST_TYPER_REPAIR"
      )

      assertEquals(template.body.toVector, originalBody)
      template.body.zip(originalBody).foreach((actual, expected) =>
        assert(actual.eq(expected))
      )
    }
  }

  test("admits total size 64 and rejects append overflow atomically") {
    withContext {
      val generated = generatedMember(
        "val appended: Int = 42",
        "<generated:u025-limit>"
      )
      val atBoundary = parseSingleTypeDef(classWithMembers(63))
      val atBoundaryBody = atBoundary.rhs.asInstanceOf[untpd.Template].body.toVector
      val success = appendOrFail(atBoundary, generated)
      assertEquals(success.rebuiltTemplate.body.size, 64)
      atBoundaryBody.indices.foreach(index =>
        assert(success.rebuiltTemplate.body(index).eq(atBoundaryBody(index)))
      )
      assert(success.rebuiltTemplate.body.last.eq(generated))

      val overflow = parseSingleTypeDef(classWithMembers(64))
      val overflowTemplate = overflow.rhs.asInstanceOf[untpd.Template]
      val overflowBody = overflowTemplate.body.toVector
      assertCode(
        ExistingUntpdClassMemberAppender.append(overflow, generated),
        "DIRECT_MEMBER_LIMIT_OVERFLOW"
      )
      assertEquals(overflowTemplate.body.toVector, overflowBody)
      overflowTemplate.body.zip(overflowBody).foreach((actual, expected) =>
        assert(actual.eq(expected))
      )
    }
  }

  test("rejects a fabricated corrupt capture without exposing a partial reconstruction") {
    withContext {
      val root = parseSingleTypeDef(
        """final class GeneratedClass:
          |  val first: Int = 1
          |  val second: Int = 2
          |""".stripMargin
      )
      val captured = ExistingUntpdClassMemberFilter.capture(root).toOption.get
      val corrupt = captured.copy(members = captured.members.reverse)
      val generated = generatedMember(
        "val appended: Int = 42",
        "<generated:u025-corrupt-capture>"
      )

      assertCode(
        ExistingUntpdClassMemberAppender.appendCapturedMember(corrupt, generated),
        "RECONSTRUCTION_INVARIANT_FAILURE"
      )
    }
  }

  private def assertShellContract(
      originalRoot: untpd.TypeDef,
      originalTemplate: untpd.Template,
      result: ExistingUntpdClassMemberAppender.Result
  )(using Context): Unit =
    assert(result.rebuiltRoot.mods.eq(originalRoot.mods))
    assert(result.rebuiltTemplate.constr.eq(originalTemplate.constr))
    assert(result.rebuiltTemplate.parentsOrDerived.eq(originalTemplate.parentsOrDerived))
    assert(result.rebuiltTemplate.derived.eq(originalTemplate.derived))
    assert(result.rebuiltTemplate.self.eq(originalTemplate.self))
    assertEquals(result.rebuiltRoot.source, originalRoot.source)
    assertEquals(result.rebuiltRoot.span, originalRoot.span)
    assertEquals(result.rebuiltTemplate.source, originalTemplate.source)
    assertEquals(result.rebuiltTemplate.span, originalTemplate.span)

  private def assertCode[A](
      result: Either[ExistingUntpdClassMemberAppendError, A],
      expected: String
  ): Unit = result match
    case Left(problem) => assertEquals(problem.code, expected)
    case Right(value) => fail(s"expected $expected, found success $value")

  private def appendOrFail(
      root: untpd.Tree,
      member: untpd.Tree
  )(using Context): ExistingUntpdClassMemberAppender.Result =
    ExistingUntpdClassMemberAppender
      .append(root, member)
      .fold(problem => fail(problem.message), identity)

  private def generatedMember(
      source: String,
      virtualSourceName: String
  )(using Context): untpd.MemberDef =
    ScalametaDefinitionGeneratedOriginBridge
      .lower(parseDefinition(source), virtualSourceName)
      .fold(problem => fail(s"${problem.code}: ${problem.detail}"), _.tree)

  private def sourceFreeMember(source: String)(using Context): untpd.MemberDef =
    ScalametaDefinitionUntypedBridge
      .lower(parseDefinition(source))
      .fold(problem => fail(s"${problem.code}: ${problem.detail}"), identity)

  private def parseDefinition(source: String): Defn =
    Scala3(source).parse[Stat].get match
      case definition: Defn => definition
      case other => fail(s"expected Defn, found ${other.productPrefix}")

  private def parseSingleTypeDef(source: String)(using outer: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U025Appender.scala", source)
    given Context = outer.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed match
      case packageDef: untpd.PackageDef =>
        packageDef.stats match
          case (root: untpd.TypeDef) :: Nil => root
          case other => fail(s"expected one TypeDef, found $other")
      case other => fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")

  private def classWithMembers(count: Int): String =
    val body = (0 until count).map(index => s"  val member$index: Int = $index").mkString("\n")
    s"final class GeneratedClass:\n$body\n"

  private final case class TreeSnapshot(
      tree: untpd.Tree,
      source: dotty.tools.dotc.util.SourceFile,
      span: Span
  )

  private def snapshot(tree: untpd.Tree)(using Context): Vector[TreeSnapshot] =
    allTrees(tree).map(node => TreeSnapshot(node, node.source, node.span))

  private def assertSnapshotUnchanged(
      before: Vector[TreeSnapshot]
  )(using Context): Unit =
    val after = before.headOption.fold(Vector.empty[untpd.Tree])(value => allTrees(value.tree))
    assertEquals(after.size, before.size)
    after.zip(before).foreach((actual, expected) => assert(actual.eq(expected.tree)))
    before.foreach(value =>
      assertEquals(value.tree.source, value.source)
      assertEquals(value.tree.span, value.span)
    )

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
