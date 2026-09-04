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

class ExistingUntpdClassMemberFilterTest extends munit.FunSuite:
  private val MixedSource =
    """@classMarker
      |final class VersionAdapter:
      |  @memberMarker def kept(): Int = 42
      |  val opaque: Int = 7
      |  type Alias = Int
      |  object Nested
      |  def same(): Int = 1
      |  def same(value: Int): Int = value
      |  def removedForThisLine(): Int = apiThatDoesNotExistHere()
      |""".stripMargin

  test("captures ordered direct members and retains an arbitrary ordered subset by index") {
    withContext {
      val root = parseSingleTypeDef(MixedSource)
      val originalTemplate = root.rhs.asInstanceOf[untpd.Template]
      val originalBody = originalTemplate.body.toVector

      val captured = ExistingUntpdClassMemberFilter.capture(root).toOption.get
      val result = ExistingUntpdClassMemberFilter
        .retain(captured, Vector(0, 1, 2, 3, 5))
        .toOption
        .get

      assertEquals(captured.members.map(_.index), Vector(0, 1, 2, 3, 4, 5, 6))
      assertEquals(
        result.rebuiltTemplate.body.map(memberLabel),
        List("def:kept", "val:opaque", "type:Alias", "object:Nested", "def:same")
      )
      Vector(0 -> 0, 1 -> 1, 2 -> 2, 3 -> 3, 4 -> 5).foreach {
        case (rebuiltIndex, originalIndex) =>
          assert(result.rebuiltTemplate.body(rebuiltIndex).eq(originalBody(originalIndex)))
      }
      assert(!result.rebuiltTemplate.body.exists(_.eq(originalBody(4))))
      assert(!result.rebuiltTemplate.body.exists(_.eq(originalBody(6))))
      assert(!result.rebuiltTemplate.body.exists(_.isEmpty))
      assert(!result.rebuiltTemplate.eq(originalTemplate))
      assert(!result.rebuiltRoot.eq(root))
      assert(result.rebuiltRoot.mods.eq(root.mods))
      assert(result.rebuiltTemplate.constr.eq(originalTemplate.constr))
      assert(result.rebuiltTemplate.parentsOrDerived.eq(originalTemplate.parentsOrDerived))
      assert(result.rebuiltTemplate.derived.eq(originalTemplate.derived))
      assert(result.rebuiltTemplate.self.eq(originalTemplate.self))
      assert(result.rebuiltRoot.source == root.source)
      assert(result.rebuiltRoot.span == root.span)
      assert(result.rebuiltTemplate.source == originalTemplate.source)
      assert(result.rebuiltTemplate.span == originalTemplate.span)
      assertEquals(originalTemplate.body.toVector, originalBody)
      originalTemplate.body.zip(originalBody).foreach((actual, expected) => assert(actual.eq(expected)))
    }
  }

  test("selection is index-authoritative for overload-like duplicate names") {
    withContext {
      val root = parseSingleTypeDef(
        """class VersionAdapter:
          |  def same(): Int = 1
          |  def same(value: Int): Int = value
          |""".stripMargin
      )
      val original = root.rhs.asInstanceOf[untpd.Template].body
      val captured = ExistingUntpdClassMemberFilter.capture(root).toOption.get

      val first = ExistingUntpdClassMemberFilter.retain(captured, Vector(0)).toOption.get
      val second = ExistingUntpdClassMemberFilter.retain(captured, Vector(1)).toOption.get

      assert(first.rebuiltTemplate.body.head.eq(original.head))
      assert(second.rebuiltTemplate.body.head.eq(original(1)))
      assertEquals(memberLabel(first.rebuiltTemplate.body.head), "def:same")
      assertEquals(memberLabel(second.rebuiltTemplate.body.head), "def:same")
    }
  }

  test("rejects null, unsupported, malformed, duplicate, foreign-index, and reordered inputs") {
    withContext {
      val root = parseSingleTypeDef(MixedSource)
      val originalTemplate = root.rhs.asInstanceOf[untpd.Template]
      val captured = ExistingUntpdClassMemberFilter.capture(root).toOption.get
      val sourceFreeMalformed =
        given SourceFile = NoSource
        val template = untpd.Template(null, Nil, Nil, untpd.EmptyValDef, List(null))
        untpd.TypeDef(typeName("Malformed"), template)
      val malformedMemberSequence =
        given SourceFile = NoSource
        val template = untpd.Template(
          originalTemplate.constr,
          originalTemplate.parentsOrDerived,
          originalTemplate.derived,
          originalTemplate.self,
          List(null)
        )
        untpd.TypeDef(typeName("MalformedMember"), template)

      assertCode(ExistingUntpdClassMemberFilter.capture(null), "CONTAINER_REQUIRED")
      assertCode(
        ExistingUntpdClassMemberFilter.capture(untpd.EmptyTree),
        "UNSUPPORTED_OUTER_TOPOLOGY"
      )
      assertCode(
        ExistingUntpdClassMemberFilter.capture(parseSingleTypeDef("type Alias = Int")),
        "TEMPLATE_REQUIRED"
      )
      assertCode(
        ExistingUntpdClassMemberFilter.capture(parseSingleTypeDef("trait VersionAdapter")),
        "UNSUPPORTED_OUTER_TOPOLOGY"
      )
      assertCode(
        ExistingUntpdClassMemberFilter.capture(sourceFreeMalformed),
        "TEMPLATE_CONSTRUCTOR_REQUIRED"
      )
      assertCode(
        ExistingUntpdClassMemberFilter.capture(malformedMemberSequence),
        "MALFORMED_DIRECT_MEMBER"
      )
      assertCode(
        ExistingUntpdClassMemberFilter.retain(null, Vector(0)),
        "CAPTURE_REQUIRED"
      )
      assertCode(
        ExistingUntpdClassMemberFilter.retain(captured, null),
        "SELECTION_REQUIRED"
      )
      assertCode(
        ExistingUntpdClassMemberFilter.retain(captured, Vector(0, 0)),
        "DUPLICATE_RETAINED_INDEX"
      )
      assertCode(
        ExistingUntpdClassMemberFilter.retain(captured, Vector(1, 0)),
        "RETAINED_INDEX_ORDER"
      )
      assertCode(
        ExistingUntpdClassMemberFilter.retain(captured, Vector(7)),
        "RETAINED_INDEX_NOT_CAPTURED"
      )
    }
  }

  test("rejects a graph that already carries compiler symbols") {
    withContext {
      val root = parseSingleTypeDef(MixedSource)
      val template = root.rhs.asInstanceOf[untpd.Template]
      val symbol = newSymbol(NoSymbol, termName("u023Symbol"), EmptyFlags, NoType)
      val symbolBearing = untpd.Ident(termName("symbolBearing")).withType(symbol.termRef)
      val changedTemplate = untpd.cpy.Template(template)(
        template.constr,
        template.parentsOrDerived,
        template.derived,
        template.self,
        symbolBearing :: template.body.tail
      )
      val changedRoot = untpd.cpy.TypeDef(root)(root.name, changedTemplate)

      assertCode(
        ExistingUntpdClassMemberFilter.capture(changedRoot),
        "PRE_TYPER_CONTAINER_REQUIRED"
      )
    }
  }

  test("rejects source-free shells because truthful insertion provenance is unavailable") {
    withContext {
      val parsed = parseSingleTypeDef(
        """class VersionAdapter:
          |  def kept(): Int = 42
          |""".stripMargin
      )
      val parsedTemplate = parsed.rhs.asInstanceOf[untpd.Template]
      given SourceFile = NoSource
      val template = untpd.Template(
        parsedTemplate.constr,
        parsedTemplate.parentsOrDerived,
        parsedTemplate.derived,
        parsedTemplate.self,
        parsedTemplate.body
      )
      val root = untpd.TypeDef(typeName("VersionAdapter"), template)

      assertCode(
        ExistingUntpdClassMemberFilter.capture(root),
        "CHANGED_SHELL_PROVENANCE_REQUIRED"
      )
    }
  }

  private def assertCode[A](
      result: Either[ExistingUntpdClassMemberFilterError, A],
      expected: String
  ): Unit = result match
    case Left(problem) => assertEquals(problem.code, expected)
    case Right(value) => fail(s"expected $expected, found success $value")

  private def parseSingleTypeDef(source: String)(using outer: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U023Filter.scala", source)
    given Context = outer.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed match
      case packageDef: untpd.PackageDef =>
        packageDef.stats match
          case (root: untpd.TypeDef) :: Nil => root
          case other => fail(s"expected one TypeDef, found $other")
      case other => fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")

  private def memberLabel(tree: untpd.Tree): String = tree match
    case value: untpd.DefDef => s"def:${value.name}"
    case value: untpd.ValDef => s"val:${value.name}"
    case value: untpd.ModuleDef => s"object:${value.name}"
    case value: untpd.TypeDef => s"type:${value.name}"
    case other => other.getClass.getSimpleName

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
