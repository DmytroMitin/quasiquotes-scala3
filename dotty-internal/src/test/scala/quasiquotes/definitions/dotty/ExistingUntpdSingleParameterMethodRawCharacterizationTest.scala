package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter

class ExistingUntpdSingleParameterMethodRawCharacterizationTest
    extends munit.FunSuite:
  private val fixtures = Vector(
    (
      "canonical",
      """class Box:
        |  def convert(x: Int): String = x.toString
        |""".stripMargin,
      0,
      "Box",
      Vector("def:convert"),
      "convert",
      "x",
      "Int",
      "String",
      "Select"
    ),
    (
      "fully-renamed",
      """class Vessel:
        |  val before: Int = 1
        |  def transform(payload: String): Boolean =
        |    payload.nonEmpty && payload.reverse.nonEmpty
        |  type After = String
        |""".stripMargin,
      1,
      "Vessel",
      Vector("val:before", "def:transform", "type:After"),
      "transform",
      "payload",
      "String",
      "Boolean",
      "InfixOp"
    )
  )

  fixtures.foreach {
    case (
          label,
          source,
          methodIndex,
          className,
          memberLabels,
          methodName,
          parameterName,
          parameterTypeName,
          resultTypeName,
          rhsKind
        ) =>
    test(s"characterizes existing single-parameter method topology: $label") {
      withContext {
        val root = parseSingleTypeDef(source)
        val template = root.rhs.asInstanceOf[untpd.Template]
        val body = template.body.toVector
        val method = body(methodIndex).asInstanceOf[untpd.DefDef]
        val parameter = method.paramss.head.head.asInstanceOf[untpd.ValDef]
        val graph = ExistingUntpdClassMemberFilter.allTrees(root)
        val captured = ExistingUntpdClassMemberFilter.capture(root).toOption.get

        assertEquals(root.name.toString, className)
        assert(root.rhs.eq(template))
        assertEquals(body.map(memberLabel), memberLabels)
        assertEquals(captured.members.map(_.index), body.indices.toVector)
        captured.members.zip(body).foreach((member, tree) => assert(member.tree.eq(tree)))
        assert(captured.members(methodIndex).tree.eq(method))
        assertEquals(method.name.toString, methodName)
        assertEquals(method.mods.flags.toString, "129")
        assertEquals(method.paramss.map(_.size), List(1))
        assertEquals(parameter.name.toString, parameterName)
        assertEquals(parameter.mods.flags.toString, "259")
        assert(parameter.rhs.isEmpty)
        assertIdentName(parameter.tpt, parameterTypeName)
        assertIdentName(method.tpt, resultTypeName)
        assertEquals(method.rhs.getClass.getSimpleName, rhsKind)
        graph.filterNot(_.isEmpty).foreach { tree =>
          assert(tree.source.exists, clues(label, node(tree)))
          assert(tree.span.exists, clues(label, node(tree)))
          assertEquals(tree.symbol, NoSymbol, clues(label, node(tree)))
        }
      }
    }
  }

  private def assertIdentName(tree: untpd.Tree, expected: String): Unit = tree match
    case ident: untpd.Ident => assertEquals(ident.name.toString, expected)
    case other => fail(s"expected Ident($expected), found ${node(other)}")

  private def parseSingleTypeDef(source: String)(using outer: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U028RawMethod.scala", source)
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
    case method: untpd.DefDef => s"def:${method.name}"
    case value: untpd.ValDef => s"val:${value.name}"
    case value: untpd.TypeDef => s"type:${value.name}"
    case other => other.getClass.getSimpleName

  private def node(tree: untpd.Tree): String =
    if tree == null then "null"
    else
      val name = tree match
        case named: untpd.NamedDefTree => s":${named.name}"
        case ident: untpd.Ident => s":${ident.name}"
        case select: untpd.Select => s":${select.name}"
        case _ => ""
      val span = tree.span
      val spanText =
        if span.exists then s"${span.start},${span.point},${span.end}"
        else "NoSpan"
      s"${tree.getClass.getSimpleName}$name($spanText)"

  private def withContext[A](body: Context ?=> A): A =
    val base = new ContextBase
    body(using base.initialCtx)
