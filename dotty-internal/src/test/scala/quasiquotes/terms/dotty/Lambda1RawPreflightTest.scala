package quasiquotes.terms.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.parsing.Tokens
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.SourceFile

class Lambda1RawPreflightTest extends munit.FunSuite:
  private val sources = Vector(
    "(x: Int) => x",
    "(x: Int) => x + 1",
    "(x: Int) => f(x)",
    "(x: String) => x",
    "(x: Boolean) => if x then false else true",
    "(x: Int) => (x: Int)",
    "(x: Int) => new java.lang.StringBuilder(x)",
    "(x: Int) => (x, if true then x else f(x))",
    "(x: Int) => s\"$x\""
  )

  sources.foreach { source =>
    test(s"records exact raw Lambda1 structure and spans: $source") {
      val base = new ContextBase
      val reporter = new StoreReporter(null)
      given Context = base.initialCtx.fresh.setReporter(reporter)
      val sourceFile = SourceFile.virtual("Lambda1RawPreflight.scala", source)
      val parser = new Parser(sourceFile)
      val raw = parser.expr()

      assertEquals(reporter.pendingMessages.map(_.message).toList, Nil)
      assertEquals(parser.in.token, Tokens.EOF)
      raw match
        case function: untpd.Function =>
          assertEquals(function.args.size, 1)
          val parameter = function.args.head match
            case value: untpd.ValDef => value
            case other =>
              fail(s"expected lambda parameter ValDef, found ${other.getClass.getName}")

          val children = directChildren(function)
          assertEquals(children, Vector(parameter, function.body))
          assertEquals(parameter.name.toString, "x")
          assert(parameter.mods.hasFlags)
          assert(parameter.rhs.isEmpty)
          assertEquals(function.span.start, 0)
          assertEquals(function.span.end, source.length)
          assert(function.span.point >= function.span.start)
          assert(function.span.point <= function.span.end)
          assert(parameter.span.exists)
          assert(parameter.tpt.span.exists)
          assert(function.body.span.exists)
          allTrees(function).foreach(tree => assertEquals(tree.symbol, NoSymbol))

          println(
            s"LAMBDA1_RAW_PREFLIGHT source=${escaped(source)} length=${source.length} " +
              s"root=${describe(function, source)}"
          )
          println(
            s"LAMBDA1_RAW_PREFLIGHT parameterName=${parameter.name} " +
              s"parameterFlags=${parameter.mods.flags} parameter=${describe(parameter, source)}"
          )
          println(
            s"LAMBDA1_RAW_PREFLIGHT parameterType=${describe(parameter.tpt, source)} " +
              s"body=${describe(function.body, source)}"
          )
          println(
            s"LAMBDA1_RAW_PREFLIGHT identifiers=${allTrees(function).collect {
                case ident: untpd.Ident =>
                  s"${ident.name}:${spanText(ident)}:${slice(ident, source)}"
              }.mkString("[", ",", "]")}"
          )
        case other =>
          fail(s"expected Function, found ${other.getClass.getName}: $other")
    }
  }

  private def describe(tree: untpd.Tree, source: String)(using Context): String =
    val detail = tree match
      case value: untpd.Function => s"arguments=${value.args.size}"
      case value: untpd.ValDef =>
        s"name=${value.name},flags=${value.mods.flags},rhsEmpty=${value.rhs.isEmpty}"
      case value: untpd.Ident => s"name=${value.name}"
      case value: untpd.Literal => s"constant=${escaped(String.valueOf(value.const.value))}"
      case _ => ""
    s"class=${tree.getClass.getName},span=${spanText(tree)},slice=${slice(tree, source)}," +
      s"children=${directChildren(tree).map(_.getClass.getSimpleName).mkString("[", ",", "]")},$detail"

  private def spanText(tree: untpd.Tree): String =
    if tree.span.exists then s"${tree.span.start}..${tree.span.point}..${tree.span.end}"
    else "NoSpan"

  private def slice(tree: untpd.Tree, source: String): String =
    if tree.span.exists && tree.span.start >= 0 && tree.span.end <= source.length then
      escaped(source.slice(tree.span.start, tree.span.end))
    else "<none>"

  private def directChildren(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree match
      case value: untpd.Function => value.args.toVector :+ value.body
      case value: untpd.ValDef => Vector(value.tpt, value.rhs).filterNot(_.isEmpty)
      case value: untpd.Select => Vector(value.qualifier)
      case value: untpd.Apply => value.fun +: value.args.toVector
      case value: untpd.New => Vector(value.tpt)
      case value: untpd.InfixOp => Vector(value.left, value.op, value.right)
      case value: untpd.PrefixOp => Vector(value.op, value.od)
      case value: untpd.Typed => Vector(value.expr, value.tpt)
      case value: untpd.Tuple => value.trees.toVector
      case value: untpd.If => Vector(value.cond, value.thenp, value.elsep)
      case value: untpd.Parens => Vector(value.t)
      case value: untpd.InterpolatedString => value.segments.toVector
      case value: untpd.Thicket => value.trees.toVector
      case value: untpd.Block => value.stats.toVector :+ value.expr
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case _ => Vector.empty

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree +: directChildren(tree).flatMap(allTrees)

  private def escaped(value: String): String =
    value
      .replace("\\", "\\\\")
      .replace("\n", "\\n")
      .replace("\r", "\\r")
      .replace("\t", "\\t")
      .replace("\"", "\\\"")
