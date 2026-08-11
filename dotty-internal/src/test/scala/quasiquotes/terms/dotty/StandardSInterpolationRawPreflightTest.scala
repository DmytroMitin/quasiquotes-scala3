package quasiquotes.terms.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.parsing.Tokens
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.SourceFile

class StandardSInterpolationRawPreflightTest extends munit.FunSuite:
  private val sources = Vector(
    "s\"\"",
    "s\"plain\"",
    "s\"hello $name\"",
    "s\"hello ${name}\"",
    "s\"value = ${foo(x)}\"",
    "s\"$a / $b\"",
    "s\"prefix ${foo(x)} suffix\"",
    "s\"literal $$ dollar\"",
    "s\"quote = \\\"$name\\\"\"",
    "s\"slash = \\\\$name\"",
    "s\"line = \\n$name\"",
    "s\"quote-only = \\\"\"",
    "s\"slash-only = " + "\\" + "\\" + "\"",
    "s\"return = \\r\"",
    "s\"tab = \\t\"",
    "s\"back = \\b\"",
    "s\"form = \\f\"",
    "s\"control = " + "\\" + "u0001\"",
    "s\"unicode = λ😀\"",
    "s\"${-x}\"",
    "s\"${foo(x)}\"",
    "s\"${(x, y)}\"",
    "s\"${if cond then x else y}\"",
    "s\"${(x: Int)}\"",
    "s\"${List[Either[Int, String]]}\""
  )

  sources.foreach { source =>
    test(s"records exact raw interpolation structure and spans for ${escaped(source)}") {
      val base = new ContextBase
      val reporter = new StoreReporter(null)
      given Context = base.initialCtx.fresh.setReporter(reporter)
      val sourceFile = SourceFile.virtual("StandardSInterpolationPreflight.scala", source)
      val parser = new Parser(sourceFile)
      val raw = parser.expr()

      assertEquals(reporter.pendingMessages.map(_.message).toList, Nil)
      assertEquals(parser.in.token, Tokens.EOF)
      raw match
        case interpolation @ untpd.InterpolatedString(prefix, segments) =>
          println(
            s"S_INTERPOLATION_PREFLIGHT source=${escaped(source)} " +
              s"sourceFile=${sourceFile.path} rootClass=${interpolation.getClass.getName} " +
              s"prefix=${prefix.toString} prefixRepresentation=scalar-name " +
              s"segments=${segments.size} root=${describe(interpolation, source)}"
          )
          segments.zipWithIndex.foreach { case (segment, index) =>
            println(
              s"S_INTERPOLATION_PREFLIGHT segment=$index ${describeTree(segment, source)}"
            )
          }
          assertEquals(prefix.toString, "s")
          assert(segments.nonEmpty)
          assertEquals(interpolation.symbol, NoSymbol)
          assert(interpolation.span.exists)
          assertEquals(interpolation.span.start, 0)
          assertEquals(interpolation.span.end, source.length)
          allTrees(interpolation).foreach(tree => assertEquals(tree.symbol, NoSymbol))
        case other =>
          fail(s"expected InterpolatedString, found ${other.getClass.getName}: $other")
    }
  }

  private def describe(tree: untpd.Tree, source: String)(using Context): String =
    val children = directChildren(tree)
    s"${describeTree(tree, source)} children=[${children.map(describe(_, source)).mkString(",")}]"

  private def describeTree(tree: untpd.Tree, source: String)(using Context): String =
    val span = tree.span
    val bounds =
      if span.exists then s"${span.start}..${span.point}..${span.end}"
      else "NoSpan"
    val slice =
      if span.exists && span.start >= 0 && span.end <= source.length then
        escaped(source.slice(span.start, span.end))
      else "<none>"
    val sourceIdentity =
      if tree.source.exists then tree.source.path else "NoSource"
    val detail = tree match
      case untpd.InterpolatedString(prefix, segments) =>
        s"prefix=${prefix.toString},segments=${segments.size}"
      case untpd.Thicket(trees) => s"trees=${trees.size}"
      case untpd.Literal(constant) =>
        s"constant=${escaped(String.valueOf(constant.value))}"
      case untpd.Block(statements, expression) =>
        s"statements=${statements.size},expression=${expression.getClass.getSimpleName}"
      case untpd.Ident(name) => s"name=${name.toString}"
      case _ => ""
    s"class=${tree.getClass.getName},span=$bounds,slice=$slice,source=$sourceIdentity," +
      s"noSymbol=${tree.symbol == NoSymbol},zeroWidth=${span.exists && span.start == span.end},$detail"

  private def directChildren(tree: untpd.Tree): Vector[untpd.Tree] =
    tree match
      case untpd.InterpolatedString(_, segments) => segments.toVector
      case untpd.Thicket(trees) => trees.toVector
      case untpd.Block(statements, expression) => statements.toVector :+ expression
      case untpd.Select(qualifier, _) => Vector(qualifier)
      case untpd.Apply(function, arguments) => function +: arguments.toVector
      case untpd.InfixOp(left, operator, right) => Vector(left, operator, right)
      case untpd.PrefixOp(operator, operand) => Vector(operator, operand)
      case untpd.Typed(expression, typeTree) => Vector(expression, typeTree)
      case untpd.Tuple(elements) => elements.toVector
      case untpd.If(condition, thenBranch, elseBranch) =>
        Vector(condition, thenBranch, elseBranch)
      case untpd.Parens(expression) => Vector(expression)
      case untpd.AppliedTypeTree(constructor, arguments) =>
        constructor +: arguments.toVector
      case _ => Vector.empty

  private def allTrees(tree: untpd.Tree): Vector[untpd.Tree] =
    tree +: directChildren(tree).flatMap(allTrees)

  private def escaped(value: String): String =
    val result = new StringBuilder
    value.foreach {
      case '\\' => result.append("\\\\")
      case '\n' => result.append("\\n")
      case '\r' => result.append("\\r")
      case '\t' => result.append("\\t")
      case '"' => result.append("\\\"")
      case char => result.append(char)
    }
    result.toString
