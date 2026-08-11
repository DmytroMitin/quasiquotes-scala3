package quasiquotes.phase74

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.parsing.Tokens
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.SourceFile

private[phase74] final case class RawProbeNode(
    kind: String,
    start: Int,
    point: Int,
    end: Int,
    source: String,
    detail: String,
    noSymbol: Boolean,
    children: Vector[RawProbeNode]
) derives CanEqual:
  def compact: String =
    val childText =
      if children.isEmpty then ""
      else children.map(_.compact).mkString("[", ",", "]")
    s"$kind($start..$point..$end,${RawBlockLambdaProbe.quoted(source)},$detail,noSymbol=$noSymbol)$childText"

private[phase74] final case class RawProbeEvidence(
    source: String,
    root: RawProbeNode,
    arrowStart: Option[Int],
    arrowEnd: Option[Int]
) derives CanEqual:
  def allNodes: Vector[RawProbeNode] =
    def loop(node: RawProbeNode): Vector[RawProbeNode] =
      node +: node.children.flatMap(loop)
    loop(root)

private[phase74] object RawBlockLambdaProbe:
  def expression(source: String): Either[List[String], RawProbeEvidence] =
    val base = new ContextBase
    val reporter = new StoreReporter(null)
    given Context = base.initialCtx.fresh.setReporter(reporter)
    val sourceFile = SourceFile.virtual("Phase74RawProbe.scala", source)
    val parser = new Parser(sourceFile)
    val tree = parser.expr()
    val messages = reporter.pendingMessages.map(_.message).toList
    if messages.nonEmpty || parser.in.token != Tokens.EOF then
      Left(messages :+ s"remainingLexeme=${Tokens.tokenString(parser.in.token)}")
    else
      val arrow = source.indexOf("=>")
      Right(
        RawProbeEvidence(
          source,
          describe(tree, source),
          Option.when(arrow >= 0)(arrow),
          Option.when(arrow >= 0)(arrow + 2)
        )
      )

  private def describe(tree: untpd.Tree, source: String)(using Context): RawProbeNode =
    val span = tree.span
    val (start, point, end, slice) =
      if span.exists && span.start >= 0 && span.end <= source.length then
        (span.start, span.point, span.end, source.slice(span.start, span.end))
      else (-1, -1, -1, "<none>")
    val detail = tree match
      case untpd.Function(arguments, _) => s"parameters=${arguments.size}"
      case definition: untpd.ValDef =>
        s"name=${definition.name},type=${kind(definition.tpt)}"
      case definition: untpd.DefDef =>
        s"name=${definition.name},parameterClauses=${definition.paramss.map(_.size).mkString("[", ",", "]")},type=${kind(definition.tpt)}"
      case untpd.Block(statements, _) => s"stats=${statements.size}"
      case untpd.Apply(_, arguments) => s"arguments=${arguments.size}"
      case untpd.Select(_, name) => s"name=$name"
      case untpd.Ident(name) => s"name=$name"
      case untpd.Number(digits, _) => s"digits=$digits"
      case untpd.Literal(constant) => s"value=${constant.value}"
      case _ => ""
    RawProbeNode(
      kind(tree),
      start,
      point,
      end,
      slice,
      detail,
      tree.symbol == NoSymbol,
      directChildren(tree).map(describe(_, source))
    )

  private def kind(tree: untpd.Tree): String =
    tree.getClass.getSimpleName.stripSuffix("$")

  private def directChildren(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree match
      case untpd.Function(arguments, body) => arguments.toVector :+ body
      case definition: untpd.ValDef => Vector(definition.tpt, definition.rhs)
      case definition: untpd.DefDef =>
        definition.paramss.flatten.toVector ++ Vector(definition.tpt, definition.rhs)
      case untpd.Block(statements, expression) => statements.toVector :+ expression
      case untpd.Apply(function, arguments) => function +: arguments.toVector
      case untpd.Select(qualifier, _) => Vector(qualifier)
      case untpd.InfixOp(left, operator, right) => Vector(left, operator, right)
      case untpd.Parens(expression) => Vector(expression)
      case untpd.If(condition, thenBranch, elseBranch) =>
        Vector(condition, thenBranch, elseBranch)
      case _ => Vector.empty

  private[phase74] def quoted(value: String): String =
    "\"" + value.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n") + "\""
