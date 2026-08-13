package quasiquotes.definitions

import _root_.dotty.tools.dotc.ast.untpd
import _root_.dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import _root_.dotty.tools.dotc.core.Flags
import _root_.dotty.tools.dotc.parsing.Parsers.Parser
import _root_.dotty.tools.dotc.reporting.StoreReporter
import _root_.dotty.tools.dotc.util.SourceFile

import quasiquotes.parser.DottySourceSpanAdapter
import quasiquotes.source.SourceSpan

private[definitions] enum ProbePlacement derives CanEqual:
  case TopLevel
  case Member
  case Local

private[definitions] final case class RawDefinitionParameterSummary(
    name: String,
    treeKind: String,
    typeTree: String,
    isContextual: Boolean
) derives CanEqual

private[definitions] final case class RawDefinitionSummary(
    kind: String,
    name: String,
    sourceName: String,
    parameterClauseSizes: List[Int],
    parameters: List[RawDefinitionParameterSummary],
    childOrder: List[String],
    typeTree: String,
    bodyTree: String,
    isMutable: Boolean,
    isLazy: Boolean,
    placement: ProbePlacement,
    definitionSpan: Option[SourceSpan],
    nameSpan: Option[SourceSpan],
    typeSpan: Option[SourceSpan],
    bodySpan: Option[SourceSpan]
) derives CanEqual:
  def stableShape: String =
    val clauses = parameterClauseSizes.mkString("[", ",", "]")
    s"$kind(name=$name,sourceName=$sourceName,paramss=$clauses,type=$typeTree,body=$bodyTree,mutable=$isMutable,lazy=$isLazy)"

private[definitions] object RawDefinitionProbe:
  def compilationUnit(source: String): Either[List[String], Vector[RawDefinitionSummary]] =
    parse(source, _.parse(), ProbePlacement.TopLevel)

  def expression(source: String): Either[List[String], Vector[RawDefinitionSummary]] =
    parse(source, _.expr(), ProbePlacement.Local)

  private def parse(
      source: String,
      entry: Parser => untpd.Tree,
      rootPlacement: ProbePlacement
  ): Either[List[String], Vector[RawDefinitionSummary]] =
    val base = new ContextBase
    val reporter = new StoreReporter(null)
    given Context = base.initialCtx.fresh.setReporter(reporter)

    val tree = entry(new Parser(SourceFile.virtual("DefinitionProbe.scala", source)))
    val messages = reporter.pendingMessages.map(_.message).toList
    if messages.nonEmpty then Left(messages)
    else Right(collect(source, tree, rootPlacement))

  private def collect(
      source: String,
      tree: untpd.Tree,
      placement: ProbePlacement
  )(using Context): Vector[RawDefinitionSummary] =
    tree match
      case packageDef: untpd.PackageDef =>
        packageDef.stats.toVector.flatMap(collect(source, _, ProbePlacement.TopLevel))
      case definition: untpd.DefDef =>
        Vector(summarizeDef(source, definition, placement))
      case definition: untpd.ValDef =>
        Vector(summarizeVal(source, definition, placement))
      case untpd.ModuleDef(_, template) =>
        template.body.toVector.flatMap(collect(source, _, ProbePlacement.Member))
      case definition: untpd.TypeDef =>
        definition.rhs match
          case template: untpd.Template =>
            template.body.toVector.flatMap(collect(source, _, ProbePlacement.Member))
          case rhs =>
            Vector(summarizeTypeAlias(source, definition, rhs, placement))
      case block: untpd.Block =>
        block.stats.toVector.flatMap(collect(source, _, ProbePlacement.Local)) ++
          collect(source, block.expr, ProbePlacement.Local)
      case _ =>
        Vector.empty

  private def summarizeDef(
      source: String,
      definition: untpd.DefDef,
      placement: ProbePlacement
  )(using Context): RawDefinitionSummary =
    val tpt = definition.tpt
    val rhs = definition.rhs
    val parameters = definition.paramss.flatten.collect {
      case parameter: untpd.ValDef =>
        RawDefinitionParameterSummary(
          parameter.name.toString,
          parameter.getClass.getSimpleName,
          treeShape(parameter.tpt),
          parameter.mods.is(Flags.Given) || parameter.mods.is(Flags.Implicit)
        )
    }
    val definitionSpan = DottySourceSpanAdapter.fromTree(definition)
    val typeSpan = DottySourceSpanAdapter.fromTree(tpt)
    RawDefinitionSummary(
      kind = "DefDef",
      name = definition.name.toString,
      sourceName = sourceName(source, definitionSpan, typeSpan, definition.name.toString),
      parameterClauseSizes = definition.paramss.map(_.size),
      parameters = parameters,
      childOrder = parameters.indices.map(index => s"parameter-$index").toList :::
        List("result-type", "body"),
      typeTree = treeShape(tpt),
      bodyTree = treeShape(rhs),
      isMutable = definition.mods.is(Flags.Mutable),
      isLazy = definition.mods.is(Flags.Lazy),
      placement = placement,
      definitionSpan = definitionSpan,
      nameSpan = nameSpan(source, definitionSpan, typeSpan, definition.name.toString),
      typeSpan = typeSpan,
      bodySpan = DottySourceSpanAdapter.fromTree(rhs)
    )

  private def summarizeVal(
      source: String,
      definition: untpd.ValDef,
      placement: ProbePlacement
  )(using Context): RawDefinitionSummary =
    val tpt = definition.tpt
    val rhs = definition.rhs
    val definitionSpan = DottySourceSpanAdapter.fromTree(definition)
    val typeSpan = DottySourceSpanAdapter.fromTree(tpt)
    RawDefinitionSummary(
      kind = "ValDef",
      name = definition.name.toString,
      sourceName = sourceName(source, definitionSpan, typeSpan, definition.name.toString),
      parameterClauseSizes = Nil,
      parameters = Nil,
      childOrder = List("declared-type", "right-hand-side"),
      typeTree = treeShape(tpt),
      bodyTree = treeShape(rhs),
      isMutable = definition.mods.is(Flags.Mutable),
      isLazy = definition.mods.is(Flags.Lazy),
      placement = placement,
      definitionSpan = definitionSpan,
      nameSpan = nameSpan(source, definitionSpan, typeSpan, definition.name.toString),
      typeSpan = typeSpan,
      bodySpan = DottySourceSpanAdapter.fromTree(rhs)
    )

  private def summarizeTypeAlias(
      source: String,
      definition: untpd.TypeDef,
      rhs: untpd.Tree,
      placement: ProbePlacement
  ): RawDefinitionSummary =
    val definitionSpan = DottySourceSpanAdapter.fromTree(definition)
    val rhsSpan = DottySourceSpanAdapter.fromTree(rhs)
    RawDefinitionSummary(
      kind = "TypeDef",
      name = definition.name.toString,
      sourceName = sourceName(source, definitionSpan, rhsSpan, definition.name.toString),
      parameterClauseSizes = Nil,
      parameters = Nil,
      childOrder = List("aliased-type"),
      typeTree = treeShape(rhs),
      bodyTree = "EmptyTree",
      isMutable = false,
      isLazy = false,
      placement = placement,
      definitionSpan = definitionSpan,
      nameSpan = nameSpan(source, definitionSpan, rhsSpan, definition.name.toString),
      typeSpan = rhsSpan,
      bodySpan = None
    )

  private def treeShape(tree: untpd.Tree): String =
    tree match
      case untpd.EmptyTree => "EmptyTree"
      case _: untpd.TypeTree => "InferredTypeTree"
      case untpd.Ident(name) => s"Ident(${name.toString})"
      case untpd.Number(digits, _) => s"Number($digits)"
      case untpd.Literal(constant) => s"Literal(${String.valueOf(constant.value)})"
      case untpd.Select(qualifier, name) => s"Select(${treeShape(qualifier)},${name.toString})"
      case untpd.Apply(function, arguments) =>
        s"Apply(${treeShape(function)},[${arguments.map(treeShape).mkString(",")}])"
      case untpd.Block(stats, expression) =>
        s"Block(stats=${stats.size},expr=${treeShape(expression)})"
      case other => other.getClass.getSimpleName

  private def sourceName(
      source: String,
      definitionSpan: Option[SourceSpan],
      followingSpan: Option[SourceSpan],
      decodedName: String
  ): String =
    nameSpan(source, definitionSpan, followingSpan, decodedName)
      .map(span => source.substring(span.start, span.end))
      .getOrElse(decodedName)

  private def nameSpan(
      source: String,
      definitionSpan: Option[SourceSpan],
      followingSpan: Option[SourceSpan],
      decodedName: String
  ): Option[SourceSpan] =
    definitionSpan.flatMap { definition =>
      val searchEnd = followingSpan.map(_.start).getOrElse(definition.end)
      val candidates = Vector(s"`$decodedName`", decodedName)
        .flatMap { spelling =>
          val index = source.indexOf(spelling, definition.start)
          Option.when(index >= definition.start && index + spelling.length <= searchEnd) {
            SourceSpan(index, index + spelling.length)
          }
        }
      candidates.sortBy(_.start).headOption
    }
