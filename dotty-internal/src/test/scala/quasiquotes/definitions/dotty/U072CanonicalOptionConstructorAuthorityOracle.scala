package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.{NoSource, SourceFile}

/** Test-only parser-derived authority oracle for U072. */
private[dotty] object U072CanonicalOptionConstructorAuthorityOracle:
  import ExistingClassUntypedRewrite.*

  enum Candidate(val syntax: String, val generatedPath: String):
    case Unqualified extends Candidate("Option[String]", "generated/U072UnqualifiedOption.scala")
    case Qualified extends Candidate("scala.Option[String]", "generated/U072QualifiedOption.scala")
    case Rooted extends Candidate("_root_.scala.Option[String]", "generated/U072RootedOption.scala")
    case RootedPair extends Candidate(
      "_root_.scala.Option[_root_.java.lang.String]",
      "generated/U072RootedOptionRootedString.scala"
    )

  final case class Error(code: String, detail: String):
    def message: String = s"$code: $detail"

  final case class PositionedType(
      candidate: Candidate,
      generated: untpd.AppliedTypeTree,
      positioned: untpd.AppliedTypeTree,
      oldSite: untpd.Tree
  )

  final case class Proof(
      reconstructed: ExistingUntpdClassMemberFilter.Reconstructed,
      descriptor: ExistingUntpdOrdinaryMethodDescriptor.Descriptor,
      oldMethod: untpd.DefDef,
      positionedMethod: untpd.DefDef,
      parameterType: Option[PositionedType],
      resultType: Option[PositionedType],
      bodyEdited: Boolean,
      untouchedIdentity: Boolean,
      orderPreserved: Boolean
  )

  def apply(capture: Capture, plan: EditPlan, candidate: Candidate)(using Context): Either[Error, Proof] =
    U071OptionStringAppliedMethodTypeOracle(capture, plan)
      .left.map(problem => Error(problem.code, problem.detail))
      .flatMap(base => replaceCandidate(capture, base, candidate))

  private[dotty] def positionCandidate(
      oldSite: untpd.Tree,
      candidate: Candidate
  )(using Context): Either[Error, PositionedType] =
    for
      site <- Option(oldSite).filterNot(_.isEmpty).toRight(error("OLD_SITE_REQUIRED", "the old direct Type site was absent."))
      _ <- Either.cond(
        Option(site.source).exists(_.exists) && site.span.exists,
        (),
        error("OLD_SITE_REQUIRED", "the old direct Type site has no source/span.")
      )
      generated <- parseCandidate(candidate)
      fresh <- parseCandidate(candidate)
      positioned = fresh.cloneIn(site.source).withSpan(site.span)
      _ <- validatePositioned(generated, positioned, site, candidate)
    yield PositionedType(candidate, generated, positioned, site)

  private[dotty] def validatePositionedForTest(
      generated: untpd.AppliedTypeTree,
      positioned: untpd.AppliedTypeTree,
      oldSite: untpd.Tree,
      candidate: Candidate
  )(using Context): Either[Error, Unit] =
    validatePositioned(generated, positioned, oldSite, candidate)

  private def replaceCandidate(
      capture: Capture,
      base: U071OptionStringAppliedMethodTypeOracle.Proof,
      candidate: Candidate
  )(using Context): Either[Error, Proof] =
    for
      parameterType <- base.parameterType match
        case None => Right(None)
        case Some(_) => positionCandidate(base.descriptor.parameterClauses.head.head.tpt, candidate).map(Some(_))
      resultType <- base.resultType match
        case None => Right(None)
        case Some(_) => positionCandidate(base.descriptor.resultType, candidate).map(Some(_))
      parameter <- parameterType match
        case None => Right(base.descriptor.parameterClauses.head.head.tree)
        case Some(value) => positionParameter(base.descriptor.parameterClauses.head.head.tree, value)
      method <- positionMethod(base, parameter, resultType)
      members = capture.exact.originalTemplate.body.toVector.updated(base.descriptor.memberIndex, method)
      reconstructed <- ExistingUntpdClassMemberFilter
        .reconstruct(capture.exact, members)
        .left.map(problem => error("OWNER_RECONSTRUCTION_REJECTED", problem.message))
      _ <- Either.cond(
        allTrees(reconstructed.root).forall(node => node != null && node.symbol == NoSymbol && !node.isInstanceOf[untpd.TypedSplice]),
        (),
        error("FINAL_GRAPH_INVALID", "the reconstructed owner contains a symbol, TypedSplice, or null node before Typer.")
      )
    yield Proof(
      reconstructed,
      base.descriptor,
      base.oldMethod,
      method,
      parameterType,
      resultType,
      base.bodyEdited,
      members.indices.forall(index => index == base.descriptor.memberIndex || members(index).eq(capture.exact.originalTemplate.body(index))),
      members.size == capture.exact.originalTemplate.body.size
    )

  private def parseCandidate(candidate: Candidate)(using outer: Context): Either[Error, untpd.AppliedTypeTree] =
    val source =
      s"""class U072Generated:
         |  def echo(value: ${candidate.syntax}): ${candidate.syntax} = value
         |""".stripMargin
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit(candidate.generatedPath, source)
    given Context = outer.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    val errors = reporter.pendingMessages.toList
    if errors.nonEmpty then Left(error("PARSER_REJECTED", errors.mkString("; ")))
    else
      parsed match
        case packageDef: untpd.PackageDef =>
          packageDef.stats.collect { case root: untpd.TypeDef => root } match
            case root :: Nil =>
              root.rhs.asInstanceOf[untpd.Template].body.collect { case method: untpd.DefDef => method } match
                case method :: Nil =>
                  method.paramss.head.head match
                    case parameter: untpd.ValDef => parameter.tpt match
                      case applied: untpd.AppliedTypeTree =>
                        Either.cond(
                          exactTopology(applied, candidate),
                          applied,
                          error("PARSER_TOPOLOGY_INVALID", s"parser graph did not match ${candidate.syntax}.")
                        )
                      case other => Left(error("PARSER_TOPOLOGY_INVALID", s"expected AppliedTypeTree, found ${other.getClass.getSimpleName}."))
                    case other => Left(error("PARSER_TOPOLOGY_INVALID", s"expected ValDef, found ${other.getClass.getSimpleName}."))
                case other => Left(error("PARSER_TOPOLOGY_INVALID", s"expected one method, found $other."))
            case other => Left(error("PARSER_TOPOLOGY_INVALID", s"expected one TypeDef, found $other."))
        case other => Left(error("PARSER_TOPOLOGY_INVALID", s"expected PackageDef, found ${other.getClass.getSimpleName}."))

  private def positionParameter(oldParameter: untpd.ValDef, positioned: PositionedType)(using Context): Either[Error, untpd.ValDef] =
    given SourceFile = NoSource
    val sourceFree = untpd.ValDef(oldParameter.name, positioned.positioned, untpd.EmptyTree).withMods(oldParameter.mods)
    val result = untpd.cpy.ValDef(sourceFree)(
      sourceFree.name,
      positioned.positioned,
      sourceFree.rhs
    ).cloneIn(oldParameter.source).withSpan(oldParameter.span)
    Either.cond(
      !result.eq(oldParameter) && result.tpt.eq(positioned.positioned) && result.rhs.isEmpty &&
        result.name == oldParameter.name && result.mods.eq(oldParameter.mods) &&
        result.source == oldParameter.source && result.span == oldParameter.span && result.symbol == NoSymbol,
      result,
      error("PARAMETER_POSITION_INVALID", "the fresh parameter shell drifted from the exact old parameter site.")
    )

  private def positionMethod(
      base: U071OptionStringAppliedMethodTypeOracle.Proof,
      parameter: untpd.ValDef,
      result: Option[PositionedType]
  )(using Context): Either[Error, untpd.DefDef] =
    val selectedResult = result.fold(base.descriptor.resultType)(_.positioned)
    given SourceFile = NoSource
    val sourceFree = untpd.DefDef(
      base.descriptor.method.name,
      List(List(parameter)),
      selectedResult,
      base.positionedMethod.rhs
    ).withMods(base.descriptor.method.mods)
    val positioned = untpd.cpy.DefDef(sourceFree)(
      sourceFree.name,
      sourceFree.paramss,
      sourceFree.tpt,
      sourceFree.rhs
    ).cloneIn(base.descriptor.method.source).withSpan(base.descriptor.method.span)
    Either.cond(
      !positioned.eq(base.descriptor.method) && positioned.name == base.descriptor.method.name &&
        positioned.mods.eq(base.descriptor.method.mods) && positioned.source == base.descriptor.method.source &&
        positioned.span == base.descriptor.method.span && positioned.symbol == NoSymbol,
      positioned,
      error("METHOD_POSITION_INVALID", "the fresh method shell drifted from the exact old method site.")
    )

  private def validatePositioned(
      generated: untpd.AppliedTypeTree,
      positioned: untpd.AppliedTypeTree,
      oldSite: untpd.Tree,
      candidate: Candidate
  )(using Context): Either[Error, Unit] =
    val generatedNodes = allTrees(generated)
    val positionedNodes = allTrees(positioned)
    val children = positionedNodes.drop(1)
    val sourceText = generated.source.content.mkString
    val checks = Vector(
      "generatedTopology" -> exactTopology(generated, candidate),
      "positionedTopology" -> exactTopology(positioned, candidate),
      "generatedCount" -> (generatedNodes.size == expectedNodeCount(candidate)),
      "positionedCount" -> (positionedNodes.size == generatedNodes.size),
      "generatedSource" -> (
        generated.source.exists && generated.source.path == candidate.generatedPath && generated.span.exists &&
          generated.source.content.slice(generated.span.start, generated.span.end).mkString == candidate.syntax
      ),
      "freshOuter" -> (!positioned.eq(generated) && !positioned.eq(oldSite)),
      "oldOuterSite" -> (positioned.source == oldSite.source && positioned.span == oldSite.span),
      "generatedChildren" -> (children.nonEmpty && children.forall(node => node.source.exists && node.source.path == candidate.generatedPath && node.span.exists)),
      "boundedChildren" -> children.forall(node =>
        node.span.exists && node.span.start >= 0 && node.span.end <= sourceText.length && node.span.start <= node.span.end
      ),
      "sourceSlices" -> children.forall(node =>
        node.source.exists && node.span.exists && node.source.content.slice(node.span.start, node.span.end).nonEmpty
      ),
      "clean" -> positionedNodes.forall(node => node.symbol == NoSymbol && !node.isInstanceOf[untpd.TypedSplice]),
      "distinct" -> (positionedNodes.distinctBy(System.identityHashCode).size == positionedNodes.size),
      "freshRecursive" -> positionedNodes.forall(positionedNode => generatedNodes.forall(generatedNode => !positionedNode.eq(generatedNode)))
    )
    val valid = checks.forall(_._2)
    Either.cond(
      valid,
      (),
      error(
        "POSITION_EVIDENCE_INVALID",
        s"${candidate.syntax} lost exact parser topology, old outer provenance, generated child provenance, or fresh identity: ${checks.mkString(", ")}; " +
          s"generated=${generatedNodes.map(node => s"${node.getClass.getSimpleName}@${node.source.path}:${node.span}").mkString("|")}; " +
          s"positioned=${positionedNodes.map(node => s"${node.getClass.getSimpleName}@${node.source.path}:${node.span}").mkString("|")}; " +
          s"shared=${positionedNodes.zipWithIndex.collect { case (node, index) if generatedNodes.exists(_.eq(node)) => index }.mkString(",")}."
      )
    )

  private def expectedNodeCount(candidate: Candidate): Int = candidate match
    case Candidate.Unqualified => 3
    case Candidate.Qualified => 4
    case Candidate.Rooted => 5
    case Candidate.RootedPair => 8

  private[dotty] def exactTopology(tree: untpd.Tree, candidate: Candidate): Boolean =
    (tree, candidate) match
      case (untpd.AppliedTypeTree(untpd.Ident(option), List(untpd.Ident(string))), Candidate.Unqualified) =>
        option.toString == "Option" && option.isTypeName && string.toString == "String" && string.isTypeName
      case (untpd.AppliedTypeTree(untpd.Select(untpd.Ident(scala), option), List(untpd.Ident(string))), Candidate.Qualified) =>
        scala.toString == "scala" && scala.isTermName && option.toString == "Option" && option.isTypeName &&
          string.toString == "String" && string.isTypeName
      case (
            untpd.AppliedTypeTree(
              untpd.Select(untpd.Select(untpd.Ident(root), scala), option),
              List(untpd.Ident(string))
            ),
            Candidate.Rooted
          ) =>
        root.toString == "_root_" && root.isTermName && scala.toString == "scala" && scala.isTermName &&
          option.toString == "Option" && option.isTypeName && string.toString == "String" && string.isTypeName
      case (
            untpd.AppliedTypeTree(
              untpd.Select(untpd.Select(untpd.Ident(rootOption), scala), option),
              List(untpd.Select(untpd.Select(untpd.Select(untpd.Ident(rootString), java), lang), string))
            ),
            Candidate.RootedPair
          ) =>
        rootOption.toString == "_root_" && rootOption.isTermName && scala.toString == "scala" && scala.isTermName &&
          option.toString == "Option" && option.isTypeName && rootString.toString == "_root_" && rootString.isTermName &&
          java.toString == "java" && java.isTermName && lang.toString == "lang" && lang.isTermName &&
          string.toString == "String" && string.isTypeName
      case _ => false

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    ExistingUntpdClassMemberFilter.allTrees(tree)

  private def error(code: String, detail: String): Error = Error(code, detail)
