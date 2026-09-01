package quasiquotes.phase150

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags.Param
import dotty.tools.dotc.core.Names.typeName
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.{NoSource, SourceFile}
import dotty.tools.dotc.util.Spans.Span

import scala.util.control.NonFatal

import quasiquotes.phase150.AuxTypeAliasSemanticPlanProbe.Plan

/** Test-only source-free raw constructor and generated-origin probe. */
private[quasiquotes] object Phase150AuxTypeAliasUntpdProbe:
  final case class Error(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  final case class Positioned(
      tree: untpd.TypeDef,
      generatedSource: String,
      sourceFile: SourceFile
  ):
    def virtualSourceName: String = sourceFile.path

  private enum Kind:
    case Outer, Lambda, Parameter, Bounds, Refined, Applied, Alias, Identifier

  private final case class TreePlan(
      kind: Kind,
      start: Int,
      end: Int,
      point: Int,
      children: Vector[TreePlan]
  ):
    def span: Span = Span(start, end, point)

  private final case class Rendered(source: String, root: TreePlan)

  def lower(plan: Plan)(using Context): Either[Error, untpd.TypeDef] =
    Option(plan).toRight(error("RAW_LOWERING_FAILED", "the validated plan was null.")).flatMap { present =>
      try
        given SourceFile = NoSource
        val parameters = present.parameters.map { parameter =>
          untpd
            .TypeDef(
              typeName(parameter.displayName),
              untpd.TypeBoundsTree(
                untpd.EmptyTree,
                untpd.Ident(typeName(parameter.upperBound.value)),
                untpd.EmptyTree
              )
            )
            .withFlags(Param)
        }.toList
        val applied = untpd.AppliedTypeTree(
          untpd.Ident(typeName(present.appliedBase.constructor.value)),
          present.appliedBase.arguments.map(argument => untpd.Ident(typeName(argument.displayName))).toList
        )
        val member = untpd.TypeDef(
          typeName(present.refinementMember.memberName),
          untpd.Ident(typeName(present.outputReference.displayName))
        )
        val raw = untpd.TypeDef(
          typeName(present.aliasName),
          untpd.LambdaTypeTree(
            parameters,
            untpd.RefinedTypeTree(applied, member :: Nil)
          )
        )
        validateRaw(raw).map(_ => raw)
      catch
        case NonFatal(exception) =>
          Left(error("RAW_LOWERING_FAILED", controlled(exception)))
    }

  def position(plan: Plan, virtualSourceName: String)(using Context): Either[Error, Positioned] =
    for
      _ <- Option(virtualSourceName)
        .filter(name => name.nonEmpty && !name.exists(character => character == '\n' || character == '\r' || character == '\u0000'))
        .toRight(error("GENERATED_ORIGIN_FAILED", "the virtual source name must be nonempty and single-line."))
      rendered <- render(plan)
      raw <- lower(plan)
      source = SourceFile.virtual(virtualSourceName, rendered.source)
      positioned <- positionTree(raw, rendered.root, source).flatMap {
        case value: untpd.TypeDef => Right(value)
        case other => Left(error("GENERATED_ORIGIN_FAILED", s"positioning returned ${other.getClass.getSimpleName}."))
      }
      _ <- validatePositioned(positioned, rendered, source)
    yield Positioned(positioned, rendered.source, source)

  private def render(plan: Plan): Either[Error, Rendered] =
    Option(plan).toRight(error("GENERATED_ORIGIN_FAILED", "the validated plan was null.")).flatMap { present =>
      val builder = new StringBuilder("type ")
      val outerPoint = builder.length
      builder.append(present.aliasName).append('[')
      val lambdaStart = builder.length
      val parameterPlans = present.parameters.zipWithIndex.map { case (parameter, index) =>
        if index > 0 then builder.append(", ")
        val parameterStart = builder.length
        val parameterName = appendIdentifier(builder, parameter.displayName)
        builder.append(" <: ")
        val boundsStart = parameterName.end + 1
        val upper = appendIdentifier(builder, parameter.upperBound.value)
        TreePlan(
          Kind.Parameter,
          parameterStart,
          upper.end,
          parameterStart,
          Vector(TreePlan(Kind.Bounds, boundsStart, upper.end, boundsStart, Vector(upper)))
        )
      }
      builder.append("] = ")
      val refinedStart = builder.length
      val constructor = appendIdentifier(builder, present.appliedBase.constructor.value)
      builder.append('[')
      val arguments = present.appliedBase.arguments.zipWithIndex.map { case (argument, index) =>
        if index > 0 then builder.append(", ")
        appendIdentifier(builder, argument.displayName)
      }
      builder.append(']')
      val applied = TreePlan(
        Kind.Applied,
        refinedStart,
        builder.length,
        refinedStart,
        constructor +: arguments
      )
      builder.append(" { type ")
      val aliasPoint = builder.length
      val aliasStart = aliasPoint - "type ".length
      builder.append(present.refinementMember.memberName).append(" = ")
      val output = appendIdentifier(builder, present.outputReference.displayName)
      val alias = TreePlan(Kind.Alias, aliasStart, output.end, aliasPoint, Vector(output))
      builder.append(" }")
      val refined = TreePlan(Kind.Refined, refinedStart, builder.length, refinedStart, Vector(applied, alias))
      val lambda = TreePlan(Kind.Lambda, lambdaStart, builder.length, lambdaStart, parameterPlans :+ refined)
      val root = TreePlan(Kind.Outer, 0, builder.length, outerPoint, Vector(lambda))
      val rendered = Rendered(builder.toString, root)
      validatePlan(rendered).map(_ => rendered)
    }

  private def appendIdentifier(builder: StringBuilder, value: String): TreePlan =
    val start = builder.length
    builder.append(value)
    TreePlan(Kind.Identifier, start, builder.length, start, Vector.empty)

  private def positionTree(raw: untpd.Tree, plan: TreePlan, source: SourceFile)(using Context): Either[Error, untpd.Tree] =
    (raw, plan.kind) match
      case (definition: untpd.TypeDef, Kind.Outer) if plan.children.size == 1 =>
        positionTree(definition.rhs, plan.children.head, source).map(rhs =>
          untpd.TypeDef(definition.name, rhs).withMods(definition.mods).cloneIn(source).withSpan(plan.span)
        )
      case (lambda: untpd.LambdaTypeTree, Kind.Lambda)
          if plan.children.size == lambda.tparams.size + 1 =>
        for
          parameters <- sequence(lambda.tparams.zip(plan.children.init).map(positionTree(_, _, source)))
          body <- positionTree(lambda.body, plan.children.last, source)
        yield untpd
          .LambdaTypeTree(parameters.map(_.asInstanceOf[untpd.TypeDef]), body)
          .cloneIn(source)
          .withSpan(plan.span)
      case (parameter: untpd.TypeDef, Kind.Parameter) if plan.children.size == 1 =>
        positionTree(parameter.rhs, plan.children.head, source).map(rhs =>
          untpd.TypeDef(parameter.name, rhs).withMods(parameter.mods).cloneIn(source).withSpan(plan.span)
        )
      case (bounds: untpd.TypeBoundsTree, Kind.Bounds)
          if plan.children.size == 1 && bounds.lo.isEmpty && bounds.alias.isEmpty =>
        positionTree(bounds.hi, plan.children.head, source).map(upper =>
          untpd.TypeBoundsTree(untpd.EmptyTree, upper).cloneIn(source).withSpan(plan.span)
        )
      case (refined: untpd.RefinedTypeTree, Kind.Refined)
          if plan.children.size == 2 && refined.refinements.size == 1 =>
        for
          base <- positionTree(refined.tpt, plan.children.head, source)
          member <- positionTree(refined.refinements.head, plan.children(1), source)
        yield untpd.RefinedTypeTree(base, member.asInstanceOf[untpd.TypeDef] :: Nil).cloneIn(source).withSpan(plan.span)
      case (applied: untpd.AppliedTypeTree, Kind.Applied)
          if plan.children.size == applied.args.size + 1 =>
        for
          constructor <- positionTree(applied.tpt, plan.children.head, source)
          arguments <- sequence(applied.args.zip(plan.children.tail).map(positionTree(_, _, source)))
        yield untpd.AppliedTypeTree(constructor, arguments).cloneIn(source).withSpan(plan.span)
      case (alias: untpd.TypeDef, Kind.Alias) if plan.children.size == 1 =>
        positionTree(alias.rhs, plan.children.head, source).map(rhs =>
          untpd.TypeDef(alias.name, rhs).withMods(alias.mods).cloneIn(source).withSpan(plan.span)
        )
      case (identifier: untpd.Ident, Kind.Identifier) if plan.children.isEmpty =>
        Right(identifier.cloneIn(source).withSpan(plan.span))
      case _ => Left(error("GENERATED_ORIGIN_FAILED", s"raw ${raw.getClass.getSimpleName} does not match planned ${plan.kind}."))

  private def sequence(values: List[Either[Error, untpd.Tree]]): Either[Error, List[untpd.Tree]] =
    values.foldRight(Right(Nil): Either[Error, List[untpd.Tree]])((value, result) =>
      for head <- value; tail <- result yield head :: tail
    )

  private def validatePlan(rendered: Rendered): Either[Error, Unit] =
    val errors = Vector.newBuilder[String]
    def visit(plan: TreePlan): Unit =
      if plan.start < 0 || plan.start > plan.point || plan.point > plan.end || plan.end > rendered.source.length
      then errors += s"${plan.kind} has an invalid span"
      plan.children.foreach { child =>
        if child.start < plan.start || child.end > plan.end then errors += s"${plan.kind} does not contain ${child.kind}"
        visit(child)
      }
      plan.children.zip(plan.children.drop(1)).foreach { case (left, right) =>
        if left.end > right.start then errors += s"${plan.kind} children overlap"
      }
    visit(rendered.root)
    val result = errors.result()
    Either.cond(result.isEmpty, (), error("GENERATED_ORIGIN_FAILED", result.mkString("; ")))

  private def validateRaw(raw: untpd.TypeDef)(using Context): Either[Error, Unit] =
    val trees = allTrees(raw)
    val invalid = trees.filter(tree => tree.source.exists || tree.span.exists || tree.symbol != NoSymbol || tree.isInstanceOf[untpd.TypedSplice])
    Either.cond(
      trees.size == 18 && invalid.isEmpty,
      (),
      error("RAW_LOWERING_FAILED", s"expected 18 source/span/symbol-free nodes; found ${trees.size} nodes and ${invalid.size} invalid nodes.")
    )

  private def validatePositioned(tree: untpd.TypeDef, rendered: Rendered, source: SourceFile)(using Context): Either[Error, Unit] =
    val trees = allTrees(tree)
    val invalid = trees.filter(current =>
      !current.source.exists || current.source.path != source.path || current.source.content.mkString != rendered.source ||
        !current.span.exists || current.span.start < 0 || current.span.start > current.span.point ||
        current.span.point > current.span.end || current.span.end > rendered.source.length ||
        current.symbol != NoSymbol || current.isInstanceOf[untpd.TypedSplice]
    )
    val containmentFailures = trees.flatMap(current => directChildren(current).filter(child => child.span.start < current.span.start || child.span.end > current.span.end))
    Either.cond(
      trees.size == 18 && invalid.isEmpty && containmentFailures.isEmpty && tree.span == rendered.root.span,
      (),
      error("GENERATED_ORIGIN_FAILED", s"expected 18 coherently positioned nodes; found ${trees.size} nodes, ${invalid.size} invalid nodes, and ${containmentFailures.size} containment failures.")
    )

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    if tree.isEmpty then Vector.empty else tree +: directChildren(tree).flatMap(allTrees)

  private def directChildren(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree match
      case value: untpd.TypeDef => Vector(value.rhs)
      case value: untpd.LambdaTypeTree => value.tparams.toVector :+ value.body
      case value: untpd.TypeBoundsTree => Vector(value.lo, value.hi, value.alias).filterNot(_.isEmpty)
      case value: untpd.RefinedTypeTree => value.tpt +: value.refinements.toVector
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case _ => Vector.empty

  private def controlled(exception: Throwable): String =
    Option(exception.getMessage).filter(_.nonEmpty).getOrElse(exception.getClass.getSimpleName)

  private def error(code: String, detail: String): Error = Error(code, detail)
