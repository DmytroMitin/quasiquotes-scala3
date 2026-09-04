package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names.typeName
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.{NoSource, SourceFile}

import quasiquotes.definitions.{
  ConstructedDefinition,
  DefinitionConstructionError,
  DefinitionName,
  DefinitionShape
}
import quasiquotes.parser.TermShape
import quasiquotes.terms.dotty.CompletedTypeUntypedLowerer
import quasiquotes.types.TypeNormalForm

private[quasiquotes] object DefinitionShapeUntypedLowerer:
  import DefinitionShapeUntypedLowererError.*

  def lower(
      shape: DefinitionShape
  )(using Context): Either[DefinitionShapeUntypedLowererError, untpd.Tree] =
    Option(shape)
      .toRight(MissingDefinitionShape)
      .flatMap {
        case alias: DefinitionShape.SimpleTypeAlias => lowerAlias(alias)
        case ordinary: DefinitionShape.ImmutableVal =>
          lowerOrdinary(ordinary, ordinary.rhs)
        case ordinary: DefinitionShape.ParameterlessDef =>
          lowerOrdinary(ordinary, ordinary.body)
        case ordinary: DefinitionShape.SingleParameterDef =>
          lowerOrdinary(ordinary, ordinary.body)
        case ordinary: DefinitionShape.TwoParameterDef =>
          lowerOrdinary(ordinary, ordinary.body)
      }

  private def lowerOrdinary(
      shape: DefinitionShape,
      body: TermShape
  )(using Context): Either[DefinitionShapeUntypedLowererError, untpd.Tree] =
    for
      _ <- Option(body).toRight(
        OrdinaryDefinitionCompletionFailure(
          DefinitionConstructionError.UnsupportedParsedDefinitionBody(
            "ordinary DefinitionShape lowering requires a present body."
          )
        )
      )
      completed <- ConstructedDefinition
        .fromShape(shape)
        .left
        .map(OrdinaryDefinitionCompletionFailure.apply)
      raw <- ConstructedDefinitionUntypedBackend
        .lower(completed)
        .left
        .map(OrdinaryDefinitionExactBackendFailure.apply)
      _ <- validateRawInvariant(raw, "ordinary DefinitionShape")
    yield raw

  private def lowerAlias(
      alias: DefinitionShape.SimpleTypeAlias
  )(using Context): Either[DefinitionShapeUntypedLowererError, untpd.Tree] =
    given SourceFile = NoSource

    for
      normalForm <- Option(alias.rhs)
        .toRight(
          SimpleTypeAliasCompletionFailure(
            quasiquotes.types.TypeQuasiquoteError(
              "the type alias right-hand side was null."
            )
          )
        )
        .flatMap(
          TypeNormalForm
            .fromShape(_)
            .left
            .map(SimpleTypeAliasCompletionFailure.apply)
        )
      rawName <- lowerAliasName(alias.name)
      rawRhs <- CompletedTypeUntypedLowerer
        .lower(normalForm)
        .left
        .map(SimpleTypeAliasCompletedTypeFailure.apply)
      raw = untpd.TypeDef(rawName, rawRhs)
      _ <- validateAlias(raw, rawRhs)
    yield raw

  private def lowerAliasName(
      name: DefinitionName
  ) =
    Option(name)
      .flatMap { value =>
        DefinitionName
          .fromSource(value.source)
          .toOption
          .filter(_.decoded == value.decoded)
          .map(_ => typeName(value.decoded))
      }
      .toRight(
        SimpleTypeAliasNameFailure(
          "the validated DefinitionName did not round-trip through its source spelling."
        )
      )

  private def validateAlias(
      raw: untpd.TypeDef,
      expectedRhs: untpd.Tree
  )(using Context): Either[DefinitionShapeUntypedLowererError, Unit] =
    if raw.name == null || !raw.name.isTypeName then
      Left(SimpleTypeAliasNameFailure("the emitted TypeDef name was not a type name."))
    else if raw.mods.hasFlags || raw.mods.hasAnnotations || raw.mods.hasPrivateWithin then
      Left(
        RawInvariantFailure(
          "simple type alias",
          "the emitted TypeDef carried unexpected modifiers."
        )
      )
    else if !(raw.rhs.asInstanceOf[AnyRef] eq expectedRhs) then
      Left(
        RawInvariantFailure(
          "simple type alias",
          "the emitted TypeDef did not retain the exact completed-type RHS."
        )
      )
    else validateRawInvariant(raw, "simple type alias")

  private[dotty] def validateRawInvariant(
      raw: untpd.Tree,
      family: String = "DefinitionShape"
  )(using Context): Either[DefinitionShapeUntypedLowererError, Unit] =
    Option(raw)
      .toRight(RawInvariantFailure(family, "the raw tree was null."))
      .flatMap { value =>
        allTrees(value).collectFirst {
          case tree if tree.source.exists =>
            s"${tree.getClass.getSimpleName} retained a source."
          case tree if tree.span.exists =>
            s"${tree.getClass.getSimpleName} retained a span."
          case _: untpd.TypedSplice =>
            "the raw tree contained a TypedSplice."
          case tree if tree.symbol != NoSymbol =>
            s"${tree.getClass.getSimpleName} retained a symbol."
        }.toLeft(())
          .left
          .map(detail => RawInvariantFailure(family, detail))
      }

  private def allTrees(tree: untpd.Tree): Vector[untpd.Tree] =
    tree +: directChildren(tree).flatMap(allTrees)

  private def directChildren(tree: untpd.Tree): Vector[untpd.Tree] =
    tree match
      case value: untpd.TypeDef => Vector(value.rhs)
      case value: untpd.DefDef =>
        value.paramss.flatten.toVector ++ Vector(
          value.tpt,
          value.unforcedRhs.asInstanceOf[untpd.Tree]
        ).filterNot(_.isEmpty)
      case value: untpd.ValDef =>
        Vector(
          value.tpt,
          value.unforcedRhs.asInstanceOf[untpd.Tree]
        ).filterNot(_.isEmpty)
      case value: untpd.Select => Vector(value.qualifier)
      case value: untpd.Apply => value.fun +: value.args.toVector
      case value: untpd.New => Vector(value.tpt)
      case value: untpd.InfixOp => Vector(value.left, value.op, value.right)
      case value: untpd.PrefixOp => Vector(value.op, value.od)
      case value: untpd.InterpolatedString => value.segments.toVector
      case value: untpd.Thicket => value.trees.toVector
      case value: untpd.Block => value.stats.toVector :+ value.expr
      case value: untpd.Typed => Vector(value.expr, value.tpt)
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case value: untpd.Tuple => value.trees.toVector
      case value: untpd.Function => value.args.toVector :+ value.body
      case value: untpd.If => Vector(value.cond, value.thenp, value.elsep)
      case value: untpd.Parens => Vector(value.t)
      case _ => Vector.empty
