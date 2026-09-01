package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags.Param
import dotty.tools.dotc.core.Names.typeName
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.{NoSource, SourceFile}

import scala.util.control.NonFatal

import quasiquotes.definitions.DefinitionName
import AuxTypeAliasUntypedLoweringInput.*
import AuxTypeAliasUntypedLoweringInput.TypeInput.*

/** Direct source-free lowering for the one admitted AUXify-039 alias family. */
private[quasiquotes] object AuxTypeAliasUntypedLowerer:
  def lower(
      validated: Validated
  )(using Context): Either[AuxTypeAliasUntypedLoweringError, untpd.TypeDef] =
    Option(validated)
      .toRight(error("VALIDATED_INPUT_REQUIRED", "the validated lowering input was null."))
      .flatMap { present =>
        try
          given SourceFile = NoSource
          val parameters = present.parameters.map { parameter =>
            val SourceName(upperBound) = parameter.upperBound.get: @unchecked
            untpd
              .TypeDef(
                decodedTypeName(parameter.displayName),
                untpd.TypeBoundsTree(
                  untpd.EmptyTree,
                  untpd.Ident(decodedTypeName(upperBound))
                )
              )
              .withFlags(Param)
          }.toList
          val SourceName(constructor) = present.target.constructor: @unchecked
          val arguments = present.target.arguments.map {
            case BinderReference(_, displayName) =>
              untpd.Ident(decodedTypeName(displayName))
            case _ => throw new IllegalStateException("validated target argument was not a binder reference")
          }.toList
          val BinderReference(_, outputName) = present.refinement.rhs: @unchecked
          val member = untpd.TypeDef(
            decodedTypeName(present.refinement.memberName),
            untpd.Ident(decodedTypeName(outputName))
          )
          val raw = untpd.TypeDef(
            decodedTypeName(present.aliasName),
            untpd.LambdaTypeTree(
              parameters,
              untpd.RefinedTypeTree(
                untpd.AppliedTypeTree(
                  untpd.Ident(decodedTypeName(constructor)),
                  arguments
                ),
                member :: Nil
              )
            )
          )
          validateRaw(raw).map(_ => raw)
        catch
          case NonFatal(exception) =>
            Left(
              error(
                "EXACT_RAW_LOWERING_FAILED",
                Option(exception.getMessage)
                  .filter(_.nonEmpty)
                  .getOrElse(exception.getClass.getSimpleName)
              )
            )
      }

  private def decodedTypeName(value: String) =
    typeName(DefinitionName.fromSource(value).toOption.get.decoded)

  private def validateRaw(
      raw: untpd.TypeDef
  )(using Context): Either[AuxTypeAliasUntypedLoweringError, Unit] =
    val trees = allTrees(raw)
    val invalid = trees.filter(tree =>
      tree.source.exists || tree.span.exists || tree.symbol != NoSymbol ||
        tree.isInstanceOf[untpd.TypedSplice]
    )
    Either.cond(
      trees.size == 18 && invalid.isEmpty,
      (),
      error(
        "EXACT_RAW_LOWERING_FAILED",
        s"expected 18 source/span/symbol-free nodes; found ${trees.size} nodes and ${invalid.size} invalid nodes."
      )
    )

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    if tree.isEmpty then Vector.empty
    else tree +: directChildren(tree).flatMap(allTrees)

  private def directChildren(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree match
      case value: untpd.TypeDef => Vector(value.rhs)
      case value: untpd.LambdaTypeTree => value.tparams.toVector :+ value.body
      case value: untpd.TypeBoundsTree =>
        Vector(value.lo, value.hi, value.alias).filterNot(_.isEmpty)
      case value: untpd.RefinedTypeTree => value.tpt +: value.refinements.toVector
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case _ => Vector.empty

  private def error(
      code: String,
      detail: String
  ): AuxTypeAliasUntypedLoweringError =
    AuxTypeAliasUntypedLoweringError(code, detail)
