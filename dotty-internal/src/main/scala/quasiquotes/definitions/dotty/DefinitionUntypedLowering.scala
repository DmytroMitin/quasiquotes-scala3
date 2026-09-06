package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags

import quasiquotes.definitions.{
  DefinitionShape,
  SemanticDefinition,
  SemanticDefinitionShapeAdapter
}
import quasiquotes.definitions.dotty.DefinitionShapeUntypedLowererError.RawInvariantFailure

/** Exact-version source-free lowering for public semantic Definition values. */
object DefinitionUntypedLowering:
  /** Stable public diagnostic boundary; callers branch on `code`. */
  final case class Failure(code: String, detail: String) derives CanEqual:
    def message: String = s"$code: $detail"

  /** Lowers one admitted semantic Definition to a fresh source-free raw member. */
  def lower(
      definition: SemanticDefinition
  )(using Context): Either[Failure, untpd.MemberDef] =
    SemanticDefinitionShapeAdapter
      .adapt(definition)
      .left
      .map(classifyAdapterFailure)
      .flatMap { shape =>
        finishLowering(shape, DefinitionShapeUntypedLowerer.lower(shape))
      }

  private def finishLowering(
      shape: DefinitionShape,
      lowered: Either[DefinitionShapeUntypedLowererError, untpd.Tree]
  )(using Context): Either[Failure, untpd.MemberDef] =
    Option(lowered)
      .toRight(invariant("the private exact lowerer returned no result container."))
      .flatMap(
        _.left
          .map(classifyExactFailure)
          .flatMap(raw => validateReturnedMember(shape, raw))
      )

  private def validateReturnedMember(
      shape: DefinitionShape,
      raw: untpd.Tree
  )(using Context): Either[Failure, untpd.MemberDef] =
    for
      presentShape <- Option(shape)
        .toRight(invariant("the adapted private Definition shape was absent."))
      presentRaw <- Option(raw)
        .toRight(invariant("the private exact lowerer returned a null raw tree."))
      _ <- DefinitionShapeUntypedLowerer
        .validateRawInvariant(presentRaw, "public semantic Definition")
        .left
        .map {
          case problem: RawInvariantFailure => invariant(problem.message)
          case problem => invariant(problem.message)
        }
      member <- presentRaw match
        case value: untpd.MemberDef => Right(value)
        case other =>
          Left(
            invariant(
              s"the private exact lowerer returned ${other.getClass.getName}, not untpd.MemberDef."
            )
          )
      _ <- validateFamilyTopology(presentShape, member)
    yield member

  private def validateFamilyTopology(
      shape: DefinitionShape,
      member: untpd.MemberDef
  )(using Context): Either[Failure, Unit] =
    def sameName: Boolean =
      shape.name != null &&
        shape.name.decoded != null &&
        member.name != null &&
        member.name.toString == shape.name.decoded

    def present(tree: untpd.Tree): Boolean = tree != null && !tree.isEmpty

    val valid =
      (shape, member) match
        case (_: DefinitionShape.ImmutableVal, value: untpd.ValDef) =>
          sameName && value.mods.flags == Flags.EmptyFlags &&
            !value.mods.hasAnnotations && !value.mods.hasPrivateWithin &&
            present(value.tpt) && present(value.unforcedRhs.asInstanceOf[untpd.Tree])
        case (_: DefinitionShape.ParameterlessDef, method: untpd.DefDef) =>
          sameName && method.mods.flags == Flags.Method &&
            !method.mods.hasAnnotations && !method.mods.hasPrivateWithin &&
            method.paramss != null && method.paramss.isEmpty &&
            present(method.tpt) && present(method.unforcedRhs.asInstanceOf[untpd.Tree])
        case (expected: DefinitionShape.SingleParameterDef, method: untpd.DefDef) =>
          sameName && method.mods.flags == Flags.Method &&
            !method.mods.hasAnnotations && !method.mods.hasPrivateWithin &&
            method.paramss != null && method.paramss.size == 1 &&
            method.paramss.head != null && method.paramss.head.size == 1 &&
            validParameter(method.paramss.head.head, expected.parameterName.decoded) &&
            present(method.tpt) && present(method.unforcedRhs.asInstanceOf[untpd.Tree])
        case (expected: DefinitionShape.TwoParameterDef, method: untpd.DefDef) =>
          sameName && method.mods.flags == Flags.Method &&
            !method.mods.hasAnnotations && !method.mods.hasPrivateWithin &&
            method.paramss != null && method.paramss.size == 1 &&
            method.paramss.head != null && method.paramss.head.size == 2 &&
            validParameter(method.paramss.head.head, expected.firstParameterName.decoded) &&
            validParameter(method.paramss.head(1), expected.secondParameterName.decoded) &&
            present(method.tpt) && present(method.unforcedRhs.asInstanceOf[untpd.Tree])
        case (_: DefinitionShape.SimpleTypeAlias, alias: untpd.TypeDef) =>
          sameName && alias.name.isTypeName &&
            !alias.mods.hasFlags && !alias.mods.hasAnnotations &&
            !alias.mods.hasPrivateWithin && present(alias.rhs)
        case _ => false

    Either.cond(
      valid,
      (),
      invariant(
        s"raw member topology ${member.getClass.getSimpleName} contradicted ${shape.getClass.getSimpleName}."
      )
    )

  private def validParameter(
      parameter: untpd.Tree,
      expectedName: String
  )(using Context): Boolean =
    parameter match
      case value: untpd.ValDef =>
        expectedName != null && value.name != null &&
          value.name.toString == expectedName &&
          value.mods.flags == Flags.Param &&
          !value.mods.hasAnnotations && !value.mods.hasPrivateWithin &&
          value.tpt != null && !value.tpt.isEmpty && value.rhs.isEmpty
      case _ => false

  private def classifyAdapterFailure(
      problem: SemanticDefinitionShapeAdapter.Error
  ): Failure =
    Option(problem)
      .map { present =>
        present.code match
          case "MISSING_INPUT" | "MALFORMED_SEMANTIC_VALUE" |
              "UNSUPPORTED_SEMANTIC_VALUE" | "SEMANTIC_ADAPTER_FAILED" =>
            Failure(present.code, present.detail)
          case unexpected =>
            invariant(
              s"the private semantic adapter returned unexpected code `${Option(unexpected).getOrElse("null")}`: ${Option(present.detail).getOrElse("")}"
            )
      }
      .getOrElse(invariant("the private semantic adapter returned a null failure."))

  private def classifyExactFailure(
      problem: DefinitionShapeUntypedLowererError
  ): Failure =
    Option(problem) match
      case Some(raw: RawInvariantFailure) => invariant(raw.message)
      case Some(other) => Failure("EXACT_LOWERING_FAILED", other.message)
      case None => invariant("the private exact lowerer returned a null failure.")

  private def invariant(detail: String): Failure =
    Failure("INTERNAL_INVARIANT_FAILED", detail)
