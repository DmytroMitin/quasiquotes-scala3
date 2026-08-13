package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.{TermName, termName}
import dotty.tools.dotc.util.{NoSource, SourceFile}

import quasiquotes.definitions.{ConstructedDefinition, DefinitionName}
import quasiquotes.terms.dotty.{
  CompletedTypeUntypedLowerer,
  ConstructedTermUntypedBackend
}

private[quasiquotes] object ConstructedDefinitionUntypedBackend:
  import ConstructedDefinitionUntypedBackendError.*

  def lower(
      constructed: ConstructedDefinition
  ): Either[ConstructedDefinitionUntypedBackendError, untpd.Tree] =
    given SourceFile = NoSource

    (constructed: @unchecked) match
      case method: ConstructedDefinition.ParameterlessDef =>
        for
          name <- lowerName(method.name)
          resultType <- lowerType(method.resultType)
          body <- lowerBody(method.body)
          raw = untpd
            .DefDef(name, Nil, resultType, body)
            .withMods(untpd.Modifiers(Flags.Method))
          _ <- validateMethod(raw, name, resultType, body)
        yield raw
      case method: ConstructedDefinition.SingleParameterDef =>
        for
          name <- lowerName(method.name)
          parameterName <- lowerName(method.parameterName)
          parameterType <- lowerType(method.parameterType)
          resultType <- lowerType(method.resultType)
          body <- lowerBodyInScope(
            method.body,
            method.parameterBinderId,
            method.parameterName.decoded
          )
          parameter = untpd
            .ValDef(parameterName, parameterType, untpd.EmptyTree)
            .withMods(untpd.Modifiers(Flags.Param))
          raw = untpd
            .DefDef(name, List(List(parameter)), resultType, body)
            .withMods(untpd.Modifiers(Flags.Method))
          _ <- validateSingleParameterMethod(
            raw,
            name,
            parameter,
            parameterName,
            parameterType,
            resultType,
            body
          )
        yield raw
      case value: ConstructedDefinition.ImmutableVal =>
        for
          name <- lowerName(value.name)
          declaredType <- lowerType(value.declaredType)
          rhs <- lowerBody(value.rhs)
          raw = untpd.ValDef(name, declaredType, rhs)
          _ <- validateValue(raw, name, declaredType, rhs)
        yield raw
      case unsupported =>
        Left(
          UnsupportedConstructedDefinitionVariant(
            unsupported.getClass.getSimpleName
          )
        )

  private def lowerName(
      name: DefinitionName
  ): Either[ConstructedDefinitionUntypedBackendError, TermName] =
    Option(name)
      .flatMap { value =>
        DefinitionName
          .fromSource(value.source)
          .toOption
          .filter(_.decoded == value.decoded)
          .map(_ => termName(value.decoded))
      }
      .toRight(
        DefinitionNameLoweringFailure(
          "the validated compiler-free name invariant was not satisfied."
        )
      )

  private def lowerType(
      normalForm: quasiquotes.types.TypeNormalForm
  ): Either[ConstructedDefinitionUntypedBackendError, untpd.Tree] =
    Option(normalForm)
      .toRight(
        DefinitionTypeLoweringFailure("the completed definition type was null.")
      )
      .flatMap(
        CompletedTypeUntypedLowerer
          .lower(_)
          .left
          .map(error => DefinitionTypeLoweringFailure(error.message))
      )

  private def lowerBody(
      term: quasiquotes.terms.ConstructedTerm
  ): Either[ConstructedDefinitionUntypedBackendError, untpd.Tree] =
    Option(term)
      .toRight(
        DefinitionBodyLoweringFailure(
          "the completed definition body was null."
        )
      )
      .flatMap(
        ConstructedTermUntypedBackend
          .lower(_)
          .left
          .map(error => DefinitionBodyLoweringFailure(error.message))
      )

  private def lowerBodyInScope(
      term: quasiquotes.terms.ConstructedTerm,
      binderId: quasiquotes.parser.BinderId,
      declarationName: String
  ): Either[ConstructedDefinitionUntypedBackendError, untpd.Tree] =
    Option(term)
      .toRight(
        DefinitionBodyLoweringFailure(
          "the completed definition body was null."
        )
      )
      .flatMap(
        ConstructedTermUntypedBackend
          .lowerInScope(_, binderId, declarationName)
          .left
          .map(error => DefinitionBodyLoweringFailure(error.message))
      )

  private def validateMethod(
      definition: untpd.DefDef,
      expectedName: TermName,
      expectedType: untpd.Tree,
      expectedBody: untpd.Tree
  ): Either[ConstructedDefinitionUntypedBackendError, Unit] =
    val valid =
      definition.name == expectedName &&
        definition.paramss.isEmpty &&
        (definition.tpt eq expectedType) &&
        (definition.unforcedRhs.asInstanceOf[AnyRef] eq expectedBody) &&
        definition.mods.flags == Flags.Method &&
        !definition.mods.hasAnnotations &&
        !definition.mods.hasPrivateWithin &&
        sourceAndSpanFree(definition, expectedType, expectedBody)
    Either.cond(
      valid,
      (),
      RawDefinitionConstructionInvariantFailure(
        "the parameterless DefDef shape diverged from the parser-observed contract."
      )
    )

  private def validateValue(
      definition: untpd.ValDef,
      expectedName: TermName,
      expectedType: untpd.Tree,
      expectedRhs: untpd.Tree
  ): Either[ConstructedDefinitionUntypedBackendError, Unit] =
    val valid =
      definition.name == expectedName &&
        (definition.tpt eq expectedType) &&
        (definition.unforcedRhs.asInstanceOf[AnyRef] eq expectedRhs) &&
        !definition.mods.hasFlags &&
        !definition.mods.hasAnnotations &&
        !definition.mods.hasPrivateWithin &&
        sourceAndSpanFree(definition, expectedType, expectedRhs)
    Either.cond(
      valid,
      (),
      RawDefinitionConstructionInvariantFailure(
        "the eager immutable ValDef shape diverged from the parser-observed contract."
      )
    )

  private def validateSingleParameterMethod(
      definition: untpd.DefDef,
      expectedName: TermName,
      expectedParameter: untpd.ValDef,
      expectedParameterName: TermName,
      expectedParameterType: untpd.Tree,
      expectedResultType: untpd.Tree,
      expectedBody: untpd.Tree
  ): Either[ConstructedDefinitionUntypedBackendError, Unit] =
    val valid =
      definition.name == expectedName &&
        definition.paramss == List(List(expectedParameter)) &&
        expectedParameter.name == expectedParameterName &&
        (expectedParameter.tpt eq expectedParameterType) &&
        expectedParameter.unforcedRhs.asInstanceOf[untpd.Tree].isEmpty &&
        expectedParameter.mods.flags == Flags.Param &&
        !expectedParameter.mods.hasAnnotations &&
        !expectedParameter.mods.hasPrivateWithin &&
        (definition.tpt eq expectedResultType) &&
        (definition.unforcedRhs.asInstanceOf[AnyRef] eq expectedBody) &&
        definition.mods.flags == Flags.Method &&
        !definition.mods.hasAnnotations &&
        !definition.mods.hasPrivateWithin &&
        sourceAndSpanFree(
          definition,
          expectedParameter,
          expectedParameterType,
          expectedResultType,
          expectedBody
        )
    Either.cond(
      valid,
      (),
      RawDefinitionConstructionInvariantFailure(
        "the single-ordinary-parameter DefDef shape diverged from the refreshed parser contract."
      )
    )

  private def sourceAndSpanFree(trees: untpd.Tree*): Boolean =
    trees.forall(allTrees(_).forall(tree => !tree.source.exists && !tree.span.exists))

  private def allTrees(tree: untpd.Tree): List[untpd.Tree] =
    val children =
      tree match
        case value: untpd.Select =>
          value.qualifier :: Nil
        case value: untpd.Apply =>
          value.fun :: value.args
        case value: untpd.InfixOp =>
          List(value.left, value.op, value.right)
        case value: untpd.PrefixOp =>
          List(value.op, value.od)
        case value: untpd.Typed =>
          List(value.expr, value.tpt)
        case value: untpd.AppliedTypeTree =>
          value.tpt :: value.args
        case value: untpd.Tuple =>
          value.trees
        case value: untpd.Function =>
          value.args :+ value.body
        case value: untpd.DefDef =>
          value.paramss.flatten ++ List(
            value.tpt,
            value.unforcedRhs.asInstanceOf[untpd.Tree]
          )
        case value: untpd.ValDef =>
          List(
            value.tpt,
            value.unforcedRhs.asInstanceOf[untpd.Tree]
          ).filterNot(_.isEmpty)
        case value: untpd.If =>
          List(value.cond, value.thenp, value.elsep)
        case value: untpd.Parens =>
          value.t :: Nil
        case _ =>
          Nil
    tree :: children.flatMap(allTrees)
