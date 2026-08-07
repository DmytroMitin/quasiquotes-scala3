package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.{TermName, TypeName, termName, typeName}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.{NoSource, SourceFile}

import quasiquotes.publicapi.{CompletedTerm, CompletedType, DefinitionResultView}

private[quasiquotes] object PublicContextualMethodUntypedBackend:
  import PublicContextualMethodUntypedBackendError.*

  def lower(
      result: DefinitionResultView
  )(using Context): Either[PublicContextualMethodUntypedBackendError, untpd.DefDef] =
    given SourceFile = NoSource

    Option(result).toRight(NullDefinitionResult).flatMap { definition =>
      for
        _ <- validateTopLevelProjection(definition)
        methodName <- lowerMethodName(definition.name)
        typeParameterName <- lowerTypeParameterName(definition.typeParameterName)
        contextualParameterName <- lowerContextualParameterName(
          definition.contextualParameterName
        )
        contextualType <- lowerType(
          definition.contextualParameterType,
          definition.typeParameterName,
          "contextual-parameter"
        )
        resultType <- lowerType(
          definition.resultType,
          definition.typeParameterName,
          "result"
        )
        body <- lowerBody(definition.body, definition.contextualParameterName)
        typeParameter = untpd
          .TypeDef(
            typeParameterName,
            untpd.TypeBoundsTree(untpd.EmptyTree, untpd.EmptyTree)
          )
          .withMods(untpd.Modifiers(Flags.Param))
        contextualParameter = untpd
          .ValDef(contextualParameterName, contextualType, untpd.EmptyTree)
          .withMods(untpd.Modifiers(Flags.Param | Flags.Given))
        raw = untpd
          .DefDef(
            methodName,
            List(typeParameter :: Nil, contextualParameter :: Nil),
            resultType,
            body
          )
          .withMods(untpd.Modifiers(Flags.Method))
        _ <- validateRaw(
          raw,
          methodName,
          typeParameterName,
          contextualParameterName,
          contextualType,
          resultType,
          body
        )
      yield raw
    }

  private def validateTopLevelProjection(
      definition: DefinitionResultView
  ): Either[PublicContextualMethodUntypedBackendError, Unit] =
    Either.cond(
      definition.kindCode == "method",
      (),
      ProjectionInvariantFailure(
        s"expected kindCode `method`, found `${String.valueOf(definition.kindCode)}`."
      )
    )

  private def lowerMethodName(
      value: String
  ): Either[PublicContextualMethodUntypedBackendError, TermName] =
    if validIdentifier(value) then Right(termName(value))
    else Left(MethodNameLoweringFailure(invalidNameDetail(value)))

  private def lowerTypeParameterName(
      value: String
  ): Either[PublicContextualMethodUntypedBackendError, TypeName] =
    if validIdentifier(value) then Right(typeName(value))
    else Left(TypeParameterLoweringFailure(invalidNameDetail(value)))

  private def lowerContextualParameterName(
      value: String
  ): Either[PublicContextualMethodUntypedBackendError, TermName] =
    if validIdentifier(value) then Right(termName(value))
    else Left(ContextualParameterLoweringFailure(invalidNameDetail(value)))

  private def lowerType(
      value: CompletedType,
      declaredTypeParameter: String,
      anchor: String
  )(using SourceFile): Either[PublicContextualMethodUntypedBackendError, untpd.Tree] =
    Option(value)
      .toRight(TypeLoweringFailure(anchor, "the completed type was null."))
      .flatMap { completed =>
        completed.kindCode match
          case "named" =>
            for
              name <- exactNameProjection(completed, anchor)
              _ <- Either.cond(
                validIdentifier(name),
                (),
                TypeLoweringFailure(anchor, invalidNameDetail(name))
              )
            yield untpd.Ident(typeName(name))
          case "type-parameter" =>
            for
              name <- exactNameProjection(completed, anchor)
              _ <- Either.cond(
                validIdentifier(name) && name == declaredTypeParameter,
                (),
                TypeLoweringFailure(
                  anchor,
                  s"type-parameter projection `${String.valueOf(name)}` does not name the single declared binder `${String.valueOf(declaredTypeParameter)}`."
                )
              )
            yield untpd.Ident(typeName(name))
          case "applied" =>
            for
              _ <- Either.cond(
                completed.name.isEmpty && completed.arguments.nonEmpty,
                (),
                ProjectionInvariantFailure(
                  s"$anchor applied type must have no name and at least one argument."
                )
              )
              constructor <- completed.constructor.toRight(
                ProjectionInvariantFailure(
                  s"$anchor applied type has no constructor projection."
                )
              )
              _ <- Either.cond(
                constructor.kindCode == "named",
                (),
                TypeLoweringFailure(
                  anchor,
                  "only a named constructor is supported in this bounded slice."
                )
              )
              loweredConstructor <- lowerType(
                constructor,
                declaredTypeParameter,
                s"$anchor constructor"
              )
              loweredArguments <- completed.arguments.zipWithIndex.foldLeft(
                Right(List.empty[untpd.Tree]): Either[PublicContextualMethodUntypedBackendError, List[untpd.Tree]]
              ) { case (acc, (argument, index)) =>
                for
                  values <- acc
                  lowered <- lowerType(
                    argument,
                    declaredTypeParameter,
                    s"$anchor argument ${index + 1}"
                  )
                yield values :+ lowered
              }
            yield untpd.AppliedTypeTree(loweredConstructor, loweredArguments)
          case other => Left(UnsupportedCompletedTypeProjection(String.valueOf(other)))
      }

  private def exactNameProjection(
      completed: CompletedType,
      anchor: String
  ): Either[PublicContextualMethodUntypedBackendError, String] =
    Either.cond(
      completed.name.nonEmpty && completed.constructor.isEmpty && completed.arguments.isEmpty,
      completed.name.get,
      ProjectionInvariantFailure(
        s"$anchor ${completed.kindCode} type does not have the exact name-only projection."
      )
    )

  private def lowerBody(
      value: CompletedTerm,
      contextualParameterName: String
  )(using SourceFile): Either[PublicContextualMethodUntypedBackendError, untpd.Tree] =
    Option(value)
      .toRight(BodyLoweringFailure("the completed body was null."))
      .flatMap { body =>
        Either.cond(
          body.kindCode == "reference" &&
            validIdentifier(body.referenceName) &&
            body.referenceName == contextualParameterName,
          untpd.Ident(termName(body.referenceName)),
          BodyLoweringFailure(
            "expected the exact stable reference to the single contextual parameter."
          )
        )
      }

  private def validateRaw(
      definition: untpd.DefDef,
      expectedMethodName: TermName,
      expectedTypeParameterName: TypeName,
      expectedContextualParameterName: TermName,
      expectedContextualType: untpd.Tree,
      expectedResultType: untpd.Tree,
      expectedBody: untpd.Tree
  )(using Context): Either[PublicContextualMethodUntypedBackendError, Unit] =
    val typeParameters = definition.paramss.headOption.toList.flatten.collect {
      case value: untpd.TypeDef => value
    }
    val valueClauses = definition.paramss.drop(1)
    val contextualParameter = valueClauses.headOption.flatMap(_.headOption).collect {
      case value: untpd.ValDef => value
    }
    val typeParameterValid = typeParameters.headOption.exists { parameter =>
      val hasWildcardBounds = parameter.rhs match
        case untpd.WildcardTypeBoundsTree() => true
        case _ => false
      parameter.name == expectedTypeParameterName &&
        parameter.mods.flags == Flags.Param &&
        !parameter.mods.hasAnnotations &&
        !parameter.mods.hasPrivateWithin &&
        hasWildcardBounds
    }
    val valid =
      definition.name == expectedMethodName &&
        definition.mods.flags == Flags.Method &&
        !definition.mods.hasAnnotations &&
        !definition.mods.hasPrivateWithin &&
        definition.paramss.size == 2 &&
        definition.paramss.head.size == 1 &&
        typeParameters.size == 1 &&
        typeParameterValid &&
        valueClauses.size == 1 &&
        valueClauses.head.size == 1 &&
        contextualParameter.exists { parameter =>
          parameter.name == expectedContextualParameterName &&
          (parameter.tpt eq expectedContextualType) &&
          parameter.unforcedRhs.asInstanceOf[untpd.Tree].isEmpty &&
          parameter.mods.flags == (Flags.Param | Flags.Given) &&
          !parameter.mods.hasAnnotations &&
          !parameter.mods.hasPrivateWithin
        } &&
        (definition.tpt eq expectedResultType) &&
        (definition.unforcedRhs.asInstanceOf[AnyRef] eq expectedBody) &&
        allTrees(definition).forall(tree =>
          !tree.source.exists && !tree.span.exists && tree.symbol == NoSymbol
        )
    Either.cond(
      valid,
      (),
      RawConstructionInvariantFailure(
        "the DefDef shape diverged from the parser-observed one-binder, one-using-clause contract."
      )
    )

  private def allTrees(tree: untpd.Tree): List[untpd.Tree] =
    val children = tree match
      case value: untpd.DefDef =>
        value.paramss.flatten ++ List(
          value.tpt,
          value.unforcedRhs.asInstanceOf[untpd.Tree]
        )
      case value: untpd.TypeDef => value.rhs :: Nil
      case value: untpd.ValDef =>
        List(value.tpt, value.unforcedRhs.asInstanceOf[untpd.Tree])
      case value: untpd.AppliedTypeTree => value.tpt :: value.args
      case _ => Nil
    tree :: children.filterNot(_.isEmpty).flatMap(allTrees)

  private val Scala3Keywords = Set(
    "abstract", "as", "case", "catch", "class", "def", "derives", "do",
    "else", "end", "enum", "export", "extends", "extension", "false",
    "final", "finally", "for", "forSome", "given", "if", "implicit",
    "import", "infix", "inline", "lazy", "macro", "match", "new", "null",
    "object", "opaque", "open", "override", "package", "private",
    "protected", "return", "sealed", "super", "then", "this", "throw",
    "trait", "transparent", "true", "try", "type", "using", "val", "var",
    "while", "with", "yield"
  )

  private def validIdentifier(value: String): Boolean =
    value != null && value.nonEmpty && value != "_" && !Scala3Keywords(value) &&
      asciiLetterOrUnderscore(value.head) &&
      value.tail.forall(asciiLetterOrDigitOrUnderscore)

  private def asciiLetterOrUnderscore(value: Char): Boolean =
    value == '_' || ('A' <= value && value <= 'Z') ||
      ('a' <= value && value <= 'z')

  private def asciiLetterOrDigitOrUnderscore(value: Char): Boolean =
    asciiLetterOrUnderscore(value) || ('0' <= value && value <= '9')

  private def invalidNameDetail(value: String): String =
    s"`${String.valueOf(value)}` is not a validated bounded public identifier."
