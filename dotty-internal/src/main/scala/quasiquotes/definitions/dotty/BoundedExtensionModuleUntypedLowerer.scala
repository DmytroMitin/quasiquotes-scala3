package quasiquotes.definitions.dotty

import scala.util.control.NonFatal

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.{termName, typeName}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.{NoSource, SourceFile}

import quasiquotes.definitions.DefinitionName
import quasiquotes.definitions.ScopedType.*
import quasiquotes.definitions.dotty.BoundedExtensionModulePlan.*

/** Parser-free source-free lowering for exactly one AUXify-045 extension module. */
private[quasiquotes] object BoundedExtensionModuleUntypedLowerer:
  def lower(
      plan: Plan
  )(using Context): Either[BoundedExtensionModuleError, untpd.ModuleDef] =
    for
      present <- Option(plan).toRight(error(
        "PLAN_REQUIRED",
        "the bounded extension-module plan must be present."
      ))
      validated <- revalidate(present)
      raw <- construct(validated)
      _ <- validateRawCandidate(raw)
    yield raw

  private def revalidate(plan: Plan): Either[BoundedExtensionModuleError, Plan] =
    BoundedExtensionModulePlan.create(
      plan.moduleDisplayName,
      plan.methodDisplayName,
      plan.typeParameter,
      plan.receiverParameter,
      plan.ordinaryArgument,
      plan.contextualParameter,
      plan.resultType,
      plan.body
    ).left.map(problem => error("PLAN_INVALID", problem.message))

  private def construct(
      plan: Plan
  )(using Context): Either[BoundedExtensionModuleError, untpd.ModuleDef] =
    try
      given SourceFile = NoSource
      for
        moduleName <- decoded(plan.moduleDisplayName, "MODULE_NAME_INVALID")
        methodName <- decoded(plan.methodDisplayName, "METHOD_NAME_INVALID")
        typeParameterName <- decoded(
          plan.typeParameter.displayName,
          "TYPE_PARAMETER_RECEIVER_INVALID"
        )
        receiverName <- decoded(
          plan.receiverParameter.displayName,
          "TYPE_PARAMETER_RECEIVER_INVALID"
        )
        argumentName <- decoded(
          plan.ordinaryArgument.displayName,
          "ORDINARY_ARGUMENT_INVALID"
        )
        evidenceName <- decoded(
          plan.contextualParameter.displayName,
          "CONTEXTUAL_PARAMETER_INVALID"
        )
        evidenceTypeName <- plan.contextualParameter.parameterType match
          case Applied(SourceName(value), Vector(_: TypeParameterReference)) =>
            decoded(value, "UNARY_EVIDENCE_TYPE_INVALID")
          case _ => Left(error(
            "UNARY_EVIDENCE_TYPE_INVALID",
            "the validated evidence Type lost its direct unary shape."
          ))
      yield
        val rawTypeParameter = untpd
          .TypeDef(
            typeName(typeParameterName),
            untpd.TypeBoundsTree(untpd.EmptyTree, untpd.EmptyTree)
          )
          .withMods(untpd.Modifiers(Flags.Param))
        val rawReceiver = untpd
          .ValDef(
            termName(receiverName),
            untpd.Ident(typeName(typeParameterName)),
            untpd.EmptyTree
          )
          .withMods(untpd.Modifiers(Flags.Param))
        val rawArgument = untpd
          .ValDef(
            termName(argumentName),
            untpd.Ident(typeName(typeParameterName)),
            untpd.EmptyTree
          )
          .withMods(untpd.Modifiers(Flags.Param))
        val rawEvidence = untpd
          .ValDef(
            termName(evidenceName),
            untpd.AppliedTypeTree(
              untpd.Ident(typeName(evidenceTypeName)),
              untpd.Ident(typeName(typeParameterName)) :: Nil
            ),
            untpd.EmptyTree
          )
          .withMods(untpd.Modifiers(Flags.Param | Flags.Given))
        val rawMethod = untpd
          .DefDef(
            termName(methodName),
            List(rawArgument :: Nil, rawEvidence :: Nil),
            untpd.Ident(typeName(typeParameterName)),
            untpd.Apply(
              untpd.Select(untpd.Ident(termName(evidenceName)), termName(methodName)),
              List(
                untpd.Ident(termName(receiverName)),
                untpd.Ident(termName(argumentName))
              )
            )
          )
          .withMods(untpd.Modifiers(Flags.Method))
        val extension = untpd.ExtMethods(
          List(rawTypeParameter :: Nil, rawReceiver :: Nil),
          rawMethod :: Nil
        )
        val template = untpd.Template(
          untpd.emptyConstructor.cloneIn(NoSource),
          Nil,
          Nil,
          untpd.EmptyValDef,
          extension :: Nil
        )
        untpd
          .ModuleDef(termName(moduleName), template)
          .withMods(untpd.Modifiers(Flags.Module))
    catch
      case NonFatal(exception) =>
        Left(error(
          "EXACT_RAW_CONSTRUCTION_FAILED",
          Option(exception.getMessage)
            .filter(_.nonEmpty)
            .getOrElse(exception.getClass.getSimpleName)
        ))

  private def decoded(
      value: String,
      code: String
  ): Either[BoundedExtensionModuleError, String] =
    DefinitionName
      .fromSource(value)
      .left
      .map(problem => error(code, problem.message))
      .map(_.decoded)

  private[dotty] def validateRawCandidate(
      raw: untpd.ModuleDef
  )(using Context): Either[BoundedExtensionModuleError, Unit] =
    if !hasExactTopology(raw) then
      Left(error(
        "UNSUPPORTED_COMPILER_TOPOLOGY",
        "the raw module is not exactly one empty object template containing one one-Type/one-receiver extension group and one method with one ordinary then one final contextual clause."
      ))
    else
      val trees = allTrees(raw)
      val invalid = trees.filter(tree =>
        tree.source.exists || tree.span.exists || tree.symbol != NoSymbol ||
          tree.isInstanceOf[untpd.TypedSplice]
      )
      Either.cond(
        trees.size == 21 && invalid.isEmpty,
        (),
        error(
          "EXACT_RAW_INVARIANT_FAILED",
          s"expected the exact 21-node source/span/symbol-free extension module; found ${trees.size} nodes and ${invalid.size} invalid nodes."
        )
      )

  private def hasExactTopology(raw: untpd.ModuleDef)(using Context): Boolean =
    raw.mods.flags == Flags.Module &&
      raw.impl.constr.paramss.isEmpty && raw.impl.constr.tpt.isEmpty &&
      raw.impl.constr.rhs.isEmpty && raw.impl.parentsOrDerived.isEmpty &&
      raw.impl.derived.isEmpty && raw.impl.self.isEmpty &&
      (raw.impl.body match
        case List(extension: untpd.ExtMethods) =>
          extension.paramss match
            case List(
                  List(typeParameter: untpd.TypeDef),
                  List(receiver: untpd.ValDef)
                ) =>
              typeParameter.mods.flags == Flags.Param &&
                (typeParameter.rhs match
                  case bounds: untpd.TypeBoundsTree =>
                    bounds.lo.isEmpty && bounds.hi.isEmpty && bounds.alias.isEmpty
                  case _ => false) &&
                receiver.mods.flags == Flags.Param && receiver.rhs.isEmpty &&
                receiver.tpt.isInstanceOf[untpd.Ident] &&
                (extension.methods match
                  case List(method: untpd.DefDef) => hasExactMethodTopology(method)
                  case _ => false)
            case _ => false
        case _ => false)

  private def hasExactMethodTopology(method: untpd.DefDef)(using Context): Boolean =
    method.mods.flags == Flags.Method &&
      (method.paramss match
        case List(
              List(argument: untpd.ValDef),
              List(contextual: untpd.ValDef)
            ) =>
          argument.mods.flags == Flags.Param && argument.rhs.isEmpty &&
            argument.tpt.isInstanceOf[untpd.Ident] &&
            contextual.mods.flags == (Flags.Param | Flags.Given) &&
            contextual.rhs.isEmpty &&
            (contextual.tpt match
              case applied: untpd.AppliedTypeTree =>
                applied.tpt.isInstanceOf[untpd.Ident] &&
                  applied.args.size == 1 &&
                  applied.args.head.isInstanceOf[untpd.Ident]
              case _ => false)
        case _ => false) &&
      method.tpt.isInstanceOf[untpd.Ident] &&
      (method.rhs match
        case untpd.Apply(
              untpd.Select(_: untpd.Ident, _),
              List(_: untpd.Ident, _: untpd.Ident)
            ) => true
        case _ => false)

  private[dotty] def allTrees(
      tree: untpd.Tree
  )(using Context): Vector[untpd.Tree] =
    if tree.isEmpty then Vector.empty
    else tree +: directChildren(tree).flatMap(allTrees)

  private[dotty] def directChildren(
      tree: untpd.Tree
  )(using Context): Vector[untpd.Tree] =
    tree match
      case value: untpd.ModuleDef => Vector(value.impl)
      case value: untpd.Template =>
        Vector(value.constr) ++ value.parentsOrDerived ++ value.derived ++
          Vector(value.self) ++ value.body
      case value: untpd.ExtMethods =>
        value.paramss.flatten.toVector ++ value.methods.toVector
      case value: untpd.DefDef =>
        value.paramss.flatten.toVector ++ Vector(value.tpt, value.rhs).filterNot(_.isEmpty)
      case value: untpd.TypeDef => Vector(value.rhs).filterNot(_.isEmpty)
      case value: untpd.ValDef => Vector(value.tpt, value.rhs).filterNot(_.isEmpty)
      case value: untpd.TypeBoundsTree =>
        Vector(value.lo, value.hi, value.alias).filterNot(_.isEmpty)
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case value: untpd.Apply => value.fun +: value.args.toVector
      case value: untpd.Select => Vector(value.qualifier)
      case _ => Vector.empty

  private def error(code: String, detail: String): BoundedExtensionModuleError =
    BoundedExtensionModuleError(code, detail)
