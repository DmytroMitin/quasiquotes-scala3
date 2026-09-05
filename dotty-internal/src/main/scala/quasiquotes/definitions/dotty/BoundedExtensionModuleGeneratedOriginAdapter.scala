package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.Spans.Span

import quasiquotes.definitions.ScopedType.*
import quasiquotes.definitions.dotty.BoundedExtensionModulePlan.Plan
import quasiquotes.terms.dotty.GeneratedOriginFragmentSupport

/** Complete deterministic generated origin for the exact U024 module tree. */
private[quasiquotes] object BoundedExtensionModuleGeneratedOriginAdapter:
  private enum PlanKind:
    case Module
    case Template
    case Constructor
    case ExtensionMethods
    case TypeParameter
    case TypeBounds
    case ReceiverParameter
    case Method
    case OrdinaryArgument
    case ContextualParameter
    case AppliedType
    case TypeIdentifier
    case TermIdentifier
    case Application
    case Selection

  private final case class TreePlan(
      kind: PlanKind,
      start: Int,
      end: Int,
      point: Int,
      children: Vector[TreePlan]
  ):
    def span: Span = Span(start, end, point)

  private final case class GeneratedPlan(source: String, root: TreePlan)

  def lower(
      plan: Plan,
      virtualSourceName: String
  )(using Context): Either[BoundedExtensionModuleError, GeneratedOriginDefinitionResult] =
    for
      present <- Option(plan).toRight(error(
        "PLAN_REQUIRED",
        "the bounded extension-module plan must be present."
      ))
      virtualName <- Option(virtualSourceName).toRight(error(
        "GENERATED_ORIGIN_INVALID",
        "the virtual source name must be present."
      ))
      _ <- GeneratedOriginFragmentSupport
        .validateVirtualSourceName(virtualName)
        .left
        .map(problem => error("GENERATED_ORIGIN_INVALID", problem.message))
      generated <- render(present)
      raw <- BoundedExtensionModuleUntypedLowerer.lower(present)
      source = SourceFile.virtual(virtualName, generated.source)
      positioned <- position(raw, generated.root, source).flatMap {
        case value: untpd.ModuleDef => Right(value)
        case other => Left(error(
          "GENERATED_ORIGIN_MISMATCH",
          s"positioning returned ${other.getClass.getSimpleName}, not ModuleDef."
        ))
      }
      _ <- validatePositioned(positioned, generated, source)
    yield new GeneratedOriginDefinitionResult(positioned, generated.source, source)

  private def render(
      plan: Plan
  ): Either[BoundedExtensionModuleError, GeneratedPlan] =
    plan.contextualParameter.parameterType match
      case Applied(SourceName(evidenceType), Vector(_: TypeParameterReference)) =>
        val builder = new StringBuilder("object ")
        val moduleName = append(builder, plan.moduleDisplayName)
        builder.append(":\n  ")
        val extensionStart = builder.length
        builder.append("extension [")
        val typeName = append(builder, plan.typeParameter.displayName)
        val typeBounds = leaf(PlanKind.TypeBounds, typeName)
        val typeParameter = node(PlanKind.TypeParameter, typeName, Vector(typeBounds))
        builder.append("](")
        val receiverStart = builder.length
        val receiverName = append(builder, plan.receiverParameter.displayName)
        builder.append(": ")
        val receiverType = typedIdentifier(builder, plan.typeParameter.displayName)
        val receiver = TreePlan(
          PlanKind.ReceiverParameter,
          receiverStart,
          receiverType.end,
          receiverName._1,
          Vector(receiverType)
        )
        builder.append(")\n    ")
        val methodStart = builder.length
        builder.append("def ")
        val methodName = append(builder, plan.methodDisplayName)
        builder.append('(')
        val argumentStart = builder.length
        val argumentName = append(builder, plan.ordinaryArgument.displayName)
        builder.append(": ")
        val argumentType = typedIdentifier(builder, plan.typeParameter.displayName)
        val argument = TreePlan(
          PlanKind.OrdinaryArgument,
          argumentStart,
          argumentType.end,
          argumentName._1,
          Vector(argumentType)
        )
        builder.append(")(using ")
        val contextualStart = builder.length
        val contextualName = append(builder, plan.contextualParameter.displayName)
        builder.append(": ")
        val appliedStart = builder.length
        val evidenceConstructor = typedIdentifier(builder, evidenceType)
        builder.append('[')
        val evidenceArgument = typedIdentifier(builder, plan.typeParameter.displayName)
        builder.append(']')
        val applied = TreePlan(
          PlanKind.AppliedType,
          appliedStart,
          builder.length,
          evidenceConstructor.point,
          Vector(evidenceConstructor, evidenceArgument)
        )
        val contextual = TreePlan(
          PlanKind.ContextualParameter,
          contextualStart,
          builder.length,
          contextualName._1,
          Vector(applied)
        )
        builder.append("): ")
        val resultType = typedIdentifier(builder, plan.typeParameter.displayName)
        builder.append(" =\n      ")
        val applicationStart = builder.length
        val evidenceReference = termIdentifier(
          builder,
          plan.contextualParameter.displayName
        )
        builder.append('.')
        val selectedPoint = builder.length
        append(builder, plan.methodDisplayName)
        val selection = TreePlan(
          PlanKind.Selection,
          applicationStart,
          builder.length,
          selectedPoint,
          Vector(evidenceReference)
        )
        builder.append('(')
        val receiverReference = termIdentifier(
          builder,
          plan.receiverParameter.displayName
        )
        builder.append(", ")
        val argumentReference = termIdentifier(
          builder,
          plan.ordinaryArgument.displayName
        )
        builder.append(")\n")
        val application = TreePlan(
          PlanKind.Application,
          applicationStart,
          builder.length - 1,
          applicationStart,
          Vector(selection, receiverReference, argumentReference)
        )
        val method = TreePlan(
          PlanKind.Method,
          methodStart,
          builder.length - 1,
          methodName._1,
          Vector(argument, contextual, resultType, application)
        )
        val extension = TreePlan(
          PlanKind.ExtensionMethods,
          extensionStart,
          builder.length - 1,
          extensionStart,
          Vector(typeParameter, receiver, method)
        )
        val constructor = TreePlan(
          PlanKind.Constructor,
          extensionStart,
          extensionStart,
          extensionStart,
          Vector.empty
        )
        val template = TreePlan(
          PlanKind.Template,
          0,
          builder.length,
          moduleName._1,
          Vector(constructor, extension)
        )
        val root = TreePlan(
          PlanKind.Module,
          0,
          builder.length,
          moduleName._1,
          Vector(template)
        )
        val generated = GeneratedPlan(builder.toString, root)
        validatePlan(generated).map(_ => generated)
      case _ => Left(error(
        "GENERATED_ORIGIN_INVALID",
        "the validated evidence Type lost its direct unary source-name shape."
      ))

  private def append(builder: StringBuilder, value: String): (Int, Int) =
    val start = builder.length
    builder.append(value)
    start -> builder.length

  private def leaf(kind: PlanKind, interval: (Int, Int)): TreePlan =
    TreePlan(kind, interval._1, interval._2, interval._1, Vector.empty)

  private def node(
      kind: PlanKind,
      interval: (Int, Int),
      children: Vector[TreePlan]
  ): TreePlan =
    TreePlan(kind, interval._1, interval._2, interval._1, children)

  private def typedIdentifier(builder: StringBuilder, value: String): TreePlan =
    leaf(PlanKind.TypeIdentifier, append(builder, value))

  private def termIdentifier(builder: StringBuilder, value: String): TreePlan =
    leaf(PlanKind.TermIdentifier, append(builder, value))

  private def validatePlan(
      generated: GeneratedPlan
  ): Either[BoundedExtensionModuleError, Unit] =
    val errors = Vector.newBuilder[String]
    def visit(plan: TreePlan): Unit =
      if plan.start < 0 || plan.start > plan.point || plan.point > plan.end ||
          plan.end > generated.source.length
      then errors += s"${plan.kind} has an invalid planned span"
      plan.children.foreach { child =>
        if child.start < plan.start || child.end > plan.end then
          errors += s"${plan.kind} does not contain ${child.kind}"
        visit(child)
      }
      plan.children.zip(plan.children.drop(1)).foreach { case (left, right) =>
        if left.end > right.start then
          errors += s"${plan.kind} children overlap or are out of source order"
      }
    visit(generated.root)
    val result = errors.result()
    Either.cond(
      result.isEmpty,
      (),
      error("GENERATED_ORIGIN_INVALID", result.mkString("; "))
    )

  private def position(
      raw: untpd.Tree,
      plan: TreePlan,
      source: SourceFile
  )(using Context): Either[BoundedExtensionModuleError, untpd.Tree] =
    (raw, plan.kind) match
      case (module: untpd.ModuleDef, PlanKind.Module)
          if plan.children.size == 1 =>
        position(module.impl, plan.children.head, source).map { implementation =>
          untpd
            .ModuleDef(module.name, implementation.asInstanceOf[untpd.Template])
            .withMods(module.mods)
            .cloneIn(source)
            .withSpan(plan.span)
        }
      case (template: untpd.Template, PlanKind.Template)
          if template.parentsOrDerived.isEmpty && template.derived.isEmpty &&
            template.self.isEmpty && template.body.size == 1 &&
            plan.children.size == 2 =>
        for
          constructor <- position(template.constr, plan.children.head, source)
          body <- position(template.body.head, plan.children(1), source)
        yield untpd
          .Template(
            constructor.asInstanceOf[untpd.DefDef],
            Nil,
            Nil,
            untpd.EmptyValDef,
            body :: Nil
          )
          .cloneIn(source)
          .withSpan(plan.span)
      case (constructor: untpd.DefDef, PlanKind.Constructor)
          if constructor.paramss.isEmpty && constructor.tpt.isEmpty &&
            constructor.rhs.isEmpty && plan.children.isEmpty =>
        Right(constructor.cloneIn(source).withSpan(plan.span))
      case (extension: untpd.ExtMethods, PlanKind.ExtensionMethods)
          if extension.paramss.map(_.size) == List(1, 1) &&
            extension.methods.size == 1 && plan.children.size == 3 =>
        for
          typeParameter <- position(
            extension.paramss.head.head,
            plan.children.head,
            source
          )
          receiver <- position(
            extension.paramss(1).head,
            plan.children(1),
            source
          )
          method <- position(extension.methods.head, plan.children(2), source)
        yield untpd
          .ExtMethods(
            List(
              typeParameter.asInstanceOf[untpd.TypeDef] :: Nil,
              receiver.asInstanceOf[untpd.ValDef] :: Nil
            ),
            method :: Nil
          )
          .cloneIn(source)
          .withSpan(plan.span)
      case (parameter: untpd.TypeDef, PlanKind.TypeParameter)
          if plan.children.size == 1 =>
        position(parameter.rhs, plan.children.head, source).map { rhs =>
          untpd
            .TypeDef(parameter.name, rhs)
            .withMods(parameter.mods)
            .cloneIn(source)
            .withSpan(plan.span)
        }
      case (bounds: untpd.TypeBoundsTree, PlanKind.TypeBounds)
          if bounds.lo.isEmpty && bounds.hi.isEmpty && bounds.alias.isEmpty &&
            plan.children.isEmpty =>
        Right(
          untpd
            .TypeBoundsTree(untpd.EmptyTree, untpd.EmptyTree)
            .cloneIn(source)
            .withSpan(plan.span)
        )
      case (parameter: untpd.ValDef,
            PlanKind.ReceiverParameter | PlanKind.OrdinaryArgument | PlanKind.ContextualParameter)
          if parameter.rhs.isEmpty && plan.children.size == 1 =>
        position(parameter.tpt, plan.children.head, source).map { parameterType =>
          untpd
            .ValDef(parameter.name, parameterType, untpd.EmptyTree)
            .withMods(parameter.mods)
            .cloneIn(source)
            .withSpan(plan.span)
        }
      case (method: untpd.DefDef, PlanKind.Method)
          if method.paramss.map(_.size) == List(1, 1) &&
            plan.children.size == 4 =>
        for
          argument <- position(method.paramss.head.head, plan.children.head, source)
          contextual <- position(method.paramss(1).head, plan.children(1), source)
          resultType <- position(method.tpt, plan.children(2), source)
          body <- position(method.rhs, plan.children(3), source)
        yield untpd
          .DefDef(
            method.name,
            List(
              argument.asInstanceOf[untpd.ValDef] :: Nil,
              contextual.asInstanceOf[untpd.ValDef] :: Nil
            ),
            resultType,
            body
          )
          .withMods(method.mods)
          .cloneIn(source)
          .withSpan(plan.span)
      case (applied: untpd.AppliedTypeTree, PlanKind.AppliedType)
          if applied.args.size == 1 && plan.children.size == 2 =>
        for
          constructor <- position(applied.tpt, plan.children.head, source)
          argument <- position(applied.args.head, plan.children(1), source)
        yield untpd
          .AppliedTypeTree(constructor, argument :: Nil)
          .cloneIn(source)
          .withSpan(plan.span)
      case (application: untpd.Apply, PlanKind.Application)
          if application.args.size == 2 && plan.children.size == 3 =>
        for
          function <- position(application.fun, plan.children.head, source)
          first <- position(application.args.head, plan.children(1), source)
          second <- position(application.args(1), plan.children(2), source)
        yield untpd
          .Apply(function, List(first, second))
          .cloneIn(source)
          .withSpan(plan.span)
      case (selection: untpd.Select, PlanKind.Selection)
          if plan.children.size == 1 =>
        position(selection.qualifier, plan.children.head, source).map { qualifier =>
          untpd
            .Select(qualifier, selection.name)
            .cloneIn(source)
            .withSpan(plan.span)
        }
      case (identifier: untpd.Ident,
            PlanKind.TypeIdentifier | PlanKind.TermIdentifier)
          if plan.children.isEmpty =>
        Right(identifier.cloneIn(source).withSpan(plan.span))
      case _ => Left(error(
        "GENERATED_ORIGIN_MISMATCH",
        s"raw ${raw.getClass.getSimpleName} does not match planned ${plan.kind}."
      ))

  private def validatePositioned(
      tree: untpd.ModuleDef,
      generated: GeneratedPlan,
      source: SourceFile
  )(using Context): Either[BoundedExtensionModuleError, Unit] =
    val trees = BoundedExtensionModuleUntypedLowerer.allTrees(tree)
    val invalid = trees.filter { current =>
      !current.source.exists || current.source.path != source.path ||
        current.source.content.mkString != generated.source ||
        !current.span.exists || current.span.start < 0 ||
        current.span.start > current.span.point ||
        current.span.point > current.span.end ||
        current.span.end > generated.source.length ||
        current.symbol != NoSymbol || current.isInstanceOf[untpd.TypedSplice]
    }
    Either.cond(
      trees.size == 21 && invalid.isEmpty && tree.span == generated.root.span,
      (),
      error(
        "GENERATED_ORIGIN_MISMATCH",
        s"expected 21 completely positioned fresh nodes; found ${trees.size} nodes and ${invalid.size} invalid nodes."
      )
    )

  private def error(code: String, detail: String): BoundedExtensionModuleError =
    BoundedExtensionModuleError(code, detail)
