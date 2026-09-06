package quasiquotes.terms

import quasiquotes.definitions.DefinitionName
import quasiquotes.parser.{BinderId, BlockStatement, TermShape, TypeShape}
import quasiquotes.types.{ConstructedType, TypeNormalForm, TypeTemplate}

import java.lang.ref.WeakReference
import scala.collection.mutable.ArrayBuffer
import scala.util.DynamicVariable
import scala.util.control.NonFatal

/** An opaque, graph-local declaration identity. */
final class TermBinder private[quasiquotes] (
    private val graphIdentity: AnyRef,
    private val binderIdentity: AnyRef
) derives CanEqual:
  override def equals(other: Any): Boolean =
    other match
      case that: TermBinder =>
        (graphIdentity eq that.graphIdentity) && binderIdentity == that.binderIdentity
      case _ => false

  override def hashCode(): Int =
    31 * System.identityHashCode(graphIdentity) + binderIdentity.hashCode

  override def toString: String = "TermBinder(<opaque>)"

  private[terms] def belongsToGraph(graph: AnyRef): Boolean = graphIdentity eq graph

  private[terms] def referenceShape(stateIdentity: AnyRef): TermShape =
    val id = binderIdentity.asInstanceOf[BinderId]
    val state = stateIdentity.asInstanceOf[TermBindingInternals.ScopeState]
    TermShape.BoundReference(id, state.displayNameFor(id).getOrElse(""))

/** Stable public failure returned by binder-safe Term operations. */
final case class TermBindingFailure(code: String, detail: String) derives CanEqual:
  def message: String = s"$code: $detail"

/** One named, explicitly typed parameter requested from a public Term builder. */
final case class TermParameterSpec(
    name: String,
    declaredType: TypeNormalForm
) derives CanEqual

/** Non-exhaustive semantic category for a public Term binding view. */
final class TermBindingCategory private (val code: String) derives CanEqual:
  override def toString: String = s"TermBindingCategory($code)"

object TermBindingCategory:
  val Ordinary: TermBindingCategory = new TermBindingCategory("ordinary")
  val BoundReference: TermBindingCategory = new TermBindingCategory("bound-reference")
  val Lambda: TermBindingCategory = new TermBindingCategory("lambda")
  val Block: TermBindingCategory = new TermBindingCategory("block")

/** Non-exhaustive semantic kind for a local definition view. */
final class TermLocalDefinitionKind private (val code: String) derives CanEqual:
  override def toString: String = s"TermLocalDefinitionKind($code)"

object TermLocalDefinitionKind:
  val ImmutableValue: TermLocalDefinitionKind =
    new TermLocalDefinitionKind("immutable-value")
  val Method: TermLocalDefinitionKind = new TermLocalDefinitionKind("method")

/** The live declarations exposed to one builder callback. */
final class TermBindingScope private[quasiquotes] (
    private val graph: AnyRef,
    private val liveBinders: Set[TermBinder],
    private val stateIdentity: AnyRef,
    private val declared: Option[TermBinder],
    private val parameters: Vector[Vector[TermBinder]]
):
  def declaredBinder: Option[TermBinder] = declared

  def parameterBinders: Vector[Vector[TermBinder]] = parameters

  def reference(binder: TermBinder): Either[TermBindingFailure, TermShape] =
    val state = stateIdentity.asInstanceOf[TermBindingInternals.ScopeState]
    if binder == null then
      Left(TermBindingInternals.missing("the referenced Term binder must be present."))
    else if !binder.belongsToGraph(graph) then
      Left(
        TermBindingInternals.failure(
          "TERM_BINDER_SCOPE_MISMATCH",
          "the referenced Term binder belongs to another semantic graph."
        )
      )
    else if !state.active || !liveBinders.contains(binder) then
      Left(
        TermBindingInternals.failure(
          "TERM_BINDER_UNBOUND",
          "the referenced Term binder is not live in this scope."
        )
      )
    else
      Right(binder.referenceShape(stateIdentity))

/** Scope-safe construction of the currently admitted binder-bearing Term families. */
object TermShapeBindings:
  def lambda(parameters: Vector[TermParameterSpec])(
      body: TermBindingScope => Either[TermBindingFailure, TermShape]
  ): Either[TermBindingFailure, TermShape] =
    TermBindingInternals.withSession { (session, outerScope) =>
      for
        parameter <- TermBindingInternals.oneParameter(parameters, "lambda")
        name <- TermBindingInternals.validName(parameter.name, "lambda parameter")
        parameterType <- TermBindingInternals.primitiveType(
          parameter.declaredType,
          Set("Int", "String", "Boolean"),
          "lambda parameter"
        )
        binderId <- session.allocate()
        _ = session.remember(binderId, name)
        binder = new TermBinder(session.graph, binderId.asInstanceOf[AnyRef])
        callbackState = new TermBindingInternals.ScopeState(session.names)
        callbackScope = new TermBindingScope(
          session.graph,
          TermBindingInternals.handles(session.graph, outerScope :+ binderId),
          callbackState,
          None,
          Vector(Vector(binder))
        )
        termBody <- TermBindingInternals.invoke(
          body,
          callbackScope,
          callbackState,
          session,
          outerScope :+ binderId,
          "lambda body"
        )
        result = TermShape.Lambda1(binderId, name, parameterType, termBody)
        checked <- TermBindingInternals.complete(result, session, outerScope)
      yield checked
    }

  def localValue(
      name: String,
      declaredType: TypeNormalForm,
      rhs: TermShape
  )(
      continuation: TermBindingScope => Either[TermBindingFailure, TermShape]
  ): Either[TermBindingFailure, TermShape] =
    TermBindingInternals.withSession { (session, outerScope) =>
      for
        validName <- TermBindingInternals.validName(name, "local value")
        typeSource <- TermBindingInternals.primitiveType(
          declaredType,
          Set("Int", "String", "Boolean", "AnyVal"),
          "local value"
        )
        presentRhs <- TermBindingInternals.presentShape(rhs, "local value rhs")
        _ <- TermBindingInternals.validateEmbedded(presentRhs, session.graph, outerScope)
        binderId <- session.allocate()
        _ = session.remember(binderId, validName)
        binder = new TermBinder(session.graph, binderId.asInstanceOf[AnyRef])
        callbackState = new TermBindingInternals.ScopeState(session.names)
        callbackScope = new TermBindingScope(
          session.graph,
          TermBindingInternals.handles(session.graph, outerScope :+ binderId),
          callbackState,
          Some(binder),
          Vector.empty
        )
        resultBody <- TermBindingInternals.invoke(
          continuation,
          callbackScope,
          callbackState,
          session,
          outerScope :+ binderId,
          "local value continuation"
        )
        result = TermShape.Block(
          List(BlockStatement.LocalVal(binderId, validName, typeSource, presentRhs)),
          resultBody
        )
        checked <- TermBindingInternals.complete(result, session, outerScope)
      yield checked
    }

  def localMethod(
      name: String,
      parameterClauses: Vector[Vector[TermParameterSpec]],
      resultType: TypeNormalForm
  )(
      body: TermBindingScope => Either[TermBindingFailure, TermShape]
  )(
      continuation: TermBindingScope => Either[TermBindingFailure, TermShape]
  ): Either[TermBindingFailure, TermShape] =
    TermBindingInternals.withSession { (session, outerScope) =>
      for
        validMethodName <- TermBindingInternals.validName(name, "local method")
        parameter <- TermBindingInternals.oneParameterClause(parameterClauses, "local method")
        validParameterName <- TermBindingInternals.validName(
          parameter.name,
          "local method parameter"
        )
        parameterTypeSource <- TermBindingInternals.primitiveType(
          parameter.declaredType,
          Set("Int", "String", "Boolean"),
          "local method parameter"
        )
        resultTypeSource <- TermBindingInternals.primitiveType(
          resultType,
          Set("Int", "String", "Boolean"),
          "local method result"
        )
        methodBinderId <- session.allocate()
        parameterBinderId <- session.allocate()
        _ = session.remember(methodBinderId, validMethodName)
        _ = session.remember(parameterBinderId, validParameterName)
        parameterBinder = new TermBinder(session.graph, parameterBinderId.asInstanceOf[AnyRef])
        bodyScopeState = new TermBindingInternals.ScopeState(session.names)
        bodyScope = new TermBindingScope(
          session.graph,
          TermBindingInternals.handles(session.graph, outerScope :+ parameterBinderId),
          bodyScopeState,
          None,
          Vector(Vector(parameterBinder))
        )
        methodBody <- TermBindingInternals.invoke(
          body,
          bodyScope,
          bodyScopeState,
          session,
          outerScope :+ parameterBinderId,
          "local method body"
        )
        methodBinder = new TermBinder(session.graph, methodBinderId.asInstanceOf[AnyRef])
        continuationScopeState = new TermBindingInternals.ScopeState(session.names)
        continuationScope = new TermBindingScope(
          session.graph,
          TermBindingInternals.handles(session.graph, outerScope :+ methodBinderId),
          continuationScopeState,
          Some(methodBinder),
          Vector.empty
        )
        resultBody <- TermBindingInternals.invoke(
          continuation,
          continuationScope,
          continuationScopeState,
          session,
          outerScope :+ methodBinderId,
          "local method continuation"
        )
        result = TermShape.Block(
          List(
            BlockStatement.LocalDef(
              methodBinderId,
              validMethodName,
              parameterBinderId,
              validParameterName,
              TypeShape.Identifier(parameterTypeSource),
              TypeShape.Identifier(resultTypeSource),
              methodBody
            )
          ),
          resultBody
        )
        checked <- TermBindingInternals.complete(result, session, outerScope)
      yield checked
    }

/** Public, non-exhaustive inspection of a TermShape's binder roles. */
object TermShapeBindingView:
  def inspect(shape: TermShape): Either[TermBindingFailure, TermBindingView] =
    TermBindingInternals.inspect(shape)

final class TermBindingView private[quasiquotes] (
    private val categoryValue: TermBindingCategory,
    private val boundReferenceValue: Option[TermBoundReferenceView],
    private val lambdaValue: Option[TermLambdaView],
    private val blockValue: Option[TermBlockView]
):
  def category: TermBindingCategory = categoryValue
  def boundReference: Option[TermBoundReferenceView] = boundReferenceValue
  def lambda: Option[TermLambdaView] = lambdaValue
  def block: Option[TermBlockView] = blockValue

final class TermBoundReferenceView private[quasiquotes] (
    private val binderValue: TermBinder
):
  def binder: TermBinder = binderValue

final class TermLambdaView private[quasiquotes] (
    private val parametersValue: Vector[TermParameterView],
    private val bodyValue: TermShape
):
  def parameters: Vector[TermParameterView] = parametersValue
  def body: TermShape = bodyValue

final class TermParameterView private[quasiquotes] (
    private val binderValue: TermBinder,
    private val nameValue: String,
    private val declaredTypeValue: TypeNormalForm
):
  def binder: TermBinder = binderValue
  def name: String = nameValue
  def declaredType: TypeNormalForm = declaredTypeValue

final class TermBlockView private[quasiquotes] (
    private val localsValue: Vector[TermLocalDefinitionView],
    private val resultValue: TermShape
):
  def locals: Vector[TermLocalDefinitionView] = localsValue
  def result: TermShape = resultValue

final class TermLocalDefinitionView private[quasiquotes] (
    private val kindValue: TermLocalDefinitionKind,
    private val binderValue: TermBinder,
    private val nameValue: String,
    private val parameterClausesValue: Vector[Vector[TermParameterView]],
    private val resultTypeValue: TypeNormalForm,
    private val bodyValue: Option[TermShape]
):
  def kind: TermLocalDefinitionKind = kindValue
  def binder: TermBinder = binderValue
  def name: String = nameValue
  def parameterClauses: Vector[Vector[TermParameterView]] = parameterClausesValue
  def resultType: TypeNormalForm = resultTypeValue
  def body: Option[TermShape] = bodyValue

private[quasiquotes] object TermBindingInternals:
  private final class Graph

  final class ScopeState(private val names: scala.collection.Map[BinderId, String]):
    var active: Boolean = true
    def displayNameFor(id: BinderId): Option[String] = names.get(id)

  final class Session:
    val graph: AnyRef = new Graph
    val names = scala.collection.mutable.Map.empty[BinderId, String]
    private var nextBinder = 0

    def allocate(): Either[TermBindingFailure, BinderId] =
      if nextBinder == Int.MaxValue then
        Left(failure("TERM_BINDING_INTERNAL_INVARIANT", "binder allocation overflow."))
      else
        val binder = BinderId(nextBinder)
        nextBinder += 1
        Right(binder)

    def remember(id: BinderId, name: String): Unit = names.update(id, name)

  private final case class BuildContext(session: Session, scope: Vector[BinderId])
  private val currentBuild = new DynamicVariable[Option[BuildContext]](None)

  final class PersistentParameters private[TermBindingInternals] (
      private[TermBindingInternals] val session: Session,
      private[TermBindingInternals] val ids: Vector[Vector[BinderId]]
  ):
    private val flattenedIds = ids.flatten

    def binderAt(
        clauseIndex: Int,
        parameterIndex: Int
    ): Either[TermBindingFailure, TermBinder] =
      binderIdAt(clauseIndex, parameterIndex)
        .map(id => new TermBinder(session.graph, id.asInstanceOf[AnyRef]))

    def referenceAt(
        clauseIndex: Int,
        parameterIndex: Int
    ): Either[TermBindingFailure, TermShape] =
      binderIdAt(clauseIndex, parameterIndex).map { id =>
        val reference = TermShape.BoundReference(id, session.names.getOrElse(id, ""))
        Registry.register(reference, session.graph, flattenedIds)
        reference
      }

    def complete(shape: TermShape): Either[TermBindingFailure, TermShape] =
      validate(shape).map { present =>
        Registry.register(present, session.graph, flattenedIds)
        present
      }

    def validateDefinitionBody(
        expectedParameterCounts: Vector[Int],
        shape: TermShape
    ): Either[TermBindingFailure, TermShape] =
      if ids.map(_.size) != expectedParameterCounts then
        Left(
          failure(
            "TERM_BINDER_SCOPE_MISMATCH",
            "the persistent method parameter scope does not match the declared clause topology."
          )
        )
      else
        validate(shape).flatMap { present =>
          Registry.requireOwnedBinderGraph(present, session.graph, flattenedIds).map(_ => present)
        }

    private def validate(shape: TermShape): Either[TermBindingFailure, TermShape] =
      presentShape(shape, "definition method body")
        .flatMap(validateEmbedded(_, session.graph, flattenedIds))
        .flatMap { present =>
          val validation =
            if flattenedIds.isEmpty then TermShapeTraversal.validateSupported(present)
            else TermShapeTraversal.validateSupportedInScope(present, flattenedIds)
          validation.left
            .map(_ => unsupported("the definition method body exceeds the admitted Core Term family."))
            .map(_ => present)
        }

    def alphaNormalize(shape: TermShape): TermShape =
      if flattenedIds.isEmpty then TermShapeTraversal.alphaNormalize(shape)
      else TermShapeTraversal.alphaNormalizeInScope(shape, flattenedIds)

    private def binderIdAt(
        clauseIndex: Int,
        parameterIndex: Int
    ): Either[TermBindingFailure, BinderId] =
      ids.lift(clauseIndex).flatMap(_.lift(parameterIndex)).toRight(
        failure(
          "TERM_BINDER_UNBOUND",
          s"no method parameter exists at clause $clauseIndex, index $parameterIndex."
        )
      )

  private final case class RegistryEntry(
      shape: WeakReference[TermShape],
      graph: AnyRef,
      scope: Vector[BinderId]
  )

  private object Registry:
    private val entries = ArrayBuffer.empty[RegistryEntry]

    def contextOf(shape: TermShape): Option[(AnyRef, Vector[BinderId])] = synchronized {
      prune()
      entries.collectFirst {
        case entry if entry.shape.get() eq shape => entry.graph -> entry.scope
      }
    }

    def register(shape: TermShape, graph: AnyRef, scope: Vector[BinderId]): Unit = synchronized {
      prune()
      registerTree(shape, graph, scope)
    }

    def foreignOwnerInTree(shape: TermShape, graph: AnyRef): Boolean = synchronized {
      prune()
      def loop(current: TermShape): Boolean =
        if current == null then false
        else
          contextOf(current).exists { case (owner, _) => !(owner eq graph) } ||
            (current match
              case TermShape.Lambda1(_, _, _, body) => loop(body)
              case TermShape.Select(qualifier, _) => loop(qualifier)
              case TermShape.Apply(function, arguments) =>
                loop(function) || (arguments != null && arguments.exists(loop))
              case TermShape.New(_, arguments) => arguments != null && arguments.exists(loop)
              case TermShape.Infix(left, _, right) => loop(left) || loop(right)
              case TermShape.Unary(_, operand) => loop(operand)
              case TermShape.InterpolatedString(_, _, arguments) =>
                arguments != null && arguments.exists(loop)
              case TermShape.Typed(expression, _) => loop(expression)
              case TermShape.Tuple(elements) => elements != null && elements.exists(loop)
              case TermShape.If(condition, thenBranch, elseBranch) =>
                loop(condition) || loop(thenBranch) || loop(elseBranch)
              case TermShape.Block(statements, result) =>
                (statements != null && statements.exists {
                  case local: BlockStatement.LocalVal => loop(local.initializer)
                  case local: BlockStatement.LocalDef => loop(local.body)
                  case term: TermShape => loop(term)
                  case null => false
                }) || loop(result)
              case TermShape.Parenthesized(expression) => loop(expression)
              case _ => false)
      loop(shape)
    }

    def requireOwnedBinderGraph(
        shape: TermShape,
        graph: AnyRef,
        scope: Vector[BinderId]
    ): Either[TermBindingFailure, Unit] = synchronized {
      if !containsBinder(shape) then Right(())
      else
        contextOf(shape) match
          case Some((owner, registeredScope))
              if (owner eq graph) && registeredScope == scope =>
            Right(())
          case _ =>
            Left(
              failure(
                "TERM_BINDER_SCOPE_MISMATCH",
                "the method body is not owned by its persistent parameter graph."
              )
            )
    }

    private def remember(shape: TermShape, graph: AnyRef, scope: Vector[BinderId]): Unit =
      if containsBinder(shape) && !entries.exists(entry => entry.shape.get() eq shape) then
        entries += RegistryEntry(new WeakReference(shape), graph, scope)

    private def registerTree(shape: TermShape, graph: AnyRef, scope: Vector[BinderId]): Unit =
      if shape == null then ()
      else
        remember(shape, graph, scope)
        shape match
          case TermShape.BoundReference(_, _) | TermShape.Identifier(_, _) | TermShape.Literal(_) |
              TermShape.Unsupported(_, _) => ()
          case TermShape.Lambda1(id, _, _, body) => registerTree(body, graph, scope :+ id)
          case TermShape.Select(qualifier, _) => registerTree(qualifier, graph, scope)
          case TermShape.Apply(function, arguments) =>
            registerTree(function, graph, scope)
            if arguments != null then arguments.foreach(registerTree(_, graph, scope))
          case TermShape.New(_, arguments) =>
            if arguments != null then arguments.foreach(registerTree(_, graph, scope))
          case TermShape.Infix(left, _, right) =>
            registerTree(left, graph, scope)
            registerTree(right, graph, scope)
          case TermShape.Unary(_, operand) => registerTree(operand, graph, scope)
          case TermShape.InterpolatedString(_, _, arguments) =>
            if arguments != null then arguments.foreach(registerTree(_, graph, scope))
          case TermShape.Typed(expression, _) => registerTree(expression, graph, scope)
          case TermShape.Tuple(elements) =>
            if elements != null then elements.foreach(registerTree(_, graph, scope))
          case TermShape.If(condition, thenBranch, elseBranch) =>
            registerTree(condition, graph, scope)
            registerTree(thenBranch, graph, scope)
            registerTree(elseBranch, graph, scope)
          case TermShape.Block(statements, result) =>
            var currentScope = scope
            if statements != null then
              statements.foreach {
                case local: BlockStatement.LocalVal =>
                  registerTree(local.initializer, graph, currentScope)
                  currentScope = currentScope :+ local.binderId
                case local: BlockStatement.LocalDef =>
                  registerTree(local.body, graph, currentScope :+ local.parameterBinderId)
                  currentScope = currentScope :+ local.methodBinderId
                case term: TermShape => registerTree(term, graph, currentScope)
                case null => ()
              }
            registerTree(result, graph, currentScope)
          case TermShape.Parenthesized(expression) => registerTree(expression, graph, scope)

    private def containsBinder(shape: TermShape): Boolean =
      if shape == null then false
      else
        shape match
          case TermShape.BoundReference(_, _) | TermShape.Lambda1(_, _, _, _) => true
          case TermShape.Block(statements, result) =>
            statements != null && (statements.exists {
              case _: BlockStatement.LocalVal | _: BlockStatement.LocalDef => true
              case term: TermShape => containsBinder(term)
              case null => false
            } || containsBinder(result))
          case TermShape.Select(qualifier, _) => containsBinder(qualifier)
          case TermShape.Apply(function, arguments) =>
            containsBinder(function) || (arguments != null && arguments.exists(containsBinder))
          case TermShape.New(_, arguments) => arguments != null && arguments.exists(containsBinder)
          case TermShape.Infix(left, _, right) => containsBinder(left) || containsBinder(right)
          case TermShape.Unary(_, operand) => containsBinder(operand)
          case TermShape.InterpolatedString(_, _, arguments) =>
            arguments != null && arguments.exists(containsBinder)
          case TermShape.Typed(expression, _) => containsBinder(expression)
          case TermShape.Tuple(elements) => elements != null && elements.exists(containsBinder)
          case TermShape.If(condition, thenBranch, elseBranch) =>
            containsBinder(condition) || containsBinder(thenBranch) || containsBinder(elseBranch)
          case TermShape.Parenthesized(expression) => containsBinder(expression)
          case _ => false

    private def prune(): Unit =
      entries.filterInPlace(_.shape.get() != null)

  def withSession(
      operation: (Session, Vector[BinderId]) => Either[TermBindingFailure, TermShape]
  ): Either[TermBindingFailure, TermShape] =
    currentBuild.value match
      case Some(context) => operation(context.session, context.scope)
      case None =>
        val session = new Session
        operation(session, Vector.empty)

  def persistentParameters(
      names: Vector[Vector[String]]
  ): Either[TermBindingFailure, PersistentParameters] =
    Option(names).toRight(missing("definition parameter clauses must be present.")).flatMap {
      presentNames =>
        val session = new Session
        presentNames
          .foldLeft[Either[TermBindingFailure, Vector[Vector[BinderId]]]](
            Right(Vector.empty)
          ) { (clausesResult, clauseNames) =>
            for
              clauses <- clausesResult
              presentClause <- Option(clauseNames)
                .toRight(missing("a definition parameter clause must be present."))
              clause <- presentClause.foldLeft[
                Either[TermBindingFailure, Vector[BinderId]]
              ](Right(Vector.empty)) { (idsResult, name) =>
                for
                  collected <- idsResult
                  presentName <- Option(name)
                    .toRight(missing("a definition parameter name must be present."))
                  id <- session.allocate()
                  _ = session.remember(id, presentName)
                yield collected :+ id
              }
            yield clauses :+ clause
          }
          .map(new PersistentParameters(session, _))
    }

  def withPersistentParameters[A](
      parameters: PersistentParameters
  )(operation: => A): A =
    currentBuild.withValue(
      Some(BuildContext(parameters.session, parameters.ids.flatten))
    )(operation)

  def handles(graph: AnyRef, ids: Vector[BinderId]): Set[TermBinder] =
    ids.iterator.map(id => new TermBinder(graph, id.asInstanceOf[AnyRef])).toSet

  def invoke(
      callback: TermBindingScope => Either[TermBindingFailure, TermShape],
      scope: TermBindingScope,
      scopeState: ScopeState,
      session: Session,
      liveScope: Vector[BinderId],
      label: String
  ): Either[TermBindingFailure, TermShape] =
    if callback == null then Left(missing(s"the $label callback must be present."))
    else
      try
        currentBuild.withValue(Some(BuildContext(session, liveScope))) {
          val returned = callback(scope)
          Option(returned)
            .toRight(invalidBody(s"the $label callback returned no result."))
            .flatMap(identity)
            .flatMap(presentShape(_, label))
            .flatMap(shape => validateEmbedded(shape, session.graph, liveScope))
        }
      catch
        case NonFatal(error) =>
          Left(invalidBody(s"the $label callback failed: ${error.getClass.getSimpleName}."))
      finally scopeState.active = false

  def complete(
      shape: TermShape,
      session: Session,
      outerScope: Vector[BinderId]
  ): Either[TermBindingFailure, TermShape] =
    validateGraph(shape, outerScope).flatMap { _ =>
      val traversalResult =
        if outerScope.isEmpty then TermShapeTraversal.validateSupported(shape)
        else TermShapeTraversal.validateSupportedInScope(shape, outerScope)
      traversalResult.left
        .map(_ => unsupported("the constructed Term exceeds the admitted Core Term family."))
        .map { _ =>
          if outerScope.isEmpty then Registry.register(shape, session.graph, Vector.empty)
          shape
        }
    }

  def inspect(shape: TermShape): Either[TermBindingFailure, TermBindingView] =
    presentShape(shape, "Term shape").flatMap { present =>
      val (graph, initialScope, isNewGraph) = Registry.contextOf(present) match
        case Some((knownGraph, scope)) => (knownGraph, scope, false)
        case None => (new Graph: AnyRef, Vector.empty[BinderId], true)

      validateGraph(present, initialScope).flatMap { _ =>
        if isNewGraph then Registry.register(present, graph, initialScope)
        view(present, graph)
      }
    }

  def validateEmbedded(
      shape: TermShape,
      graph: AnyRef,
      scope: Vector[BinderId]
  ): Either[TermBindingFailure, TermShape] =
    Registry.foreignOwnerInTree(shape, graph) match
      case true =>
        Left(
          failure(
            "TERM_BINDER_SCOPE_MISMATCH",
            "a binder-bearing child belongs to another semantic graph."
          )
        )
      case false => validateGraph(shape, scope).map(_ => shape)

  def oneParameter(
      parameters: Vector[TermParameterSpec],
      label: String
  ): Either[TermBindingFailure, TermParameterSpec] =
    Option(parameters).toRight(missing(s"$label parameters must be present.")).flatMap {
      case Vector(parameter) if parameter != null => Right(parameter)
      case Vector(null) => Left(missing(s"the $label parameter must be present."))
      case other =>
        Left(unsupported(s"$label currently requires exactly one parameter; found ${other.size}."))
    }

  def oneParameterClause(
      clauses: Vector[Vector[TermParameterSpec]],
      label: String
  ): Either[TermBindingFailure, TermParameterSpec] =
    Option(clauses).toRight(missing(s"$label parameter clauses must be present.")).flatMap {
      case Vector(clause) => oneParameter(clause, label)
      case other =>
        Left(
          unsupported(
            s"$label currently requires one clause containing one parameter; found ${other.size} clauses."
          )
        )
    }

  def validName(value: String, label: String): Either[TermBindingFailure, String] =
    Option(value)
      .toRight(missing(s"the $label name must be present."))
      .flatMap(name =>
        DefinitionName.fromSource(name)
          .left
          .map(_ => failure("TERM_BINDING_INVALID_NAME", s"invalid $label name `$name`."))
          .map(_.source)
      )

  def primitiveType(
      value: TypeNormalForm,
      admitted: Set[String],
      label: String
  ): Either[TermBindingFailure, String] =
    try
      Option(value).toRight(missing(s"the $label type must be present.")).flatMap { normalForm =>
        TypeTemplate.validateConstructed(normalForm).left
          .map(error => failure("TERM_BINDING_INVALID_TYPE", error.message))
          .flatMap { _ =>
            val source = ConstructedType.renderSource(normalForm)
            Either.cond(
              admitted(source),
              source,
              unsupported(s"$label type `$source` is outside the currently admitted family.")
            )
          }
      }
    catch
      case NonFatal(error) =>
        Left(
          failure(
            "TERM_BINDING_INVALID_TYPE",
            s"invalid $label type: ${error.getClass.getSimpleName}."
          )
        )

  def presentShape(value: TermShape, label: String): Either[TermBindingFailure, TermShape] =
    Option(value).toRight(missing(s"the $label must be present."))

  def failure(code: String, detail: String): TermBindingFailure =
    TermBindingFailure(code, detail)

  def missing(detail: String): TermBindingFailure = failure("TERM_BINDING_MISSING", detail)

  private def unsupported(detail: String): TermBindingFailure =
    failure("TERM_BINDING_UNSUPPORTED", detail)

  private def invalidBody(detail: String): TermBindingFailure =
    failure("TERM_BINDING_INVALID_BODY", detail)

  private def view(shape: TermShape, graph: AnyRef): Either[TermBindingFailure, TermBindingView] =
    shape match
      case TermShape.BoundReference(id, _) =>
        Right(
          new TermBindingView(
            TermBindingCategory.BoundReference,
            Some(new TermBoundReferenceView(new TermBinder(graph, id.asInstanceOf[AnyRef]))),
            None,
            None
          )
        )
      case TermShape.Lambda1(id, name, declaredType, body) =>
        normalFormFromSource(declaredType, "lambda parameter").map { parameterType =>
          val parameter = new TermParameterView(
            new TermBinder(graph, id.asInstanceOf[AnyRef]),
            name,
            parameterType
          )
          new TermBindingView(
            TermBindingCategory.Lambda,
            None,
            Some(new TermLambdaView(Vector(parameter), body)),
            None
          )
        }
      case TermShape.Block(statements, result) =>
        collect(
          statements.collect {
            case local: BlockStatement.LocalVal => localValueView(local, graph)
            case local: BlockStatement.LocalDef => localMethodView(local, graph)
          }
        ).map { locals =>
          new TermBindingView(
            TermBindingCategory.Block,
            None,
            None,
            Some(new TermBlockView(locals, result))
          )
        }
      case _ =>
        Right(new TermBindingView(TermBindingCategory.Ordinary, None, None, None))

  private def localValueView(
      local: BlockStatement.LocalVal,
      graph: AnyRef
  ): Either[TermBindingFailure, TermLocalDefinitionView] =
    normalFormFromSource(local.declaredType, "local value").map { declaredType =>
      new TermLocalDefinitionView(
        TermLocalDefinitionKind.ImmutableValue,
        new TermBinder(graph, local.binderId.asInstanceOf[AnyRef]),
        local.displayName,
        Vector.empty,
        declaredType,
        Some(local.initializer)
      )
    }

  private def localMethodView(
      local: BlockStatement.LocalDef,
      graph: AnyRef
  ): Either[TermBindingFailure, TermLocalDefinitionView] =
    for
      parameterType <- normalFormFromShape(local.parameterType, "local method parameter")
      resultType <- normalFormFromShape(local.resultType, "local method result")
    yield
      val parameter = new TermParameterView(
        new TermBinder(graph, local.parameterBinderId.asInstanceOf[AnyRef]),
        local.parameterDisplayName,
        parameterType
      )
      new TermLocalDefinitionView(
        TermLocalDefinitionKind.Method,
        new TermBinder(graph, local.methodBinderId.asInstanceOf[AnyRef]),
        local.methodDisplayName,
        Vector(Vector(parameter)),
        resultType,
        Some(local.body)
      )

  private def validateGraph(
      shape: TermShape,
      initialScope: Vector[BinderId]
  ): Either[TermBindingFailure, Unit] =
    val declaredBinders = scala.collection.mutable.Set.from(initialScope)

    def children(values: List[TermShape], scope: Vector[BinderId]): Either[TermBindingFailure, Unit] =
      Option(values).toRight(missing("Term children must be present.")).flatMap(
        _.foldLeft[Either[TermBindingFailure, Unit]](Right(())) { (result, child) =>
          result.flatMap(_ => presentShape(child, "Term child").flatMap(loop(_, scope)))
        }
      )

    def fresh(id: BinderId, scope: Vector[BinderId]): Either[TermBindingFailure, Unit] =
      if id == null then Left(missing("binder identity must be present."))
      else if scope.contains(id) || declaredBinders.contains(id) then
        Left(
          failure(
            "TERM_BINDER_COLLISION",
            "a declaration binder collides with another identity in its semantic graph."
          )
        )
      else
        declaredBinders += id
        Right(())

    def ordinaryName(value: String, label: String): Either[TermBindingFailure, Unit] =
      Option(value).toRight(missing(s"$label must be present.")).map(_ => ())

    def loop(current: TermShape, scope: Vector[BinderId]): Either[TermBindingFailure, Unit] =
      if current == null then Left(missing("Term shape must be present."))
      else
        current match
          case TermShape.BoundReference(id, displayName) =>
            for
              _ <- ordinaryName(displayName, "bound-reference display name")
              _ <- Either.cond(
                id != null && scope.contains(id),
                (),
                failure("TERM_BINDER_UNBOUND", "a bound reference has no live declaration.")
              )
            yield ()
          case TermShape.Lambda1(id, name, declaredType, body) =>
            for
              _ <- fresh(id, scope)
              _ <- validName(name, "lambda parameter")
              _ <- normalFormFromSource(declaredType, "lambda parameter")
              presentBody <- presentShape(body, "lambda body")
              _ <- loop(presentBody, scope :+ id)
            yield ()
          case TermShape.Identifier(name, _) => ordinaryName(name, "identifier name")
          case TermShape.Literal(value) => ordinaryName(value, "literal value")
          case TermShape.Select(qualifier, name) =>
            ordinaryName(name, "selection name").flatMap(_ => presentShape(qualifier, "selection qualifier").flatMap(loop(_, scope)))
          case TermShape.Apply(function, arguments) =>
            presentShape(function, "application function").flatMap(loop(_, scope))
              .flatMap(_ => children(arguments, scope))
          case TermShape.New(constructor, arguments) =>
            ordinaryName(constructor, "constructor name").flatMap(_ => children(arguments, scope))
          case TermShape.Infix(left, operator, right) =>
            ordinaryName(operator, "infix operator")
              .flatMap(_ => presentShape(left, "infix left operand").flatMap(loop(_, scope)))
              .flatMap(_ => presentShape(right, "infix right operand").flatMap(loop(_, scope)))
          case TermShape.Unary(operator, operand) =>
            ordinaryName(operator, "unary operator")
              .flatMap(_ => presentShape(operand, "unary operand").flatMap(loop(_, scope)))
          case TermShape.InterpolatedString(prefix, parts, arguments) =>
            if prefix == null || parts == null || arguments == null then
              Left(missing("interpolation prefix, parts, and arguments must be present."))
            else if parts.exists(_ == null) then Left(missing("interpolation parts must be present."))
            else children(arguments, scope)
          case TermShape.Typed(expression, typeName) =>
            ordinaryName(typeName, "ascribed type")
              .flatMap(_ => presentShape(expression, "typed expression").flatMap(loop(_, scope)))
          case TermShape.Tuple(elements) => children(elements, scope)
          case TermShape.If(condition, thenBranch, elseBranch) =>
            presentShape(condition, "if condition").flatMap(loop(_, scope))
              .flatMap(_ => presentShape(thenBranch, "then branch").flatMap(loop(_, scope)))
              .flatMap(_ => presentShape(elseBranch, "else branch").flatMap(loop(_, scope)))
          case TermShape.Block(statements, result) =>
            Option(statements).toRight(missing("block statements must be present.")).flatMap {
              case (local: BlockStatement.LocalVal) :: Nil =>
                for
                  _ <- fresh(local.binderId, scope)
                  _ <- validName(local.displayName, "local value")
                  _ <- normalFormFromSource(local.declaredType, "local value")
                  initializer <- presentShape(local.initializer, "local value rhs")
                  _ <- loop(initializer, scope)
                  presentResult <- presentShape(result, "block result")
                  _ <- loop(presentResult, scope :+ local.binderId)
                yield ()
              case (local: BlockStatement.LocalDef) :: Nil =>
                for
                  _ <- fresh(local.methodBinderId, scope)
                  _ <- fresh(local.parameterBinderId, scope :+ local.methodBinderId)
                  _ <- validName(local.methodDisplayName, "local method")
                  _ <- validName(local.parameterDisplayName, "local method parameter")
                  _ <- normalFormFromShape(local.parameterType, "local method parameter")
                  _ <- normalFormFromShape(local.resultType, "local method result")
                  methodBody <- presentShape(local.body, "local method body")
                  _ <- loop(methodBody, scope :+ local.parameterBinderId)
                  presentResult <- presentShape(result, "block result")
                  _ <- loop(presentResult, scope :+ local.methodBinderId)
                yield ()
              case expressionStatements if expressionStatements.forall(_.isInstanceOf[TermShape]) =>
                children(expressionStatements.map(_.asInstanceOf[TermShape]), scope)
                  .flatMap(_ => presentShape(result, "block result").flatMap(loop(_, scope)))
              case _ => Left(unsupported("the block binder topology is not admitted."))
            }
          case TermShape.Parenthesized(expression) =>
            presentShape(expression, "parenthesized expression").flatMap(loop(_, scope))
          case TermShape.Unsupported(_, _) => Left(unsupported("an unsupported Term node cannot be inspected."))

    try loop(shape, initialScope)
    catch
      case NonFatal(error) =>
        Left(
          failure(
            "TERM_BINDING_INTERNAL_INVARIANT",
            s"Term graph validation failed: ${error.getClass.getSimpleName}."
          )
        )

  private def normalFormFromShape(
      shape: TypeShape,
      label: String
  ): Either[TermBindingFailure, TypeNormalForm] =
    try
      Option(shape).toRight(missing(s"the $label type must be present.")).flatMap(
        TypeNormalForm.fromShape(_).left.map(error =>
          failure("TERM_BINDING_INVALID_TYPE", s"invalid $label type: ${error.message}")
        )
      )
    catch
      case NonFatal(error) =>
        Left(
          failure(
            "TERM_BINDING_INVALID_TYPE",
            s"invalid $label type: ${error.getClass.getSimpleName}."
          )
        )

  private def normalFormFromSource(
      source: String,
      label: String
  ): Either[TermBindingFailure, TypeNormalForm] =
    Option(source).toRight(missing(s"the $label type must be present.")).flatMap { present =>
      RenderedTypeParser.parse(present)
        .left
        .map(detail => failure("TERM_BINDING_INVALID_TYPE", s"invalid $label type: $detail"))
        .flatMap(normalFormFromShape(_, label))
    }

  private object RenderedTypeParser:
    def parse(source: String): Either[String, TypeShape] =
      val value = source.trim
      if value.isEmpty then Left("the rendered type is empty.")
      else
        topLevelArrow(value) match
          case Some(index) =>
            val left = value.substring(0, index).trim
            val right = value.substring(index + 2).trim
            for
              arguments <- parseFunctionArguments(left)
              result <- parse(right)
            yield TypeShape.Function(arguments, result)
          case None => parseNonFunction(value)

    private def parseNonFunction(value: String): Either[String, TypeShape] =
      if wrapped(value, '(', ')') then
        splitTopLevel(value.substring(1, value.length - 1), ',').flatMap { pieces =>
          collectParsed(pieces.map(parse)).map(TypeShape.Tuple(_))
        }
      else
        topLevelOpening(value, '[') match
          case Some(index) if value.endsWith("]") =>
            val constructor = value.substring(0, index).trim
            splitTopLevel(value.substring(index + 1, value.length - 1), ',').flatMap { pieces =>
              for
                parsedConstructor <- parseIdentifier(constructor)
                arguments <- collectParsed(pieces.map(parse))
              yield TypeShape.Apply(parsedConstructor, arguments)
            }
          case Some(_) => Left(s"malformed rendered type `$value`.")
          case None => parseIdentifier(value)

    private def parseFunctionArguments(value: String): Either[String, List[TypeShape]] =
      if wrapped(value, '(', ')') then
        splitTopLevel(value.substring(1, value.length - 1), ',').flatMap(parts => collectParsed(parts.map(parse)))
      else parse(value).map(_ :: Nil)

    private def parseIdentifier(value: String): Either[String, TypeShape] =
      if value.nonEmpty &&
          (value.head == '_' || value.head.isLetter) &&
          value.tail.forall(char => char == '_' || char.isLetterOrDigit)
      then Right(TypeShape.Identifier(value))
      else Left(s"unsupported rendered type identifier `$value`.")

    private def wrapped(value: String, open: Char, close: Char): Boolean =
      value.length >= 2 && value.head == open && value.last == close && matchingClose(value, 0) == value.length - 1

    private def matchingClose(value: String, opening: Int): Int =
      var depth = 0
      var index = opening
      while index < value.length do
        value.charAt(index) match
          case '(' | '[' => depth += 1
          case ')' | ']' =>
            depth -= 1
            if depth == 0 then return index
          case _ => ()
        index += 1
      -1

    private def topLevelArrow(value: String): Option[Int] =
      var round = 0
      var square = 0
      var index = 0
      while index < value.length - 1 do
        value.charAt(index) match
          case '(' => round += 1
          case ')' => round -= 1
          case '[' => square += 1
          case ']' => square -= 1
          case '=' if value.charAt(index + 1) == '>' && round == 0 && square == 0 => return Some(index)
          case _ => ()
        index += 1
      None

    private def topLevelOpening(value: String, target: Char): Option[Int] =
      var round = 0
      var square = 0
      var index = 0
      while index < value.length do
        val char = value.charAt(index)
        if char == target && round == 0 && square == 0 then return Some(index)
        char match
          case '(' => round += 1
          case ')' => round -= 1
          case '[' => square += 1
          case ']' => square -= 1
          case _ => ()
        index += 1
      None

    private def splitTopLevel(value: String, delimiter: Char): Either[String, List[String]] =
      val parts = List.newBuilder[String]
      var round = 0
      var square = 0
      var start = 0
      var index = 0
      while index < value.length do
        value.charAt(index) match
          case '(' => round += 1
          case ')' => round -= 1
          case '[' => square += 1
          case ']' => square -= 1
          case char if char == delimiter && round == 0 && square == 0 =>
            parts += value.substring(start, index).trim
            start = index + 1
          case _ => ()
        if round < 0 || square < 0 then return Left(s"malformed rendered type `$value`.")
        index += 1
      if round != 0 || square != 0 then Left(s"malformed rendered type `$value`.")
      else
        parts += value.substring(start).trim
        val result = parts.result()
        if result.exists(_.isEmpty) then Left(s"malformed rendered type `$value`.") else Right(result)

    private def collectParsed[A](values: List[Either[String, A]]): Either[String, List[A]] =
      values.foldRight[Either[String, List[A]]](Right(Nil)) { (value, accumulated) =>
        for
          head <- value
          tail <- accumulated
        yield head :: tail
      }

  private def collect[A](
      values: List[Either[TermBindingFailure, A]]
  ): Either[TermBindingFailure, Vector[A]] =
    values.foldLeft[Either[TermBindingFailure, Vector[A]]](Right(Vector.empty)) {
      (result, value) =>
        for
          accumulated <- result
          next <- value
        yield accumulated :+ next
    }
