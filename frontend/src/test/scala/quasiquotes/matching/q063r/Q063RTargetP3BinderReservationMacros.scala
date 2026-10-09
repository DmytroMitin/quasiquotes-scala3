package quasiquotes.matching.q063r

import scala.quoted.*

import quasiquotes.matching.{QuasiPattern, TargetBlockStatementView, TargetTermView}
import quasiquotes.matching.QuasiPattern.*

object Q063RTargetP3BinderReservationMacros:
  final case class CollisionEvidence(
      sourceAdmitted: Boolean,
      matched: Boolean,
      exactCaptureIdentity: Boolean,
      methodBinderId: Int,
      parameterBinderId: Int,
      lambdaBinderId: Int,
      lambdaParameterDepth: Int,
      enclosingMethodDepth: Int
  )

  inline def collisionEvidence(inline expression: Int): CollisionEvidence =
    ${ collisionEvidenceImpl('expression) }

  inline def mismatchMatches(inline expression: Int): Boolean =
    ${ mismatchMatchesImpl('expression) }

  inline def directCaptureKeepsOriginal(inline expression: Int): Boolean =
    ${ directCaptureKeepsOriginalImpl('expression) }

  private def collisionEvidenceImpl(expression: Expr[Int])(using q: Quotes): Expr[CollisionEvidence] =
    import q.reflect.*

    def unwrap(term: Term): Term =
      term match
        case Inlined(_, _, inner) => unwrap(inner)
        case q.reflect.Block(Nil, inner: Term) => unwrap(inner)
        case other => other

    def firstLambda(
        view: TargetTermView[Term]
    ): Option[TargetTermView.Lambda1[Term]] =
      view match
        case lambda @ TargetTermView.Lambda1(_, _, _, _, _, _) => Some(lambda.asInstanceOf[TargetTermView.Lambda1[Term]])
        case TargetTermView.Select(qualifier, _, _) => firstLambda(qualifier)
        case TargetTermView.Apply(function, arguments, _) =>
          firstLambda(function).orElse(arguments.view.flatMap(firstLambda).headOption)
        case TargetTermView.New(_, arguments, _) =>
          arguments.view.flatMap(firstLambda).headOption
        case TargetTermView.Infix(left, _, right, _) =>
          firstLambda(left).orElse(firstLambda(right))
        case TargetTermView.Unary(_, operand, _) => firstLambda(operand)
        case TargetTermView.InterpolatedString(_, _, arguments, _) =>
          arguments.view.flatMap(firstLambda).headOption
        case TargetTermView.Typed(inner, _, _) => firstLambda(inner)
        case TargetTermView.Tuple(elements, _) =>
          elements.view.flatMap(firstLambda).headOption
        case TargetTermView.If(condition, thenBranch, elseBranch, _) =>
          firstLambda(condition).orElse(firstLambda(thenBranch)).orElse(firstLambda(elseBranch))
        case TargetTermView.Block(statements, result, _) =>
          statements.view
            .flatMap {
              case TargetBlockStatementView.LocalVal(_, _, _, _, initializer, _) =>
                firstLambda(initializer)
              case TargetBlockStatementView.LocalDef(_, _, _, _, _, _, _, _, body, _) =>
                firstLambda(body)
              case term => firstLambda(term.asInstanceOf[TargetTermView[Term]])
            }
            .headOption
            .orElse(firstLambda(result))
        case _ => None

    def boundReferences(
        view: TargetTermView[Term]
    ): List[TargetTermView.BoundReference[Term]] =
      view match
        case reference @ TargetTermView.BoundReference(_, _, _) => List(reference.asInstanceOf[TargetTermView.BoundReference[Term]])
        case TargetTermView.Lambda1(_, _, _, _, body, _) => boundReferences(body)
        case TargetTermView.Select(qualifier, _, _) => boundReferences(qualifier)
        case TargetTermView.Apply(function, arguments, _) =>
          boundReferences(function) ++ arguments.flatMap(boundReferences)
        case TargetTermView.New(_, arguments, _) => arguments.flatMap(boundReferences)
        case TargetTermView.Infix(left, _, right, _) =>
          boundReferences(left) ++ boundReferences(right)
        case TargetTermView.Unary(_, operand, _) => boundReferences(operand)
        case TargetTermView.InterpolatedString(_, _, arguments, _) =>
          arguments.flatMap(boundReferences)
        case TargetTermView.Typed(inner, _, _) => boundReferences(inner)
        case TargetTermView.Tuple(elements, _) => elements.flatMap(boundReferences)
        case TargetTermView.If(condition, thenBranch, elseBranch, _) =>
          boundReferences(condition) ++ boundReferences(thenBranch) ++ boundReferences(elseBranch)
        case TargetTermView.Block(statements, result, _) =>
          statements.flatMap {
            case TargetBlockStatementView.LocalVal(_, _, _, _, initializer, _) =>
              boundReferences(initializer)
            case TargetBlockStatementView.LocalDef(_, _, _, _, _, _, _, _, body, _) =>
              boundReferences(body)
            case term => boundReferences(term.asInstanceOf[TargetTermView[Term]])
          } ++ boundReferences(result)
        case _ => Nil

    val target = unwrap(expression.asTerm)
    val compiledPattern = QuasiPattern.term(
      "{ def id(value: Int): Int = value; id((((n: Int) => id(n)), $captured)._2) }"
    )
    val matched = compiledPattern.flatMap(_.matchTerm(target)).isRight

    val captured = target match
      case qq"""{
        def id(value: Int): Int = value
        id((((n: Int) => id(n)), $captured)._2)
      }""" => Some(captured)
      case _ => None

    var rawCaptured: Option[Term] = None
    val captureFinder = new TreeTraverser:
      override def traverseTree(tree: Tree)(owner: Symbol): Unit =
        tree match
          case literal @ q.reflect.Literal(IntConstant(41)) if rawCaptured.isEmpty =>
            rawCaptured = Some(literal)
          case _ if rawCaptured.isEmpty => super.traverseTree(tree)(owner)
          case _ => ()
    captureFinder.traverseTree(target)(Symbol.spliceOwner)
    val exactCaptureIdentity =
      (captured, rawCaptured) match
        case (Some(actual), Some(expected)) =>
          actual.asInstanceOf[AnyRef].eq(expected.asInstanceOf[AnyRef])
        case _ => false

    val viewEvidence = TargetTermView.fromTerm(target).toOption.flatMap {
      case TargetTermView.Block(
            List(
              TargetBlockStatementView.LocalDef(
                methodBinderId,
                _,
                methodSymbol,
                parameterBinderId,
                _,
                _,
                _,
                _,
                _,
                _
              )
            ),
            result,
            _
          ) =>
        firstLambda(result).map { lambda =>
          val scope = List(lambda.binderId, methodBinderId)
          val references = boundReferences(lambda.body)
          val lambdaReference = references.find(_.original.symbol == lambda.binderSymbol)
          val methodReference = references.find(_.original.symbol == methodSymbol)
          (
            methodBinderId.value,
            parameterBinderId.value,
            lambda.binderId.value,
            lambdaReference.map(reference => scope.indexOf(reference.binderId)).getOrElse(-1),
            methodReference.map(reference => scope.indexOf(reference.binderId)).getOrElse(-1)
          )
        }
      case _ => None
    }

    val (methodId, parameterId, lambdaId, lambdaDepth, methodDepth) =
      viewEvidence.getOrElse((-1, -1, -1, -1, -1))

    '{
      CollisionEvidence(
        ${ Expr(compiledPattern.isRight) },
        ${ Expr(matched) },
        ${ Expr(exactCaptureIdentity) },
        ${ Expr(methodId) },
        ${ Expr(parameterId) },
        ${ Expr(lambdaId) },
        ${ Expr(lambdaDepth) },
        ${ Expr(methodDepth) }
      )
    }

  private def mismatchMatchesImpl(expression: Expr[Int])(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    val matched = expression.asTerm match
      case qq"""{
        def id(value: Int): Int = value
        id((((n: Int) => id(n)), $captured)._2)
      }""" => true
      case _ => false
    Expr(matched)

  private def directCaptureKeepsOriginalImpl(expression: Expr[Int])(using q: Quotes): Expr[Boolean] =
    import q.reflect.*

    def unwrap(term: Term): Term =
      term match
        case Inlined(_, _, inner) => unwrap(inner)
        case q.reflect.Block(Nil, inner: Term) => unwrap(inner)
        case other => other

    val target = unwrap(expression.asTerm)
    val rawArgument = target match
      case q.reflect.Block(List(_: DefDef), q.reflect.Apply(_, List(argument))) => Some(argument)
      case _ => None
    val captured = target match
      case qq"""{ def id(value: Int): Int = value; id($argument) }""" => Some(argument)
      case _ => None
    Expr(
      (captured, rawArgument) match
        case (Some(actual), Some(expected)) =>
          actual.asInstanceOf[AnyRef].eq(expected.asInstanceOf[AnyRef])
        case _ => false
    )
