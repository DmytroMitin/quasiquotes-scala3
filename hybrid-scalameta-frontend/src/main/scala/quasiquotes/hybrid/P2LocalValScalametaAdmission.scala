package quasiquotes.hybrid

import scala.meta.*

import _root_.quasiquotes.parser.P2LocalValAdmission

private[quasiquotes] object P2LocalValScalametaAdmission:
  def validate(tree: scala.meta.Term): Either[P2LocalValAdmission.Violation, Unit] =
    val tracker = new P2LocalValAdmission.Tracker

    def sequence(terms: List[scala.meta.Term]): Either[P2LocalValAdmission.Violation, Unit] =
      terms.foldLeft[Either[P2LocalValAdmission.Violation, Unit]](Right(())) {
        (result, term) => result.flatMap(_ => loop(term))
      }

    def loop(current: scala.meta.Term): Either[P2LocalValAdmission.Violation, Unit] =
      current match
        case function: scala.meta.Term.Function =>
          function.paramClause.values match
            case parameter :: Nil =>
              tracker.withinLambda(parameter.name.value)(loop(function.body))
            case _ => loop(function.body)
        case select: scala.meta.Term.Select => loop(select.qual)
        case unary: scala.meta.Term.ApplyUnary => loop(unary.arg)
        case fresh: scala.meta.Term.New => sequence(fresh.init.argss.flatten)
        case application: scala.meta.Term.Apply =>
          loop(application.fun).flatMap(_ => sequence(application.args))
        case infix: scala.meta.Term.ApplyInfix =>
          loop(infix.lhs).flatMap(_ => sequence(infix.argClause.values))
        case tuple: scala.meta.Term.Tuple => sequence(tuple.args)
        case conditional: scala.meta.Term.If =>
          loop(conditional.cond)
            .flatMap(_ => loop(conditional.thenp))
            .flatMap(_ => loop(conditional.elsep))
        case block: scala.meta.Term.Block =>
          block.stats match
            case (definition: scala.meta.Defn.Val) :: (result: scala.meta.Term) :: Nil
                if isP2Candidate(definition) =>
              val displayName = definition.pats.head.asInstanceOf[scala.meta.Pat.Var].name.value
              tracker
                .introduceLocalVal(displayName)
                .flatMap(_ => loop(definition.rhs))
                .flatMap(_ => tracker.withinLocalValResult(displayName)(loop(result)))
            case stats =>
              sequence(stats.collect { case term: scala.meta.Term => term })
        case ascription: scala.meta.Term.Ascribe => loop(ascription.expr)
        case interpolation: scala.meta.Term.Interpolate => sequence(interpolation.args)
        case _ => Right(())

    loop(tree)

  private def isP2Candidate(definition: scala.meta.Defn.Val): Boolean =
    definition.mods.isEmpty &&
      definition.decltpe.nonEmpty &&
      (definition.pats match
        case scala.meta.Pat.Var(_) :: Nil => true
        case _ => false)
