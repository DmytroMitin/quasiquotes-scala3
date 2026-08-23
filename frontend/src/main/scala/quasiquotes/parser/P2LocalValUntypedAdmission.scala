package quasiquotes.parser

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Flags

private[quasiquotes] object P2LocalValUntypedAdmission:
  def validate(tree: untpd.Tree): Either[P2LocalValAdmission.Violation, Unit] =
    val tracker = new P2LocalValAdmission.Tracker

    def sequence(trees: List[untpd.Tree]): Either[P2LocalValAdmission.Violation, Unit] =
      trees.foldLeft[Either[P2LocalValAdmission.Violation, Unit]](Right(())) {
        (result, tree) => result.flatMap(_ => loop(tree))
      }

    def loop(current: untpd.Tree): Either[P2LocalValAdmission.Violation, Unit] =
      current match
        case untpd.Function((parameter: untpd.ValDef) :: Nil, body) =>
          tracker.withinLambda(parameter.name.toString)(loop(body))
        case untpd.Function(_, body) => loop(body)
        case untpd.Select(qualifier, _) => loop(qualifier)
        case untpd.Apply(function, arguments) => loop(function).flatMap(_ => sequence(arguments))
        case untpd.InfixOp(left, _, right) => loop(left).flatMap(_ => loop(right))
        case untpd.PrefixOp(_, operand) => loop(operand)
        case untpd.InterpolatedString(_, segments) => sequence(segments)
        case untpd.Typed(expression, _) => loop(expression)
        case untpd.Tuple(elements) => sequence(elements)
        case untpd.If(condition, thenBranch, elseBranch) =>
          loop(condition).flatMap(_ => loop(thenBranch)).flatMap(_ => loop(elseBranch))
        case untpd.Block((value: untpd.ValDef) :: Nil, result) if isP2Candidate(value) =>
          val displayName = value.name.toString
          tracker
            .introduceLocalVal(displayName)
            .flatMap(_ => loop(value.unforcedRhs.asInstanceOf[untpd.Tree]))
            .flatMap(_ => tracker.withinLocalValResult(displayName)(loop(result)))
        case untpd.Block(statements, result) => sequence(statements).flatMap(_ => loop(result))
        case untpd.Parens(inner) => loop(inner)
        case untpd.TypedSplice(inner) => loop(inner)
        case _ => Right(())

    loop(tree)

  private def isP2Candidate(value: untpd.ValDef): Boolean =
    val name = value.name.toString
    !value.mods.is(Flags.Mutable) &&
      !value.mods.is(Flags.Lazy) &&
      !value.tpt.isEmpty &&
      name != "_" &&
      name.matches("[A-Za-z_$][A-Za-z0-9_$]*")
