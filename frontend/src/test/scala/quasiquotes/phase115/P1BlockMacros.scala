package quasiquotes.phase115

import scala.quoted.*

import quasiquotes.construct.{QuasiquoteBuilder, Quasiquotes}
import quasiquotes.matching.{QuasiPattern, TargetTermView}

object P1BlockMacros:
  inline def constructOrdered(
      inline first: Unit,
      inline second: Unit,
      inline result: Int
  ): Int = ${ constructOrderedImpl('first, 'second, 'result) }

  inline def captureOrdered(inline expression: Int): (Int, Int, Int) =
    ${ captureOrderedImpl('expression) }

  inline def captureIdentity(inline expression: Int): Boolean =
    ${ captureIdentityImpl('expression) }

  inline def matchesThreeChildren(inline expression: Int): Boolean =
    ${ matchesThreeChildrenImpl('expression) }

  inline def programmaticEvidence: (Boolean, Boolean, Boolean) =
    ${ programmaticEvidenceImpl }

  inline def rejectionMessages: (String, String, String, String) =
    ${ rejectionMessagesImpl }

  private def constructOrderedImpl(
      first: Expr[Unit],
      second: Expr[Unit],
      result: Expr[Int]
  )(using q: Quotes): Expr[Int] =
    import q.reflect.*
    import Quasiquotes.*
    val firstTerm = first.asTerm
    val secondTerm = second.asTerm
    val resultTerm = result.asTerm
    qr"{ $firstTerm; $secondTerm; $resultTerm }".asExprOf[Int]

  private def captureOrderedImpl(expression: Expr[Int])(using q: Quotes): Expr[(Int, Int, Int)] =
    import q.reflect.*
    import QuasiPattern.*

    expression.asTerm match
      case qq"{ $first; $second; $result }" =>
        '{ (${first.asExprOf[Int]}, ${second.asExprOf[Int]}, ${result.asExprOf[Int]}) }
      case _ =>
        val diagnostic = QuasiPattern
          .termOrThrow("{ $first; $second; $result }")
          .matchTerm(expression.asTerm)
          .left
          .toOption
          .map(_.message)
          .getOrElse("qq extractor fell through after a successful programmatic match")
        q.reflect.report.errorAndAbort(diagnostic)

  private def captureIdentityImpl(expression: Expr[Int])(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import QuasiPattern.*

    def unwrap(term: Term): Term =
      term match
        case Inlined(_, _, inner) => unwrap(inner)
        case other => other

    val target = unwrap(expression.asTerm)
    val originals = target match
      case Block(prefix, result) => prefix.map(_.asInstanceOf[Term]) :+ result
      case other => report.errorAndAbort(s"expected typed block, obtained ${other.show(using Printer.TreeStructure)}")
    target match
      case qq"{ $first; $second; $result }" =>
        Expr(
          first.asInstanceOf[AnyRef].eq(originals(0).asInstanceOf[AnyRef]) &&
            second.asInstanceOf[AnyRef].eq(originals(1).asInstanceOf[AnyRef]) &&
            result.asInstanceOf[AnyRef].eq(originals(2).asInstanceOf[AnyRef])
        )
      case _ => Expr(false)

  private def matchesThreeChildrenImpl(expression: Expr[Int])(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import QuasiPattern.*

    expression.asTerm match
      case qq"{ $first; $second; $result }" => Expr(true)
      case _ => Expr(false)

  private def programmaticEvidenceImpl(using q: Quotes): Expr[(Boolean, Boolean, Boolean)] =
    import q.reflect.*

    val first = Literal(IntConstant(1))
    val same = Literal(IntConstant(1))
    val different = Literal(IntConstant(2))
    val generatedEqual = Block(List(first), same)
    val generatedDifferent = Block(List(first), different)
    val pattern = QuasiPattern.termOrThrow("{ $value; $value }")
    val equal = pattern.matchTerm(generatedEqual).isRight
    val unequal = pattern.matchTerm(generatedDifferent).isLeft
    val captured = QuasiPattern
      .termOrThrow("{ $prefix; $result }")
      .matchTerm(generatedDifferent)
      .toOption
      .get
    val identity =
      captured.bindings("prefix").asInstanceOf[AnyRef].eq(first.asInstanceOf[AnyRef]) &&
        captured.bindings("result").asInstanceOf[AnyRef].eq(different.asInstanceOf[AnyRef]) &&
        TargetTermView.fromTerm(generatedDifferent).toOption.exists(_.original.asInstanceOf[AnyRef].eq(generatedDifferent.asInstanceOf[AnyRef]))
    Expr((equal, unequal, identity))

  private def rejectionMessagesImpl(using q: Quotes): Expr[(String, String, String, String)] =
    def buildMessage(source: String): String =
      QuasiquoteBuilder.build(Seq(source), Nil).left.toOption.map(_.message).getOrElse("accepted")
    def patternMessage(source: String): String =
      QuasiPattern.termLocated(source).left.toOption.map(_.diagnostic.message).getOrElse("accepted")

    Expr(
      (
        buildMessage("{ val x = 1; x }"),
        buildMessage("{ def x = 1; x }"),
        patternMessage("{ val x = 1; x }"),
        patternMessage("{ def x = 1; x }")
      )
    )
