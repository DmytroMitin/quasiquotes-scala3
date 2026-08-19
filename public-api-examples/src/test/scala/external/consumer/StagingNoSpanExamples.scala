package external.consumer

import scala.quoted.{Expr, Quotes, staging}

import quasiquotes.construct.Quasiquotes.qr

object StagingNoSpanExamples:
  final case class PositionEvidence(
      sourceCode: Either[String, Option[String]],
      start: Either[String, Int],
      end: Either[String, Int]
  )

  inline def add(left: Int, right: Int): Int =
    ${ addImpl('left, 'right) }

  def addImpl(
      left: Expr[Int],
      right: Expr[Int]
  )(using quotes: Quotes): Expr[Int] =
    import quotes.reflect.*
    addImplTerm(left.asTerm, right.asTerm).asExprOf[Int]

  def addImplTerm(using q: Quotes)(
      left: q.reflect.Term,
      right: q.reflect.Term
  ): q.reflect.Term =
    import quasiquotes.matching.QuasiPattern.qq

    val tree = qr"$left + $right"
    tree match
      case qq"$capturedLeft + $capturedRight" =>
        val (targetLeft, targetRight) = additionOperands(tree)
        assertSameTree(capturedLeft, targetLeft)
        assertSameTree(capturedRight, targetRight)
        tree
      case _ =>
        throw new AssertionError("qq did not match the qr addition")

  private def additionOperands(using q: Quotes)(
      tree: q.reflect.Term
  ): (q.reflect.Term, q.reflect.Term) =
    import q.reflect.*

    tree match
      case Apply(Select(left, "+"), right :: Nil) =>
        (unwrapMatchingWrappers(left), unwrapMatchingWrappers(right))
      case other =>
        throw new AssertionError(
          s"unexpected qr addition shape: ${other.show(using Printer.TreeStructure)}"
        )

  private def unwrapMatchingWrappers(using q: Quotes)(
      tree: q.reflect.Term
  ): q.reflect.Term =
    import q.reflect.*

    tree match
      case Inlined(_, _, inner) => unwrapMatchingWrappers(inner)
      case Block(Nil, inner: Term) => unwrapMatchingWrappers(inner)
      case ident: Ident if ident.symbol.exists && ident.symbol.pos.nonEmpty =>
        ident.symbol.tree match
          case ValDef(_, _, Some(rhs)) => unwrapMatchingWrappers(rhs)
          case _ => tree
      case _ => tree

  private def assertSameTree(using q: Quotes)(
      captured: q.reflect.Term,
      target: q.reflect.Term
  ): Unit =
    if !(captured.asInstanceOf[AnyRef] eq target.asInstanceOf[AnyRef]) then
      throw new AssertionError("qq rebuilt a caller-owned target subtree")

  def inspectWithQuotes(
      left: Int,
      right: Int
  ): (String, String) =
    given staging.Compiler =
      staging.Compiler.make(getClass.getClassLoader)

    staging.withQuotes:
      val q = summon[Quotes]
      import q.reflect.*
      import quasiquotes.matching.QuasiPattern.qq

      val leftTerm = Expr(left).asTerm
      val rightTerm = Expr(right).asTerm
      val tree = addImplTerm(using q)(leftTerm, rightTerm)
      tree match
        case qq"$capturedLeft + $capturedRight" =>
          (capturedLeft.show, capturedRight.show)
        case _ =>
          throw new AssertionError("qq did not match inside staging.withQuotes")

  def runAdd(left: Int, right: Int): Int =
    given staging.Compiler =
      staging.Compiler.make(getClass.getClassLoader)

    staging.run:
      addImpl(Expr(left), Expr(right))

  def sourceFreePositionEvidence(value: Int): PositionEvidence =
    given staging.Compiler =
      staging.Compiler.make(getClass.getClassLoader)

    staging.withQuotes:
      val q = summon[Quotes]
      import q.reflect.*

      val position = Expr(value).asTerm.pos
      PositionEvidence(
        assertionOutcome(position.sourceCode),
        assertionOutcome(position.start),
        assertionOutcome(position.end)
      )

  private def assertionOutcome[A](value: => A): Either[String, A] =
    try Right(value)
    catch
      case error: AssertionError =>
        Left(Option(error.getMessage).getOrElse("<no assertion message>"))
