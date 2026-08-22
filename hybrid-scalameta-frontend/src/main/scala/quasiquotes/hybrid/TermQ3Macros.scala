package quasiquotes.hybrid

import scala.quoted.*

import quasiquotes.construct.*
import quasiquotes.construct.hybrid.ScalametaTermFrontend
import quasiquotes.matching.{QuasiPattern, TargetTermView, TermMatcher}
import quasiquotes.matching.hybrid.ScalametaPatternFrontend
import quasiquotes.types.{ConstructedType, TypeNormalForm}

/** Compile-time evidence for the unpublished side-by-side term tranche. */
object TermQ3Macros:
  inline def constructionEvidence: List[String] = ${ constructionEvidenceImpl }
  inline def identifierValue: Int = ${ identifierValueImpl }
  inline def matchingEvidence: List[String] = ${ matchingEvidenceImpl }
  inline def currentEngineEvidence: (Int, (Int, Int)) = ${ currentEngineEvidenceImpl }

  private def orDie[E, A](value: Either[E, A]): A =
    value.fold(error => throw new IllegalArgumentException(error.toString), identity)

  private def constructionEvidenceImpl(using q: Quotes): Expr[List[String]] =
    import q.reflect.*

    def current(parts: Seq[String], arguments: Seq[Term | QuasiTypeSplice] = Nil): Term =
      orDie(QuasiquoteBuilder.build(parts, arguments))

    def candidate(parts: Seq[String], arguments: Seq[Term | QuasiTypeSplice] = Nil): Term =
      orDie(ScalametaTermFrontend.lower(parts, arguments))

    def render(term: Term): String =
      orDie(TargetTermView.fromTerm(term)).render

    val literalCurrent = current(Seq("42"))
    val literalCandidate = candidate(Seq("42"))

    val ordinaryHole = '{ 7 }.asTerm
    val loweredHole = candidate(Seq("", ""), Seq(ordinaryHole))
    val ordinaryOriginal = loweredHole.asInstanceOf[AnyRef] eq ordinaryHole.asInstanceOf[AnyRef]

    val first = '{ 1 }.asTerm
    val second = '{ 2 }.asTerm
    val ternary = '{ (marker: String, left: Int, right: Int) => left + right }.asTerm
    val ternaryApply = Select.unique(ternary, "apply")
    val collisionApplication = candidate(
      Seq("", "(\"__qq_term_hole_0\", ", ", ", ")"),
      Seq(ternaryApply, first, second)
    )
    val collisionArguments = collisionApplication match
      case Apply(_, arguments) => arguments
      case other => throw new IllegalArgumentException(s"expected application, obtained ${other.show(using Printer.TreeStructure)}")
    val firstOriginal = collisionArguments(1).asInstanceOf[AnyRef] eq first.asInstanceOf[AnyRef]
    val secondOriginal = collisionArguments(2).asInstanceOf[AnyRef] eq second.asInstanceOf[AnyRef]

    val selectableFunction = '{ (value: Int) => value + 1 }.asTerm
    val index = '{ 1 }.asTerm
    val selectionCurrent = current(Seq("", ".apply(", ")"), Seq(selectableFunction, index))
    val selectionCandidate = candidate(Seq("", ".apply(", ")"), Seq(selectableFunction, index))
    val selectionEqual = render(selectionCurrent) == render(selectionCandidate)

    val unary = '{ (value: Int) => value + 1 }.asTerm
    val unaryApply = Select.unique(unary, "apply")
    val nestedCurrent = current(
      Seq("", "(", "(", "))"),
      Seq(unaryApply, unaryApply, ordinaryHole)
    )
    val nestedCandidate = candidate(
      Seq("", "(", "(", "))"),
      Seq(unaryApply, unaryApply, ordinaryHole)
    )
    val nestedEqual = render(nestedCurrent) == render(nestedCandidate)

    val constructedInt = ConstructedType(TypeNormalForm.STypeIdent("Int"))
    val typeSplice = QuasiTypeSplices.typeSplice(constructedInt)
    val typedCurrent = current(Seq("(", ": ", ")"), Seq(ordinaryHole, typeSplice))
    val typedCandidate = candidate(Seq("(", ": ", ")"), Seq(ordinaryHole, typeSplice))
    val typeSpliceEqual = render(typedCurrent) == render(typedCandidate)

    Expr.ofList(
      List(
        s"literal current=${render(literalCurrent)} scalameta=${render(literalCandidate)}",
        s"ordinary-hole-original=$ordinaryOriginal",
        s"multiple-holes-original=$firstOriginal,$secondOriginal",
        s"selection-application-equal=$selectionEqual",
        s"nested-equal=$nestedEqual",
        s"constructed-type-splice-equal=$typeSpliceEqual"
      ).map(Expr(_))
    )

  private def identifierValueImpl(using q: Quotes): Expr[Int] =
    ScalametaTermFrontend
      .lower(Seq("namedValue"), Nil)
      .fold(failure => throw new IllegalArgumentException(failure.message), identity)
      .asExprOf[Int]

  private def matchingEvidenceImpl(using q: Quotes): Expr[List[String]] =
    import q.reflect.*

    def compile(source: String) = orDie(ScalametaPatternFrontend.compile(source))
    def matched(source: String, target: Term) = orDie(TermMatcher.matchTerm(compile(source), target))

    val oneTarget = '{ 42 }.asTerm
    val oneOriginal = orDie(TargetTermView.fromTerm(oneTarget)).original
    val oneCapture = matched("$value", oneTarget).bindings("value")
    val oneIdentity = oneCapture.asInstanceOf[AnyRef] eq oneOriginal.asInstanceOf[AnyRef]

    val first = Literal(IntConstant(1))
    val second = Literal(IntConstant(2))
    val pairTarget = Expr.ofTupleFromSeq(List(first.asExpr, second.asExpr)).asTerm
    val pairView = orDie(TargetTermView.fromTerm(pairTarget)) match
      case TargetTermView.Tuple(elements, _) => elements.map(_.original)
      case other => throw new IllegalArgumentException(s"expected tuple, obtained ${other.render}")
    val pairMatch = matched("($left, $right)", pairTarget)
    val leftIdentity = pairMatch.bindings("left").asInstanceOf[AnyRef] eq pairView.head.asInstanceOf[AnyRef]
    val rightIdentity = pairMatch.bindings("right").asInstanceOf[AnyRef] eq pairView(1).asInstanceOf[AnyRef]

    val receiver = '{ (value: Int) => value + 1 }.asTerm
    val index = Literal(IntConstant(1))
    val selectedApplication = Select.unique(receiver, "apply").appliedTo(index)
    val selectionApplication = TermMatcher
      .matchTerm(compile("$receiver.apply($index)"), selectedApplication)
      .isRight

    val nestedTarget = Expr.ofTupleFromSeq(
      List(first.asExpr, Expr.ofTupleFromSeq(List(second.asExpr, oneTarget.asExpr)).asTerm.asExpr)
    ).asTerm
    val nestedView = orDie(TargetTermView.fromTerm(nestedTarget)) match
      case TargetTermView.Tuple(_ :: TargetTermView.Tuple(elements, _) :: Nil, _) => elements
      case other => throw new IllegalArgumentException(s"expected nested tuple, obtained ${other.render}")
    val nestedCapture = matched("($left, ($middle, $nested))", nestedTarget).bindings("nested")
    val nestedIdentity = nestedCapture.asInstanceOf[AnyRef] eq nestedView(1).original.asInstanceOf[AnyRef]

    val mismatch = TermMatcher.matchTerm(compile("42"), first).isLeft

    val independentTarget = '{ 20 + 22 }.asTerm
    val independentOriginal = orDie(TargetTermView.fromTerm(independentTarget)).original
    val independentCapture = matched("$whole", independentTarget).bindings("whole")
    val independentIdentity = independentCapture.asInstanceOf[AnyRef] eq independentOriginal.asInstanceOf[AnyRef]

    Expr.ofList(
      List(
        s"one-capture-original=$oneIdentity",
        s"ordered-original=$leftIdentity,$rightIdentity",
        s"selection-application=$selectionApplication",
        s"nested-original=$nestedIdentity",
        s"ordinary-mismatch=$mismatch",
        s"independent-target-original=$independentIdentity"
      ).map(Expr(_))
    )

  private def currentEngineEvidenceImpl(using q: Quotes): Expr[(Int, (Int, Int))] =
    import q.reflect.*
    import Quasiquotes.*
    import QuasiPattern.*

    val constructed = qr"42".asExprOf[Int]
    val target = '{ 20 + 22 }.asTerm
    val captures = target match
      case qq"$left + $right" =>
        '{ (${left.asExprOf[Int]}, ${right.asExprOf[Int]}) }
      case _ => '{ (-1, -1) }
    '{ ($constructed, $captures) }
