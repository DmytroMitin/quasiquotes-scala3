package quasiquotes.matching

import scala.quoted.*

import quasiquotes.construct.SelectedMemberName

import Q073InfixSelectedNameProbe.q073qq
import QuasiPattern.qq

object Q073InfixSelectedNameConsumers:
  inline def selectedPlus(
      inline left: Q073Operand,
      inline right: Q073Operand
  ): String = ${ selectedPlusImpl('left, 'right, explicit = false) }

  inline def explicitPlus(
      inline left: Q073Operand,
      inline right: Q073Operand
  ): String = ${ selectedPlusImpl('left, 'right, explicit = true) }

  inline def selectedMinus(
      inline left: Q073Operand,
      inline right: Q073Operand
  ): String = ${ selectedMinusImpl('left, 'right) }

  inline def publicDynamicInfixMatches(
      inline left: Q073Operand,
      inline right: Q073Operand
  ): Boolean = ${ publicDynamicInfixMatchesImpl('left, 'right) }

  inline def publicFixedInfixMatches(
      inline left: Q073Operand,
      inline right: Q073Operand
  ): Boolean = ${ publicFixedInfixMatchesImpl('left, 'right) }

  inline def directSelectedComparator(
      inline left: Q073Operand,
      inline right: Q073Operand
  ): String = ${ directSelectedComparatorImpl('left, 'right) }

  inline def targetShapeObservation(
      inline left: Q073Operand,
      inline right: Q073Operand,
      inline context: String
  ): String = ${ targetShapeObservationImpl('left, 'right, 'context) }

  inline def strictNegativeObservation(
      inline left: Q073Operand,
      inline right: Q073Operand,
      inline context: String
  ): String = ${ strictNegativeObservationImpl('left, 'right, 'context) }

  private def selectedPlusImpl(
      left: Expr[Q073Operand],
      right: Expr[Q073Operand],
      explicit: Boolean
  )(using q: Quotes): Expr[String] =
    import q.reflect.*
    val target = stripInlined(
      if explicit then '{ $left.+($right) }.asTerm
      else '{ $left + $right }.asTerm
    )
    val (originalLeft, select, originalRight) = binaryApply(target).getOrElse(
      report.errorAndAbort("Q073 + fixture did not have Apply(Select(left,+),right) shape")
    )
    if Q073InfixSelectedNameProbe.matchSelected(target).isEmpty then
      val raw = TargetTermView.fromTerm(target).fold(_.message, _.render)
      val normalized = MatchNormalizer.normalizedView(target).fold(_.message, _.render)
      val concrete = TermPattern.Infix(
        TermPattern.Hole("q073Left"),
        "+",
        TermPattern.Hole("q073Right")
      )
      val matched = TermMatcher.matchTerm(concrete, target).fold(_.message, _.bindings.keys.mkString(","))
      report.errorAndAbort(
        s"Q073 direct model failed before extractor: raw=$raw normalized=$normalized matched=$matched"
      )
    target match
      case q073qq"$capturedLeft $operatorName $capturedRight" =>
        Expr(
          List(
            operatorName.decoded,
            capturedLeft eq stripInlined(originalLeft),
            capturedRight eq stripInlined(originalRight),
            select.symbol.exists,
            select.symbol.name == "+",
            target.tpe =:= TypeRepr.of[Q073Operand]
          ).mkString(":")
        )
      case _ =>
        report.errorAndAbort(
          "Q073 selected + bridge did not match: " + target.show(using Printer.TreeStructure)
        )

  private def selectedMinusImpl(
      left: Expr[Q073Operand],
      right: Expr[Q073Operand]
  )(using q: Quotes): Expr[String] =
    import q.reflect.*
    val target = stripInlined('{ $left - $right }.asTerm)
    target match
      case q073qq"$capturedLeft $operatorName $capturedRight" =>
        Expr(s"${operatorName.decoded}:${capturedLeft ne capturedRight}")
      case _ =>
        report.errorAndAbort(
          "Q073 selected - bridge did not match: " + target.show(using Printer.TreeStructure)
        )

  private def publicDynamicInfixMatchesImpl(
      left: Expr[Q073Operand],
      right: Expr[Q073Operand]
  )(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    stripInlined('{ $left + $right }.asTerm) match
      case qq"$capturedLeft $operatorName $capturedRight" => Expr(true)
      case _ => Expr(false)

  private def publicFixedInfixMatchesImpl(
      left: Expr[Q073Operand],
      right: Expr[Q073Operand]
  )(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    stripInlined('{ $left + $right }.asTerm) match
      case qq"$capturedLeft + $capturedRight" => Expr(true)
      case _ => Expr(false)

  private def directSelectedComparatorImpl(
      left: Expr[Q073Operand],
      right: Expr[Q073Operand]
  )(using q: Quotes): Expr[String] =
    import q.reflect.*
    val target = stripInlined('{ $left + $right }.asTerm)
    val (originalLeft, _, originalRight) = binaryApply(target).getOrElse(
      report.errorAndAbort("Q073 direct selected comparator target shape changed")
    )
    target match
      case qq"$capturedLeft.$operatorName($capturedRight)" =>
        Expr(
          s"${operatorName.decoded}:${capturedLeft eq stripInlined(originalLeft)}:" +
            s"${capturedRight eq stripInlined(originalRight)}"
        )
      case _ =>
        report.errorAndAbort(
          "accepted direct selected comparator did not match: " +
            target.show(using Printer.TreeStructure)
        )

  private def targetShapeObservationImpl(
      left: Expr[Q073Operand],
      right: Expr[Q073Operand],
      context: Expr[String]
  )(using q: Quotes): Expr[String] =
    import q.reflect.*
    val infix = stripInlined('{ $left + $right }.asTerm)
    val explicit = stripInlined('{ $left.+($right) }.asTerm)
    val word = stripInlined('{ $left combine $right }.asTerm)
    val unary = stripInlined('{ -$left }.asTerm)
    val rightAssociated = stripInlined('{ $left :: $right }.asTerm)
    val generic = stripInlined('{ $left.generic[Q073Operand]($right) }.asTerm)
    val named = stripInlined('{ $left.combine(other = $right) }.asTerm)
    val contextual = stripInlined('{ $left.contextual($right)(using $context) }.asTerm)

    val infixBinary = binaryApply(infix)
    val explicitBinary = binaryApply(explicit)
    val wordBinary = binaryApply(word)
    val unaryApplyOrSelect = unary match
      case Apply(Select(_, "unary_-"), Nil) => true
      case Select(_, "unary_-") => true
      case _ => false
    val rightAssociationUsesTemporaryBlock = rightAssociated match
      case Block(
            (_: ValDef) :: Nil,
            Apply(Select(_, "::"), _ :: Nil)
          ) => true
      case _ => false
    val genericTypeApply = generic match
      case Apply(TypeApply(Select(_, "generic"), _), _ :: Nil) => true
      case _ => false
    val namedArgument = named match
      case Apply(Select(_, "combine"), NamedArg("other", _) :: Nil) => true
      case _ => false
    val contextualClauses = contextual match
      case Apply(Apply(Select(_, "contextual"), _ :: Nil), _ :: Nil) => true
      case _ => false
    val rawViewIsApply = TargetTermView.fromTerm(infix).toOption.exists {
      case TargetTermView.Apply(TargetTermView.Select(_, "+", _), _ :: Nil, _) => true
      case _ => false
    }
    val normalizedViewIsInfix = MatchNormalizer.normalizedView(infix).toOption.exists {
      case TargetTermView.Infix(_, "+", _, _) => true
      case _ => false
    }

    Expr(
      List(
        infixBinary.exists(_._2.name == "+"),
        explicitBinary.exists(_._2.name == "+"),
        wordBinary.exists(_._2.name == "combine"),
        unaryApplyOrSelect,
        rightAssociationUsesTemporaryBlock,
        genericTypeApply,
        namedArgument,
        contextualClauses,
        rawViewIsApply,
        normalizedViewIsInfix
      ).mkString(":")
    )

  private def strictNegativeObservationImpl(
      left: Expr[Q073Operand],
      right: Expr[Q073Operand],
      context: Expr[String]
  )(using q: Quotes): Expr[String] =
    import q.reflect.*
    val word = stripInlined('{ $left combine $right }.asTerm)
    val generatedLooking = stripInlined(
      '{ $left __qq_selected_name_placeholder $right }.asTerm
    )
    val unary = stripInlined('{ -$left }.asTerm)
    val rightAssociated = stripInlined('{ $left :: $right }.asTerm)
    val generic = stripInlined('{ $left.generic[Q073Operand]($right) }.asTerm)
    val named = stripInlined('{ $left.combine(other = $right) }.asTerm)
    val contextual = stripInlined('{ $left.contextual($right)(using $context) }.asTerm)
    val many = stripInlined('{ $left.many($right, $left) }.asTerm)
    val localP3 = stripInlined('{
      {
        def local(value: Q073Operand): Q073Operand = value
        local($left)
      }
    }.asTerm)
    val fixedPlusRejectsMinus =
      val minus = stripInlined('{ $left - $right }.asTerm)
      QuasiPattern.term("$q073Left + $q073Right").toOption.exists(
        _.matchTerm(minus).isLeft
      )
    val nullRejected =
      Q073InfixSelectedNameProbe
        .matchSelected(null.asInstanceOf[q.reflect.Term])
        .isEmpty

    Expr(
      List(
        Q073InfixSelectedNameProbe.matchSelected(word).isEmpty,
        Q073InfixSelectedNameProbe.matchSelected(generatedLooking).isEmpty,
        Q073InfixSelectedNameProbe.matchSelected(unary).isEmpty,
        Q073InfixSelectedNameProbe.matchSelected(rightAssociated).isEmpty,
        Q073InfixSelectedNameProbe.matchSelected(generic).isEmpty,
        Q073InfixSelectedNameProbe.matchSelected(named).isEmpty,
        Q073InfixSelectedNameProbe.matchSelected(contextual).isEmpty,
        Q073InfixSelectedNameProbe.matchSelected(many).isEmpty,
        Q073InfixSelectedNameProbe.matchSelected(localP3).isEmpty,
        fixedPlusRejectsMinus,
        nullRejected,
        SelectedMemberName.from("naïve").isLeft,
        SelectedMemberName.from("__qq_selected_name_placeholder").isRight
      ).mkString(":")
    )

  private def binaryApply(using q: Quotes)(
      term: q.reflect.Term
  ): Option[(q.reflect.Term, q.reflect.Select, q.reflect.Term)] =
    import q.reflect.*
    term match
      case Apply(select @ Select(receiver, _), argument :: Nil) =>
        Some((receiver, select, argument))
      case _ => None

  private def stripInlined(using q: Quotes)(term: q.reflect.Term): q.reflect.Term =
    import q.reflect.*
    term match
      case Inlined(_, Nil, body) => stripInlined(body)
      case other => other
