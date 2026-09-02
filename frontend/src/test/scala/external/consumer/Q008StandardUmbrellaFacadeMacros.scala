package external.consumer

import scala.quoted.*

final case class Q008StandardUmbrellaEvidence(
    scalarConstruction: Int,
    sequenceConstruction: List[Int],
    scalarCapture: (Int, Int),
    rankedApplyCapture: List[Int],
    rankedNewCapture: List[Int],
    typeConstruction: Boolean,
    typeCaptureIdentity: Boolean,
    definitionResult: Int,
    definitionOwnerBinderAndTypeIdentity: Boolean,
    definitionCaptureIdentity: Boolean,
    scalarRankedFallthrough: Boolean,
    definitionFallthrough: Boolean
)

object Q008StandardUmbrellaTargets:
  def three(first: Int, second: Int, third: Int): List[Int] =
    List(first, second, third)

final class Q008StandardUmbrellaConstructor(
    val first: Int,
    val second: Int,
    val third: Int
)

object Q008StandardUmbrellaFacadeMacros:
  inline def evidence(inline value: Int): Q008StandardUmbrellaEvidence =
    ${ evidenceImpl('value) }

  inline def legacyImportsRemainUsable: Boolean =
    ${ legacyImportsRemainUsableImpl }

  private def evidenceImpl(
      value: Expr[Int]
  )(using q: Quotes): Expr[Q008StandardUmbrellaEvidence] =
    import q.reflect.*
    import quasiquotes.Quasiquotes.{dqq, dqr, qq, qr, tqq, tqr}
    import quasiquotes.construct.TermSequenceSplices.termSplice

    val scalarConstruction = qr"20 + 22"

    val sequenceArguments = Seq(Expr(1).asTerm, Expr(2).asTerm, Expr(3).asTerm)
    val sequenceCarrier = termSplice(sequenceArguments)
    val sequenceConstruction =
      qr"external.consumer.Q008StandardUmbrellaTargets.three(..$sequenceCarrier)"

    val scalarCapture = '{ 20 + 22 }.asTerm match
      case qq"$left + $right" =>
        val _: q.reflect.Term = left
        val _: q.reflect.Term = right
        (left, right)
      case _ => report.errorAndAbort("Q008 standard umbrella scalar qq did not match")

    val rankedApplyCapture =
      '{ external.consumer.Q008StandardUmbrellaTargets.three(1, 2, 3) }.asTerm match
        case qq"$function(..$arguments)" =>
          val _: q.reflect.Term = function
          val _: Seq[q.reflect.Term] = arguments
          arguments
        case _ => report.errorAndAbort("Q008 standard umbrella ranked Apply qq did not match")

    val rankedNewCapture =
      '{ new external.consumer.Q008StandardUmbrellaConstructor(1, 2, 3) }.asTerm match
        case qq"new external.consumer.Q008StandardUmbrellaConstructor(..$arguments)" =>
          val _: Seq[q.reflect.Term] = arguments
          arguments
        case _ => report.errorAndAbort("Q008 standard umbrella ranked New qq did not match")

    val intType = TypeRepr.of[Int]
    val stringType = TypeRepr.of[String]
    val constructedType: q.reflect.TypeRepr = tqr"Either[$intType, $stringType]"
    val typeConstruction = constructedType =:= TypeRepr.of[Either[Int, String]]

    val targetType = TypeRepr.of[Either[List[Int], Option[String]]]
    val expectedTypeChildren = targetType match
      case AppliedType(_, left :: right :: Nil) => (left, right)
      case _ => report.errorAndAbort("Q008 standard umbrella unexpected Type target")
    val typeCaptureIdentity = targetType match
      case tqq"Either[$left, $right]" =>
        val _: q.reflect.TypeRepr = left
        val _: q.reflect.TypeRepr = right
        sameReference(left, expectedTypeChildren._1) &&
          sameReference(right, expectedTypeChildren._2)
      case _ => false

    val definition =
      dqr"def umbrellaIdentity(value: $intType): $intType = value"
    val parameter = definition.paramss.head.asInstanceOf[TermParamClause].params.head
    val definitionBody = definition.rhs.get
    val definitionOwnerBinderAndTypeIdentity =
      definition.symbol.owner == Symbol.spliceOwner &&
        parameter.symbol.owner == definition.symbol &&
        sameReference(parameter.tpt.tpe, intType) &&
        sameReference(definition.returnTpt.tpe, intType) &&
        (definitionBody match
          case reference: Ref => reference.symbol == parameter.symbol
          case _ => false)

    val definitionCaptureIdentity = definition match
      case dqq"def umbrellaIdentity(value: Int): Int = $body" =>
        val _: q.reflect.Term = body
        body.asInstanceOf[AnyRef] eq definitionBody.asInstanceOf[AnyRef]
      case _ => false

    val scalarMismatch =
      '{ external.consumer.Q008StandardUmbrellaTargets.three(1, 2, 3) }.asTerm match
        case qq"$function($only)" => false
        case _ => true
    val rankedMismatch = Expr(1).asTerm match
      case qq"$function(..$arguments)" => false
      case _ => true

    val definitionFallthrough = definition match
      case dqq"def other(value: Int): Int = $body" => false
      case _ => true

    val definitionResult =
      Block(
        List(definition),
        Apply(Ref(definition.symbol), List(value.asTerm))
      ).asExprOf[Int]

    '{
      Q008StandardUmbrellaEvidence(
        ${ scalarConstruction.asExprOf[Int] },
        ${ sequenceConstruction.asExprOf[List[Int]] },
        (${ scalarCapture._1.asExprOf[Int] }, ${ scalarCapture._2.asExprOf[Int] }),
        ${ Expr.ofList(rankedApplyCapture.toList.map(_.asExprOf[Int])) },
        ${ Expr.ofList(rankedNewCapture.toList.map(_.asExprOf[Int])) },
        ${ Expr(typeConstruction) },
        ${ Expr(typeCaptureIdentity) },
        $definitionResult,
        ${ Expr(definitionOwnerBinderAndTypeIdentity) },
        ${ Expr(definitionCaptureIdentity) },
        ${ Expr(scalarMismatch && rankedMismatch) },
        ${ Expr(definitionFallthrough) }
      )
    }

  private def legacyImportsRemainUsableImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import quasiquotes.construct.Quasiquotes.*
    import quasiquotes.matching.QuasiPattern.*
    import quasiquotes.types.QuasiTypequotes.*
    import quasiquotes.matching.DefinitionPattern.*

    val term: q.reflect.Term = qr"1 + 2"
    val termMatched = term match
      case qq"$left + $right" =>
        val _: q.reflect.Term = left
        val _: q.reflect.Term = right
        true
      case _ => false

    val reflectedType: q.reflect.TypeRepr = tqr"Int"
    val typeMatched = reflectedType match
      case tqq"Int" => true
      case _ => false

    val definition = dqr"def legacy(value: $reflectedType): $reflectedType = value"
    val definitionMatched = definition match
      case dqq"def legacy(value: Int): Int = $body" =>
        val _: q.reflect.Term = body
        body.asInstanceOf[AnyRef] eq definition.rhs.get.asInstanceOf[AnyRef]
      case _ => false

    Expr(termMatched && typeMatched && definitionMatched)

  private def sameReference(left: Any, right: Any): Boolean =
    left.asInstanceOf[AnyRef] eq right.asInstanceOf[AnyRef]
