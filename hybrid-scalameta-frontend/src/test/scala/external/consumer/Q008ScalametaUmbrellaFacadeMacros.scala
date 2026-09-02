package external.consumer

import scala.quoted.*

final case class Q008ScalametaUmbrellaEvidence(
    termConstructionIdentity: Boolean,
    termConstructionResult: Int,
    orderedTermCaptures: (Int, Int),
    typeConstruction: Boolean,
    typeCaptureIdentity: Boolean,
    definitionResult: Int,
    definitionOwnerBinderAndTypeIdentity: Boolean,
    definitionCaptureIdentity: Boolean,
    mismatchFallthrough: Boolean
)

object Q008ScalametaUmbrellaFacadeMacros:
  inline def evidence(inline value: Int): Q008ScalametaUmbrellaEvidence =
    ${ evidenceImpl('value) }

  inline def legacyImportsRemainUsable: Boolean =
    ${ legacyImportsRemainUsableImpl }

  private def evidenceImpl(
      value: Expr[Int]
  )(using q: Quotes): Expr[Q008ScalametaUmbrellaEvidence] =
    import q.reflect.*
    import quasiquotes.scalameta.Quasiquotes.{dqq, dqr, qq, qr, tqq, tqr}

    val termHole = Expr(42).asTerm
    val constructedTerm: q.reflect.Term = qr"$termHole"
    val termConstructionIdentity = sameReference(constructedTerm, termHole)

    val orderedTermCaptures = '{ 20 + 22 }.asTerm match
      case qq"$left + $right" =>
        val _: q.reflect.Term = left
        val _: q.reflect.Term = right
        (left, right)
      case _ => report.errorAndAbort("Q008 Scalameta umbrella qq did not match")

    val intType = TypeRepr.of[Int]
    val stringType = TypeRepr.of[String]
    val constructedType: q.reflect.TypeRepr = tqr"Either[$intType, $stringType]"
    val typeConstruction = constructedType =:= TypeRepr.of[Either[Int, String]]

    val targetType = TypeRepr.of[Either[List[Int], Option[String]]]
    val expectedTypeChildren = targetType match
      case AppliedType(_, left :: right :: Nil) => (left, right)
      case _ => report.errorAndAbort("Q008 Scalameta umbrella unexpected Type target")
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
        sameReference(body, definitionBody)
      case _ => false

    val termMismatch = Expr(1).asTerm match
      case qq"$left + $right" => false
      case _ => true
    val definitionMismatch = definition match
      case dqq"def other(value: Int): Int = $body" => false
      case _ => true

    val definitionResult =
      Block(
        List(definition),
        Apply(Ref(definition.symbol), List(value.asTerm))
      ).asExprOf[Int]

    '{
      Q008ScalametaUmbrellaEvidence(
        ${ Expr(termConstructionIdentity) },
        ${ constructedTerm.asExprOf[Int] },
        (${ orderedTermCaptures._1.asExprOf[Int] }, ${ orderedTermCaptures._2.asExprOf[Int] }),
        ${ Expr(typeConstruction) },
        ${ Expr(typeCaptureIdentity) },
        $definitionResult,
        ${ Expr(definitionOwnerBinderAndTypeIdentity) },
        ${ Expr(definitionCaptureIdentity) },
        ${ Expr(termMismatch && definitionMismatch) }
      )
    }

  private def legacyImportsRemainUsableImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import quasiquotes.scalameta.ScalametaQuasiquotes.*
    import quasiquotes.scalameta.ScalametaQuasiPattern.*

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
        sameReference(body, definition.rhs.get)
      case _ => false

    Expr(termMatched && typeMatched && definitionMatched)

  private def sameReference(left: Any, right: Any): Boolean =
    left.asInstanceOf[AnyRef] eq right.asInstanceOf[AnyRef]
