package quasiquotes.q011

import scala.quoted.*

import quasiquotes.matching.{
  DefinitionPattern,
  DefinitionPatternExtractor,
  SingleParameterDefinitionPattern,
}

object Q011DefinitionPatternApiTypingProbe:
  inline def verify(): Boolean = ${ verifyImpl }

  private def verifyImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*

    val intType = TypeRepr.of[Int]
    val stringType = TypeRepr.of[String]

    def target(name: String, resultType: TypeRepr, selectSecond: Boolean): DefDef =
      val methodType = MethodType(List("left", "right"))(
        _ => List(intType, stringType),
        _ => resultType
      )
      val symbol = Symbol.newMethod(Symbol.spliceOwner, name, methodType)
      DefDef(symbol, clauses =>
        clauses match
          case List(List(first, second)) =>
            Some(Ref(if selectSecond then second.symbol else first.symbol))
          case _ => report.errorAndAbort("Q011 API typing fixture lost its parameters")
      )

    locally {
      import DefinitionPattern.dqq
      val single = DefinitionPattern.dqq(
        StringContext("def identity(value: Int): Int = ", "")
      )(using q)
      val _: SingleParameterDefinitionPattern = single
      val two = DefinitionPattern.dqq(
        StringContext("def first(left: Int, right: String): Int = ", "")
      )(using q)
      val _: DefinitionPatternExtractor = two
      target("first", intType, selectSecond = false) match
        case dqq"def first(left: Int, right: String): Int = $body" =>
          val _: q.reflect.Term = body
        case _ => report.errorAndAbort("Q011 Strategy A exact-two pattern did not match")
    }

    locally {
      import quasiquotes.Quasiquotes.dqq
      val single = dqq(
        StringContext("def identity(value: Int): Int = ", "")
      )(using q)
      val _: SingleParameterDefinitionPattern = single
      val two = dqq(
        StringContext("def second(left: Int, right: String): String = ", "")
      )(using q)
      val _: DefinitionPatternExtractor = two
      target("second", stringType, selectSecond = true) match
        case dqq"def second(left: Int, right: String): String = $body" =>
          val _: q.reflect.Term = body
        case _ => report.errorAndAbort("Q011 production same-spelling exact-two pattern did not match")
    }

    Expr(true)
