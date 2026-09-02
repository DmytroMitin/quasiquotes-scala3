package quasiquotes.q012r

import scala.quoted.*

import quasiquotes.matching.{
  DefinitionPattern,
  DefinitionPatternExtractor,
  SingleParameterDefinitionMatch,
  SingleParameterDefinitionPattern
}

object Q012RSameSpellingDefinitionProbe:
  inline def verify: Boolean = ${ verifyImpl }

  private def verifyImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*

    def target(name: String, resultType: TypeRepr, selectSecond: Boolean): DefDef =
      val methodType = MethodType(List("left", "right"))(
        _ => List(TypeRepr.of[Int], TypeRepr.of[String]),
        _ => resultType
      )
      val symbol = Symbol.newMethod(Symbol.spliceOwner, name, methodType)
      DefDef(symbol, clauses =>
        clauses match
          case List(List(first, second)) =>
            Some(Ref(if selectSecond then second.symbol else first.symbol))
          case _ => report.errorAndAbort("Q012R fixture lost its parameters")
      )

    val single: SingleParameterDefinitionPattern = DefinitionPattern.dqq(
      StringContext("def identity(value: Int): Int = ", "")
    )(using q)
    val _: q.reflect.DefDef => Option[
      SingleParameterDefinitionMatch[q.reflect.TypeRepr, q.reflect.Term]
    ] = single.matchDefinition(using q)

    val exactTwo: DefinitionPatternExtractor = DefinitionPattern.dqq(
      StringContext("def first(left: Int, right: String): Int = ", "")
    )(using q)
    val first = target("first", TypeRepr.of[Int], selectSecond = false)
    val direct = locally {
      import DefinitionPattern.dqq
      first match
        case dqq"def first(left: Int, right: String): Int = $body" =>
          val _: q.reflect.Term = body
          first.rhs.exists(_ eq body)
        case _ => false
    }

    val umbrella = locally {
      import quasiquotes.Quasiquotes.dqq
      val pattern: DefinitionPatternExtractor = dqq(
        StringContext("def second(left: Int, right: String): String = ", "")
      )(using q)
      val second = target("second", TypeRepr.of[String], selectSecond = true)
      second match
        case dqq"def second(left: Int, right: String): String = $body" =>
          val _: q.reflect.Term = body
          second.rhs.exists(_ eq body) && pattern.unapply(second).nonEmpty
        case _ => false
    }

    def dynamic(context: StringContext): SingleParameterDefinitionPattern =
      DefinitionPattern.dqq(context)(using q)

    val fallback = dynamic(
      StringContext("def identity(value: Int): Int = ", "")
    )

    Expr(direct && umbrella && exactTwo.unapply(first).nonEmpty && fallback != null)
