package quasiquotes.q012r

import scala.language.experimental.erasedDefinitions
import scala.quoted.*

import quasiquotes.matching.{DefinitionPattern, TwoParameterDefinitionPattern}

object Q012RDefinitionEvidence:
  inline def construction: List[(String, Boolean, Boolean, Boolean, Boolean, Boolean)] =
    ${ constructionImpl }

  inline def mismatches: List[(String, Boolean)] = ${ mismatchesImpl }

  private def constructionImpl(using q: Quotes)
      : Expr[List[(String, Boolean, Boolean, Boolean, Boolean, Boolean)]] =
    import q.reflect.*
    import quasiquotes.Quasiquotes.{dqq, dqr}

    def row(
        label: String,
        firstType: TypeRepr,
        secondType: TypeRepr,
        resultType: TypeRepr,
        selectSecond: Boolean
    ): Expr[(String, Boolean, Boolean, Boolean, Boolean, Boolean)] =
      val methodName = if selectSecond then "second" else "first"
      val definition =
        if selectSecond then
          dqr"def second(left: $firstType, right: $secondType): $resultType = right"
        else
          dqr"def first(left: $firstType, right: $secondType): $resultType = left"
      val clause = definition.paramss.head.asInstanceOf[TermParamClause]
      val List(first, second) = clause.params: @unchecked
      val selected = if selectSecond then second else first
      val pattern: TwoParameterDefinitionPattern =
        if selectSecond then
          dqq(StringContext("def second(left: Int, right: String): String = ", ""))(using q)
        else
          dqq(StringContext("def first(left: Int, right: String): Int = ", ""))(using q)
      val captured = pattern.unapply(definition)

      Expr((
        label,
        definition.symbol.owner == Symbol.spliceOwner &&
          first.symbol.owner == definition.symbol && second.symbol.owner == definition.symbol,
        !clause.isImplicit && !clause.isGiven && !clause.isErased &&
          clause.params.map(_.name) == List("left", "right"),
        definition.symbol.paramSymss == List(List(first.symbol, second.symbol)),
        (first.tpt.tpe.asInstanceOf[AnyRef] eq firstType.asInstanceOf[AnyRef]) &&
          (second.tpt.tpe.asInstanceOf[AnyRef] eq secondType.asInstanceOf[AnyRef]) &&
          (definition.returnTpt.tpe.asInstanceOf[AnyRef] eq resultType.asInstanceOf[AnyRef]),
        definition.rhs.exists {
          case reference: Ref => reference.symbol == selected.symbol
          case _ => false
        } && captured.exists(body => definition.rhs.exists(_ eq body)) &&
          definition.name == methodName
      ))

    Expr.ofList(List(
      row("first", TypeRepr.of[Int], TypeRepr.of[String], TypeRepr.of[Int], false),
      row("second", TypeRepr.of[Int], TypeRepr.of[String], TypeRepr.of[String], true)
    ))

  private def mismatchesImpl(using q: Quotes): Expr[List[(String, Boolean)]] =
    import q.reflect.*

    def definition(expression: Expr[Any]): DefDef =
      expression.asTerm match
        case Inlined(_, _, Block(statements, _)) =>
          statements.collectFirst { case value: DefDef => value }.getOrElse(
            report.errorAndAbort("Q012R fixture did not contain a Definition")
          )
        case Block(statements, _) =>
          statements.collectFirst { case value: DefDef => value }.getOrElse(
            report.errorAndAbort("Q012R fixture did not contain a Definition")
          )
        case other => report.errorAndAbort(s"Q012R fixture did not remain a block: ${other.show}")

    val exact = definition('{
      def first(left: Int, right: String): Int = left + right.length
      ()
    })
    val wrongMethod = definition('{ def other(left: Int, right: String): Int = left; () })
    val wrongFirstName = definition('{ def first(value: Int, right: String): Int = value; () })
    val wrongSecondName = definition('{ def first(left: Int, value: String): Int = left; () })
    val wrongFirstType = definition('{ def first(left: Boolean, right: String): Int = 0; () })
    val wrongSecondType = definition('{ def first(left: Int, right: Boolean): Int = left; () })
    val wrongResult = definition('{ def first(left: Int, right: String): String = right; () })
    val one = definition('{ def first(left: Int): Int = left; () })
    val three = definition('{ def first(left: Int, right: String, third: Boolean): Int = left; () })
    val twoClauses = definition('{ def first(left: Int)(right: String): Int = left; () })
    val default = definition('{ def first(left: Int = 0, right: String): Int = left; () })
    val contextual = definition('{ def first(using left: Int, right: String): Int = left; () })
    val implicitClause = definition('{ def first(implicit left: Int, right: String): Int = left; () })
    val erasedClause = definition('{
      def first(erased left: Int, erased right: String): Int = 0
      ()
    })
    val foreign = definition('{ def first(left: Int, right: String): Int = left; () })
    val pattern: TwoParameterDefinitionPattern = DefinitionPattern.dqq(
      StringContext("def first(left: Int, right: String): Int = ", "")
    )(using q)

    val exactClause = exact.paramss.head.asInstanceOf[TermParamClause]
    val wrongOrder = DefDef.copy(exact)(
      exact.name,
      List(TermParamClause(exactClause.params.reverse)),
      exact.returnTpt,
      exact.rhs
    )
    val foreignOwner = DefDef.copy(exact)(exact.name, foreign.paramss, exact.returnTpt, exact.rhs)

    def rejected(label: String, target: DefDef): Expr[(String, Boolean)] =
      Expr((label, pattern.unapply(target).isEmpty))

    val rows = List(
      Expr(("success-arbitrary-rhs", pattern.unapply(exact).exists(body => exact.rhs.exists(_ eq body)))),
      rejected("method-name", wrongMethod),
      rejected("first-name", wrongFirstName),
      rejected("second-name", wrongSecondName),
      rejected("first-type", wrongFirstType),
      rejected("second-type", wrongSecondType),
      rejected("result-type", wrongResult),
      rejected("one-parameter", one),
      rejected("three-parameters", three),
      rejected("two-clauses", twoClauses),
      rejected("default", default),
      rejected("contextual-given", contextual),
      rejected("implicit", implicitClause),
      rejected("erased", erasedClause),
      rejected("foreign-owner", foreignOwner),
      rejected("wrong-param-order", wrongOrder),
      Expr(("null", pattern.unapply(null.asInstanceOf[DefDef]).isEmpty))
    )
    Expr.ofList(rows)
