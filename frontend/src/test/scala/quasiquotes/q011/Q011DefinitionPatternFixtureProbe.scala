package quasiquotes.q011

import scala.quoted.*

import quasiquotes.matching.{DefinitionPattern, TwoParameterDefinitionPattern}

object Q011DefinitionPatternFixtureProbe:
  inline def evidence: List[(String, Boolean, Boolean)] = ${ evidenceImpl }

  private def evidenceImpl(using q: Quotes): Expr[List[(String, Boolean, Boolean)]] =
    import q.reflect.*

    def definition(expression: Expr[Any]): DefDef =
      expression.asTerm match
        case Inlined(_, _, Block(statements, _)) =>
          statements.collectFirst { case value: DefDef => value }.getOrElse(
            report.errorAndAbort("Q011 fixture did not contain a Definition")
          )
        case Block(statements, _) =>
          statements.collectFirst { case value: DefDef => value }.getOrElse(
            report.errorAndAbort("Q011 fixture did not contain a Definition")
          )
        case other => report.errorAndAbort(s"Q011 fixture did not remain a block: ${other.show}")

    val exact = definition('{
      def first(left: Int, right: String): Int = left + right.length
      ()
    })
    val wrongMethod = definition('{
      def wrongMethod(left: Int, right: String): Int = left
      ()
    })
    val wrongFirstName = definition('{
      def first(value: Int, right: String): Int = value
      ()
    })
    val wrongSecondName = definition('{
      def first(left: Int, value: String): Int = left
      ()
    })
    val wrongFirstType = definition('{
      def first(left: Boolean, right: String): Int = 0
      ()
    })
    val wrongSecondType = definition('{
      def first(left: Int, right: Boolean): Int = left
      ()
    })
    val wrongResult = definition('{
      def first(left: Int, right: String): String = right
      ()
    })
    val oneParameter = definition('{
      def first(left: Int): Int = left
      ()
    })
    val threeParameters = definition('{
      def first(left: Int, right: String, extra: Boolean): Int = left
      ()
    })
    val twoClauses = definition('{
      def first(left: Int)(right: String): Int = left
      ()
    })
    val defaultParameter = definition('{
      def first(left: Int = 0, right: String): Int = left
      ()
    })
    val contextual = definition('{
      def first(using left: Int, right: String): Int = left
      ()
    })
    val foreign = definition('{
      def first(left: Int, right: String): Int = left
      ()
    })
    val pattern: TwoParameterDefinitionPattern = DefinitionPattern.dqq(
      StringContext("def first(left: Int, right: String): Int = ", "")
    )(using q)

    def result(label: String, target: DefDef): Expr[(String, Boolean, Boolean)] =
      val captured = pattern.unapply(using q)(target)
      Expr((
        label,
        captured.nonEmpty,
        captured.exists(body => target.rhs.exists(_ eq body))
      ))

    val foreignOwner =
      DefDef.copy(exact)(
        exact.name,
        foreign.paramss,
        exact.returnTpt,
        exact.rhs
      )

    val rows = List(
      result("success-arbitrary-rhs", exact),
      result("method-name", wrongMethod),
      result("first-parameter-name", wrongFirstName),
      result("second-parameter-name", wrongSecondName),
      result("first-parameter-type", wrongFirstType),
      result("second-parameter-type", wrongSecondType),
      result("result-type", wrongResult),
      result("one-parameter", oneParameter),
      result("three-parameters", threeParameters),
      result("two-clauses", twoClauses),
      result("default", defaultParameter),
      result("contextual", contextual),
      result("foreign-owner", foreignOwner)
    )

    '{ List(${ Varargs(rows) }*) }
