package quasiquotes.hybrid

import scala.compiletime.testing.typeCheckErrors
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

final class Q017ScalametaDefinitionParamssCaptureFeasibilityTest extends munit.FunSuite:
  test("both typed-Scalameta candidates expose exact external-package binder types"):
    val _ = external.consumer.Q017ExternalScalametaDefinitionParamssConsumer

  test("typed-Scalameta selectors preserve the same zero empty and ordinary topology"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val rows = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*

      def definition(expression: Expr[Any]): DefDef =
        expression.asTerm match
          case Inlined(_, _, Block(statements, _)) =>
            statements.collectFirst { case value: DefDef => value }.get
          case Block(statements, _) =>
            statements.collectFirst { case value: DefDef => value }.get
          case other => report.errorAndAbort(s"unexpected Q017 Scalameta fixture: ${other.show}")

      val targets = List(
        definition('{ def collect: Int = 0; () }),
        definition('{ def collect(): Int = 0; () }),
        definition('{ def collect(first: String): Int = first.length; () }),
        definition('{ def collect(first: String)(second: Boolean, third: Int): Int = if second then first.length + third else third; () }),
        definition('{ def collect(first: String)()(third: Int, fourth: Long): Int = first.length + third + fourth.toInt; () })
      )
      val expected = List(Nil, List(0), List(1), List(1, 2), List(1, 0, 2))

      targets.zip(expected).map { (target, expectedCounts) =>
        val originalClauses = target.paramss.collect { case clause: TermParamClause => clause }
        val a =
          import quasiquotes.hybrid.q017.Q017CandidateAScalametaPattern.dqq
          target match
            case dqq"def collect(...$paramss): Int = $body" =>
              (
                paramss.map(_.size).toList,
                paramss.zip(originalClauses).forall((parameters, clause) =>
                  parameters.zip(clause.params).forall((left, right) => left eq right)
                ),
                target.rhs.exists(_ eq body)
              )
            case _ => (List(-1), false, false)
        val b =
          import quasiquotes.hybrid.q017.Q017CandidateBScalametaPattern.dqq
          target match
            case dqq"def collect(...$clauses): Int = $body" =>
              (
                clauses.map(_.params.size).toList,
                clauses.zip(originalClauses).forall((left, right) => left eq right),
                target.rhs.exists(_ eq body)
              )
            case _ => (List(-1), false, false)
        (
          expectedCounts,
          a._1,
          b._1,
          a._2,
          b._2,
          a._3 && b._3
        )
      }

    rows.foreach { row =>
      assertEquals(row._2, row._1, row)
      assertEquals(row._3, row._1, row)
      assert(row._4, row)
      assert(row._5, row)
      assert(row._6, row)
    }

  test("typed-Scalameta structural classifier rejects non-admitted rank-3 templates"):
    inline def messages(inline source: String): List[String] =
      typeCheckErrors(source).map(_.message)

    val cases = List(
      messages(
        """import scala.quoted.*; import quasiquotes.hybrid.q017.Q017CandidateAScalametaPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def collect(..$params): Int = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.hybrid.q017.Q017CandidateAScalametaPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"...$mods def collect(): Int = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.hybrid.q017.Q017CandidateAScalametaPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def collect(...$left)(...$right): Int = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.hybrid.q017.Q017CandidateAScalametaPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def collect(..$params)(...$paramss): Int = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.hybrid.q017.Q017CandidateAScalametaPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def collect(...): Int = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.hybrid.q017.Q017CandidateAScalametaPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def collect(...$paramss): Int = $left + $right" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.hybrid.q017.Q017CandidateAScalametaPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def collect(first: Int)(...$paramss): Int = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.hybrid.q017.Q017CandidateAScalametaPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def collect(...$paramss)(last: Int): Int = $body" => ()
             case _ => ()"""
      )
    )

    assert(cases.forall(_.nonEmpty), cases)
    assert(cases.flatten.forall(_.contains("Invalid Q017 typed-Scalameta dqq")), cases)

  test("typed-Scalameta dynamic rank-3 selection and production rank-3 remain closed"):
    val dynamicErrors = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.matching.RankedDefinitionPatternExtractor
        import quasiquotes.hybrid.q017.Q017CandidateAScalametaPattern
        def dynamic(using q: Quotes)(context: StringContext): RankedDefinitionPatternExtractor[
          q.reflect.DefDef,
          (Seq[Seq[q.reflect.ValDef]], q.reflect.Term)
        ] = Q017CandidateAScalametaPattern.dqq(context)(using q)
      }"""
    )
    val productionErrors = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.scalameta.ScalametaQuasiPattern.dqq
        def compile(using q: Quotes)(target: q.reflect.DefDef): Unit =
          target match
            case dqq"def collect(...$paramss): Int = $body" => ()
            case _ => ()
      }"""
    )
    assert(dynamicErrors.nonEmpty, dynamicErrors)
    assert(productionErrors.exists(_.message.contains("rank-3 captures are not supported")), productionErrors)
