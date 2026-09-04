package quasiquotes.hybrid

import scala.compiletime.testing.typeCheckErrors
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

final class RankedScalametaDefinitionParamssPatternTest extends munit.FunSuite:
  test("external direct and umbrella imports receive exact typed-Scalameta rank-3 Definition type"):
    val _ = external.consumer.Q018ExternalScalametaDefinitionParamssConsumer

  test("typed-Scalameta rank-3 matching preserves zero empty and N clause topology"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val rows = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*
      import quasiquotes.scalameta.ScalametaQuasiPattern.dqq

      def definition(expression: Expr[Any]): DefDef =
        expression.asTerm match
          case Inlined(_, _, Block(statements, _)) =>
            statements.collectFirst { case value: DefDef => value }.get
          case Block(statements, _) =>
            statements.collectFirst { case value: DefDef => value }.get
          case other => report.errorAndAbort(s"unexpected typed-Scalameta rank-3 fixture: ${other.show}")

      val targets = List(
        definition('{ def collect: Int = 0; () }),
        definition('{ def collect(): Int = 0; () }),
        definition('{ def collect(first: String): Int = first.length; () }),
        definition('{ def collect(first: String)(second: Boolean, third: Int): Int = if second then first.length + third else third; () }),
        definition('{ def collect(first: String)()(third: Int, fourth: Long): Int = first.length + third + fourth.toInt; () }),
        definition('{ def collect(first: String)()(third: Int, fourth: Long)(fifth: Byte): Int = first.length + third + fourth.toInt + fifth.toInt; () })
      )
      val expectedCounts = List(Nil, List(0), List(1), List(1, 2), List(1, 0, 2), List(1, 0, 2, 1))

      targets.zip(expectedCounts).map { (target, expected) =>
        val originalClauses = target.paramss.collect { case clause: TermParamClause => clause }
        target match
          case dqq"def collect(...$paramss): Int = $body" =>
            val flattened = paramss.flatten
            (
              expected,
              paramss.map(_.size).toList,
              paramss.zip(originalClauses).forall((parameters, clause) =>
                parameters.zip(clause.params).forall((captured, original) => captured eq original)
              ),
              paramss.map(_.map(_.symbol)) == originalClauses.map(_.params.map(_.symbol)),
              flattened.map(_.symbol).distinct.size == flattened.size,
              flattened.forall(parameter =>
                parameter.symbol != Symbol.noSymbol && parameter.symbol.owner == target.symbol
              ),
              target.symbol.paramSymss == paramss.map(_.map(_.symbol).toList).toList,
              target.rhs.exists(_ eq body),
              target.returnTpt.tpe =:= TypeRepr.of[Int]
            )
          case _ => (expected, List(-1), false, false, false, false, false, false, false)
      }

    rows.foreach { row =>
      assertEquals(row._2, row._1, row)
      assert(row._3, row)
      assert(row._4, row)
      assert(row._5, row)
      assert(row._6, row)
      assert(row._7, row)
      assert(row._8, row)
      assert(row._9, row)
    }

  test("typed-Scalameta static diagnostics reject unsupported rank-3 Definition shapes"):
    inline def messages(inline source: String): List[String] =
      typeCheckErrors(source).map(_.message)

    val rankMismatch = messages(
      """import scala.quoted.*
         import quasiquotes.matching.RankedDefinitionPatternExtractor
         import quasiquotes.scalameta.ScalametaQuasiPattern
         def f(using q: Quotes) =
           val _: RankedDefinitionPatternExtractor[
             q.reflect.DefDef,
             (Seq[Seq[q.reflect.ValDef]], q.reflect.Term)
           ] = ScalametaQuasiPattern.dqq(StringContext("def collect(..", "): Int = ", ""))(using q)"""
    )
    val cases = List(
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"...$mods def collect: Int = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def collect(...$left)(...$right): Int = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def collect(..$params)(...$paramss): Int = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def collect(...): Int = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def collect(...$paramss): Int = $left + $right" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def collect(first: Int)(...$paramss): Int = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def collect(...$paramss)(last: Int): Int = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def other(...$paramss): Int = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def collect(...$paramss): String = $body" => ()
             case _ => ()"""
      )
    )

    assert(rankMismatch.nonEmpty, rankMismatch)
    assert(cases.forall(_.nonEmpty), cases)
    assert(cases.flatten.forall(_.contains("Invalid Scalameta dqq definition-pattern template")), cases)

  test("dynamic typed-Scalameta rank-3 selection keeps the exact-one fallback"):
    val errors = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.matching.RankedDefinitionPatternExtractor
        import quasiquotes.scalameta.ScalametaQuasiPattern
        def dynamic(using q: Quotes)(context: StringContext): RankedDefinitionPatternExtractor[
          q.reflect.DefDef,
          (Seq[Seq[q.reflect.ValDef]], q.reflect.Term)
        ] = ScalametaQuasiPattern.dqq(context)(using q)
      }"""
    )
    assert(errors.nonEmpty, errors)
