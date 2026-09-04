package quasiquotes.hybrid

import scala.compiletime.testing.typeCheckErrors
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

final class RankedScalametaDefinitionPatternTest extends munit.FunSuite:
  test("external package receives exact typed-Scalameta ranked Definition binder types"):
    val _ = external.consumer.RankedScalametaDefinitionPatternExternalConsumer

  test("typed-Scalameta ranked matching preserves zero through five parameters and body identity"):
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
          case other => report.errorAndAbort(s"unexpected ranked Scalameta fixture: ${other.show}")

      val targets = List(
        definition('{ def collect(): Int = 0; () }),
        definition('{ def collect(first: String): Int = first.length; () }),
        definition('{ def collect(first: String, second: Boolean): Int = if second then first.length else 0; () }),
        definition('{ def collect(first: String, second: Boolean, third: Int): Int = if second then first.length + third else third; () }),
        definition('{ def collect(first: String, second: Boolean, third: Int, fourth: Long): Int = first.length + third + fourth.toInt; () }),
        definition('{ def collect(first: String, second: Boolean, third: Int, fourth: Long, fifth: Byte): Int = first.length + third + fourth.toInt + fifth.toInt; () })
      )

      targets.zipWithIndex.map { (target, expectedSize) =>
        target match
          case dqq"def collect(..$params): Int = $body" =>
            val original = target.paramss.head.asInstanceOf[TermParamClause].params
            (
              expectedSize,
              params.size,
              params.zip(original).forall((captured, expected) => captured eq expected),
              params.map(_.symbol) == original.map(_.symbol),
              params.map(_.symbol).distinct.size == params.size,
              params.forall(parameter =>
                parameter.symbol != Symbol.noSymbol && parameter.symbol.owner == target.symbol
              ),
              target.symbol.paramSymss == List(params.map(_.symbol).toList),
              target.rhs.exists(_ eq body)
            )
          case _ => (expectedSize, -1, false, false, false, false, false, false)
      }

    rows.foreach { row =>
      assertEquals(row._2, row._1, row)
      assert(row._3, row)
      assert(row._4, row)
      assert(row._5, row)
      assert(row._6, row)
      assert(row._7, row)
      assert(row._8, row)
    }

  test("typed-Scalameta static diagnostics reject unsupported ranked Definition shapes"):
    inline def messages(inline source: String): List[String] =
      typeCheckErrors(source).map(_.message)

    val cases = List(
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"..$mods def collect(): Int = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def collect(..$left, ..$right): Int = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def collect(..): Int = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def collect(..$params): Int = $left + $right" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def collect(..$params)(other: Int): Int = $body" => ()
             case _ => ()"""
      )
    )

    assert(cases.forall(_.nonEmpty), cases)
    assert(cases.flatten.forall(_.contains("Invalid Scalameta dqq definition-pattern template")), cases)

  test("dynamic typed-Scalameta ranked selection keeps the exact-one fallback"):
    val errors = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.matching.RankedDefinitionPatternExtractor
        import quasiquotes.scalameta.ScalametaQuasiPattern
        def dynamic(using q: Quotes)(context: StringContext): RankedDefinitionPatternExtractor[
          q.reflect.DefDef,
          (Seq[q.reflect.ValDef], q.reflect.Term)
        ] = ScalametaQuasiPattern.dqq(context)(using q)
      }"""
    )
    assert(errors.nonEmpty, errors)
