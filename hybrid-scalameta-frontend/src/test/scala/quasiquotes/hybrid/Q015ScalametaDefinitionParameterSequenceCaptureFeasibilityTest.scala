package quasiquotes.hybrid

import scala.compiletime.testing.typeCheckErrors
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

import quasiquotes.hybrid.q015.Q015StrategyBScalametaDefinitionPattern.dqq

final class Q015ScalametaDefinitionParameterSequenceCaptureFeasibilityTest extends munit.FunSuite:
  test("strategy B exposes the same exact binder types to an external package"):
    val _ = external.consumer.Q015ExternalScalametaDefinitionPatternConsumer

  test("typed-Scalameta strategy A anonymous extractor is rejected externally too"):
    val errors = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.hybrid.q015.Q015StrategyAScalametaDefinitionPattern.dqq
        def compile(using q: Quotes)(target: q.reflect.DefDef): Unit =
          target match
            case dqq"def collect(..$params): Int = $body" => ()
            case _ => ()
      }"""
    )
    assert(errors.exists(_.message.contains("cannot be used as an extractor")), errors)

  test("typed-Scalameta strategy B preserves zero one two and three parameter and body identity"):
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
          case other => report.errorAndAbort(s"unexpected Q015 hybrid fixture: ${other.show}")

      val targets = List(
        definition('{ def collect(): Int = 0; () }),
        definition('{ def collect(first: String): Int = first.length; () }),
        definition('{ def collect(first: String, second: Boolean): Int = if second then first.length else 0; () }),
        definition('{ def collect(first: String, second: Boolean, third: Int): Int = if second then first.length + third else third; () })
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
              params.forall(parameter =>
                parameter.symbol != Symbol.noSymbol && parameter.symbol.owner == target.symbol
              ),
              target.symbol.paramSymss == List(params.map(_.symbol).toList),
              target.rhs.exists(_ eq body)
            )
          case _ => (expectedSize, -1, false, false, false, false, false)
      }

    rows.foreach { row =>
      assertEquals(row._2, row._1, row)
      assert(row._3, row)
      assert(row._4, row)
      assert(row._5, row)
      assert(row._6, row)
      assert(row._7, row)
    }
