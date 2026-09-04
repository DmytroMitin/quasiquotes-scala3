package quasiquotes.hybrid

import scala.compiletime.testing.typeCheckErrors
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

final class Q019ScalametaDefinitionCaptureFeasibilityTest extends munit.FunSuite:
  test("Q019 typed-Scalameta candidates expose exact external-package binder types"):
    val _ = external.consumer.Q019ExternalScalametaDefinitionCaptureConsumer

  test("typed-Scalameta selectors preserve names paramss results and bodies across the success matrix"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val rows = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*

      def definition(expression: Expr[Any], expectedName: String): DefDef =
        val definitions = scala.collection.mutable.ListBuffer.empty[DefDef]
        val traversal = new TreeTraverser:
          override def traverseTree(tree: Tree)(owner: Symbol): Unit =
            tree match
              case value: DefDef if value.name == expectedName => definitions += value
              case _ => ()
            super.traverseTree(tree)(owner)
        traversal.traverseTree(expression.asTerm)(Symbol.spliceOwner)
        definitions.head

      val targets = List(
        ("plain", Nil, definition('{ def plain: Int = 1; () }, "plain")),
        ("mixed_Name42", List(0), definition('{ def mixed_Name42(): String = "ok"; () }, "mixed_Name42")),
        ("type", List(1), definition('{ def `type`(value: Int): List[Int] = List(value); () }, "type")),
        ("++", List(1, 1), definition('{ def ++(left: Int)(right: String): Either[Int, String] = Left(left); () }, "++")),
        ("tupleResult", List(1, 0, 2), definition('{ def tupleResult(first: Int)()(second: String, third: Long): (Int, String) = (first, second); () }, "tupleResult")),
        ("functionResult", List(1), definition('{ def functionResult(prefix: String): Int => String = value => prefix + value; () }, "functionResult"))
      )

      targets.map { (expectedName, expectedCounts, target) =>
        val originalClauses = target.paramss.collect { case clause: TermParamClause => clause }
        val semantic =
          import quasiquotes.hybrid.q019.Q019SemanticScalametaPattern.dqq
          target match
            case dqq"def $name(...$paramss): $result = $body" =>
              (
                name,
                paramss.map(_.size).toList,
                paramss.zip(originalClauses).forall((captured, original) =>
                  captured.zip(original.params).forall((left, right) => left eq right)
                ),
                result.show == target.returnTpt.tpe.show,
                target.rhs.exists(_ eq body),
                result.show
              )
            case _ => ("<no-match>", List(-1), false, false, false, "")
        val tree =
          import quasiquotes.hybrid.q019.Q019TreeScalametaPattern.dqq
          target match
            case dqq"def $symbol(...$paramss): $resultTree = $body" =>
              (
                symbol == target.symbol,
                paramss.map(_.size).toList,
                resultTree eq target.returnTpt,
                target.rhs.exists(_ eq body)
              )
            case _ => (false, List(-1), false, false)
        (
          expectedName,
          expectedCounts,
          target.name,
          target.symbol.name,
          semantic,
          tree
        )
      }

    rows.foreach { row =>
      println(s"Q019_SCALAMETA_NAME_RESULT $row")
      assertEquals(row._3, row._1, row)
      assertEquals(row._4, row._1, row)
      assertEquals(row._5._1, row._1, row)
      assertEquals(row._5._2, row._2, row)
      assert(row._5._3, row)
      assert(row._5._4, row)
      assert(row._5._5, row)
      assert(row._5._6.nonEmpty, row)
      assert(row._6._1, row)
      assertEquals(row._6._2, row._2, row)
      assert(row._6._3, row)
      assert(row._6._4, row)
    }

  test("typed-Scalameta structural classifier rejects non-admitted Q019 templates"):
    inline def messages(inline source: String): List[String] =
      typeCheckErrors(source).map(_.message)

    val accepted = messages(
      """import scala.quoted.*; import quasiquotes.hybrid.q019.Q019SemanticScalametaPattern.dqq
         def f(using q: Quotes)(d: q.reflect.DefDef) = d match
           case dqq"def $name(...$paramss): $result = $body" => ()
           case _ => ()"""
    )
    val cases = List(
      messages(
        """import scala.quoted.*; import quasiquotes.hybrid.q019.Q019SemanticScalametaPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def fixed(...$paramss): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.hybrid.q019.Q019SemanticScalametaPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name(...$paramss) = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.hybrid.q019.Q019SemanticScalametaPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name(...$paramss): Int = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.hybrid.q019.Q019SemanticScalametaPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name(..$params): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.hybrid.q019.Q019SemanticScalametaPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name(first: Int)(...$paramss): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.hybrid.q019.Q019SemanticScalametaPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name(...$paramss)(last: Int): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.hybrid.q019.Q019SemanticScalametaPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name(...$left)(...$right): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.hybrid.q019.Q019SemanticScalametaPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name(..$params)(...$paramss): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.hybrid.q019.Q019SemanticScalametaPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name(...): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.hybrid.q019.Q019SemanticScalametaPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name(...$paramss): $result = $body + 1" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.hybrid.q019.Q019SemanticScalametaPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name(...$paramss): $result = $left + $right" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.hybrid.q019.Q019SemanticScalametaPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"private def $name(...$paramss): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.hybrid.q019.Q019SemanticScalametaPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name[A](...$paramss): $result = $body" => ()
             case _ => ()"""
      )
    )

    assertEquals(accepted, Nil)
    assert(cases.forall(_.nonEmpty), cases)
    assert(cases.flatten.forall(_.contains("Invalid Q019 typed-Scalameta dqq")), cases)

  test("Q019 typed-Scalameta candidate dynamic selection remains closed"):
    val dynamicErrors = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.matching.RankedDefinitionPatternExtractor
        import quasiquotes.hybrid.q019.Q019SemanticScalametaPattern
        def dynamic(using q: Quotes)(context: StringContext): RankedDefinitionPatternExtractor[
          q.reflect.DefDef,
          (String, Seq[Seq[q.reflect.ValDef]], q.reflect.TypeRepr, q.reflect.Term)
        ] = Q019SemanticScalametaPattern.dqq(context)(using q)
      }"""
    )
    assert(dynamicErrors.nonEmpty, dynamicErrors)
