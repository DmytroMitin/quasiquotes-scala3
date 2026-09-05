package quasiquotes.hybrid

import scala.compiletime.testing.typeCheckErrors
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

final class ScalametaSemanticEmptyNamedUsingDefinitionClauseCaptureProductionTest
    extends munit.FunSuite:
  test("external packages receive exact direct and umbrella Q029 capture types"):
    val _ = external.consumer.Q029ExternalScalametaSemanticEmptyNamedUsingDefinitionClauseConsumer

  test("typed-Scalameta Q029 captures 1 2 3 and N binders and delegates modifier semantics"):
    import quasiquotes.scalameta.ScalametaQuasiPattern.dqq

    given Compiler = Compiler.make(getClass.getClassLoader)
    val rows = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*

      val definitions = scala.collection.mutable.Map.empty[String, DefDef]
      val traversal = new TreeTraverser:
        override def traverseTree(tree: Tree)(owner: Symbol): Unit =
          tree match
            case value: DefDef if !value.symbol.isClassConstructor => definitions.update(value.name, value)
            case _ => ()
          super.traverseTree(tree)(owner)
      traversal.traverseTree(
        '{
          class Fixture:
            def one(using ordering: Ordering[Int]): Int = 1
            def two(using ordering: Ordering[Int], numeric: Numeric[Int]): Int = 2
            def three(using first: Ordering[Int], second: Numeric[Int], third: CanEqual[Int, Int]): Int = 3
            def many(using first: Ordering[Int], second: Numeric[Int], third: CanEqual[Int, Int], fourth: ValueOf[1]): Int = 4
            final def modified(using ordering: Ordering[Int]): Int = 1
          ()
        }.asTerm
      )(Symbol.spliceOwner)

      val omitted = dqq(StringContext("def ", "(using ..", "): ", " = ", ""))(using q)
      val captured = dqq(StringContext("", " def ", "(using ..", "): ", " = ", ""))(using q)
      List("one", "two", "three", "many").map { name =>
        val target = definitions(name)
        val clause = target.paramss.head.asInstanceOf[TermParamClause]
        val result = omitted.unapply(target).get
        (
          name,
          result._1 == target.name,
          result._2.size,
          result._2.zip(clause.params).forall((left, right) => left eq right),
          result._2.map(_.symbol) == clause.params.map(_.symbol),
          result._2.forall(_.symbol.owner == target.symbol),
          target.symbol.paramSymss == List(clause.params.map(_.symbol)),
          result._3 =:= target.returnTpt.tpe,
          target.rhs.exists(_ eq result._4),
          captured.unapply(target).nonEmpty
        )
      } -> (
        captured.unapply(definitions("modified")).nonEmpty,
        omitted.unapply(definitions("modified")).isEmpty
      )

    val (captures, contrast) = rows
    captures.foreach { row =>
      assert(row._2, row)
      assert(row._4, row)
      assert(row._5, row)
      assert(row._6, row)
      assert(row._7, row)
      assert(row._8, row)
      assert(row._9, row)
      assert(row._10, row)
    }
    assertEquals(captures.map(row => row._1 -> row._3).toMap, Map("one" -> 1, "two" -> 2, "three" -> 3, "many" -> 4))
    assertEquals(contrast, (true, true))

  test("typed-Scalameta Q029 preserves anonymous context-bound and complete-paramss closure"):
    import quasiquotes.scalameta.ScalametaQuasiPattern.dqq

    given Compiler = Compiler.make(getClass.getClassLoader)
    val result = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*
      def definition(expression: Expr[Any], expectedName: String): DefDef =
        val found = scala.collection.mutable.ListBuffer.empty[DefDef]
        val traversal = new TreeTraverser:
          override def traverseTree(tree: Tree)(owner: Symbol): Unit =
            tree match
              case value: DefDef if value.name == expectedName => found += value
              case _ => ()
            super.traverseTree(tree)(owner)
        traversal.traverseTree(expression.asTerm)(Symbol.spliceOwner)
        found.head

      val named = definition('{ def named(using ordering: Ordering[Int]): Int = 1; () }, "named")
      val anonymous = definition('{ def anonymous(using Ordering[Int]): Int = 1; () }, "anonymous")
      val bounded = definition('{ def bounded[A: Ordering]: Int = 1; () }, "bounded")
      val boundedOrdinary = definition('{ def boundedOrdinary[A: Ordering](value: A): A = value; () }, "boundedOrdinary")
      val boundedUsing = definition('{ def boundedUsing[A: Ordering](using marker: Numeric[Int]): Int = 1; () }, "boundedUsing")
      val q029 = dqq(StringContext("def ", "(using ..", "): ", " = ", ""))(using q)
      val q020 = dqq(StringContext("def ", "(...", "): ", " = ", ""))(using q)
      (
        q029.unapply(named).nonEmpty,
        q029.unapply(anonymous).isEmpty,
        q029.unapply(bounded).isEmpty,
        q029.unapply(boundedOrdinary).isEmpty,
        q029.unapply(boundedUsing).isEmpty,
        q020.unapply(named).isEmpty
      )

    assertEquals(result, (true, true, true, true, true, true))

  test("typed-Scalameta selector keeps Q029 and Q028 as exact distinct static grammars"):
    inline def messages(inline source: String): List[String] = typeCheckErrors(source).map(_.message)
    inline def patternMessages(inline pattern: String): List[String] =
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             """ + pattern + """
             case _ => ()"""
      )

    val q029 = patternMessages("""case dqq"def $name(using ..$params): $result = $body" => ()""")
    val q028 = patternMessages("""case dqq"$mods def $name(using ..$params): $result = $body" => ()""")
    val q031 = patternMessages("""case dqq"$mods def $name(implicit ..$params): $result = $body" => ()""")
    val rejected = List(
      patternMessages("""case dqq"private def $name(using ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"def fixed(using ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(usingx ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(using fixed: Int, ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(using ..$params, fixed: Int): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(using ...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name[..$tparams](using ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(value: Int)(using ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(using ..$first)(using ..$second): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(using erased ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(using ..$params): Int = $body" => ()"""),
      patternMessages("""case dqq"def $name(using ..$params): $result = $body + 1" => ()"""),
      patternMessages("""case dqq"def $name(using ..$params): $result = $left + $right" => ()"""),
      patternMessages("""case dqq"def $name(using ..$params): $result = $body extra" => ()"""),
      patternMessages("""case dqq"def $name(using .$params): $result = $body" => ()""")
    )

    assertEquals(q029, Nil)
    assertEquals(q028, Nil)
    assertEquals(q031, Nil)
    assert(rejected.forall(_.nonEmpty), rejected)
    assert(rejected.flatten.forall(_.contains("Invalid Scalameta dqq definition-pattern template")), rejected)

  test("typed-Scalameta dynamic Q029 selection retains exact-one fallback"):
    val errors = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.matching.RankedDefinitionPatternExtractor; import quasiquotes.scalameta.ScalametaQuasiPattern
         def f(using q: Quotes)(context: StringContext): RankedDefinitionPatternExtractor[
           q.reflect.DefDef,
           (String, Seq[q.reflect.ValDef], q.reflect.TypeRepr, q.reflect.Term)
         ] = ScalametaQuasiPattern.dqq(context)(using q)"""
    )
    assert(errors.nonEmpty, errors)
