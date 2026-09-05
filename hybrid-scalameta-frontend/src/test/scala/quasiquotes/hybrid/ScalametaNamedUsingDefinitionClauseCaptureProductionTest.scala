package quasiquotes.hybrid

import scala.compiletime.testing.typeCheckErrors
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

final class ScalametaNamedUsingDefinitionClauseCaptureProductionTest extends munit.FunSuite:
  test("external packages receive exact direct and umbrella Q028 capture types"):
    val _ = external.consumer.Q028ExternalScalametaNamedUsingDefinitionClauseConsumer

  test("typed-Scalameta Q028 delegates target semantics and leaves complete paramss closed"):
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

      val named = definition('{
        class Fixture:
          final def named(using first: Ordering[Int], second: Numeric[Int]): Int = 2
        ()
      }, "named")
      val anonymous = definition('{ def anonymous(using Ordering[Int]): Int = 1; () }, "anonymous")
      val bounded = definition('{ def bounded[A: Ordering]: Int = 1; () }, "bounded")
      val q028 = dqq(StringContext("", " def ", "(using ..", "): ", " = ", ""))(using q)
      val q026 = dqq(StringContext("", " def ", "(...", "): ", " = ", ""))(using q)
      val captured = q028.unapply(named).get
      (
        captured._1.flags == named.symbol.flags,
        captured._2 == named.name,
        captured._3.zip(named.paramss.head.asInstanceOf[TermParamClause].params).forall((left, right) => left eq right),
        captured._4 =:= named.returnTpt.tpe,
        named.rhs.exists(_ eq captured._5),
        q028.unapply(anonymous).isEmpty,
        q028.unapply(bounded).isEmpty,
        q026.unapply(named).isEmpty
      )

    assertEquals(result, (true, true, true, true, true, true, true, true))

  test("typed-Scalameta selector admits only the exact Q028 static grammar"):
    inline def messages(inline source: String): List[String] = typeCheckErrors(source).map(_.message)
    inline def patternMessages(inline pattern: String): List[String] =
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             """ + pattern + """
             case _ => ()"""
      )

    val accepted = patternMessages("""case dqq"$mods def $name(using ..$params): $result = $body" => ()""")
    val q031 = patternMessages("""case dqq"$mods def $name(implicit ..$params): $result = $body" => ()""")
    val rejected = List(
      patternMessages("""case dqq"private def $name(using ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods final def $name(using ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$left $right def $name(using ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$modsdef $name(using ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def fixed(using ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name[..$tparams](using ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(using ...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(using fixed: Int, ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(using ..$params, fixed: Int): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(using ..$first)(using ..$second): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(using erased ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(using ..$params): Int = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(using ..$params): $result = $left + $right" => ()"""),
      patternMessages("""case dqq"$mods def $name(using ..$params): $result = $body extra" => ()""")
    )

    assertEquals(accepted, Nil)
    assertEquals(q031, Nil)
    assert(rejected.forall(_.nonEmpty), rejected)
    assert(rejected.flatten.forall(_.contains("Invalid Scalameta dqq definition-pattern template")), rejected)

  test("typed-Scalameta dynamic Q028 selection retains exact-one fallback"):
    val errors = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.matching.{DefinitionModifiers, RankedDefinitionPatternExtractor}; import quasiquotes.scalameta.ScalametaQuasiPattern
         def f(using q: Quotes)(context: StringContext): RankedDefinitionPatternExtractor[
           q.reflect.DefDef,
           (DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term], String, Seq[q.reflect.ValDef], q.reflect.TypeRepr, q.reflect.Term)
         ] = ScalametaQuasiPattern.dqq(context)(using q)"""
    )
    assert(errors.nonEmpty, errors)
