package quasiquotes.q044

import scala.compiletime.testing.typeCheckErrors
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

final class Q044UnifiedOrdinaryRank2ProductionTest extends munit.FunSuite:
  test("real standard dqq admits the unified ordinary rank-2 matrix and preserves identity"):
    import quasiquotes.matching.DefinitionPattern.dqq

    val _ = external.consumer.Q044ExternalUnifiedOrdinaryRank2DefinitionConsumer
    given Compiler = Compiler.make(getClass.getClassLoader)
    val report = withQuotes:
      val q = summon[Quotes]
      val rank2 = dqq(StringContext("", " def ", "(..", "): ", " = ", ""))(using q)
      val rank3 = dqq(StringContext("", " def ", "(...", "): ", " = ", ""))(using q)
      Q044UnifiedOrdinaryRank2Harness.inspect(using q)(rank2, rank3)

    println(s"Q044_STANDARD $report")
    assert(report.positive.nonEmpty && report.positive.values.forall(identity), report)
    assert(report.negative.nonEmpty && report.negative.values.forall(identity), report)
    assert(report.rank3.nonEmpty && report.rank3.values.forall(identity), report)
    assertEquals(report.modes("empty"), Nil)
    assertEquals(report.modes("strict5").size, 5)
    assertEquals(report.modes("plainSeq"), List("strict"))
    assertEquals(report.modes("allModes"), List("strict", "by-name", "repeated"))

  test("real standard dqq accepts only the exact static Q044 grammar"):
    inline def messages(inline source: String): List[String] = typeCheckErrors(source).map(_.message)
    inline def patternMessages(inline pattern: String): List[String] =
      messages(
        """import scala.quoted.*; import quasiquotes.matching.DefinitionPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             """ + pattern + """
             case _ => ()"""
      )

    val accepted = patternMessages("""case dqq"$mods def $name(..$params): $result = $body" => ()""")
    val rejected = List(
      patternMessages("""case dqq"def $name(..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"final $mods def $name(..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def fixed(..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name[..$tparams](..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(fixed: Int, ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(..$second): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params): Int = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params): $result = $left + $right" => ()"""),
      patternMessages("""case dqq"$mods def $name(.$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params): $result = $body trailing" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params): $result = $params" => ()""")
    )
    val dynamic = messages(
      """import scala.quoted.*
         import quasiquotes.matching.{DefinitionModifiers, DefinitionPattern, RankedDefinitionPatternExtractor}
         import DefinitionPattern.dqq
         def f(using q: Quotes)(context: StringContext): RankedDefinitionPatternExtractor[
           q.reflect.DefDef,
           (DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term], String,
             Seq[q.reflect.ValDef], q.reflect.TypeRepr, q.reflect.Term)
         ] = context.dqq"""
    )
    val neighboring = List(
      patternMessages("""case dqq"$mods def $name(...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(using ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(implicit ..$params): $result = $body" => ()""")
    )
    assertEquals(accepted, Nil)
    assert(rejected.forall(_.nonEmpty), rejected)
    assert(neighboring.forall(_.isEmpty), neighboring)
    assert(dynamic.nonEmpty, dynamic)
