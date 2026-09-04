package quasiquotes.hybrid

import scala.compiletime.testing.typeCheckErrors

final class ScalametaNongenericDefinitionModifierCaptureProductionTest extends munit.FunSuite:
  test("external packages receive exact direct and umbrella five-capture types"):
    val _ = external.consumer.Q026ExternalScalametaNongenericDefinitionModifierCaptureConsumer

  test("typed-Scalameta selector structurally accepts only the exact six-part layout"):
    inline def messages(inline source: String): List[String] = typeCheckErrors(source).map(_.message)
    inline def patternMessages(inline pattern: String): List[String] =
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             """ + pattern + """
             case _ => ()"""
      )

    val accepted = messages(
      """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.dqq
         def f(using q: Quotes)(d: q.reflect.DefDef) = d match
           case dqq"$mods def $name(...$paramss): $result = $body" => ()
           case _ => ()"""
    )
    val existingQ020 = messages(
      """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.dqq
         def f(using q: Quotes)(d: q.reflect.DefDef) = d match
           case dqq"def $name(...$paramss): $result = $body" => ()
           case _ => ()"""
    )
    val existingQ025 = messages(
      """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.dqq
         def f(using q: Quotes)(d: q.reflect.DefDef) = d match
           case dqq"$mods def $name[..$tparams](...$paramss): $result = $body" => ()
           case _ => ()"""
    )
    val rejected = List(
      patternMessages("""case dqq"private def $name(...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods final def $name(...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"$left $right def $name(...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"$modsdef $name(...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def fixed(...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name[A](...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(first: Int)(...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(...$paramss)(last: Int): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(...$paramss): Int = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(...$paramss) = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(...$paramss): $result = $left + $right" => ()"""),
      patternMessages("""case dqq"$mods def $name(...$paramss): $result = $body extra" => ()""")
    )

    assertEquals(accepted, Nil)
    assertEquals(existingQ020, Nil)
    assertEquals(existingQ025, Nil)
    assert(rejected.forall(_.nonEmpty), rejected)
    assert(rejected.flatten.forall(_.contains("Invalid Scalameta dqq definition-pattern template")), rejected)

  test("typed-Scalameta dynamic five-capture selection retains exact-one fallback"):
    val errors = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.matching.{DefinitionModifiers, RankedDefinitionPatternExtractor}; import quasiquotes.scalameta.ScalametaQuasiPattern
         def f(using q: Quotes)(context: StringContext): RankedDefinitionPatternExtractor[
           q.reflect.DefDef,
           (DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term], String, Seq[Seq[q.reflect.ValDef]], q.reflect.TypeRepr, q.reflect.Term)
         ] = ScalametaQuasiPattern.dqq(context)(using q)"""
    )
    assert(errors.nonEmpty, errors)
