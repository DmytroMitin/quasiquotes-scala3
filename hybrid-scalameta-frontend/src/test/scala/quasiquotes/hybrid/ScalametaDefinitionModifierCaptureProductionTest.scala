package quasiquotes.hybrid

import scala.compiletime.testing.typeCheckErrors

final class ScalametaDefinitionModifierCaptureProductionTest extends munit.FunSuite:
  test("external packages receive exact direct and umbrella six-capture types"):
    val _ = external.consumer.Q025ExternalScalametaDefinitionModifierCaptureConsumer

  test("typed-Scalameta selector structurally accepts only the exact seven-part layout"):
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
           case dqq"$mods def $name[..$tparams](...$paramss): $result = $body" => ()
           case _ => ()"""
    )
    val existingFive = messages(
      """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.dqq
         def f(using q: Quotes)(d: q.reflect.DefDef) = d match
           case dqq"def $name[..$tparams](...$paramss): $result = $body" => ()
           case _ => ()"""
    )
    val rejected = List(
      patternMessages("""case dqq"private def $name[..$tparams](...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods final def $name[..$tparams](...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"$left $right def $name[..$tparams](...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"$modsdef $name[..$tparams](...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def fixed[..$tparams](...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name[...$tparams](...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name[..$tparams](..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name[..$tparams](first: Int)(...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name[..$tparams](...$paramss): Int = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name[..$tparams](...$paramss): $result = $left + $right" => ()""")
    )

    assertEquals(accepted, Nil)
    assertEquals(existingFive, Nil)
    assert(rejected.forall(_.nonEmpty), rejected)
    assert(rejected.flatten.forall(_.contains("Invalid Scalameta dqq definition-pattern template")), rejected)

  test("typed-Scalameta dynamic six-capture selection retains exact-one fallback"):
    val errors = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.matching.{DefinitionModifiers, RankedDefinitionPatternExtractor}; import quasiquotes.scalameta.ScalametaQuasiPattern
         def f(using q: Quotes)(context: StringContext): RankedDefinitionPatternExtractor[
           q.reflect.DefDef,
           (DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term], String, Seq[q.reflect.TypeDef], Seq[Seq[q.reflect.ValDef]], q.reflect.TypeRepr, q.reflect.Term)
         ] = ScalametaQuasiPattern.dqq(context)(using q)"""
    )
    assert(errors.nonEmpty, errors)
