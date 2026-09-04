package quasiquotes.hybrid

import scala.compiletime.testing.typeCheckErrors

final class Q023ScalametaDefinitionModifierCaptureFeasibilityTest extends munit.FunSuite:
  test("Q023 typed-Scalameta candidates expose exact external-package binder types"):
    val _ = external.consumer.Q023ExternalScalametaDefinitionModifierCaptureConsumer

  test("typed-Scalameta selector structurally accepts only the exact six-capture layout"):
    inline def messages(inline source: String): List[String] = typeCheckErrors(source).map(_.message)

    val accepted = messages(
      """import scala.quoted.*; import quasiquotes.hybrid.q023.Q023StructuredScalametaPattern.dqq
         def f(using q: Quotes)(d: q.reflect.DefDef) = d match
           case dqq"$mods def $name[..$tparams](...$paramss): $result = $body" => ()
           case _ => ()"""
    )
    val missingMods = messages(
      """import scala.quoted.*; import quasiquotes.hybrid.q023.Q023StructuredScalametaPattern.dqq
         def f(using q: Quotes)(d: q.reflect.DefDef) = d match
           case dqq"def $name[..$tparams](...$paramss): $result = $body" => ()
           case _ => ()"""
    )
    val fixedMods = messages(
      """import scala.quoted.*; import quasiquotes.hybrid.q023.Q023StructuredScalametaPattern.dqq
         def f(using q: Quotes)(d: q.reflect.DefDef) = d match
           case dqq"private def $name[..$tparams](...$paramss): $result = $body" => ()
           case _ => ()"""
    )
    val malformedRanks = messages(
      """import scala.quoted.*; import quasiquotes.hybrid.q023.Q023StructuredScalametaPattern.dqq
         def f(using q: Quotes)(d: q.reflect.DefDef) = d match
           case dqq"$mods def $name[...$tparams](..$params): $result = $body" => ()
           case _ => ()"""
    )
    val partialBody = messages(
      """import scala.quoted.*; import quasiquotes.hybrid.q023.Q023StructuredScalametaPattern.dqq
         def f(using q: Quotes)(d: q.reflect.DefDef) = d match
           case dqq"$mods def $name[..$tparams](...$paramss): $result = $left + $right" => ()
           case _ => ()"""
    )

    assertEquals(accepted, Nil)
    assert(missingMods.nonEmpty, missingMods)
    assert(fixedMods.nonEmpty, fixedMods)
    assert(malformedRanks.nonEmpty, malformedRanks)
    assert(partialBody.nonEmpty, partialBody)

  test("Q023 typed-Scalameta candidate dynamic selection remains closed"):
    val errors = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.hybrid.q023.Q023StructuredScalametaPattern
        def dynamic(using q: Quotes)(context: StringContext) =
          Q023StructuredScalametaPattern.dqq(context)(using q)
      }"""
    )
    assert(errors.nonEmpty, errors)
