package quasiquotes.q057

import quasiquotes.matching.TermPattern
import quasiquotes.scalameta.TermFrontend

final class Q057ScalametaSelectedMemberParityFeasibilityTest extends munit.FunSuite:
  private val PlaceholderName = "__q057_selected_name_placeholder"

  private def compile(source: String): TermPattern =
    val compiled = TermFrontend.compile(source).fold(
      failure => fail(failure.message),
      identity
    )
    assertEquals(compiled.engine, TermFrontend.Engine.Scalameta)
    assertEquals(compiled.primaryFailure, None)
    compiled.pattern

  test("Scalameta-primary compilation preserves the direct selected-name placeholder sidecar site"):
    assertEquals(
      compile(s"$$receiver.$PlaceholderName"),
      TermPattern.Select(TermPattern.Hole("receiver"), PlaceholderName)
    )

  test("Scalameta-primary compilation preserves empty and one-argument selected calls"):
    assertEquals(
      compile(s"$$receiver.$PlaceholderName()"),
      TermPattern.Apply(
        TermPattern.Select(TermPattern.Hole("receiver"), PlaceholderName),
        Nil
      )
    )
    assertEquals(
      compile(s"$$receiver.$PlaceholderName($$argument)"),
      TermPattern.Apply(
        TermPattern.Select(TermPattern.Hole("receiver"), PlaceholderName),
        List(TermPattern.Hole("argument"))
      )
    )
