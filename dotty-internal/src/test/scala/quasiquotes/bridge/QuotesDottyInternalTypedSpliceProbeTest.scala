package quasiquotes.bridge

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Decorators.toTermName
import dotty.tools.dotc.util.{NoSource, SourceFile}

final class QuotesDottyInternalTypedSpliceProbeTest extends munit.FunSuite:
  test("typed Expr leaves survive an untpd TypedSplice shell and public reflection conversion"):
    assertEquals(QuotesDottyInternalTypedSpliceProbe.add(20, 22), 42)

  test("tpd.applyOverloaded constructs the typed addition with overload resolution"):
    assertEquals(
      QuotesDottyInternalTypedSpliceProbe.addTypedOverloaded(1, 2),
      3
    )

  test("the pure untpd addition constructor preserves the exact raw shell"):
    given SourceFile = NoSource
    val left = untpd.Ident("left".toTermName)
    val right = untpd.Ident("right".toTermName)

    QuotesDottyInternalTypedSpliceProbe.addUntpdTree(left, right) match
      case untpd.Apply(untpd.Select(actualLeft, name), actualRight :: Nil) =>
        assert(actualLeft eq left)
        assert(actualRight eq right)
        assertEquals(name.toString, "+")
      case other => fail(s"unexpected raw addition shell: $other")
