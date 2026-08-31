package quasiquotes.phase148

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Names.termName
import dotty.tools.dotc.util.{NoSource, SourceFile}

import quasiquotes.parser.TermShape
import quasiquotes.parser.TermShape.{Apply, Identifier, Select}

class Phase148CrossSurfaceCapabilityTest extends munit.FunSuite:
  test("one generic algorithm replaces the first N argument and preserves other identities") {
    val function = Select(Identifier("service", false), "invoke")
    val oldArgument = Identifier("oldArg", false)
    val keptArgument = Identifier("keptArg", false)
    val original = Apply(function, List(oldArgument, keptArgument))
    val replacement = Identifier("replacement", false)

    val rewritten = CrossSurfaceCallRewrite
      .replaceFirstArgument(NeutralTermCapabilities)(original, replacement)
      .fold(problem => fail(problem), identity)

    rewritten match
      case Apply(rebuiltFunction, rebuiltArguments) =>
        assert(rebuiltFunction.eq(function))
        assert(rebuiltArguments.head.eq(replacement))
        assert(rebuiltArguments(1).eq(keptArgument))
      case other => fail(s"expected N Apply, found $other")

    assertEquals(
      CrossSurfaceCallRewrite.topology(NeutralTermCapabilities)(rewritten),
      List("Apply", "Select(invoke)", "Identifier(service)", "Identifier(replacement)", "Identifier(keptArg)")
    )
  }

  test("the same generic algorithm replaces the first U argument and preserves raw holes") {
    withContext {
      val original = ExactUntpdStructuralRewriteProbe.parseTerm(
        "service.invoke(oldArg, keptArg)"
      )
      val (function, oldArgument, keptArgument) = original match
        case untpd.Apply(rawFunction, first :: second :: Nil) =>
          (rawFunction, first, second)
        case other => fail(s"expected U Apply, found ${other.getClass.getSimpleName}")

      given SourceFile = NoSource
      val replacement = untpd.Ident(termName("replacement"))
      val rewritten = CrossSurfaceCallRewrite
        .replaceFirstArgument(UntypedTermCapabilities)(original, replacement)
        .fold(problem => fail(problem), identity)

      rewritten match
        case untpd.Apply(rebuiltFunction, rebuiltArguments) =>
          assert(rebuiltFunction.eq(function))
          assert(rebuiltArguments.head.eq(replacement))
          assert(rebuiltArguments(1).eq(keptArgument))
          assert(!rebuiltArguments.head.eq(oldArgument))
        case other => fail(s"expected rebuilt U Apply, found ${other.getClass.getSimpleName}")

      assert(!rewritten.source.exists)
      assert(!rewritten.span.exists)
      assertEquals(
        CrossSurfaceCallRewrite.topology(UntypedTermCapabilities)(rewritten),
        List("Apply", "Select(invoke)", "Identifier(service)", "Identifier(replacement)", "Identifier(keptArg)")
      )
    }
  }

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
