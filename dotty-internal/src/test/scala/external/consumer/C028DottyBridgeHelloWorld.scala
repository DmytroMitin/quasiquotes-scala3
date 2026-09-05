package external.consumer

// snippet:c028-dotty-source-free:start
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}

import quasiquotes.definitions.dotty.ScalametaDefinitionUntypedBridge
import quasiquotes.terms.dotty.ScalametaTermUntypedBridge
import quasiquotes.types.dotty.ScalametaTypeUntypedBridge

import scala.meta.*
import scala.meta.dialects.Scala3

object C028DottySourceFreeHelloWorld:
  def check(): Unit = withContext:
    val term = ScalametaTermUntypedBridge
      .lower(q"1 + 2")
      .fold(error => sys.error(s"${error.code}: ${error.detail}"), identity)
    val sourceType = ScalametaTypeUntypedBridge
      .lower(t"List[Int]")
      .fold(error => sys.error(s"${error.code}: ${error.detail}"), identity)
    val definitions = Vector(
      "val foo: Int = 42",
      "def foo(x: Int): String = x.toString",
      "type T = Int"
    ).map(parseDefinition).map { definition =>
      ScalametaDefinitionUntypedBridge
        .lower(definition)
        .fold(error => sys.error(s"${error.code}: ${error.detail}"), identity)
    }

    assert(term.isInstanceOf[untpd.InfixOp])
    assert(sourceType.isInstanceOf[untpd.AppliedTypeTree])
    assert(definitions.map(_.name.toString) == Vector("foo", "foo", "T"))
    (term +: sourceType +: definitions).foreach { tree =>
      assert(!tree.source.exists)
      assert(!tree.span.exists)
    }

  private def parseDefinition(source: String): Defn =
    Scala3(source).parse[Stat].get.asInstanceOf[Defn]

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
// snippet:c028-dotty-source-free:end

// snippet:c028-dotty-generated-origin:start
import _root_.quasiquotes.definitions.dotty.ScalametaDefinitionGeneratedOriginBridge
import _root_.quasiquotes.terms.dotty.ScalametaTermGeneratedOriginBridge

object C028DottyGeneratedOriginHelloWorld:
  def check(): Unit = withContext:
    val term = ScalametaTermGeneratedOriginBridge
      .lower(q"1 + 2", "C028GeneratedTerm.scala")
      .fold(error => sys.error(s"${error.code}: ${error.detail}"), identity)
    val definition = ScalametaDefinitionGeneratedOriginBridge
      .lower(
        q"def foo(x: Int): String = x.toString".asInstanceOf[Defn.Def],
        "C028GeneratedDefinition.scala"
      )
      .fold(error => sys.error(s"${error.code}: ${error.detail}"), identity)
    val aliasFailure = ScalametaDefinitionGeneratedOriginBridge
      .lower(q"type T = Int".asInstanceOf[Defn.Type], "C028GeneratedAlias.scala")
      .left
      .toOption
      .get

    assert(term.tree.source.path == term.virtualSourceName)
    assert(term.tree.span.start == 0 && term.tree.span.end == term.generatedSource.length)
    assert(definition.tree.source.path == definition.virtualSourceName)
    assert(
      definition.tree.span.start == 0 &&
        definition.tree.span.end == definition.generatedSource.length
    )
    assert(aliasFailure.code == "GENERATED_ORIGIN_FAMILY_UNSUPPORTED")

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
// snippet:c028-dotty-generated-origin:end

// snippet:c028-generic-specialized-definition:start
import _root_.quasiquotes.definitions.dotty.ContextualMethodPeerBridge

object C028GenericVsSpecializedDefinitionHelloWorld:
  def check(): Unit = withContext:
    val contextual =
      q"def apply[A](using inst: Show[A]): Show[A] = inst".asInstanceOf[Defn.Def]

    val genericFailure = ScalametaDefinitionUntypedBridge
      .lower(contextual)
      .left
      .toOption
      .get
    val specialized = ContextualMethodPeerBridge
      .lower(contextual, "C028ContextualApply.scala")
      .fold(error => sys.error(s"${error.code}: ${error.detail}"), identity)

    assert(genericFailure.code == "NEUTRAL_PROJECTION_FAILED")
    assert(specialized.tree.name.toString == "apply")
    assert(specialized.tree.source.path == specialized.virtualSourceName)

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
// snippet:c028-generic-specialized-definition:end

final class C028DottyBridgeHelloWorldTest extends munit.FunSuite:
  test("C028 source-free bridge hello world"):
    C028DottySourceFreeHelloWorld.check()

  test("C028 generated-origin bridge hello world"):
    C028DottyGeneratedOriginHelloWorld.check()

  test("C028 generic versus specialized Definition hello world"):
    C028GenericVsSpecializedDefinitionHelloWorld.check()
