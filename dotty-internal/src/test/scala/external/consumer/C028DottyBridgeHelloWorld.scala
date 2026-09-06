package external.consumer

// snippet:dotty-source-free:start
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}

import quasiquotes.definitions.dotty.{DefinitionUntypedLowering, ScalametaDefinitionUntypedBridge}
import quasiquotes.neutral.*
import quasiquotes.terms.dotty.{ScalametaTermUntypedBridge, TermUntypedLowering}
import quasiquotes.types.dotty.{ScalametaTypeUntypedBridge, TypeUntypedLowering}

import scala.meta.*
import scala.meta.dialects.Scala3

object DottySourceFreeHelloWorld:
  def check(): Unit = withContext:
    val sourceTerm = q"1 + 2"
    val termShape = ScalametaTermProjection
      .project(sourceTerm)
      .fold(error => sys.error(error.message), _.shape)
    val term = TermUntypedLowering
      .lower(termShape)
      .fold(error => sys.error(s"${error.code}: ${error.detail}"), identity)
    val bridgedTerm = ScalametaTermUntypedBridge
      .lower(sourceTerm)
      .fold(error => sys.error(s"${error.code}: ${error.detail}"), identity)

    val sourceType = t"List[Int]"
    val normalForm = ScalametaTypeNormalFormProjection
      .project(sourceType)
      .fold(error => sys.error(error.message), _.normalForm)
    val loweredType = TypeUntypedLowering
      .lower(normalForm)
      .fold(error => sys.error(s"${error.code}: ${error.detail}"), identity)
    val bridgedType = ScalametaTypeUntypedBridge
      .lower(sourceType)
      .fold(error => sys.error(s"${error.code}: ${error.detail}"), identity)

    val sourceDefinitions = Vector(
      "val foo: Int = 42",
      "def foo(x: Int): String = x.toString",
      "type T = Int"
    ).map(parseDefinition)
    val semanticDefinitions = sourceDefinitions.map { source =>
      ScalametaDefinitionProjection
        .project(source)
        .fold(error => sys.error(error.message), _.definition)
    }
    val authoredDefinitions = semanticDefinitions.map { definition =>
      ScalametaDefinitionAuthoring
        .author(definition)
        .fold(error => sys.error(error.message), identity)
    }
    val loweredDefinitions = semanticDefinitions.map { definition =>
      DefinitionUntypedLowering
        .lower(definition)
        .fold(error => sys.error(s"${error.code}: ${error.detail}"), identity)
    }
    val bridgedDefinitions = authoredDefinitions.map { source =>
      ScalametaDefinitionUntypedBridge
        .lower(source)
        .fold(error => sys.error(s"${error.code}: ${error.detail}"), identity)
    }

    assert(term.isInstanceOf[untpd.InfixOp])
    assert(bridgedTerm.isInstanceOf[untpd.InfixOp])
    assert(loweredType.isInstanceOf[untpd.AppliedTypeTree])
    assert(bridgedType.isInstanceOf[untpd.AppliedTypeTree])
    assert(loweredDefinitions.map(_.name.toString) == Vector("foo", "foo", "T"))
    assert(bridgedDefinitions.map(_.name.toString) == Vector("foo", "foo", "T"))
    assert(loweredDefinitions(0).isInstanceOf[untpd.ValDef])
    assert(loweredDefinitions(1).isInstanceOf[untpd.DefDef])
    assert(loweredDefinitions(2).isInstanceOf[untpd.TypeDef])
    (Vector(term, bridgedTerm, loweredType, bridgedType) ++ loweredDefinitions ++ bridgedDefinitions)
      .foreach { tree =>
        assert(!tree.source.exists)
        assert(!tree.span.exists)
      }

  private def parseDefinition(source: String): Defn =
    Scala3(source).parse[Stat].get.asInstanceOf[Defn]

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
// snippet:dotty-source-free:end

// snippet:dotty-generated-origin:start
import _root_.quasiquotes.definitions.dotty.ScalametaDefinitionGeneratedOriginBridge
import _root_.quasiquotes.terms.dotty.ScalametaTermGeneratedOriginBridge

object DottyGeneratedOriginHelloWorld:
  def check(): Unit = withContext:
    val term = ScalametaTermGeneratedOriginBridge
      .lower(q"1 + 2", "SemanticGuideGeneratedTerm.scala")
      .fold(error => sys.error(s"${error.code}: ${error.detail}"), identity)
    val definition = ScalametaDefinitionGeneratedOriginBridge
      .lower(
        q"def foo(x: Int): String = x.toString".asInstanceOf[Defn.Def],
        "SemanticGuideGeneratedDefinition.scala"
      )
      .fold(error => sys.error(s"${error.code}: ${error.detail}"), identity)
    val aliasFailure = ScalametaDefinitionGeneratedOriginBridge
      .lower(q"type T = Int".asInstanceOf[Defn.Type], "SemanticGuideGeneratedAlias.scala")
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
// snippet:dotty-generated-origin:end

// snippet:generic-specialized-definition:start
import _root_.quasiquotes.definitions.dotty.ContextualMethodPeerBridge

object GenericVsSpecializedDefinitionHelloWorld:
  def check(): Unit = withContext:
    val contextual =
      q"def apply[A](using inst: Show[A]): Show[A] = inst".asInstanceOf[Defn.Def]

    val genericFailure = ScalametaDefinitionUntypedBridge
      .lower(contextual)
      .left
      .toOption
      .get
    val specialized = ContextualMethodPeerBridge
      .lower(contextual, "SemanticGuideContextualApply.scala")
      .fold(error => sys.error(s"${error.code}: ${error.detail}"), identity)

    assert(genericFailure.code == "NEUTRAL_PROJECTION_FAILED")
    assert(specialized.tree.name.toString == "apply")
    assert(specialized.tree.source.path == specialized.virtualSourceName)

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
// snippet:generic-specialized-definition:end

final class C028DottyBridgeHelloWorldTest extends munit.FunSuite:
  test("source-free semantic lowering and bridge hello world"):
    DottySourceFreeHelloWorld.check()

  test("generated-origin bridge hello world"):
    DottyGeneratedOriginHelloWorld.check()

  test("generic versus specialized Definition hello world"):
    GenericVsSpecializedDefinitionHelloWorld.check()
