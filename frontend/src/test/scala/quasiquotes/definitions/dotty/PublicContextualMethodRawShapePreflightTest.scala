package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Names.{termName, typeName}
import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.{NoSource, SourceFile}

import quasiquotes.parser.{
  DottySourceSpanAdapter,
  TermShapeInspector,
  TypeShapeInspector
}

class PublicContextualMethodRawShapePreflightTest extends munit.FunSuite:
  private val source = "def provide[A](using show: Show[A]): A = show"

  test("parser and direct constructor agree on the bounded contextual method raw shape") {
    val base = new ContextBase
    given Context = base.initialCtx
    val parsed = parseOne(source)
    val constructed = constructEquivalent()

    println(s"RAW_CONTEXTUAL_METHOD_PREFLIGHT source=$source")
    println(s"RAW_CONTEXTUAL_METHOD_PREFLIGHT parsed=${summary(parsed)}")
    println(s"RAW_CONTEXTUAL_METHOD_PREFLIGHT constructed=${summary(constructed)}")

    assertEquals(summary(constructed, includePosition = false), summary(parsed, includePosition = false))
    assert(!parsed.source.exists)
    assert(DottySourceSpanAdapter.fromTree(parsed).nonEmpty)
    assert(!constructed.source.exists)
    assertEquals(DottySourceSpanAdapter.fromTree(constructed), None)
  }

  private def parseOne(value: String): untpd.DefDef =
    val base = new ContextBase
    val reporter = new StoreReporter(null)
    given Context = base.initialCtx.fresh.setReporter(reporter)
    val parsed =
      new Parser(SourceFile.virtual("PublicContextualMethodPreflight.scala", value))
        .parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed match
      case packageDef: untpd.PackageDef =>
        assertEquals(packageDef.stats.size, 1)
        packageDef.stats.head.asInstanceOf[untpd.DefDef]
      case other =>
        fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")

  private def constructEquivalent(): untpd.DefDef =
    given SourceFile = NoSource
    val typeParameter =
      untpd
        .TypeDef(typeName("A"), untpd.TypeBoundsTree(untpd.EmptyTree, untpd.EmptyTree))
        .withMods(untpd.Modifiers(Flags.Param))
    val contextualParameter =
      untpd
        .ValDef(
          termName("show"),
          untpd.AppliedTypeTree(
            untpd.Ident(typeName("Show")),
            untpd.Ident(typeName("A")) :: Nil
          ),
          untpd.EmptyTree
        )
        .withMods(untpd.Modifiers(Flags.Param | Flags.Given))
    untpd
      .DefDef(
        termName("provide"),
        List(typeParameter :: Nil, contextualParameter :: Nil),
        untpd.Ident(typeName("A")),
        untpd.Ident(termName("show"))
      )
      .withMods(untpd.Modifiers(Flags.Method))

  private def summary(
      definition: untpd.DefDef,
      includePosition: Boolean = true
  )(using Context): String =
    val typeParameters = definition.leadingTypeParams.map { parameter =>
      s"${parameter.name}:${TypeShapeInspector.rawStructure(parameter.rhs)}:${parameter.mods.flags}"
    }
    val clauses = definition.trailingParamss.map(_.map {
      case parameter: untpd.ValDef =>
        s"${parameter.name}:${TypeShapeInspector.rawStructure(parameter.tpt)}:${parameter.mods.flags}"
      case parameter: untpd.TypeDef =>
        s"unexpected-type-${parameter.name}:${TypeShapeInspector.rawStructure(parameter.rhs)}:${parameter.mods.flags}"
    })
    val position =
      if includePosition then
        s",source=${definition.source.exists},span=${DottySourceSpanAdapter.fromTree(definition)}"
      else ""
    s"DefDef(name=${definition.name},typeParams=$typeParameters,clauses=$clauses,tpt=${TypeShapeInspector.rawStructure(definition.tpt)},rhs=${TermShapeInspector.rawStructure(definition.rhs)},flags=${definition.mods.flags},annotations=${definition.mods.annotations.size},privateWithin=${definition.mods.hasPrivateWithin}$position)"
