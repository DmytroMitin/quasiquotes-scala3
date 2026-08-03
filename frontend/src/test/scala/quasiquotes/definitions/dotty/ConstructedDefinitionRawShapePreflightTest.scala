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

class ConstructedDefinitionRawShapePreflightTest extends munit.FunSuite:
  private val fixtures = Vector(
    "def value: Int = 1",
    "val value: String = \"text\"",
    "def `type`: List[String] = if true then \"yes\" else \"no\"",
    "val `val`: Option[Int] = (1: Int)"
  )

  fixtures.foreach { source =>
    test(s"parser and direct constructor agree on exact raw definition shape: $source") {
      val base = new ContextBase
      given Context = base.initialCtx
      val parsed = parseOne(source)
      val constructed = constructEquivalent(source)

      println(s"RAW_DEFINITION_PREFLIGHT source=$source")
      println(s"RAW_DEFINITION_PREFLIGHT parsed=${summary(parsed)}")
      println(s"RAW_DEFINITION_PREFLIGHT constructed=${summary(constructed)}")

      assertEquals(summary(constructed, includePosition = false), summary(parsed, includePosition = false))
      assert(!parsed.source.exists)
      assert(DottySourceSpanAdapter.fromTree(parsed).nonEmpty)
      assert(!constructed.source.exists)
      assertEquals(DottySourceSpanAdapter.fromTree(constructed), None)
    }
  }

  private def parseOne(source: String): untpd.Tree =
    val base = new ContextBase
    val reporter = new StoreReporter(null)
    given Context = base.initialCtx.fresh.setReporter(reporter)
    val parsed =
      new Parser(SourceFile.virtual("ConstructedDefinitionPreflight.scala", source))
        .parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed match
      case packageDef: untpd.PackageDef =>
        assertEquals(packageDef.stats.size, 1)
        packageDef.stats.head
      case other =>
        fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")

  private def constructEquivalent(source: String): untpd.Tree =
    given SourceFile = NoSource

    source match
      case "def value: Int = 1" =>
        plainDef(
          "value",
          untpd.Ident(typeName("Int")),
          untpd.Number("1", untpd.NumberKind.Whole(10))
        )
      case "val value: String = \"text\"" =>
        untpd.ValDef(
          termName("value"),
          untpd.Ident(typeName("String")),
          untpd.Literal(dotty.tools.dotc.core.Constants.Constant("text"))
        )
      case "def `type`: List[String] = if true then \"yes\" else \"no\"" =>
        plainDef(
          "type",
          untpd.AppliedTypeTree(
            untpd.Ident(typeName("List")),
            untpd.Ident(typeName("String")) :: Nil
          ),
          untpd.If(
            untpd.Literal(dotty.tools.dotc.core.Constants.Constant(true)),
            untpd.Literal(dotty.tools.dotc.core.Constants.Constant("yes")),
            untpd.Literal(dotty.tools.dotc.core.Constants.Constant("no"))
          )
        )
      case "val `val`: Option[Int] = (1: Int)" =>
        untpd.ValDef(
          termName("val"),
          untpd.AppliedTypeTree(
            untpd.Ident(typeName("Option")),
            untpd.Ident(typeName("Int")) :: Nil
          ),
          untpd.Parens(
            untpd.Typed(
              untpd.Number("1", untpd.NumberKind.Whole(10)),
              untpd.Ident(typeName("Int"))
            )
          )
        )

  private def plainDef(
      name: String,
      resultType: untpd.Tree,
      body: untpd.Tree
  )(using SourceFile): untpd.DefDef =
    untpd
      .DefDef(termName(name), Nil, resultType, body)
      .withMods(untpd.Modifiers(Flags.Method))

  private def summary(
      tree: untpd.Tree,
      includePosition: Boolean = true
  )(using Context): String =
    val position =
      if includePosition then
        s",source=${tree.source.exists},span=${DottySourceSpanAdapter.fromTree(tree)}"
      else ""

    tree match
      case definition: untpd.DefDef =>
        s"DefDef(name=${definition.name},paramss=${definition.paramss.map(_.size)},tpt=${TypeShapeInspector.rawStructure(definition.tpt)},rhs=${TermShapeInspector.rawStructure(definition.rhs)},flags=${definition.mods.flags},annotations=${definition.mods.annotations.size},privateWithin=${definition.mods.hasPrivateWithin}$position)"
      case definition: untpd.ValDef =>
        s"ValDef(name=${definition.name},tpt=${TypeShapeInspector.rawStructure(definition.tpt)},rhs=${TermShapeInspector.rawStructure(definition.rhs)},flags=${definition.mods.flags},mutable=${definition.mods.is(Flags.Mutable)},lazy=${definition.mods.is(Flags.Lazy)},annotations=${definition.mods.annotations.size},privateWithin=${definition.mods.hasPrivateWithin}$position)"
      case other =>
        s"${other.getClass.getSimpleName}$position"
