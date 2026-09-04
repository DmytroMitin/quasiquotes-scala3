package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.SourceFile

import quasiquotes.definitions.{DefinitionName, DefinitionShape}
import quasiquotes.parser.{TinyTypeParser, TypeShapeInspector}

class DefinitionShapeSimpleTypeAliasParserOracleTest extends munit.FunSuite:
  private val fixtures = Vector(
    ("Alias", "Int", "Ident(Int)"),
    ("Text", "String", "Ident(String)"),
    ("Flag", "Boolean", "Ident(Boolean)"),
    (
      "Nested",
      "List[Option[Int]]",
      "AppliedTypeTree(Ident(List), [AppliedTypeTree(Ident(Option), [Ident(Int)])])"
    ),
    ("Pair", "(Int, String)", "Tuple([Ident(Int), Ident(String)])"),
    ("Mapper", "Int => String", "Function([Ident(Int)], Ident(String))")
  )

  test("records parser topology and exact source-free lowering for simple, applied, tuple, and function aliases") {
    withContext {
      fixtures.foreach { (aliasName, rhsSource, expectedRhsStructure) =>
        val parsed = parseOne(s"type $aliasName = $rhsSource")
        val parsedStructure = TypeShapeInspector.rawStructure(parsed.rhs)
        assertEquals(parsedStructure, expectedRhsStructure, clues(aliasName))
        assertEquals(parsed.name.toString, aliasName)
        assert(parsed.name.isTypeName)
        assert(!parsed.mods.hasFlags)
        assert(!parsed.mods.hasAnnotations)
        assert(!parsed.mods.hasPrivateWithin)
        allTrees(parsed).foreach { tree =>
          assert(tree.span.exists, clues(aliasName, tree.getClass.getSimpleName))
          assertEquals(tree.symbol, NoSymbol)
        }

        val semanticRhs = TinyTypeParser.parseOrThrow(rhsSource).shape
        val semanticAlias = DefinitionShape
          .simpleTypeAlias(name(aliasName), semanticRhs)
          .fold(error => fail(error.message), identity)
        val lowered = DefinitionShapeUntypedLowerer
          .lower(semanticAlias)
          .fold(error => fail(error.message), identity)
          .asInstanceOf[untpd.TypeDef]

        assertEquals(lowered.name.toString, parsed.name.toString)
        assertEquals(
          TypeShapeInspector.rawStructure(lowered.rhs),
          parsedStructure,
          clues(aliasName)
        )
        allTrees(lowered).foreach { tree =>
          assert(!tree.source.exists, clues(aliasName, tree.getClass.getSimpleName))
          assert(!tree.span.exists, clues(aliasName, tree.getClass.getSimpleName))
          assertEquals(tree.symbol, NoSymbol)
          assert(!tree.isInstanceOf[untpd.TypedSplice])
        }
      }
    }
  }

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)

  private def parseOne(source: String)(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    given Context = outerContext.fresh.setReporter(reporter)
    val parsed = new Parser(SourceFile.virtual("U022AliasOracle.scala", source)).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed match
      case packageDef: untpd.PackageDef =>
        packageDef.stats match
          case (alias: untpd.TypeDef) :: Nil => alias
          case other => fail(s"expected one TypeDef, found $other")
      case other => fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")

  private def name(source: String): DefinitionName =
    DefinitionName.fromSource(source).fold(error => fail(error.message), identity)

  private def allTrees(tree: untpd.Tree): Vector[untpd.Tree] =
    tree +: (tree match
      case value: untpd.TypeDef => allTrees(value.rhs)
      case value: untpd.AppliedTypeTree =>
        allTrees(value.tpt) ++ value.args.toVector.flatMap(allTrees)
      case value: untpd.Tuple => value.trees.toVector.flatMap(allTrees)
      case value: untpd.Function =>
        value.args.toVector.flatMap(allTrees) ++ allTrees(value.body)
      case value: untpd.Parens => allTrees(value.t)
      case _ => Vector.empty)
