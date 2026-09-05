package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags.EmptyFlags
import dotty.tools.dotc.core.Names.{termName, typeName}
import dotty.tools.dotc.core.Symbols.{NoSymbol, newSymbol}
import dotty.tools.dotc.core.Types.NoType
import dotty.tools.dotc.util.{NoSource, SourceFile}
import dotty.tools.dotc.util.Spans.Span

import quasiquotes.definitions.{DefinitionName, DefinitionShape}
import quasiquotes.parser.{TermShape, TypeShape}

import scala.util.{Success, Try}

import SimpleTypeAliasGeneratedOriginError.*

class SimpleTypeAliasGeneratedOriginAdapterTest extends munit.FunSuite:
  private val intType = TypeShape.Identifier("Int")
  private val stringType = TypeShape.Identifier("String")
  private val booleanType = TypeShape.Identifier("Boolean")

  test("positions exactly the U022 simple-alias admission set without changing raw semantics") {
    val fixtures = Vector(
      ("Alias", intType, "type Alias = Int"),
      (
        "RenamedAlias",
        TypeShape.Apply(TypeShape.Identifier("List"), List(intType)),
        "type RenamedAlias = List[Int]"
      ),
      (
        "PairAlias",
        TypeShape.Tuple(List(intType, stringType)),
        "type PairAlias = (Int, String)"
      ),
      (
        "FunctionAlias",
        TypeShape.Function(List(intType, stringType), booleanType),
        "type FunctionAlias = (Int, String) => Boolean"
      )
    )

    withContext {
      fixtures.zipWithIndex.foreach { case ((aliasName, rhs, expectedSource), index) =>
        val shape = alias(aliasName, rhs)
        val sourceFree = DefinitionShapeUntypedLowerer
          .lower(shape)
          .fold(problem => fail(problem.message), identity)
          .asInstanceOf[untpd.TypeDef]
        val result = SimpleTypeAliasGeneratedOriginAdapter
          .lower(shape, s"<quasiquotes-generated:u026-$index>")
          .fold(problem => fail(problem.message), identity)
        val positioned = result.tree.asInstanceOf[untpd.TypeDef]

        assertEquals(result.generatedSource, expectedSource)
        assertEquals(positioned.name.toString, aliasName)
        assert(positioned.name.isTypeName)
        assert(!positioned.mods.hasFlags)
        assert(!positioned.mods.hasAnnotations)
        assert(!positioned.mods.hasPrivateWithin)
        assertEquals(topology(positioned), topology(sourceFree))
        assert(!positioned.asInstanceOf[AnyRef].eq(sourceFree.asInstanceOf[AnyRef]))
        assert(!positioned.rhs.asInstanceOf[AnyRef].eq(sourceFree.rhs.asInstanceOf[AnyRef]))
        assertEquals(positioned.span.start, 0)
        assertEquals(positioned.span.end, expectedSource.length)
        assertEquals(positioned.span.point, "type ".length)

        val trees = allTrees(positioned)
        assert(trees.nonEmpty)
        trees.foreach { tree =>
          assert(tree.source.exists, clues(aliasName, tree.getClass.getSimpleName))
          assertEquals(tree.source.path, result.virtualSourceName)
          assertEquals(tree.source.content.mkString, expectedSource)
          assert(tree.span.exists, clues(aliasName, tree.getClass.getSimpleName))
          assert(tree.span.start >= 0)
          assert(tree.span.start <= tree.span.point)
          assert(tree.span.point <= tree.span.end)
          assert(tree.span.end <= expectedSource.length)
          assertEquals(tree.symbol, NoSymbol)
          assert(!tree.isInstanceOf[untpd.TypedSplice])
          directChildren(tree).foreach { child =>
            assert(child.span.start >= tree.span.start)
            assert(child.span.end <= tree.span.end)
          }
        }
      }
    }
  }

  test("renders a validated backticked alias name and points the root at its decoded spelling") {
    withContext {
      val shape = alias("`type`", intType)
      val result = SimpleTypeAliasGeneratedOriginAdapter
        .lower(shape, "<u026-backticked-name>")
        .fold(problem => fail(problem.message), identity)
      val positioned = result.tree.asInstanceOf[untpd.TypeDef]

      assertEquals(result.generatedSource, "type `type` = Int")
      assertEquals(positioned.name.toString, "type")
      assert(positioned.name.isTypeName)
      assertEquals(positioned.span.point, "type `".length)
    }
  }

  test("returns distinct semantic and virtual-source categories before positioning") {
    withContext {
      assertEquals(
        SimpleTypeAliasGeneratedOriginAdapter.lower(null, "<u026-null>"),
        Left(MissingDefinitionShape)
      )

      val ordinary = DefinitionShape
        .immutableVal(name("value"), intType, TermShape.Literal("1"))
        .fold(problem => fail(problem.message), identity)
      assert(
        SimpleTypeAliasGeneratedOriginAdapter
          .lower(ordinary, "<u026-wrong-family>")
          .left
          .toOption
          .exists(_.isInstanceOf[WrongDefinitionShapeFamily])
      )

      val nullRhs = reflectedAlias(name("NullRhs"), null)
      val unsupportedRhs = reflectedAlias(
        name("UnsupportedRhs"),
        TypeShape.Unsupported("CorruptedType", "test-only")
      )
      Vector(nullRhs, unsupportedRhs).foreach { shape =>
        assert(
          DefinitionShapeUntypedLowerer
            .lower(shape)
            .left
            .toOption
            .exists(
              _.isInstanceOf[
                DefinitionShapeUntypedLowererError.SimpleTypeAliasCompletionFailure
              ]
            )
        )
        val result = Try(
          SimpleTypeAliasGeneratedOriginAdapter.lower(shape, "<u026-semantic>")
        )
        assert(
          result match
            case Success(Left(_: AliasCompletionFailure)) => true
            case _ => false,
          clues(result)
        )
      }

      val invalidName = reflectedAlias(null, intType)
      assert(
        DefinitionShapeUntypedLowerer
          .lower(invalidName)
          .left
          .toOption
          .exists(
            _.isInstanceOf[
              DefinitionShapeUntypedLowererError.SimpleTypeAliasNameFailure
            ]
          )
      )
      assert(
        SimpleTypeAliasGeneratedOriginAdapter
          .lower(invalidName, "<u026-name>")
          .left
          .toOption
          .exists(_.isInstanceOf[AliasNameFailure])
      )
      val corruptNameShape = reflectedAlias(corruptNameSource(name("Corrupt")), intType)
      val sourceFreeCorruptName = Try(
        DefinitionShapeUntypedLowerer.lower(corruptNameShape)
      )
      assert(
        sourceFreeCorruptName match
          case Success(
                Left(
                  _: DefinitionShapeUntypedLowererError.SimpleTypeAliasNameFailure
                )
              ) => true
          case _ => false,
        clues(sourceFreeCorruptName)
      )
      val generatedCorruptName = Try(
        SimpleTypeAliasGeneratedOriginAdapter.lower(
          corruptNameShape,
          "<u026-corrupt-name>"
        )
      )
      assert(
        generatedCorruptName match
          case Success(Left(_: AliasNameFailure)) => true
          case _ => false,
        clues(generatedCorruptName)
      )
      val corruptSpellingShape = reflectedAlias(
        corruptNameSpelling(name("CorruptSpelling")),
        intType
      )
      val sourceFreeCorruptSpelling = Try(
        DefinitionShapeUntypedLowerer.lower(corruptSpellingShape)
      )
      assert(
        sourceFreeCorruptSpelling match
          case Success(
                Left(
                  _: DefinitionShapeUntypedLowererError.SimpleTypeAliasNameFailure
                )
              ) => true
          case _ => false,
        clues(sourceFreeCorruptSpelling)
      )
      val generatedCorruptSpelling = Try(
        SimpleTypeAliasGeneratedOriginAdapter.lower(
          corruptSpellingShape,
          "<u026-corrupt-spelling>"
        )
      )
      assert(
        generatedCorruptSpelling match
          case Success(Left(_: AliasNameFailure)) => true
          case _ => false,
        clues(generatedCorruptSpelling)
      )
      val unsupportedCompleted = alias(
        "WideAlias",
        TypeShape.Identifier("AnyVal")
      )
      assert(
        DefinitionShapeUntypedLowerer
          .lower(unsupportedCompleted)
          .left
          .toOption
          .exists(
            _.isInstanceOf[
              DefinitionShapeUntypedLowererError.SimpleTypeAliasCompletedTypeFailure
            ]
          )
      )
      assert(
        SimpleTypeAliasGeneratedOriginAdapter
          .lower(unsupportedCompleted, "<u026-wide>")
          .left
          .toOption
          .exists(_.isInstanceOf[CompletedTypeExactLoweringFailure])
      )

      val valid = alias("Alias", intType)
      Vector[String](null, "", " leading", "trailing ", "line\nbreak").foreach {
        sourceName =>
          val result = Try(
            SimpleTypeAliasGeneratedOriginAdapter.lower(valid, sourceName)
          )
          assert(
            result match
              case Success(Left(_: InvalidVirtualSourceName)) => true
              case _ => false,
            clues(sourceName, result)
          )
      }
    }
  }

  test("rejects corrupted generated plans and raw topology without a partial result") {
    withContext {
      val shape = alias("Alias", intType)
      val corruptPlan =
        SimpleTypeAliasGeneratedOriginAdapter.validateGeneratedSourceForTest(
          shape,
          "type Alias = String"
        )
      assert(
        corruptPlan.left.toOption
          .exists(_.isInstanceOf[GeneratedSourcePlanMismatch])
      )

      given SourceFile = NoSource
      val wrongRhs = untpd.TypeDef(
        typeName("Alias"),
        untpd.Ident(typeName("String"))
      )
      val wrongTopology =
        SimpleTypeAliasGeneratedOriginAdapter.positionRawForTest(
          wrongRhs,
          shape,
          "<u026-wrong-topology>"
        )
      assert(
        wrongTopology.left.toOption.exists(_.isInstanceOf[RawTopologyMismatch])
      )
      assert(wrongTopology.toOption.isEmpty)

      val sourcedRhs = untpd
        .Ident(typeName("Int"))
        .cloneIn(SourceFile.virtual("U026CorruptRaw.scala", "Int"))
      val sourcedResult =
        SimpleTypeAliasGeneratedOriginAdapter.positionRawForTest(
          untpd.TypeDef(typeName("Alias"), sourcedRhs),
          shape,
          "<u026-sourced-raw>"
        )
      assert(
        sourcedResult.left.toOption
          .exists(_.isInstanceOf[SourceFreeInvariantFailure])
      )
      assert(sourcedResult.toOption.isEmpty)

      val symbol = newSymbol(NoSymbol, termName("u026Symbol"), EmptyFlags, NoType)
      val symbolBearing = untpd
        .Ident(typeName("Int"))
        .withType(symbol.termRef)
      val symbolResult =
        SimpleTypeAliasGeneratedOriginAdapter.positionRawForTest(
          untpd.TypeDef(typeName("Alias"), symbolBearing),
          shape,
          "<u026-symbol>"
        )
      assert(
        symbolResult.left.toOption
          .exists(_.isInstanceOf[SourceFreeInvariantFailure])
      )
      assert(symbolResult.toOption.isEmpty)

      val typedSpliceResult =
        SimpleTypeAliasGeneratedOriginAdapter.positionRawForTest(
          untpd.TypeDef(typeName("Alias"), untpd.TypedSplice(symbolBearing)),
          shape,
          "<u026-typed-splice>"
        )
      assert(
        typedSpliceResult.left.toOption
          .exists(_.isInstanceOf[SourceFreeInvariantFailure])
      )
      assert(typedSpliceResult.toOption.isEmpty)
    }
  }

  test("rejects source and span corruption at the positioned validation seam") {
    withContext {
      val shape = alias("Alias", intType)
      val result = SimpleTypeAliasGeneratedOriginAdapter
        .lower(shape, "<u026-positioned>")
        .fold(problem => fail(problem.message), identity)
      val positioned = result.tree.asInstanceOf[untpd.TypeDef]

      val wrongSource = positioned.cloneIn(
        SourceFile.virtual("<u026-other>", result.generatedSource)
      )
      val sourceFailure =
        SimpleTypeAliasGeneratedOriginAdapter.validatePositionedForTest(
          shape,
          wrongSource,
          result
        )
      assert(
        sourceFailure.left.toOption
          .exists(_.isInstanceOf[PositionedInvariantFailure])
      )

      val invalidSpan = positioned.withSpan(
        Span(0, result.generatedSource.length + 1, positioned.span.point)
      )
      val spanFailure =
        SimpleTypeAliasGeneratedOriginAdapter.validatePositionedForTest(
          shape,
          invalidSpan,
          result
        )
      assert(
        spanFailure.left.toOption
          .exists(_.isInstanceOf[PositionedInvariantFailure])
      )

      val appliedShape = alias(
        "AppliedAlias",
        TypeShape.Apply(TypeShape.Identifier("List"), List(intType))
      )
      val appliedResult = SimpleTypeAliasGeneratedOriginAdapter
        .lower(appliedShape, "<u026-shifted-descendant>")
        .fold(problem => fail(problem.message), identity)
      val appliedRoot = appliedResult.tree.asInstanceOf[untpd.TypeDef]
      val appliedRhs = appliedRoot.rhs.asInstanceOf[untpd.AppliedTypeTree]
      val originalArgument = appliedRhs.args.head
      val shiftedArgument = originalArgument.withSpan(
        Span(
          originalArgument.span.start + 1,
          originalArgument.span.end + 1,
          originalArgument.span.point + 1
        )
      )
      val shiftedRhs = untpd
        .cpy
        .AppliedTypeTree(appliedRhs)(appliedRhs.tpt, shiftedArgument :: Nil)
        .cloneIn(appliedResult.sourceFile)
        .withSpan(appliedRhs.span)
      val shiftedRoot = untpd
        .TypeDef(appliedRoot.name, shiftedRhs)
        .withMods(appliedRoot.mods)
        .cloneIn(appliedResult.sourceFile)
        .withSpan(appliedRoot.span)
      val shiftedFailure =
        SimpleTypeAliasGeneratedOriginAdapter.validatePositionedForTest(
          appliedShape,
          shiftedRoot,
          appliedResult
        )
      assert(
        shiftedFailure.left.toOption
          .exists(_.isInstanceOf[PositionedInvariantFailure])
      )
    }
  }

  private def alias(name: String, rhs: TypeShape): DefinitionShape.SimpleTypeAlias =
    DefinitionShape
      .simpleTypeAlias(
        DefinitionName.fromSource(name).fold(problem => fail(problem.message), identity),
        rhs
      )
      .fold(problem => fail(problem.message), identity)

  private def name(source: String): DefinitionName =
    DefinitionName.fromSource(source).fold(problem => fail(problem.message), identity)

  private def reflectedAlias(
      definitionName: DefinitionName,
      rhs: TypeShape
  ): DefinitionShape.SimpleTypeAlias =
    val constructor =
      classOf[DefinitionShape.SimpleTypeAlias].getDeclaredConstructors.head
    constructor.setAccessible(true)
    constructor
      .newInstance(definitionName, rhs)
      .asInstanceOf[DefinitionShape.SimpleTypeAlias]

  private def corruptNameSource(valid: DefinitionName): DefinitionName =
    val constructor = valid.getClass.getDeclaredConstructors.head
    constructor.setAccessible(true)
    constructor
      .newInstance(valid.decoded, null, valid.spelling)
      .asInstanceOf[DefinitionName]

  private def corruptNameSpelling(valid: DefinitionName): DefinitionName =
    val constructor = valid.getClass.getDeclaredConstructors.head
    constructor.setAccessible(true)
    constructor
      .newInstance(valid.decoded, valid.source, null)
      .asInstanceOf[DefinitionName]

  private def topology(tree: untpd.Tree)(using Context): String =
    tree match
      case value: untpd.TypeDef =>
        s"TypeDef(${value.name},${value.mods.flags},${topology(value.rhs)})"
      case value: untpd.AppliedTypeTree =>
        s"Applied(${topology(value.tpt)},${value.args.map(topology)})"
      case value: untpd.Tuple => s"Tuple(${value.trees.map(topology)})"
      case value: untpd.Function =>
        s"Function(${value.args.map(topology)},${topology(value.body)})"
      case value: untpd.Ident => s"Ident(${value.name})"
      case other => other.getClass.getSimpleName

  private def allTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree +: directChildren(tree).flatMap(allTrees)

  private def directChildren(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree match
      case value: untpd.TypeDef => Vector(value.rhs)
      case value: untpd.AppliedTypeTree => value.tpt +: value.args.toVector
      case value: untpd.Tuple => value.trees.toVector
      case value: untpd.Function => value.args.toVector :+ value.body
      case _ => Vector.empty

  private def withContext(body: Context ?=> Unit): Unit =
    val base = new ContextBase
    given Context = base.initialCtx
    body
