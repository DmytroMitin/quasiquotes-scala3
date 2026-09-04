package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags.EmptyFlags
import dotty.tools.dotc.core.Names.{termName, typeName}
import dotty.tools.dotc.core.Symbols.{NoSymbol, newSymbol}
import dotty.tools.dotc.core.Types.NoType
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.Spans.Span

import quasiquotes.definitions.{
  ConstructedDefinition,
  DefinitionConstructionError,
  DefinitionName,
  DefinitionShape
}
import quasiquotes.parser.{BinderId, TermShape, TypeShape, TypeShapeInspector}

import scala.util.{Success, Try}

import DefinitionShapeUntypedLowererError.*

class DefinitionShapeUntypedLowererTest extends munit.FunSuite:
  private val intType = TypeShape.Identifier("Int")
  private val stringType = TypeShape.Identifier("String")
  private val valueName = name("value")
  private val methodName = name("method")
  private val xName = name("x")
  private val yName = name("y")

  test("dispatches the four ordinary families through their existing authorities and lowers the fifth as a simple TypeDef") {
    withContext {
      val firstBinder = BinderId(1)
      val secondBinder = BinderId(2)
      val ordinary = Vector[DefinitionShape](
        DefinitionShape.immutableVal(valueName, intType, TermShape.Literal("1")).toOption.get,
        DefinitionShape.parameterlessDef(methodName, intType, TermShape.Literal("1")).toOption.get,
        DefinitionShape
          .singleParameterDef(
            methodName,
            firstBinder,
            xName,
            intType,
            intType,
            TermShape.BoundReference(firstBinder, "x")
          )
          .toOption
          .get,
        DefinitionShape
          .twoParameterDef(
            methodName,
            firstBinder,
            xName,
            intType,
            secondBinder,
            yName,
            stringType,
            TypeShape.Tuple(List(intType, stringType)),
            TermShape.Tuple(
              List(
                TermShape.BoundReference(firstBinder, "x"),
                TermShape.BoundReference(secondBinder, "y")
              )
            )
          )
          .toOption
          .get
      )

      ordinary.foreach { shape =>
        val constructed = ConstructedDefinition
          .fromShape(shape)
          .fold(error => fail(error.message), identity)
        val expected = ConstructedDefinitionUntypedBackend
          .lower(constructed)
          .fold(error => fail(error.message), identity)
        val actual = DefinitionShapeUntypedLowerer
          .lower(shape)
          .fold(error => fail(error.message), identity)

        assertEquals(structure(actual), structure(expected), clues(shape.render))
      }

      val aliasShape = DefinitionShape
        .simpleTypeAlias(
          name("Alias"),
          TypeShape.Apply(TypeShape.Identifier("List"), List(intType))
        )
        .toOption
        .get
      val alias = DefinitionShapeUntypedLowerer
        .lower(aliasShape)
        .fold(error => fail(error.message), identity)
        .asInstanceOf[untpd.TypeDef]

      assertEquals(alias.name.toString, "Alias")
      assert(alias.name.isTypeName)
      assert(!alias.mods.hasFlags)
      assert(!alias.mods.hasAnnotations)
      assert(!alias.mods.hasPrivateWithin)
      assertEquals(
        TypeShapeInspector.rawStructure(alias.rhs),
        "AppliedTypeTree(Ident(List), [Ident(Int)])"
      )
    }
  }

  test("reports null, ordinary completion, ordinary backend, alias completion, alias name, and alias backend categories") {
    withContext {
      assertEquals(
        DefinitionShapeUntypedLowerer.lower(null),
        Left(MissingDefinitionShape)
      )

      val ordinaryCompletionFailure = reflectedImmutableVal(
        valueName,
        TypeShape.Unsupported("CorruptedType", "test-only"),
        TermShape.Literal("1")
      )
      assert(
        DefinitionShapeUntypedLowerer
          .lower(ordinaryCompletionFailure)
          .left
          .toOption
          .get
          .isInstanceOf[OrdinaryDefinitionCompletionFailure]
      )

      val ordinaryBackendFailure = reflectedImmutableVal(
        null,
        intType,
        TermShape.Literal("1")
      )
      assert(
        DefinitionShapeUntypedLowerer
          .lower(ordinaryBackendFailure)
          .left
          .toOption
          .get
          .isInstanceOf[OrdinaryDefinitionExactBackendFailure]
      )

      val aliasCompletionFailure = reflectedAlias(
        name("BrokenAlias"),
        TypeShape.Unsupported("CorruptedType", "test-only")
      )
      assert(
        DefinitionShapeUntypedLowerer
          .lower(aliasCompletionFailure)
          .left
          .toOption
          .get
          .isInstanceOf[SimpleTypeAliasCompletionFailure]
      )

      val aliasNameFailure = reflectedAlias(null, intType)
      assert(
        DefinitionShapeUntypedLowerer
          .lower(aliasNameFailure)
          .left
          .toOption
          .get
          .isInstanceOf[SimpleTypeAliasNameFailure]
      )

      val aliasBackendFailure = DefinitionShape
        .simpleTypeAlias(name("WideAlias"), TypeShape.Identifier("AnyVal"))
        .toOption
        .get
      assert(
        DefinitionShapeUntypedLowerer
          .lower(aliasBackendFailure)
          .left
          .toOption
          .get
          .isInstanceOf[SimpleTypeAliasCompletedTypeFailure]
      )
    }
  }

  test("rejects forged null bodies for all four ordinary families through the completion category") {
    withContext {
      val firstBinder = BinderId(1)
      val secondBinder = BinderId(2)
      val forged = Vector[DefinitionShape](
        reflectedDefinition(
          classOf[DefinitionShape.ImmutableVal],
          valueName,
          intType,
          null
        ),
        reflectedDefinition(
          classOf[DefinitionShape.ParameterlessDef],
          methodName,
          intType,
          null
        ),
        reflectedDefinition(
          classOf[DefinitionShape.SingleParameterDef],
          methodName,
          firstBinder,
          xName,
          intType,
          intType,
          null
        ),
        reflectedDefinition(
          classOf[DefinitionShape.TwoParameterDef],
          methodName,
          firstBinder,
          xName,
          intType,
          secondBinder,
          yName,
          intType,
          intType,
          null
        )
      )

      val results = forged.map(shape => Try(DefinitionShapeUntypedLowerer.lower(shape)))

      results.foreach { result =>
        assert(
          result match
            case Success(
                  Left(
                    OrdinaryDefinitionCompletionFailure(
                      _: DefinitionConstructionError.UnsupportedParsedDefinitionBody
                    )
                  )
                ) => true
            case _ => false,
          clues(result)
        )
      }
    }
  }

  test("rejects sourced, spanned, symbol-bearing, and TypedSplice descendants recursively") {
    withContext {
      val sourced = untpd
        .Ident(typeName("Int"))
        .cloneIn(SourceFile.virtual("U022Sourced.scala", "Int"))
      assertRawInvariantFailure(untpd.TypeDef(typeName("Alias"), sourced), "source")

      val spanned = untpd.Ident(typeName("Int")).withSpan(Span(0, 1, 0))
      assertRawInvariantFailure(untpd.TypeDef(typeName("Alias"), spanned), "span")

      val symbol = newSymbol(NoSymbol, termName("u022Symbol"), EmptyFlags, NoType)
      val symbolBearing = untpd.Ident(termName("symbolBearing")).withType(symbol.termRef)
      assertRawInvariantFailure(
        untpd.ValDef(termName("value"), untpd.Ident(typeName("Int")), symbolBearing),
        "symbol"
      )

      assertRawInvariantFailure(
        untpd.ValDef(
          termName("value"),
          untpd.Ident(typeName("Int")),
          untpd.TypedSplice(symbolBearing)
        ),
        "TypedSplice"
      )
    }
  }

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)

  private def name(source: String) =
    DefinitionName.fromSource(source).fold(error => fail(error.message), identity)

  private def reflectedImmutableVal(
      definitionName: DefinitionName,
      declaredType: TypeShape,
      rhs: TermShape
  ): DefinitionShape =
    val constructor =
      classOf[DefinitionShape.ImmutableVal].getDeclaredConstructors.head
    constructor.setAccessible(true)
    constructor
      .newInstance(definitionName, declaredType, rhs)
      .asInstanceOf[DefinitionShape]

  private def reflectedAlias(
      definitionName: DefinitionName,
      rhs: TypeShape
  ): DefinitionShape =
    val constructor =
      classOf[DefinitionShape.SimpleTypeAlias].getDeclaredConstructors.head
    constructor.setAccessible(true)
    constructor
      .newInstance(definitionName, rhs)
      .asInstanceOf[DefinitionShape]

  private def reflectedDefinition[A <: DefinitionShape](
      definitionClass: Class[A],
      arguments: AnyRef*
  ): DefinitionShape =
    val constructor = definitionClass.getDeclaredConstructors.head
    constructor.setAccessible(true)
    constructor.newInstance(arguments*).asInstanceOf[DefinitionShape]

  private def assertRawInvariantFailure(
      raw: untpd.Tree,
      expectedDetail: String
  )(using Context): Unit =
    DefinitionShapeUntypedLowerer
      .validateRawInvariant(raw, "test")
      .fold(
        {
          case RawInvariantFailure("test", detail) =>
            assert(detail.contains(expectedDetail), clues(detail, expectedDetail))
          case other => fail(other.message)
        },
        _ => fail(s"expected raw invariant failure containing $expectedDetail")
      )

  private def structure(tree: untpd.Tree)(using Context): String =
    tree match
      case value: untpd.DefDef =>
        s"DefDef(${value.name},${value.mods.flags},${value.paramss.map(_.map(structure))},${structure(value.tpt)},${structure(value.rhs)})"
      case value: untpd.ValDef =>
        s"ValDef(${value.name},${value.mods.flags},${structure(value.tpt)},${structure(value.rhs)})"
      case value: untpd.TypeDef =>
        s"TypeDef(${value.name},${value.mods.flags},${structure(value.rhs)})"
      case value: untpd.Select => s"Select(${structure(value.qualifier)},${value.name})"
      case value: untpd.Apply => s"Apply(${structure(value.fun)},${value.args.map(structure)})"
      case value: untpd.InfixOp =>
        s"Infix(${structure(value.left)},${structure(value.op)},${structure(value.right)})"
      case value: untpd.Typed => s"Typed(${structure(value.expr)},${structure(value.tpt)})"
      case value: untpd.AppliedTypeTree =>
        s"Applied(${structure(value.tpt)},${value.args.map(structure)})"
      case value: untpd.Tuple => s"Tuple(${value.trees.map(structure)})"
      case value: untpd.Function =>
        s"Function(${value.args.map(structure)},${structure(value.body)})"
      case value: untpd.Ident => s"Ident(${value.name})"
      case value: untpd.Number => s"Number(${value.digits},${value.kind})"
      case value: untpd.Literal => s"Literal(${value.const})"
      case value if value.isEmpty => "Empty"
      case other => other.getClass.getSimpleName
