package quasiquotes.definitions.dotty

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.{NoSource, SourceFile}
import dotty.tools.dotc.util.Spans.Span

import quasiquotes.definitions.{ConstructedDefinition, DefinitionName}
import quasiquotes.parser.{
  TermShape,
  TermShapeInspector,
  TinyTermParser,
  TinyTypeParser,
  TypeShapeInspector
}
import quasiquotes.terms.ConstructedTerm
import quasiquotes.terms.dotty.GeneratedOriginFragmentSupport
import quasiquotes.types.TypeNormalForm

class ConstructedDefinitionGeneratedOriginAdapterTest extends munit.FunSuite:
  import ConstructedDefinitionGeneratedOriginError.*
  import TypeNormalForm.*

  private val typeFamilies = Vector(
    "Int" -> STypeIdent("Int"),
    "String" -> STypeIdent("String"),
    "Boolean" -> STypeIdent("Boolean"),
    "List[Int]" -> STypeApply(STypeIdent("List"), List(STypeIdent("Int"))),
    "Option[String]" ->
      STypeApply(STypeIdent("Option"), List(STypeIdent("String"))),
    "(Int, String)" ->
      STypeTuple(List(STypeIdent("Int"), STypeIdent("String"))),
    "(Int, String, Boolean)" ->
      STypeTuple(
        List(STypeIdent("Int"), STypeIdent("String"), STypeIdent("Boolean"))
      ),
    "Int => String" ->
      STypeFunction(List(STypeIdent("Int")), STypeIdent("String")),
    "(Int, String) => Boolean" ->
      STypeFunction(
        List(STypeIdent("Int"), STypeIdent("String")),
        STypeIdent("Boolean")
      )
  )

  test("emits the exact four preflight sources and parser-observed root conventions") {
    val fixtures = Vector(
      (
        method(plain("value"), STypeIdent("Int"), term("1")),
        "def value: Int = 1",
        4,
        11,
        14,
        17,
        18
      ),
      (
        value(plain("value"), STypeIdent("String"), term("\"text\"")),
        "val value: String = \"text\"",
        4,
        11,
        17,
        20,
        26
      ),
      (
        method(
          keyword("`type`"),
          STypeApply(STypeIdent("List"), List(STypeIdent("String"))),
          term("if true then \"yes\" else \"no\"")
        ),
        "def `type`: List[String] = if true then \"yes\" else \"no\"",
        5,
        12,
        24,
        27,
        55
      ),
      (
        value(
          keyword("`val`"),
          STypeApply(STypeIdent("Option"), List(STypeIdent("Int"))),
          term("(1: Int)")
        ),
        "val `val`: Option[Int] = (1: Int)",
        5,
        11,
        22,
        25,
        33
      )
    )

    withContext {
      fixtures.zipWithIndex.foreach {
        case ((constructed, source, point, typeStart, typeEnd, bodyStart, bodyEnd), index) =>
          val result =
            lower(constructed, s"<generated-definition-basic-$index>")
          assertEquals(result.generatedSource, source)
          assertSpan(result.tree, 0, source.length, point)
          val (definitionType, body) = children(result.tree)
          assertEquals(definitionType.span.start, typeStart)
          assertEquals(definitionType.span.end, typeEnd)
          assertEquals(body.span.start, bodyStart)
          assertEquals(body.span.end, bodyEnd)
          assertComplete(result)
          assertRawAgreement(result.tree, constructed)
      }
    }
  }

  test("both definition variants reuse every admitted type fragment family") {
    withContext {
      typeFamilies.zipWithIndex.foreach { case ((source, normalForm), index) =>
        val definitions = Vector(
          method(plain(s"method$index"), normalForm, term("1")),
          value(plain(s"value$index"), normalForm, term("1"))
        )
        definitions.zipWithIndex.foreach { case (constructed, variant) =>
          val result =
            lower(
              constructed,
              s"<generated-definition-type-$index-$variant>"
            )
          val (definitionType, _) = children(result.tree)
          assertEquals(
            TypeShapeInspector.rawStructure(definitionType),
            TinyTypeParser.parseOrThrow(source).rawStructure
          )
          assert(result.generatedSource.contains(s": $source = "))
          assertComplete(result)
          assertRawAgreement(result.tree, constructed)
        }
      }
    }
  }

  test("definition bodies retain application selection infix tuple if sidecars and unary boundaries") {
    val outerType =
      STypeTuple(
        List(STypeIdent("Int"), STypeIdent("String"), STypeIdent("Boolean"))
      )
    val firstInner =
      STypeApply(STypeIdent("List"), List(STypeIdent("Int")))
    val secondInner =
      STypeFunction(
        List(STypeIdent("Int"), STypeIdent("String")),
        STypeIdent("Boolean")
      )
    val root =
      TermShape.If(
        ident("ready"),
        TermShape.Apply(
          TermShape.Select(ident("service"), "compute"),
          List(
            TermShape.Typed(ident("left"), renderType(firstInner)),
            TermShape.Infix(ident("right"), "+", TermShape.Literal("1"))
          )
        ),
        TermShape.Tuple(
          List(
            TermShape.Typed(ident("fallback"), renderType(secondInner)),
            TermShape.Unary("-", TermShape.Literal("-1"))
          )
        )
      )
    val body =
      ConstructedTerm
        .create(root, Vector(firstInner, secondInner))
        .toOption
        .get
    val constructed =
      method(keyword("`type`"), outerType, body)

    withContext {
      val result =
        lower(constructed, "<generated-definition-complete-body>")
      assertEquals(
        result.generatedSource,
        "def `type`: (Int, String, Boolean) = if ready then service.compute((left): List[Int], right + 1) else ((fallback): ((Int, String) => Boolean), -(-1))"
      )
      assertComplete(result)
      assertRawAgreement(result.tree, constructed)
      val (_, positionedBody) = children(result.tree)
      assertEquals(TermShapeInspector.inspect(positionedBody), root)
    }
  }

  test("positions Tuple2 Tuple3 Tuple22 repeated text and escaped UTF-16 strings") {
    val semantic =
      "\"quote=\" slash=\\ newline=\n BMP=λ supplementary=😀\""
    val repeated = TermShape.Literal("\"same\"")
    val bodies = Vector(
      TermShape.Tuple(List(ident("a"), ident("b"))),
      TermShape.Tuple(List(ident("a"), ident("b"), ident("c"))),
      TermShape.Tuple((1 to 22).map(index => ident(s"value$index")).toList),
      TermShape.Apply(ident("f"), List(repeated, repeated, repeated)),
      TermShape.Literal(semantic),
      TermShape.Literal("-214748364900000000000000000000"),
      TermShape.Literal("true")
    )

    withContext {
      bodies.zipWithIndex.foreach { case (shape, index) =>
        val constructed =
          value(
            plain(s"value$index"),
            STypeIdent("String"),
            fromShape(shape)
          )
        val result =
          lower(constructed, s"<generated-definition-boundary-$index>")
        assertComplete(result)
        assertRawAgreement(result.tree, constructed)
        assertEquals(result.tree.span.end, result.generatedSource.length)
      }
    }
  }

  test("complete keyword-name definition map has exact named descendant spans and no fake name child") {
    val constructed =
      method(
        keyword("`type`"),
        STypeApply(STypeIdent("List"), List(STypeIdent("String"))),
        term("if true then \"yes\" else \"no\"")
      )

    withContext {
      val result =
        lower(constructed, "<generated-definition-map>")
      result.tree match
        case root @ untpd.DefDef(name, paramss, applied @ untpd.AppliedTypeTree(
              constructor,
              argument :: Nil
            ), conditional @ untpd.If(condition, thenBranch, elseBranch)) =>
          assertEquals(name.toString, "type")
          assertEquals(paramss, Nil)
          assertSpan(root, 0, 55, 5)
          assertSpan(applied, 12, 24, 12)
          assertSpan(constructor, 12, 16, 12)
          assertSpan(argument, 17, 23, 17)
          assertSpan(conditional, 27, 55, 27)
          assertSpan(condition, 30, 34, 30)
          assertSpan(thenBranch, 40, 45, 40)
          assertSpan(elseBranch, 51, 55, 51)
          assertEquals(
            GeneratedOriginFragmentSupport.directChildren(root),
            Vector(applied, conditional)
          )
          assert(
            !GeneratedOriginFragmentSupport
              .allTrees(root)
              .exists {
                case ident: untpd.Ident => ident.name.toString == "type"
                case _ => false
              }
          )
        case other =>
          fail(s"unexpected positioned tree: ${other.getClass.getSimpleName}")
    }
  }

  test("ordinary and defensive failures use stable definition-specific categories") {
    withContext {
      Vector("", " whitespace ", "bad\nname", "bad\rname", "bad\u0000name")
        .foreach { name =>
          assert(
            ConstructedDefinitionGeneratedOriginAdapter
              .lower(
                method(plain("value"), STypeIdent("Int"), term("1")),
                name
              )
              .left
              .toOption
              .exists(_.isInstanceOf[InvalidVirtualSourceName])
          )
        }

      assert(
        ConstructedDefinitionGeneratedOriginAdapter
          .lower(
            reflectedMethod(null, STypeIdent("Int"), term("1")),
            "<generated-definition-null-name>"
          )
          .left
          .toOption
          .exists(_.isInstanceOf[DefinitionNameRenderingFailure])
      )
      assert(
        ConstructedDefinitionGeneratedOriginAdapter
          .lower(
            reflectedMethod(
              plain("value"),
              STypeIdent("AnyVal"),
              term("1")
            ),
            "<generated-definition-invalid-type>"
          )
          .left
          .toOption
          .exists(_.isInstanceOf[DefinitionTypePlanningFailure])
      )
      assert(
        ConstructedDefinitionGeneratedOriginAdapter
          .lower(
            reflectedMethod(
              plain("value"),
              STypeIdent("Int"),
              reflectedTerm(
                TermShape.Unsupported("CorruptedBody", "test-only")
              )
            ),
            "<generated-definition-invalid-body>"
          )
          .left
          .toOption
          .exists(_.isInstanceOf[DefinitionBodyPlanningFailure])
      )
      assert(
        ConstructedDefinitionGeneratedOriginAdapter
          .lower(null, "<generated-definition-null>")
          .left
          .toOption
          .exists(_.isInstanceOf[RawDefinitionLoweringFailure])
      )
    }
  }

  test("empty virtual source name has one exact definition-specific wrapper") {
    withContext {
      val failure =
        ConstructedDefinitionGeneratedOriginAdapter
          .lower(
            method(plain("value"), STypeIdent("Int"), term("1")),
            ""
          )
          .left
          .toOption
          .get

      assertEquals(
        failure.message,
        "Invalid generated-definition virtual source name: the name is empty."
      )
    }
  }

  test("detects source-free source-mismatched out-of-bounds and noncontained maps") {
    val constructed =
      method(plain("value"), STypeIdent("Int"), term("1"))
    withContext {
      val result = lower(constructed, "<generated-definition-validation>")
      given SourceFile = NoSource
      val raw =
        ConstructedDefinitionUntypedBackend.lower(constructed).toOption.get

      assert(
        validateForTest(raw, result)
          .left
          .toOption
          .exists(_.isInstanceOf[IncompleteDefinitionPositionMap])
      )
      val otherSource =
        SourceFile.virtual("<other-generated-definition>", result.generatedSource)
      assert(
        validateForTest(result.tree.cloneIn(otherSource), result)
          .left
          .toOption
          .exists(_.message.contains("instead of"))
      )
      assert(
        validateForTest(
          result.tree.withSpan(
            Span(0, result.generatedSource.length + 1, result.tree.span.point)
          ),
          result
        ).left.toOption.exists(_.message.contains("does not cover"))
      )
      val noncontained =
        result.tree match
          case method: untpd.DefDef =>
            val widened =
              method.tpt.withSpan(
                Span(0, result.generatedSource.length, 0)
              )
            untpd.cpy.DefDef(method)(
              method.name,
              method.paramss,
              widened,
              method.rhs
            )
          case other =>
            fail(s"expected DefDef, found ${other.getClass.getSimpleName}")
      assert(
        validateForTest(noncontained, result)
          .left
          .toOption
          .exists(_.message.contains("overlap"))
      )
      val reordered =
        result.tree match
          case method: untpd.DefDef =>
            untpd.cpy.DefDef(method)(
              method.name,
              method.paramss,
              method.rhs,
              method.tpt
            )
          case other =>
            fail(s"expected DefDef, found ${other.getClass.getSimpleName}")
      assert(
        validateForTest(reordered, result)
          .left
          .toOption
          .exists(_.message.contains("source order"))
      )
    }
  }

  test("detects raw variant and raw child-plan mismatches without returning partial trees") {
    val methodDefinition =
      method(plain("value"), STypeIdent("Int"), term("1"))
    val valueDefinition =
      value(plain("value"), STypeIdent("Int"), term("1"))
    withContext {
      val rawValue =
        ConstructedDefinitionUntypedBackend
          .lower(valueDefinition)
          .toOption
          .get
      val variantMismatch =
        ConstructedDefinitionGeneratedOriginAdapter.positionRawForTest(
          rawValue,
          methodDefinition,
          "<generated-definition-variant-mismatch>"
        )
      assert(
        variantMismatch.left.toOption
          .exists(_.isInstanceOf[RawDefinitionPlanMismatch])
      )
      assert(variantMismatch.toOption.isEmpty)

      val rawMethod =
        ConstructedDefinitionUntypedBackend
          .lower(methodDefinition)
          .toOption
          .get
          .asInstanceOf[untpd.DefDef]
      given SourceFile = NoSource
      val wrongTypeShape =
        untpd.cpy.DefDef(rawMethod)(
          rawMethod.name,
          rawMethod.paramss,
          untpd.Tuple(Nil),
          rawMethod.rhs
        )
      val childMismatch =
        ConstructedDefinitionGeneratedOriginAdapter.positionRawForTest(
          wrongTypeShape,
          methodDefinition,
          "<generated-definition-child-mismatch>"
        )
      assert(
        childMismatch.left.toOption
          .exists(_.isInstanceOf[RawDefinitionPlanMismatch])
      )
      assert(childMismatch.toOption.isEmpty)
    }
  }

  test("error messages are bounded and production adapters remain parser-free and search-free") {
    assertEquals(
      DefinitionNameRenderingFailure("invalid name").message,
      "Generated-definition name rendering failed: invalid name"
    )
    assertEquals(
      DefinitionTypePlanningFailure("invalid type").message,
      "Generated-definition type planning failed: invalid type"
    )
    assertEquals(
      DefinitionBodyPlanningFailure("invalid body").message,
      "Generated-definition body planning failed: invalid body"
    )
    assertEquals(
      RawDefinitionPlanMismatch("invalid plan").message,
      "Generated-definition raw tree/plan mismatch: invalid plan"
    )

    val roots = Vector(
      Path.of(
        "src",
        "main",
        "scala",
        "quasiquotes",
        "definitions",
        "dotty",
        "ConstructedDefinitionGeneratedOriginAdapter.scala"
      ),
      Path.of(
        "src",
        "main",
        "scala",
        "quasiquotes",
        "terms",
        "dotty",
        "GeneratedOriginFragmentSupport.scala"
      )
    )
    val forbidden = Vector(
      "TinyTermParser",
      "TinyTypeParser",
      "RawDefinitionParserAdapter",
      "Scala3ParserBridge",
      "dotty.tools.dotc.parsing",
      "scala.quoted",
      "quotes.reflect",
      "Expr[",
      "TypeRepr",
      ".indexOf(",
      "MacroParadise",
      "trait Backend"
    )
    roots.foreach { path =>
      val source = Files.readString(path, StandardCharsets.UTF_8)
      forbidden.foreach(value => assert(!source.contains(value), clues(path, value)))
    }
    val support =
      Files.readString(roots(1), StandardCharsets.UTF_8)
    assert(!support.contains("quasiquotes.definitions"))
    assert(!support.contains("ConstructedDefinition"))
    val rawBackend =
      Files.readString(
        Path.of(
          "src",
          "main",
          "scala",
          "quasiquotes",
          "definitions",
          "dotty",
          "ConstructedDefinitionUntypedBackend.scala"
        ),
        StandardCharsets.UTF_8
      )
    assert(!rawBackend.contains("GeneratedOrigin"))
  }

  private def lower(
      constructed: ConstructedDefinition,
      sourceName: String
  )(using Context): GeneratedOriginDefinitionResult =
    ConstructedDefinitionGeneratedOriginAdapter
      .lower(constructed, sourceName)
      .fold(error => fail(error.message), identity)

  private def validateForTest(
      tree: untpd.Tree,
      result: GeneratedOriginDefinitionResult
  )(using Context): Either[ConstructedDefinitionGeneratedOriginError, Unit] =
    ConstructedDefinitionGeneratedOriginAdapter.validatePositionedForTest(
      tree,
      result.generatedSource,
      result.sourceFile,
      "value",
      "value"
    )

  private def assertComplete(
      result: GeneratedOriginDefinitionResult
  )(using Context): Unit =
    GeneratedOriginFragmentSupport.allTrees(result.tree).foreach { tree =>
      assert(tree.source.exists, clues(tree.getClass.getSimpleName))
      assertEquals(tree.source.path, result.virtualSourceName)
      assert(tree.span.exists, clues(tree.getClass.getSimpleName))
      assert(tree.span.start >= 0)
      assert(tree.span.start <= tree.span.point)
      assert(tree.span.point <= tree.span.end)
      assert(tree.span.end <= result.generatedSource.length)
      assertEquals(tree.symbol, NoSymbol)
      assert(!tree.isInstanceOf[untpd.TypedSplice])
    }

  private def assertRawAgreement(
      positioned: untpd.Tree,
      constructed: ConstructedDefinition
  )(using Context): Unit =
    val raw =
      ConstructedDefinitionUntypedBackend.lower(constructed).toOption.get
    assertEquals(definitionSummary(positioned), definitionSummary(raw))

  private def definitionSummary(tree: untpd.Tree)(using Context): String =
    tree match
      case method: untpd.DefDef =>
        s"DefDef(${method.name},${method.paramss.size},${method.mods.flags},${TypeShapeInspector.rawStructure(method.tpt)},${TermShapeInspector.rawStructure(method.rhs)})"
      case value: untpd.ValDef =>
        s"ValDef(${value.name},${value.mods.flags},${TypeShapeInspector.rawStructure(value.tpt)},${TermShapeInspector.rawStructure(value.rhs)})"
      case other =>
        other.getClass.getSimpleName

  private def children(
      tree: untpd.Tree
  )(using Context): (untpd.Tree, untpd.Tree) =
    tree match
      case method: untpd.DefDef => method.tpt -> method.rhs
      case value: untpd.ValDef => value.tpt -> value.rhs
      case other =>
        fail(s"expected definition, found ${other.getClass.getSimpleName}")

  private def assertSpan(
      tree: untpd.Tree,
      start: Int,
      end: Int,
      point: Int
  ): Unit =
    assertEquals(tree.span.start, start)
    assertEquals(tree.span.end, end)
    assertEquals(tree.span.point, point)

  private def withContext(body: Context ?=> Unit): Unit =
    val base = new ContextBase
    given Context = base.initialCtx
    body

  private def plain(source: String): DefinitionName =
    DefinitionName.plain(source).toOption.get

  private def keyword(source: String): DefinitionName =
    DefinitionName.backticked(source).toOption.get

  private def term(source: String): ConstructedTerm =
    fromShape(TinyTermParser.parseOrThrow(source).shape)

  private def fromShape(shape: TermShape): ConstructedTerm =
    ConstructedTerm.fromShape(shape).toOption.get

  private def ident(name: String): TermShape =
    TermShape.Identifier(name, isPlaceholder = false)

  private def method(
      name: DefinitionName,
      resultType: TypeNormalForm,
      body: ConstructedTerm
  ): ConstructedDefinition =
    ConstructedDefinition
      .parameterlessDef(name, resultType, body)
      .toOption
      .get

  private def value(
      name: DefinitionName,
      declaredType: TypeNormalForm,
      rhs: ConstructedTerm
  ): ConstructedDefinition =
    ConstructedDefinition
      .immutableVal(name, declaredType, rhs)
      .toOption
      .get

  private def renderType(normalForm: TypeNormalForm): String =
    normalForm match
      case STypeIdent(name) => name
      case STypeApply(constructor, arguments) =>
        s"${renderType(constructor)}[${arguments.map(renderType).mkString(", ")}]"
      case STypeTuple(elements) =>
        s"(${elements.map(renderType).mkString(", ")})"
      case STypeFunction(argument :: Nil, result) =>
        s"${renderType(argument)} => ${renderType(result)}"
      case STypeFunction(arguments, result) =>
        s"(${arguments.map(renderType).mkString(", ")}) => ${renderType(result)}"

  private def reflectedMethod(
      name: DefinitionName,
      resultType: TypeNormalForm,
      body: ConstructedTerm
  ): ConstructedDefinition =
    val constructor =
      classOf[ConstructedDefinition.ParameterlessDef]
        .getDeclaredConstructors
        .head
    constructor.setAccessible(true)
    constructor
      .newInstance(name, resultType, body)
      .asInstanceOf[ConstructedDefinition]

  private def reflectedTerm(shape: TermShape): ConstructedTerm =
    val constructor =
      classOf[ConstructedTerm].getDeclaredConstructors.head
    constructor.setAccessible(true)
    constructor
      .newInstance(shape, Vector.empty)
      .asInstanceOf[ConstructedTerm]
