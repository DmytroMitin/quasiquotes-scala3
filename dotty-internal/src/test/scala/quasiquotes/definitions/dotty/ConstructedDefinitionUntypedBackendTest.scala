package quasiquotes.definitions.dotty

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol

import quasiquotes.definitions.{ConstructedDefinition, DefinitionName}
import quasiquotes.parser.{
  TermShape,
  TermShapeInspector,
  TinyTermParser,
  TinyTypeParser,
  TypeShapeInspector
}
import quasiquotes.terms.ConstructedTerm
import quasiquotes.types.TypeNormalForm

class ConstructedDefinitionUntypedBackendTest extends munit.FunSuite:
  import ConstructedDefinitionUntypedBackendError.*
  import TypeNormalForm.*

  private final case class Fixture(
      source: String,
      definition: ConstructedDefinition,
      expectedClass: Class[? <: untpd.Tree],
      expectedName: String,
      expectedTypeSource: String,
      expectedBodySource: String
  )

  private val fixtures = Vector(
    Fixture(
      "def value: Int = 1",
      method(plain("value"), STypeIdent("Int"), term("1")),
      classOf[untpd.DefDef],
      "value",
      "Int",
      "1"
    ),
    Fixture(
      "val value: String = \"text\"",
      value(plain("value"), STypeIdent("String"), term("\"text\"")),
      classOf[untpd.ValDef],
      "value",
      "String",
      "\"text\""
    ),
    Fixture(
      "def `type`: List[String] = if true then \"yes\" else \"no\"",
      method(
        keyword("`type`"),
        STypeApply(STypeIdent("List"), List(STypeIdent("String"))),
        term("if true then \"yes\" else \"no\"")
      ),
      classOf[untpd.DefDef],
      "type",
      "List[String]",
      "if true then \"yes\" else \"no\""
    ),
    Fixture(
      "val `val`: Option[Int] = (1: Int)",
      value(
        keyword("`val`"),
        STypeApply(STypeIdent("Option"), List(STypeIdent("Int"))),
        term("(1: Int)")
      ),
      classOf[untpd.ValDef],
      "val",
      "Option[Int]",
      "(1: Int)"
    )
  )

  fixtures.foreach { fixture =>
    test(s"lowers exact completed definition fixture: ${fixture.source}") {
      val base = new ContextBase
      given Context = base.initialCtx
      val raw =
        ConstructedDefinitionUntypedBackend.lower(fixture.definition).toOption.get

      assert(fixture.expectedClass.isInstance(raw))
      raw match
        case definition: untpd.DefDef =>
          assertEquals(definition.name.toString, fixture.expectedName)
          assertEquals(definition.paramss, Nil)
          assertEquals(definition.leadingTypeParams, Nil)
          assertEquals(definition.trailingParamss, Nil)
          assertEquals(definition.mods.flags, Flags.Method)
          assertEquals(
            TypeShapeInspector.rawStructure(definition.tpt),
            TinyTypeParser.parseOrThrow(fixture.expectedTypeSource).rawStructure
          )
          assertEquals(
            TermShapeInspector.rawStructure(definition.rhs),
            TinyTermParser.parseOrThrow(fixture.expectedBodySource).rawStructure
          )
        case definition: untpd.ValDef =>
          assertEquals(definition.name.toString, fixture.expectedName)
          assert(!definition.mods.hasFlags)
          assert(!definition.mods.is(Flags.Mutable))
          assert(!definition.mods.is(Flags.Lazy))
          assertEquals(
            TypeShapeInspector.rawStructure(definition.tpt),
            TinyTypeParser.parseOrThrow(fixture.expectedTypeSource).rawStructure
          )
          assertEquals(
            TermShapeInspector.rawStructure(definition.rhs),
            TinyTermParser.parseOrThrow(fixture.expectedBodySource).rawStructure
          )
        case other =>
          fail(s"unexpected raw definition: ${other.getClass.getSimpleName}")
    }
  }

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

  typeFamilies.foreach { case (source, normalForm) =>
    test(s"definition types use authoritative completed-type lowering: $source") {
      val base = new ContextBase
      given Context = base.initialCtx
      val raw =
        ConstructedDefinitionUntypedBackend
          .lower(method(plain("value"), normalForm, term("1")))
          .toOption
          .get
          .asInstanceOf[untpd.DefDef]

      assertEquals(
        TypeShapeInspector.rawStructure(raw.tpt),
        TinyTypeParser.parseOrThrow(source).rawStructure
      )
    }
  }

  test("nontrivial bodies preserve application infix tuple if and typed-sidecar preorder") {
    val source =
      "if ready then f((left: List[Int]), right + 1) else ((fallback: Option[String]), other)"
    val body =
      ConstructedTerm
        .create(
          TinyTermParser.parseOrThrow(source).shape,
          Vector(
            STypeApply(STypeIdent("List"), List(STypeIdent("Int"))),
            STypeApply(STypeIdent("Option"), List(STypeIdent("String")))
          )
        )
        .toOption
        .get
    val completed =
      method(plain("value"), STypeIdent("Int"), body)
    val base = new ContextBase
    given Context = base.initialCtx
    val raw =
      ConstructedDefinitionUntypedBackend
        .lower(completed)
        .toOption
        .get
        .asInstanceOf[untpd.DefDef]

    assertEquals(
      TermShapeInspector.rawStructure(raw.rhs),
      TinyTermParser.parseOrThrow(source).rawStructure
    )
  }

  test("structural lowering accepts unary minus over a negative decimal without generated text") {
    val shape =
      TermShape.Unary("-", TermShape.Literal("-1"))
    val body = ConstructedTerm.fromShape(shape).toOption.get
    val completed = value(plain("value"), STypeIdent("Int"), body)
    val base = new ContextBase
    given Context = base.initialCtx
    val raw =
      ConstructedDefinitionUntypedBackend
        .lower(completed)
        .toOption
        .get
        .asInstanceOf[untpd.ValDef]

    assertEquals(TermShapeInspector.inspect(raw.rhs), shape)
  }

  test("long decimal Boolean and semantic String literals retain child backend values") {
    val shapes =
      Vector(
        TermShape.Literal("-2147483649"),
        TermShape.Literal("true"),
        TermShape.Literal("\"a quoted \"value\"\"")
      )
    val base = new ContextBase
    given Context = base.initialCtx

    shapes.foreach { shape =>
      val raw =
        ConstructedDefinitionUntypedBackend
          .lower(
            value(
              plain("value"),
              STypeIdent("Int"),
              ConstructedTerm.fromShape(shape).toOption.get
            )
          )
          .toOption
          .get
          .asInstanceOf[untpd.ValDef]
      assertEquals(TermShapeInspector.inspect(raw.rhs), shape)
    }
  }

  test("every raw definition child has no source span symbol owner claim or typed splice") {
    val base = new ContextBase
    given Context = base.initialCtx

    fixtures.foreach { fixture =>
      val raw =
        ConstructedDefinitionUntypedBackend.lower(fixture.definition).toOption.get
      allTrees(raw).foreach { tree =>
        assert(!tree.source.exists)
        assert(!tree.span.exists)
        assertEquals(tree.symbol, NoSymbol)
        assert(!tree.isInstanceOf[untpd.TypedSplice])
      }
    }
  }

  test("defensive constructor bypasses map to name type and body categories") {
    val validBody = term("1")
    val nullName =
      reflectedMethod(null, STypeIdent("Int"), validBody)
    val invalidType =
      reflectedMethod(plain("value"), STypeIdent("AnyVal"), validBody)
    val corruptedBody =
      reflectedMethod(
        plain("value"),
        STypeIdent("Int"),
        reflectedTerm(TermShape.Unsupported("CorruptedBody", "test-only"))
      )

    assert(
      ConstructedDefinitionUntypedBackend
        .lower(nullName)
        .left
        .toOption
        .get
        .isInstanceOf[DefinitionNameLoweringFailure]
    )
    assert(
      ConstructedDefinitionUntypedBackend
        .lower(invalidType)
        .left
        .toOption
        .get
        .isInstanceOf[DefinitionTypeLoweringFailure]
    )
    assert(
      ConstructedDefinitionUntypedBackend
        .lower(corruptedBody)
        .left
        .toOption
        .get
        .isInstanceOf[DefinitionBodyLoweringFailure]
    )
  }

  test("definition backend error messages are stable and bounded") {
    assertEquals(
      DefinitionNameLoweringFailure("invalid name").message,
      "Constructed-definition name lowering failed: invalid name"
    )
    assertEquals(
      DefinitionTypeLoweringFailure("invalid type").message,
      "Constructed-definition type lowering failed: invalid type"
    )
    assertEquals(
      DefinitionBodyLoweringFailure("invalid body").message,
      "Constructed-definition body lowering failed: invalid body"
    )
    assertEquals(
      RawDefinitionConstructionInvariantFailure("invalid raw tree").message,
      "Raw constructed-definition invariant failed: invalid raw tree"
    )
    assertEquals(
      UnsupportedConstructedDefinitionVariant("FutureVariant").message,
      "Unsupported constructed-definition variant at the exact-version untyped backend boundary: FutureVariant."
    )
  }

  test("compiler-free core and exact-version definition backend keep their dependency boundaries") {
    val definitions =
      Path.of("core", "src", "main", "scala", "quasiquotes", "definitions")
    val backendDefinitions =
      Path.of("dotty-internal", "src", "main", "scala", "quasiquotes", "definitions")
    val compilerFreeForbidden =
      Vector(
        "dotty.tools.dotc",
        "scala.quoted",
        "quotes.reflect",
        "Quotes",
        "Expr[",
        "TypeRepr",
        "untpd",
        "ConstructedDefinitionUntypedBackend",
        "ConstructedDefinitionGeneratedOriginAdapter",
        "macroparadise"
      )
    Vector(
      "DefinitionName.scala",
      "DefinitionShape.scala",
      "DefinitionSourceMetadata.scala",
      "DefinitionError.scala",
      "DefinitionTemplate.scala",
      "ConstructedDefinition.scala",
      "DefinitionConstructionError.scala"
    ).foreach { file =>
      val source =
        Files.readString(definitions.resolve(file), StandardCharsets.UTF_8)
      compilerFreeForbidden.foreach(value =>
        assert(!source.contains(value), clues(file, value))
      )
    }

    val backendForbidden =
      Vector(
        "TinyTermParser",
        "TinyTypeParser",
        "RawDefinitionParserAdapter",
        "Scala3ParserBridge",
        "dotty.tools.dotc.parsing",
        "scala.quoted",
        "quotes.reflect",
        "Expr[",
        "TypeRepr",
        "ConstructedTermGeneratedOriginAdapter",
        "MacroParadise",
        "trait Backend"
      )
    val stream = Files.walk(backendDefinitions.resolve("dotty"))
    try
      stream
        .filter(path => path.toString.endsWith(".scala"))
        .filter(path => path.getFileName.toString.startsWith("ConstructedDefinition"))
        .forEach { path =>
          val source = Files.readString(path, StandardCharsets.UTF_8)
          backendForbidden.foreach(value =>
            assert(!source.contains(value), clues(path, value))
          )
        }
    finally stream.close()
  }

  private def plain(source: String): DefinitionName =
    DefinitionName.plain(source).toOption.get

  private def keyword(source: String): DefinitionName =
    DefinitionName.backticked(source).toOption.get

  private def term(source: String): ConstructedTerm =
    ConstructedTerm.fromShape(TinyTermParser.parseOrThrow(source).shape).toOption.get

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

  private def allTrees(tree: untpd.Tree)(using Context): List[untpd.Tree] =
    val children =
      tree match
        case value: untpd.DefDef =>
          value.tpt :: value.rhs :: Nil
        case value: untpd.ValDef =>
          value.tpt :: value.rhs :: Nil
        case value: untpd.Select =>
          value.qualifier :: Nil
        case value: untpd.Apply =>
          value.fun :: value.args
        case value: untpd.InfixOp =>
          List(value.left, value.op, value.right)
        case value: untpd.PrefixOp =>
          List(value.op, value.od)
        case value: untpd.Typed =>
          List(value.expr, value.tpt)
        case value: untpd.AppliedTypeTree =>
          value.tpt :: value.args
        case value: untpd.Tuple =>
          value.trees
        case value: untpd.Function =>
          value.args :+ value.body
        case value: untpd.If =>
          List(value.cond, value.thenp, value.elsep)
        case value: untpd.Parens =>
          value.t :: Nil
        case _ =>
          Nil
    tree :: children.flatMap(allTrees)
