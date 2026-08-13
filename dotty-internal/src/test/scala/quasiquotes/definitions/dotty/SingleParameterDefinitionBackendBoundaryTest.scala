package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.parsing.Parsers.Parser
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.SourceFile

import quasiquotes.definitions.{ConstructedDefinition, DefinitionName}
import quasiquotes.parser.{BinderId, TermShape}
import quasiquotes.terms.ConstructedTerm
import quasiquotes.terms.dotty.GeneratedOriginFragmentSupport
import quasiquotes.types.TypeNormalForm

class SingleParameterDefinitionBackendBoundaryTest extends munit.FunSuite:
  test("source-free lowering emits one ordinary parameter and resolves binding by BinderId") {
    withContext {
      val raw = ConstructedDefinitionUntypedBackend
        .lower(definition(boundDisplay = "misleading"))
        .toOption
        .get
        .asInstanceOf[untpd.DefDef]
      val parameter = raw.paramss.head.head.asInstanceOf[untpd.ValDef]
      val reference = raw.rhs.asInstanceOf[untpd.Ident]

      assertEquals(raw.name.toString, "id")
      assertEquals(raw.paramss.map(_.size), List(1))
      assertEquals(raw.mods.flags, Flags.Method)
      assertEquals(parameter.name.toString, "x")
      assertEquals(parameter.mods.flags, Flags.Param)
      assert(parameter.rhs.isEmpty)
      assertEquals(reference.name.toString, "x")
      allDefinitionTrees(raw).foreach { tree =>
        assert(!tree.source.exists)
        assert(!tree.span.exists)
        assertEquals(tree.symbol, NoSymbol)
      }
    }
  }

  test("free same-text identifier remains free beside the bound parameter reference") {
    val binder = BinderId(0)
    val body = ConstructedTerm
      .fromShapeInScope(
        TermShape.Tuple(
          List(
            TermShape.BoundReference(binder, "not-x"),
            TermShape.Identifier("x", isPlaceholder = false)
          )
        ),
        binder
      )
      .toOption
      .get
    withContext {
      val raw = ConstructedDefinitionUntypedBackend
        .lower(definition(body = body))
        .toOption
        .get
        .asInstanceOf[untpd.DefDef]
      val names = raw.rhs.asInstanceOf[untpd.Tuple].trees.map {
        _.asInstanceOf[untpd.Ident].name.toString
      }
      assertEquals(names, List("x", "x"))
    }
  }

  test("generated-origin lowering emits parser-equivalent canonical source and parameter positions") {
    withContext {
      val result = ConstructedDefinitionGeneratedOriginAdapter
        .lower(definition(boundDisplay = "misleading"), "<single-parameter-definition>")
        .toOption
        .get
      val method = result.tree.asInstanceOf[untpd.DefDef]
      val parameter = method.paramss.head.head.asInstanceOf[untpd.ValDef]
      val reference = method.rhs.asInstanceOf[untpd.Ident]

      assertEquals(result.generatedSource, "def id(x: Int): Int = x")
      assertSpan(method, 0, 23, 4)
      assertSpan(parameter, 7, 13, 7)
      assertSpan(parameter.tpt, 10, 13, 10)
      assertSpan(method.tpt, 16, 19, 16)
      assertSpan(reference, 22, 23, 22)
      allDefinitionTrees(method).foreach { tree =>
        assertEquals(tree.source.path, method.source.path)
        assertEquals(tree.symbol, NoSymbol)
      }
    }
  }

  test("generated-origin lowering renders a hostile bound-reference display with declaration spelling") {
    withContext {
      val result = ConstructedDefinitionGeneratedOriginAdapter
        .lower(definition(boundDisplay = "foreignText"), "<hostile-bound-display>")
        .toOption
        .get
      assertEquals(result.generatedSource, "def id(x: Int): Int = x")
      assertEquals(
        result.tree.asInstanceOf[untpd.DefDef].rhs.asInstanceOf[untpd.Ident].name.toString,
        "x"
      )
    }
  }

  test("source-free and generated-origin methods agree with an independent parser oracle") {
    val binder = BinderId(7)
    val bound = TermShape.BoundReference(binder, "hostile-display")
    val fixtures = Vector(
      "def id(x: Int): Int = x" -> bound,
      "def inc(x: Int): Int = x + 1" ->
        TermShape.Infix(bound, "+", TermShape.Literal("1")),
      "def keep(x: String): String = x" -> bound,
      "def choose(x: Boolean): Boolean = if x then false else true" ->
        TermShape.If(bound, TermShape.Literal("false"), TermShape.Literal("true")),
      "def ascribed(x: Int): Int = (x: Int)" ->
        TermShape.Parenthesized(TermShape.Typed(bound, "Int"))
    )

    fixtures.zipWithIndex.foreach { case ((source, shape), index) =>
      val parameterType =
        if source.contains("String") then TypeNormalForm.STypeIdent("String")
        else if source.contains("Boolean") then TypeNormalForm.STypeIdent("Boolean")
        else TypeNormalForm.STypeIdent("Int")
      val sidecars =
        if source.startsWith("def ascribed") then Vector(TypeNormalForm.STypeIdent("Int"))
        else Vector.empty
      val body = ConstructedTerm
        .createInScope(shape, sidecars, binder)
        .toOption
        .get
      val completed = singleParameterDefinition(
        source.split('(').head.stripPrefix("def "),
        "x",
        binder,
        parameterType,
        parameterType,
        body
      )

      withContext {
        val generated = ConstructedDefinitionGeneratedOriginAdapter
          .lower(completed, s"<single-parameter-oracle-$index>")
          .toOption
          .get
        val parsed = parseOne(generated.generatedSource)
        val sourceFree = ConstructedDefinitionUntypedBackend
          .lower(completed)
          .toOption
          .get
          .asInstanceOf[untpd.DefDef]

        assertEquals(generated.generatedSource, source)
        assertEquals(
          definitionSummary(generated.tree.asInstanceOf[untpd.DefDef], source, includePositions = true),
          definitionSummary(parsed, source, includePositions = true)
        )
        assertEquals(
          definitionSummary(sourceFree, source, includePositions = false),
          definitionSummary(parsed, source, includePositions = false)
        )
      }
    }
  }

  test("parameter result and body ascription types retain their independent order") {
    val binder = BinderId(11)
    val parameterType = TypeNormalForm.STypeApply(
      TypeNormalForm.STypeIdent("List"),
      List(TypeNormalForm.STypeIdent("Int"))
    )
    val resultType = TypeNormalForm.STypeApply(
      TypeNormalForm.STypeIdent("Option"),
      List(TypeNormalForm.STypeIdent("String"))
    )
    val body = ConstructedTerm
      .createInScope(
        TermShape.Parenthesized(
          TermShape.Typed(TermShape.BoundReference(binder, "wrong"), "Boolean")
        ),
        Vector(TypeNormalForm.STypeIdent("Boolean")),
        binder
      )
      .toOption
      .get
    val completed = singleParameterDefinition(
      "typed",
      "x",
      binder,
      parameterType,
      resultType,
      body
    )

    withContext {
      val generated = ConstructedDefinitionGeneratedOriginAdapter
        .lower(completed, "<definition-type-order>")
        .toOption
        .get
      assertEquals(
        generated.generatedSource,
        "def typed(x: List[Int]): Option[String] = (x: Boolean)"
      )
      val parsed = parseOne(generated.generatedSource)
      assertEquals(
        definitionSummary(generated.tree.asInstanceOf[untpd.DefDef], generated.generatedSource, includePositions = true),
        definitionSummary(parsed, generated.generatedSource, includePositions = true)
      )
    }
  }

  test("foreign method-bound references fail closed in both backend modes") {
    val declared = BinderId(20)
    val foreign = BinderId(21)
    val foreignBody = ConstructedTerm
      .fromShapeInScope(TermShape.BoundReference(foreign, "x"), foreign)
      .toOption
      .get
    val malformed = reflectedDefinition(declared, foreignBody)

    withContext {
      val sourceFree = ConstructedDefinitionUntypedBackend.lower(malformed).left.toOption.get
      val generated = ConstructedDefinitionGeneratedOriginAdapter
        .lower(malformed, "<foreign-definition-binder>")
        .left
        .toOption
        .get
      assertEquals(
        sourceFree.message,
        "Constructed-definition body lowering failed: Cannot lower bound reference for inactive binder identity 21."
      )
      assertEquals(
        generated.message,
        "Generated-definition body planning failed: Cannot render bound reference for inactive binder identity 21."
      )
    }
  }

  test("missing and unconsumed body type sidecars report deterministic ordinals") {
    val binder = BinderId(30)
    val missing = reflectedTerm(
      TermShape.Parenthesized(
        TermShape.Typed(TermShape.BoundReference(binder, "x"), "Int")
      ),
      Vector.empty
    )
    val unconsumed = reflectedTerm(
      TermShape.BoundReference(binder, "x"),
      Vector(TypeNormalForm.STypeIdent("Int"))
    )

    withContext {
      Vector(
        reflectedDefinition(binder, missing) -> "ordinal 0",
        reflectedDefinition(binder, unconsumed) -> "consumed 0 of 1"
      ).zipWithIndex.foreach { case ((malformed, expected), index) =>
        val sourceFree = ConstructedDefinitionUntypedBackend.lower(malformed).left.toOption.get
        val generated = ConstructedDefinitionGeneratedOriginAdapter
          .lower(malformed, s"<definition-sidecar-$index>")
          .left
          .toOption
          .get
        assert(sourceFree.message.contains(expected), clues(sourceFree.message))
        assert(generated.message.contains(expected), clues(generated.message))
      }
    }
  }

  test("malformed parameter and result types retain truthful generated roles") {
    val binder = BinderId(40)
    val body = ConstructedTerm
      .fromShapeInScope(TermShape.BoundReference(binder, "x"), binder)
      .toOption
      .get
    val unsupported = TypeNormalForm.STypeIdent("AnyVal")
    val valid = TypeNormalForm.STypeIdent("Int")
    val malformedParameter = reflectedDefinition(
      binder,
      body,
      parameterType = unsupported,
      resultType = valid
    )
    val malformedResult = reflectedDefinition(
      binder,
      body,
      parameterType = valid,
      resultType = unsupported
    )

    withContext {
      Vector(
        malformedParameter -> "parameter type",
        malformedResult -> "result type"
      ).zipWithIndex.foreach { case ((malformed, role), index) =>
        val error = ConstructedDefinitionGeneratedOriginAdapter
          .lower(malformed, s"<definition-invalid-type-$index>")
          .left
          .toOption
          .get
        assert(error.message.contains(role), clues(error.message))
      }
    }
  }

  private def definition(
      boundDisplay: String = "x",
      body: ConstructedTerm | Null = null
  ): ConstructedDefinition =
    val binder = BinderId(0)
    val completedBody = Option(body).getOrElse(
      ConstructedTerm
        .fromShapeInScope(TermShape.BoundReference(binder, boundDisplay), binder)
        .toOption
        .get
    )
    ConstructedDefinition
      .singleParameterDef(
        DefinitionName.plain("id").toOption.get,
        binder,
        DefinitionName.plain("x").toOption.get,
        TypeNormalForm.STypeIdent("Int"),
        TypeNormalForm.STypeIdent("Int"),
        completedBody
      )
      .toOption
      .get

  private def singleParameterDefinition(
      methodName: String,
      parameterName: String,
      binder: BinderId,
      parameterType: TypeNormalForm,
      resultType: TypeNormalForm,
      body: ConstructedTerm
  ): ConstructedDefinition =
    ConstructedDefinition
      .singleParameterDef(
        DefinitionName.plain(methodName).toOption.get,
        binder,
        DefinitionName.plain(parameterName).toOption.get,
        parameterType,
        resultType,
        body
      )
      .toOption
      .get

  private def reflectedDefinition(
      binder: BinderId,
      body: ConstructedTerm,
      parameterType: TypeNormalForm = TypeNormalForm.STypeIdent("Int"),
      resultType: TypeNormalForm = TypeNormalForm.STypeIdent("Int")
  ): ConstructedDefinition =
    val constructor = classOf[ConstructedDefinition.SingleParameterDef]
      .getDeclaredConstructors
      .head
    constructor.setAccessible(true)
    constructor
      .newInstance(
        DefinitionName.plain("id").toOption.get,
        binder,
        DefinitionName.plain("x").toOption.get,
        parameterType,
        resultType,
        body
      )
      .asInstanceOf[ConstructedDefinition]

  private def reflectedTerm(
      shape: TermShape,
      sidecars: Vector[TypeNormalForm]
  ): ConstructedTerm =
    val constructor = classOf[ConstructedTerm].getDeclaredConstructors.head
    constructor.setAccessible(true)
    constructor.newInstance(shape, sidecars).asInstanceOf[ConstructedTerm]

  private def parseOne(source: String)(using Context): untpd.DefDef =
    val reporter = new StoreReporter(null)
    val parserContext = summon[Context].fresh.setReporter(reporter)
    val parsed =
      new Parser(SourceFile.virtual("SingleParameterDefinitionOracle.scala", source))(
        using parserContext
      ).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed.asInstanceOf[untpd.PackageDef].stats.head.asInstanceOf[untpd.DefDef]

  private def definitionSummary(
      method: untpd.DefDef,
      source: String,
      includePositions: Boolean
  )(using Context): String =
    def loop(tree: untpd.Tree): String =
      val position =
        if includePositions then
          s"@${tree.span.start}..${tree.span.point}..${tree.span.end}:${source.slice(tree.span.start, tree.span.end)}"
        else ""
      val detail = tree match
        case value: untpd.DefDef => s"(${value.name},${value.mods.flags})"
        case value: untpd.ValDef => s"(${value.name},${value.mods.flags},rhsEmpty=${value.rhs.isEmpty})"
        case value: untpd.Ident => s"(${value.name})"
        case value: untpd.Select => s"(${value.name})"
        case _ => ""
      val children = tree match
        case value: untpd.DefDef =>
          value.paramss.flatten.toVector ++ Vector(value.tpt, value.rhs)
        case _ => GeneratedOriginFragmentSupport.directChildren(tree)
      s"${tree.getClass.getSimpleName}$detail$position[${children.map(loop).mkString(",")}]"
    loop(method)

  private def allDefinitionTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree match
      case method: untpd.DefDef =>
        method +: (
          method.paramss.flatten.toVector ++ Vector(method.tpt, method.rhs)
        ).flatMap(allDefinitionTrees)
      case _ =>
        GeneratedOriginFragmentSupport.allTrees(tree)

  private def assertSpan(tree: untpd.Tree, start: Int, end: Int, point: Int): Unit =
    assertEquals(tree.span.start, start)
    assertEquals(tree.span.end, end)
    assertEquals(tree.span.point, point)

  private def withContext(body: Context ?=> Unit): Unit =
    val base = new ContextBase
    given Context = base.initialCtx
    body
