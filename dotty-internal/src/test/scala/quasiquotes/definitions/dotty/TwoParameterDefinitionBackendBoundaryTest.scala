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
import quasiquotes.terms.dotty.{
  ConstructedTermUntypedBackend,
  GeneratedOriginFragmentSupport
}
import quasiquotes.types.TypeNormalForm

class TwoParameterDefinitionBackendBoundaryTest extends munit.FunSuite:
  test("source-free lowering emits two ordered ordinary parameters and resolves both binders by identity") {
    withContext {
      val completed = definition()
      val raw = ConstructedDefinitionUntypedBackend
        .lower(completed)
        .toOption
        .get
        .asInstanceOf[untpd.DefDef]
      val parameters = raw.paramss.head.map(_.asInstanceOf[untpd.ValDef])
      val references = raw.rhs.asInstanceOf[untpd.Tuple].trees.map {
        _.asInstanceOf[untpd.Ident].name.toString
      }

      assertEquals(raw.name.toString, "pair")
      assertEquals(raw.paramss.map(_.size), List(2))
      assertEquals(raw.mods.flags, Flags.Method)
      assertEquals(parameters.map(_.name.toString), List("x", "y"))
      assert(parameters.forall(_.mods.flags == Flags.Param))
      assert(parameters.forall(_.rhs.isEmpty))
      assertEquals(references, List("x", "y"))
      allDefinitionTrees(raw).foreach { tree =>
        assert(!tree.source.exists)
        assert(!tree.span.exists)
        assertEquals(tree.symbol, NoSymbol)
      }
    }
  }

  test("source-free lowering preserves free same-text identifiers beside both bound references") {
    val binders = Vector(BinderId(11), BinderId(12))
    val body = ConstructedTerm
      .fromShapeInScope(
        TermShape.Tuple(
          List(
            TermShape.BoundReference(binders(0), "hostile-y"),
            TermShape.Identifier("x", isPlaceholder = false),
            TermShape.BoundReference(binders(1), "hostile-x"),
            TermShape.Identifier("y", isPlaceholder = false)
          )
        ),
        binders
      )
      .toOption
      .get

    withContext {
      val raw = ConstructedDefinitionUntypedBackend
        .lower(definition(binders = binders, body = body))
        .toOption
        .get
        .asInstanceOf[untpd.DefDef]
      val names = raw.rhs.asInstanceOf[untpd.Tuple].trees.map {
        _.asInstanceOf[untpd.Ident].name.toString
      }
      assertEquals(names, List("x", "x", "y", "y"))
    }
  }

  test("duplicate and foreign exact-two binder identities fail closed") {
    val declared = Vector(BinderId(20), BinderId(21))
    val foreign = BinderId(22)
    val foreignBody = ConstructedTerm
      .fromShapeInScope(TermShape.BoundReference(foreign, "x"), foreign)
      .toOption
      .get
    val validBody = ConstructedTerm
      .fromShapeInScope(
        TermShape.BoundReference(declared(0), "x"),
        declared
      )
      .toOption
      .get

    withContext {
      val duplicate = reflectedDefinition(
        Vector(declared(0), declared(0)),
        validBody
      )
      val malformedForeign = reflectedDefinition(declared, foreignBody)
      val duplicateError = ConstructedDefinitionUntypedBackend
        .lower(duplicate)
        .left
        .toOption
        .get
      val foreignError = ConstructedDefinitionUntypedBackend
        .lower(malformedForeign)
        .left
        .toOption
        .get

      assert(duplicateError.message.contains("duplicate binder identity 20"))
      assert(foreignError.message.contains("inactive binder identity 22"))
    }
  }

  test("generated-origin lowering emits canonical exact-two source and parser-equivalent positions") {
    withContext {
      val generated = ConstructedDefinitionGeneratedOriginAdapter
        .lower(definition(), "<two-parameter-definition>")
        .toOption
        .get
      val expected = "def pair(x: Int, y: String): (Int, String) = (x, y)"
      val positioned = generated.tree.asInstanceOf[untpd.DefDef]
      val parsed = parseOne(expected)

      assertEquals(generated.generatedSource, expected)
      assertEquals(
        definitionSummary(positioned, expected, includePositions = true),
        definitionSummary(parsed, expected, includePositions = true)
      )
      allDefinitionTrees(positioned).foreach { tree =>
        assertEquals(tree.source.path, generated.sourceFile.path)
        assertEquals(tree.symbol, NoSymbol)
        assert(!tree.isInstanceOf[untpd.TypedSplice])
      }
    }
  }

  test("generated-origin parser equivalence preserves first second and both references") {
    val binders = Vector(BinderId(31), BinderId(32))
    val first = TermShape.BoundReference(binders(0), "misleading-y")
    val second = TermShape.BoundReference(binders(1), "misleading-x")
    val fixtures = Vector(
      ("def first(x: Int, y: Int): Int = x", "first", first),
      ("def second(x: Int, y: Int): Int = y", "second", second),
      (
        "def plus(x: Int, y: Int): Int = x + y",
        "plus",
        TermShape.Infix(first, "+", second)
      )
    )

    fixtures.zipWithIndex.foreach { case ((expected, name, shape), index) =>
      val body = ConstructedTerm
        .fromShapeInScope(shape, binders)
        .toOption
        .get
      val completed = definition(
        binders = binders,
        body = body,
        methodName = name,
        secondParameterType = TypeNormalForm.STypeIdent("Int"),
        resultType = TypeNormalForm.STypeIdent("Int")
      )
      withContext {
        val generated = ConstructedDefinitionGeneratedOriginAdapter
          .lower(completed, s"<two-parameter-oracle-$index>")
          .toOption
          .get
        val parsed = parseOne(expected)
        assertEquals(generated.generatedSource, expected)
        assertEquals(
          definitionSummary(
            generated.tree.asInstanceOf[untpd.DefDef],
            expected,
            includePositions = true
          ),
          definitionSummary(parsed, expected, includePositions = true)
        )
      }
    }
  }

  test("first second result and body sidecar types retain independent order") {
    val binders = Vector(BinderId(41), BinderId(42))
    val body = ConstructedTerm
      .createInScope(
        TermShape.Parenthesized(
          TermShape.Typed(
            TermShape.BoundReference(binders(1), "misleading-first"),
            "Boolean"
          )
        ),
        Vector(TypeNormalForm.STypeIdent("Boolean")),
        binders
      )
      .toOption
      .get
    val completed = definition(
      binders = binders,
      body = body,
      methodName = "typed",
      firstParameterType = TypeNormalForm.STypeApply(
        TypeNormalForm.STypeIdent("List"),
        List(TypeNormalForm.STypeIdent("Int"))
      ),
      secondParameterType = TypeNormalForm.STypeApply(
        TypeNormalForm.STypeIdent("Option"),
        List(TypeNormalForm.STypeIdent("String"))
      ),
      resultType = TypeNormalForm.STypeIdent("Boolean")
    )

    withContext {
      val generated = ConstructedDefinitionGeneratedOriginAdapter
        .lower(completed, "<two-parameter-type-order>")
        .toOption
        .get
      val expected =
        "def typed(x: List[Int], y: Option[String]): Boolean = (y: Boolean)"
      assertEquals(generated.generatedSource, expected)
      assertEquals(
        definitionSummary(
          generated.tree.asInstanceOf[untpd.DefDef],
          expected,
          includePositions = true
        ),
        definitionSummary(parseOne(expected), expected, includePositions = true)
      )
    }
  }

  test("generated-origin duplicate and foreign binder identities fail closed") {
    val declared = Vector(BinderId(50), BinderId(51))
    val foreign = BinderId(52)
    val validBody = ConstructedTerm
      .fromShapeInScope(TermShape.BoundReference(declared(0), "x"), declared)
      .toOption
      .get
    val foreignBody = ConstructedTerm
      .fromShapeInScope(TermShape.BoundReference(foreign, "x"), foreign)
      .toOption
      .get

    withContext {
      val duplicateError = ConstructedDefinitionGeneratedOriginAdapter
        .lower(
          reflectedDefinition(Vector(declared(0), declared(0)), validBody),
          "<two-parameter-duplicate-binder>"
        )
        .left
        .toOption
        .get
      val foreignError = ConstructedDefinitionGeneratedOriginAdapter
        .lower(
          reflectedDefinition(declared, foreignBody),
          "<two-parameter-foreign-binder>"
        )
        .left
        .toOption
        .get
      assert(duplicateError.message.contains("duplicate binder identity 50"))
      assert(foreignError.message.contains("inactive binder identity 52"))
    }
  }

  test("ordered raw and generated binder seams reject null duplicate and empty mappings") {
    val binder = BinderId(60)
    val body = ConstructedTerm
      .fromShapeInScope(TermShape.BoundReference(binder, "hostile"), binder)
      .toOption
      .get
    val cases = Vector(
      null.asInstanceOf[Vector[(BinderId, String)]] -> "binding vector was null",
      Vector(null.asInstanceOf[(BinderId, String)]) -> "binding 0 was null",
      Vector((null.asInstanceOf[BinderId], "x")) -> "binder identity at binding 0 was null",
      Vector((binder, null.asInstanceOf[String])) -> "declaration",
      Vector((binder, "x"), (binder, "y")) -> "duplicate binder identity 60",
      Vector((binder, "")) -> "declaration"
    )

    cases.foreach { case (bindings, expected) =>
      val rawError = ConstructedTermUntypedBackend
        .lowerInScopes(body, bindings)
        .left
        .toOption
        .get
      val generatedError = GeneratedOriginFragmentSupport
        .planDefinitionBodyInScopes(body, bindings)
        .left
        .toOption
        .get
      assert(rawError.message.contains(expected), clues(rawError.message))
      assert(generatedError.message.contains(expected), clues(generatedError.message))
    }
  }

  test("null exact-two names types and body fail through controlled backend errors") {
    val binders = Vector(BinderId(70), BinderId(71))
    val body = ConstructedTerm
      .fromShapeInScope(
        TermShape.BoundReference(binders(0), "x"),
        binders
      )
      .toOption
      .get
    val validType = TypeNormalForm.STypeIdent("Int")
    val validName = DefinitionName.plain("valid").toOption.get
    val cases = Vector(
      reflectedDefinitionFields(
        null,
        binders,
        validName,
        validType,
        validName,
        validType,
        validType,
        body
      ) -> "name",
      reflectedDefinitionFields(
        validName,
        binders,
        validName,
        null,
        validName,
        validType,
        validType,
        body
      ) -> "type",
      reflectedDefinitionFields(
        validName,
        binders,
        validName,
        validType,
        validName,
        validType,
        null,
        body
      ) -> "type",
      reflectedDefinitionFields(
        validName,
        binders,
        validName,
        validType,
        validName,
        validType,
        validType,
        null
      ) -> "body"
    )

    cases.zipWithIndex.foreach { case ((malformed, expected), index) =>
      withContext {
        val sourceFree = ConstructedDefinitionUntypedBackend
          .lower(malformed)
          .left
          .toOption
          .get
        val generated = ConstructedDefinitionGeneratedOriginAdapter
          .lower(malformed, s"<two-parameter-null-$index>")
          .left
          .toOption
          .get
        assert(sourceFree.message.toLowerCase.contains(expected))
        assert(generated.message.toLowerCase.contains(expected))
      }
    }
  }

  test("unsupported body nodes and missing or unconsumed sidecars fail deterministically") {
    val binders = Vector(BinderId(80), BinderId(81))
    val unsupported = reflectedTerm(
      TermShape.Unsupported("TwoParameterUnsupported", "bad"),
      Vector.empty
    )
    val missing = reflectedTerm(
      TermShape.Parenthesized(
        TermShape.Typed(
          TermShape.BoundReference(binders(0), "x"),
          "Int"
        )
      ),
      Vector.empty
    )
    val unconsumed = reflectedTerm(
      TermShape.BoundReference(binders(1), "y"),
      Vector(TypeNormalForm.STypeIdent("Int"))
    )
    val cases = Vector(
      unsupported -> "TwoParameterUnsupported",
      missing -> "ordinal 0",
      unconsumed -> "consumed 0 of 1"
    )

    cases.zipWithIndex.foreach { case ((malformedBody, expected), index) =>
      val malformed = reflectedDefinition(binders, malformedBody)
      withContext {
        val sourceFree = ConstructedDefinitionUntypedBackend
          .lower(malformed)
          .left
          .toOption
          .get
        val generated = ConstructedDefinitionGeneratedOriginAdapter
          .lower(malformed, s"<two-parameter-body-$index>")
          .left
          .toOption
          .get
        assert(sourceFree.message.contains(expected), clues(sourceFree.message))
        assert(generated.message.contains(expected), clues(generated.message))
      }
    }
  }

  private def definition(
      binders: Vector[BinderId] = Vector(BinderId(1), BinderId(2)),
      body: ConstructedTerm | Null = null,
      methodName: String = "pair",
      firstParameterType: TypeNormalForm = TypeNormalForm.STypeIdent("Int"),
      secondParameterType: TypeNormalForm = TypeNormalForm.STypeIdent("String"),
      resultType: TypeNormalForm = TypeNormalForm.STypeTuple(
        List(TypeNormalForm.STypeIdent("Int"), TypeNormalForm.STypeIdent("String"))
      )
  ): ConstructedDefinition.TwoParameterDef =
    val completedBody = Option(body).getOrElse(
      ConstructedTerm
        .fromShapeInScope(
          TermShape.Tuple(
            List(
              TermShape.BoundReference(binders(0), "hostile-y"),
              TermShape.BoundReference(binders(1), "hostile-x")
            )
          ),
          binders
        )
        .toOption
        .get
    )
    ConstructedDefinition
      .twoParameterDef(
        DefinitionName.plain(methodName).toOption.get,
        binders(0),
        DefinitionName.plain("x").toOption.get,
        firstParameterType,
        binders(1),
        DefinitionName.plain("y").toOption.get,
        secondParameterType,
        resultType,
        completedBody
      )
      .toOption
      .get

  private def reflectedDefinition(
      binders: Vector[BinderId],
      body: ConstructedTerm
  ): ConstructedDefinition.TwoParameterDef =
    reflectedDefinitionFields(
      DefinitionName.plain("pair").toOption.get,
      binders,
      DefinitionName.plain("x").toOption.get,
      TypeNormalForm.STypeIdent("Int"),
      DefinitionName.plain("y").toOption.get,
      TypeNormalForm.STypeIdent("String"),
      TypeNormalForm.STypeTuple(
        List(TypeNormalForm.STypeIdent("Int"), TypeNormalForm.STypeIdent("String"))
      ),
      body
    )

  private def reflectedDefinitionFields(
      name: DefinitionName | Null,
      binders: Vector[BinderId],
      firstName: DefinitionName | Null,
      firstType: TypeNormalForm | Null,
      secondName: DefinitionName | Null,
      secondType: TypeNormalForm | Null,
      resultType: TypeNormalForm | Null,
      body: ConstructedTerm | Null
  ): ConstructedDefinition.TwoParameterDef =
    val constructor = classOf[ConstructedDefinition.TwoParameterDef]
      .getDeclaredConstructors
      .head
    constructor.setAccessible(true)
    constructor
      .newInstance(
        name,
        binders(0),
        firstName,
        firstType,
        binders(1),
        secondName,
        secondType,
        resultType,
        body
      )
      .asInstanceOf[ConstructedDefinition.TwoParameterDef]

  private def reflectedTerm(
      shape: TermShape,
      sidecars: Vector[TypeNormalForm]
  ): ConstructedTerm =
    val constructor = classOf[ConstructedTerm].getDeclaredConstructors.head
    constructor.setAccessible(true)
    constructor.newInstance(shape, sidecars).asInstanceOf[ConstructedTerm]

  private def allDefinitionTrees(tree: untpd.Tree)(using Context): Vector[untpd.Tree] =
    tree match
      case method: untpd.DefDef =>
        method +: (
          method.paramss.flatten.toVector ++ Vector(method.tpt, method.rhs)
        ).flatMap(allDefinitionTrees)
      case _ => GeneratedOriginFragmentSupport.allTrees(tree)

  private def parseOne(source: String)(using Context): untpd.DefDef =
    val reporter = new StoreReporter(null)
    val parserContext = summon[Context].fresh.setReporter(reporter)
    val parsed = new Parser(
      SourceFile.virtual("TwoParameterDefinitionOracle.scala", source)
    )(using parserContext).parse()
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
        case value: untpd.ValDef =>
          s"(${value.name},${value.mods.flags},rhsEmpty=${value.rhs.isEmpty})"
        case value: untpd.Ident => s"(${value.name})"
        case _ => ""
      val children = tree match
        case value: untpd.DefDef =>
          value.paramss.flatten.toVector ++ Vector(value.tpt, value.rhs)
        case _ => GeneratedOriginFragmentSupport.directChildren(tree)
      s"${tree.getClass.getSimpleName}$detail$position[${children.map(loop).mkString(",")}]"
    loop(method)

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    val reporter = new StoreReporter(null)
    given Context = base.initialCtx.fresh.setReporter(reporter)
    run
