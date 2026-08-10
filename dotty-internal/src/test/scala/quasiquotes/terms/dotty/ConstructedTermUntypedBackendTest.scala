package quasiquotes.terms.dotty

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.core.Symbols.NoSymbol

import quasiquotes.parser.{
  DottySourceSpanAdapter,
  TermShape,
  TermShapeInspector,
  TinyTermParser,
  TinyTypeParser,
  TypeShapeInspector
}
import quasiquotes.terms.ConstructedTerm
import quasiquotes.types.TypeNormalForm

class ConstructedTermUntypedBackendTest extends munit.FunSuite:
  import ConstructedTermUntypedBackendError.*
  import TypeNormalForm.*

  private val simpleRoundTrips = Vector(
    "value",
    "1",
    "-1",
    "\"text\"",
    "true",
    "service.answer",
    "f(left, right)",
    "left + right",
    "!condition",
    "value: Int",
    "(left, right)",
    "if condition then left else right",
    "(value)"
  )

  private val completedTypeSidecars = Vector(
    STypeIdent("Int"),
    STypeIdent("String"),
    STypeIdent("Boolean"),
    STypeApply(STypeIdent("List"), List(STypeIdent("Int"))),
    STypeApply(STypeIdent("Option"), List(STypeIdent("String"))),
    STypeApply(
      STypeIdent("List"),
      List(STypeApply(STypeIdent("Option"), List(STypeIdent("Int"))))
    ),
    STypeApply(
      STypeIdent("Either"),
      List(
        STypeApply(STypeIdent("List"), List(STypeIdent("Int"))),
        STypeApply(STypeIdent("Option"), List(STypeIdent("String")))
      )
    ),
    STypeTuple(List(STypeIdent("Int"), STypeIdent("String"))),
    STypeTuple(
      List(STypeIdent("Int"), STypeIdent("String"), STypeIdent("Boolean"))
    ),
    STypeFunction(List(STypeIdent("Int")), STypeIdent("String")),
    STypeFunction(
      List(STypeIdent("Int"), STypeIdent("String")),
      STypeIdent("Boolean")
    )
  )

  simpleRoundTrips.foreach { source =>
    test(s"round-trips the parser-observed shape and raw structure: $source") {
      val parsed = TinyTermParser.parseOrThrow(source)
      val constructed = ConstructedTerm.fromShape(parsed.shape).toOption.get
      val raw = ConstructedTermUntypedBackend.lower(constructed).toOption.get

      assertEquals(TermShapeInspector.inspect(raw), constructed.root)
      assertEquals(TermShapeInspector.rawStructure(raw), parsed.rawStructure)
    }
  }

  completedTypeSidecars.foreach { normalForm =>
    test(s"lowers completed type sidecar structurally: ${normalForm.render}") {
      val source = renderTypeSource(normalForm)
      val shape =
        TermShape.Typed(
          TermShape.Identifier("value", isPlaceholder = false),
          source
        )
      val constructed =
        ConstructedTerm.create(shape, Vector(normalForm)).toOption.get
      val raw = ConstructedTermUntypedBackend.lower(constructed).toOption.get
      val rawType = raw match
        case untpd.Typed(_, typeTree) => typeTree
        case other =>
          fail(s"expected Typed, found ${other.getClass.getSimpleName}")

      assertEquals(TermShapeInspector.inspect(raw), constructed.root)
      assertEquals(
        TypeShapeInspector.rawStructure(rawType),
        TinyTypeParser.parseOrThrow(source).rawStructure
      )
    }
  }

  test("consumes nested completed type sidecars in exact typed-node preorder") {
    val outer = STypeTuple(
      List(STypeIdent("Int"), STypeIdent("String"), STypeIdent("Boolean"))
    )
    val firstInner =
      STypeApply(
        STypeIdent("List"),
        List(STypeApply(STypeIdent("Option"), List(STypeIdent("Int"))))
      )
    val secondInner =
      STypeFunction(
        List(STypeIdent("Int"), STypeIdent("String")),
        STypeIdent("Boolean")
      )
    val root =
      TermShape.Typed(
        TermShape.Tuple(
          List(
            TermShape.Typed(ident("left"), renderTypeSource(firstInner)),
            TermShape.Typed(ident("right"), renderTypeSource(secondInner))
          )
        ),
        renderTypeSource(outer)
      )
    val constructed =
      ConstructedTerm
        .create(root, Vector(outer, firstInner, secondInner))
        .toOption
        .get
    val raw = ConstructedTermUntypedBackend.lower(constructed).toOption.get

    assertEquals(TermShapeInspector.inspect(raw), root)
    raw match
      case untpd.Typed(
            untpd.Tuple(
              untpd.Typed(_, firstType) ::
                untpd.Typed(_, secondType) ::
                Nil
            ),
            outerType
          ) =>
        assertEquals(
          TypeShapeInspector.rawStructure(outerType),
          TinyTypeParser.parseOrThrow(renderTypeSource(outer)).rawStructure
        )
        assertEquals(
          TypeShapeInspector.rawStructure(firstType),
          TinyTypeParser.parseOrThrow(renderTypeSource(firstInner)).rawStructure
        )
        assertEquals(
          TypeShapeInspector.rawStructure(secondType),
          TinyTypeParser.parseOrThrow(renderTypeSource(secondInner)).rawStructure
        )
      case other =>
        fail(s"unexpected nested typed raw tree: ${other.getClass.getSimpleName}")
  }

  test("preserves sidecar order across applications and tuples with multiple typed descendants") {
    val root =
      TermShape.Apply(
        ident("f"),
        List(
          TermShape.Typed(ident("first"), "Int"),
          TermShape.Tuple(
            List(
              TermShape.Typed(ident("second"), "String"),
              TermShape.Typed(ident("third"), "Boolean")
            )
          )
        )
      )
    val constructed =
      ConstructedTerm
        .create(
          root,
          Vector(STypeIdent("Int"), STypeIdent("String"), STypeIdent("Boolean"))
        )
        .toOption
        .get
    val raw = ConstructedTermUntypedBackend.lower(constructed).toOption.get

    assertEquals(TermShapeInspector.inspect(raw), root)
  }

  test("preserves sidecar order across typed if condition then and else branches") {
    val root =
      TermShape.If(
        TermShape.Typed(ident("condition"), "Boolean"),
        TermShape.Typed(ident("thenValue"), "Int"),
        TermShape.Typed(ident("elseValue"), "String")
      )
    val constructed =
      ConstructedTerm
        .create(
          root,
          Vector(STypeIdent("Boolean"), STypeIdent("Int"), STypeIdent("String"))
        )
        .toOption
        .get
    val raw = ConstructedTermUntypedBackend.lower(constructed).toOption.get

    assertEquals(TermShapeInspector.inspect(raw), root)
  }

  test("preserves decimal integer Boolean and semantic String literal values") {
    val values = Vector(
      "0",
      "-2147483649",
      "true",
      "false",
      "\"\"",
      "\"a quoted \"value\"\"",
      "\"a\\backslash\"",
      "\"first\nsecond\""
    )

    values.foreach { value =>
      val shape = TermShape.Literal(value)
      val constructed = ConstructedTerm.fromShape(shape).toOption.get
      val raw = ConstructedTermUntypedBackend.lower(constructed).toOption.get
      assertEquals(TermShapeInspector.inspect(raw), shape)
    }
  }

  test("rejects literal categories that the semantic text cannot lower exactly") {
    Vector("3.14", "'x'", "null", "1_000").foreach { value =>
      val constructed =
        ConstructedTerm.fromShape(TermShape.Literal(value)).toOption.get
      assertEquals(
        ConstructedTermUntypedBackend.lower(constructed),
        Left(UnsupportedLiteral(value))
      )
    }
  }

  test("lowers the admitted Tuple22 boundary without widening arity") {
    val root = TermShape.Tuple((1 to 22).map(index => ident(s"value$index")).toList)
    val constructed = ConstructedTerm.fromShape(root).toOption.get
    val raw = ConstructedTermUntypedBackend.lower(constructed).toOption.get

    assertEquals(TermShapeInspector.inspect(raw), root)
    raw match
      case untpd.Tuple(elements) => assertEquals(elements.size, 22)
      case other => fail(s"expected Tuple, found ${other.getClass.getSimpleName}")
  }

  test("creates no source or position claim") {
    val parsed =
      TinyTermParser.parseOrThrow(
        "if condition then service.answer(value: Int) else (fallback)"
      )
    val constructed = ConstructedTerm.fromShape(parsed.shape).toOption.get
    val raw = ConstructedTermUntypedBackend.lower(constructed).toOption.get

    allTrees(raw).foreach { tree =>
      assertEquals(DottySourceSpanAdapter.fromTree(tree), None)
      assert(!tree.source.exists)
    }
  }

  test("assigns no symbols owners or typed splices") {
    val constructed =
      ConstructedTerm
        .fromShape(
          TermShape.Apply(
            TermShape.Select(ident("service"), "answer"),
            List(TermShape.Literal("1"))
          )
        )
        .toOption
        .get
    val raw = ConstructedTermUntypedBackend.lower(constructed).toOption.get
    val base = new ContextBase
    given Context = base.initialCtx

    allTrees(raw).foreach(tree => assertEquals(tree.symbol, NoSymbol))
    assert(!allTrees(raw).exists(_.isInstanceOf[untpd.TypedSplice]))
  }

  test("keeps parser Quotes and Macro-Paradise dependencies out of the backend") {
    val root =
      Path.of("dotty-internal", "src", "main", "scala", "quasiquotes", "terms", "dotty")
    val forbidden =
      Vector(
        "Scala3ParserBridge",
        "TinyTermParser",
        "TinyTypeParser",
        "dotty.tools.dotc.parsing",
        "scala.quoted",
        "Quotes",
        "Expr[",
        "TypeRepr",
        "macroparadise"
      )
    val stream = Files.walk(root)
    try
      stream
        .filter(path => path.toString.endsWith(".scala"))
        .forEach { path =>
          val source = Files.readString(path, StandardCharsets.UTF_8)
          forbidden.foreach(value =>
            assert(!source.contains(value), clues(path, value))
          )
        }
    finally stream.close()
  }

  test("backend errors have stable exact-boundary messages") {
    assertEquals(
      UnsupportedLiteral("3.14").message,
      "Unsupported constructed-term literal `3.14`: expected a decimal integer, Boolean, or semantic String value enclosed by quotes."
    )
    assertEquals(
      MissingTypeSidecar(2).message,
      "Missing completed type sidecar at typed ordinal 2."
    )
    assertEquals(
      UnconsumedTypeSidecars(1, 2).message,
      "Constructed-term lowering consumed 1 of 2 completed type sidecars."
    )
  }

  private def ident(name: String): TermShape =
    TermShape.Identifier(name, isPlaceholder = false)

  private def renderTypeSource(normalForm: TypeNormalForm): String =
    normalForm match
      case STypeIdent(name) => name
      case STypeApply(constructor, arguments) =>
        s"${renderTypeSource(constructor)}[${arguments.map(renderTypeSource).mkString(", ")}]"
      case STypeTuple(elements) =>
        s"(${elements.map(renderTypeSource).mkString(", ")})"
      case STypeFunction(argument :: Nil, result) =>
        s"${renderTypeSource(argument)} => ${renderTypeSource(result)}"
      case STypeFunction(arguments, result) =>
        s"(${arguments.map(renderTypeSource).mkString(", ")}) => ${renderTypeSource(result)}"

  private def allTrees(tree: untpd.Tree): List[untpd.Tree] =
    tree match
      case untpd.Select(qualifier, _) =>
        tree :: allTrees(qualifier)
      case untpd.Apply(function, arguments) =>
        tree :: allTrees(function) ::: arguments.flatMap(allTrees)
      case untpd.InfixOp(left, operator, right) =>
        tree :: allTrees(left) ::: allTrees(operator) ::: allTrees(right)
      case untpd.PrefixOp(operator, operand) =>
        tree :: allTrees(operator) ::: allTrees(operand)
      case untpd.Typed(expression, typeTree) =>
        tree :: allTrees(expression) ::: allTrees(typeTree)
      case untpd.AppliedTypeTree(constructor, arguments) =>
        tree :: allTrees(constructor) ::: arguments.flatMap(allTrees)
      case untpd.Tuple(elements) =>
        tree :: elements.flatMap(allTrees)
      case untpd.Function(arguments, result) =>
        tree :: arguments.flatMap(allTrees) ::: allTrees(result)
      case untpd.If(condition, thenBranch, elseBranch) =>
        tree :: allTrees(condition) ::: allTrees(thenBranch) ::: allTrees(elseBranch)
      case untpd.Parens(expression) =>
        tree :: allTrees(expression)
      case _ =>
        tree :: Nil
