package quasiquotes.definitions

import quasiquotes.parser.{TermShape, TypeShape}

class DefinitionShapeTest extends munit.FunSuite:
  private val plainName = DefinitionName.plain("answer").toOption.get
  private val keywordName = DefinitionName.backticked("`type`").toOption.get
  private val intType = TypeShape.Identifier("Int")
  private val literal = TermShape.Literal("42")

  test("simple type alias preserves one validated reusable semantic structure") {
    val rhs =
      TypeShape.Apply(
        TypeShape.Identifier("Option"),
        List(TypeShape.Identifier("Int"))
      )
    val alias = DefinitionShape.simpleTypeAlias(plainName, rhs).toOption.get
    val sameAlias = DefinitionShape.simpleTypeAlias(plainName, rhs).toOption.get

    assertEquals(alias.name, plainName)
    assertEquals(alias.rhs, rhs)
    assertEquals(alias, sameAlias)
    assertEquals(alias.hashCode, sameAlias.hashCode)
    assertEquals(
      alias.render,
      "SimpleTypeAlias(name=PlainName(answer), rhs=TypeApply(TypeIdent(Option), [TypeIdent(Int)]))"
    )
    assertEquals(alias.toString, alias.render)
  }

  test("simple type alias reuses existing Type normal-form admission") {
    val unsupported = TypeShape.Select(TypeShape.Identifier("scala"), "Int")

    assertEquals(
      DefinitionShape.simpleTypeAlias(keywordName, unsupported),
      Left(DefinitionError.UnsupportedDefinitionType("type alias right-hand side"))
    )
    assertEquals(
      DefinitionShape
        .simpleTypeAlias(keywordName, unsupported)
        .left
        .toOption
        .get
        .message,
      "Unsupported type alias right-hand side: expected the currently supported compiler-free structural type subset."
    )
  }

  test("simple type alias does not widen the older ConstructedDefinition family") {
    val alias = DefinitionShape.simpleTypeAlias(plainName, intType).toOption.get

    assertEquals(
      ConstructedDefinition.fromShape(alias),
      Left(
        DefinitionConstructionError.UnsupportedParsedDefinitionType(
          "simple type aliases are outside the current constructed-definition family"
        )
      )
    )
  }

  test("parameterless def and immutable val preserve distinct validated structures") {
    val method = DefinitionShape.parameterlessDef(plainName, intType, literal).toOption.get
    val value = DefinitionShape.immutableVal(plainName, intType, literal).toOption.get

    assertEquals(method.name, plainName)
    assertEquals(method.resultType, intType)
    assertEquals(method.body, literal)
    assertEquals(value.name, plainName)
    assertEquals(value.declaredType, intType)
    assertEquals(value.rhs, literal)
    assert(method != value)
  }

  test("plain and backticked shapes have stable structural equality and rendering") {
    val method = DefinitionShape.parameterlessDef(keywordName, intType, literal).toOption.get
    val sameMethod = DefinitionShape.parameterlessDef(keywordName, intType, literal).toOption.get
    val value = DefinitionShape.immutableVal(keywordName, intType, literal).toOption.get

    assertEquals(method, sameMethod)
    assertEquals(method.hashCode, sameMethod.hashCode)
    assertEquals(
      method.render,
      "ParameterlessDef(name=BacktickedKeywordName(`type`), resultType=TypeIdent(Int), body=Literal(42))"
    )
    assertEquals(
      value.render,
      "ImmutableVal(name=BacktickedKeywordName(`type`), declaredType=TypeIdent(Int), rhs=Literal(42))"
    )
    assertEquals(method.toString, method.render)
  }

  test("supported type and term nodes are validated recursively") {
    val resultType =
      TypeShape.Function(
        List(TypeShape.Identifier("Int"), TypeShape.Identifier("String")),
        TypeShape.Apply(TypeShape.Identifier("Option"), List(TypeShape.Identifier("Boolean")))
      )
    val body =
      TermShape.If(
        TermShape.Identifier("condition", false),
        TermShape.Apply(
          TermShape.Select(TermShape.Identifier("service", false), "answer"),
          List(TermShape.Tuple(List(TermShape.Literal("1"), TermShape.Literal("2"))))
        ),
        TermShape.Parenthesized(TermShape.Unary("-", TermShape.Literal("1")))
      )

    assert(DefinitionShape.parameterlessDef(plainName, resultType, body).isRight)
    assert(DefinitionShape.immutableVal(plainName, resultType, body).isRight)
  }

  test("root nested and selected unsupported types use component-specific stable errors") {
    val rootUnsupported = TypeShape.Unsupported("CompilerTypeTree", "raw detail")
    val nestedUnsupported =
      TypeShape.Apply(TypeShape.Identifier("Option"), List(rootUnsupported))
    val selected = TypeShape.Select(TypeShape.Identifier("scala"), "Int")

    assertEquals(
      DefinitionShape.parameterlessDef(plainName, rootUnsupported, literal),
      Left(DefinitionError.UnsupportedDefinitionType("method result type"))
    )
    assertEquals(
      DefinitionShape.parameterlessDef(plainName, nestedUnsupported, literal),
      Left(DefinitionError.UnsupportedDefinitionType("method result type"))
    )
    assertEquals(
      DefinitionShape.immutableVal(plainName, selected, literal),
      Left(DefinitionError.UnsupportedDefinitionType("value declared type"))
    )
    assertEquals(
      DefinitionShape
        .parameterlessDef(plainName, rootUnsupported, literal)
        .left
        .toOption
        .get
        .message,
      "Unsupported method result type: expected the currently supported compiler-free structural type subset."
    )
  }

  test("root and nested unsupported bodies reject the first unsupported child without compiler names") {
    val first = TermShape.Unsupported("CompilerFirstTree", "first raw detail")
    val second = TermShape.Unsupported("CompilerSecondTree", "second raw detail")
    val nested = TermShape.Apply(TermShape.Identifier("f", false), List(first, second))
    val error = DefinitionShape.parameterlessDef(plainName, intType, nested).left.toOption.get

    assertEquals(
      error,
      DefinitionError.UnsupportedDefinitionBody(
        "method body",
        "the body contains a term shape outside the currently supported structural subset"
      )
    )
    assert(!error.message.contains("CompilerFirstTree"))
    assert(!error.message.contains("CompilerSecondTree"))
    assertEquals(
      DefinitionShape.immutableVal(plainName, intType, first).left.toOption.get.message,
      "Unsupported value right-hand side: the body contains a term shape outside the currently supported structural subset."
    )
  }

  test("placeholder identifiers require later authoritative template metadata") {
    val placeholder = TermShape.Identifier("__qq_term_x", true)

    assertEquals(
      DefinitionShape.parameterlessDef(plainName, intType, placeholder),
      Left(
        DefinitionError.UnsupportedDefinitionBody(
          "method body",
          "placeholder identifiers require authoritative template metadata and are not representation-core bodies"
        )
      )
    )
  }

  test("unsupported tuple arity is rejected as a body representation error") {
    val error =
      DefinitionShape
        .immutableVal(plainName, intType, TermShape.Tuple(List(TermShape.Literal("1"))))
        .left
        .toOption
        .get

    assertEquals(
      error.message,
      "Unsupported value right-hand side: tuple bodies must contain between 2 and 22 elements."
    )
  }

  test("manually constructed unsupported unary and typed nodes are rejected") {
    val unary =
      DefinitionShape
        .parameterlessDef(plainName, intType, TermShape.Unary("++", TermShape.Literal("1")))
        .left
        .toOption
        .get
    val typed =
      DefinitionShape
        .immutableVal(plainName, intType, TermShape.Typed(TermShape.Literal("1"), "Long"))
        .left
        .toOption
        .get

    assertEquals(
      unary.message,
      "Unsupported method body: unary bodies support only +, -, !, and ~."
    )
    assertEquals(
      typed.message,
      "Unsupported value right-hand side: typed bodies support only Int, String, and Boolean ascriptions."
    )
  }
