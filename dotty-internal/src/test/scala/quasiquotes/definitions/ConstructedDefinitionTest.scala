package quasiquotes.definitions

import quasiquotes.parser.{TermShape, TypeShape}
import quasiquotes.terms.dotty.ConstructedTermUntypedBackend
import quasiquotes.types.TypeNormalForm

class ConstructedDefinitionTest extends munit.FunSuite:
  import DefinitionConstructionTestFixtures.*

  private val intForm = TypeNormalForm.STypeIdent("Int")
  private val stringForm = TypeNormalForm.STypeIdent("String")
  private val one = constructed(TermShape.Literal("1"))
  private val two = constructed(TermShape.Literal("2"))

  test("factories preserve both completed variants and fields") {
    val method =
      ConstructedDefinition
        .parameterlessDef(plainName, intForm, one)
        .toOption
        .get
    val value =
      ConstructedDefinition
        .immutableVal(keywordName, stringForm, two)
        .toOption
        .get

    assertEquals(method.name, plainName)
    assertEquals(method.resultType, intForm)
    assertEquals(method.body, one)
    assertEquals(value.name, keywordName)
    assertEquals(value.declaredType, stringForm)
    assertEquals(value.rhs, two)
  }

  test("factories reject invalid completed type normal forms") {
    val invalid = TypeNormalForm.STypeIdent("AnyVal")
    val result =
      ConstructedDefinition.parameterlessDef(plainName, invalid, one)

    assert(
      result.left.toOption.get
        .isInstanceOf[
          DefinitionConstructionError.InvalidConstructedDefinitionType
        ]
    )
  }

  test("completed equality and hashing are structural and variant sensitive") {
    val first =
      ConstructedDefinition
        .parameterlessDef(plainName, intForm, one)
        .toOption
        .get
    val second =
      ConstructedDefinition
        .parameterlessDef(plainName, intForm, one)
        .toOption
        .get
    val third =
      ConstructedDefinition
        .parameterlessDef(plainName, intForm, one)
        .toOption
        .get
    val value =
      ConstructedDefinition
        .immutableVal(plainName, intForm, one)
        .toOption
        .get
    val otherName =
      ConstructedDefinition
        .parameterlessDef(keywordName, intForm, one)
        .toOption
        .get
    val otherType =
      ConstructedDefinition
        .parameterlessDef(plainName, stringForm, one)
        .toOption
        .get
    val otherBody =
      ConstructedDefinition
        .parameterlessDef(plainName, intForm, two)
        .toOption
        .get

    assertEquals(first, first)
    assertEquals(first, second)
    assertEquals(second, third)
    assertEquals(first, third)
    assertEquals(first.hashCode, second.hashCode)
    assert(first != value)
    assert(first != otherName)
    assert(first != otherType)
    assert(first != otherBody)
  }

  test("completed rendering is deterministic") {
    val method =
      ConstructedDefinition
        .parameterlessDef(plainName, intForm, one)
        .toOption
        .get

    assertEquals(
      method.render,
      "ConstructedParameterlessDef(name=PlainName(value), resultType=STypeIdent(Int), body=ConstructedTerm(root=Literal(1), ascriptions=[]))"
    )
    assertEquals(method.toString, method.render)
  }

  test("DefinitionShape converts both variants and preserves names") {
    val methodShape =
      DefinitionShape
        .parameterlessDef(
          keywordName,
          TypeShape.Identifier("Int"),
          TermShape.Literal("1")
        )
        .toOption
        .get
    val valueShape =
      DefinitionShape
        .immutableVal(
          plainName,
          TypeShape.Identifier("String"),
          TermShape.Literal("2")
        )
        .toOption
        .get
    val method =
      ConstructedDefinition.fromShape(methodShape).toOption.get
        .asInstanceOf[ConstructedDefinition.ParameterlessDef]
    val value =
      ConstructedDefinition.fromShape(valueShape).toOption.get
        .asInstanceOf[ConstructedDefinition.ImmutableVal]

    assertEquals(method.name, keywordName)
    assertEquals(method.resultType, intForm)
    assertEquals(method.body.root, TermShape.Literal("1"))
    assertEquals(value.name, plainName)
    assertEquals(value.declaredType, stringForm)
    assertEquals(value.rhs.root, TermShape.Literal("2"))
  }

  test("DefinitionShape conversion preserves supported typed bodies") {
    val shape =
      DefinitionShape
        .parameterlessDef(
          plainName,
          TypeShape.Identifier("Int"),
          TermShape.Typed(ident("value"), "String")
        )
        .toOption
        .get
    val completed =
      ConstructedDefinition.fromShape(shape).toOption.get
        .asInstanceOf[ConstructedDefinition.ParameterlessDef]

    assertEquals(completed.body.ascriptionTypes, Vector(stringForm))
  }

  test("DefinitionShape conversion maps a parsed type outside construction") {
    val shape =
      DefinitionShape
        .parameterlessDef(
          plainName,
          TypeShape.Identifier("AnyVal"),
          TermShape.Literal("1")
        )
        .toOption
        .get
    val error =
      ConstructedDefinition.fromShape(shape).left.toOption.get

    assert(
      error
        .isInstanceOf[
          DefinitionConstructionError.UnsupportedParsedDefinitionType
        ]
    )
  }

  test("DefinitionShape conversion maps a defensively corrupted parsed body") {
    val constructor =
      classOf[DefinitionShape.ParameterlessDef]
        .getDeclaredConstructors
        .head
    constructor.setAccessible(true)
    val corrupted =
      constructor
        .newInstance(
          plainName,
          TypeShape.Identifier("Int"),
          TermShape.Unsupported("CorruptedParsedBody", "test-only")
        )
        .asInstanceOf[DefinitionShape.ParameterlessDef]
    val error =
      ConstructedDefinition.fromShape(corrupted).left.toOption.get

    assert(
      error
        .isInstanceOf[
          DefinitionConstructionError.UnsupportedParsedDefinitionBody
        ]
    )
  }

  test("completed body remains usable by the existing term backend only") {
    val definition =
      ConstructedDefinition
        .parameterlessDef(plainName, intForm, one)
        .toOption
        .get
    val lowered =
      ConstructedTermUntypedBackend.lower(definition.body)

    assert(lowered.isRight)
    assertEquals(
      lowered.toOption.get.getClass.getSimpleName,
      "Number"
    )
  }

  test("neutral child-failure categories remain stable and compiler-free") {
    val typeFailure =
      DefinitionConstructionError.DefinitionTypeConstructionFailure(
        "missing logical component"
      )
    val bodyFailure =
      DefinitionConstructionError.BodyConstructionFailure(
        "invalid completed child"
      )
    val parsedBodyFailure =
      DefinitionConstructionError.UnsupportedParsedDefinitionBody(
        "unsupported child"
      )

    assertEquals(
      typeFailure.message,
      "Definition type construction failed: missing logical component"
    )
    assertEquals(
      bodyFailure.message,
      "Definition body construction failed: invalid completed child"
    )
    assertEquals(
      parsedBodyFailure.message,
      "Unsupported parsed definition body: unsupported child"
    )
  }
