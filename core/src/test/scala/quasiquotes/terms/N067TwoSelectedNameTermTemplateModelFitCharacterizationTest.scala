package quasiquotes.terms

import quasiquotes.construct.SelectedMemberName
import quasiquotes.definitions.DefinitionName
import quasiquotes.parser.{BinderId, BlockStatement, TermShape}
import quasiquotes.source.*
import quasiquotes.types.{TypeNormalForm, TypeTemplate}

import scala.compiletime.testing.typeCheckErrors

final class N067TwoSelectedNameTermTemplateModelFitCharacterizationTest
    extends munit.FunSuite:
  import TermCoreTestFixtures.*

  private val firstTransport = "__n067_first"
  private val secondTransport = "__n067_second"
  private val scalarTransport = "__n067_scalar"
  private val repeatedTransport = "__n067_repeated"
  private val typeTransport = "__n067_type"

  test("the cardinality-one guard is separate from distinct two-hole completion"):
    val root = selectedChain(firstTransport, secondTransport)
    val current = TermTemplate.createWithSelectedNameHole(
      root,
      GeneratedHoleIndex.empty,
      Vector.empty,
      GeneratedHoleIndex.empty,
      Vector.empty,
      index("first" -> firstTransport, "second" -> secondTransport),
      Vector(
        SelectedNameHoleOccurrence("first", 1),
        SelectedNameHoleOccurrence("second", 0)
      ),
      GeneratedHoleIndex.empty,
      Vector.empty
    )

    assertEquals(current, Left(TermConstructionError.DuplicateSelectedNameHole()))

    val model = distinctModel(root)
    assertEquals(model.requiredBindings, Vector("second", "first"))
    assertEquals(
      model.complete(selectedBindings("alpha", "beta")).map(_.root),
      Right(selectedChain("alpha", "beta"))
    )

  test("completes distinct holes in direct nullary and unary contexts"):
    val direct = selectedChain(firstTransport, secondTransport)
    val roots = Vector(
      direct,
      TermShape.Apply(direct, Nil),
      TermShape.Apply(direct, List(TermShape.Literal("1")))
    )
    val expected = Vector(
      selectedChain("alpha", "beta"),
      TermShape.Apply(selectedChain("alpha", "beta"), Nil),
      TermShape.Apply(
        selectedChain("alpha", "beta"),
        List(TermShape.Literal("1"))
      )
    )

    roots.zip(expected).foreach { case (root, completed) =>
      val model = distinctModel(root)
      assertEquals(
        model.occurrences.map(occurrence =>
          occurrence.selectOrdinal -> occurrence.name
        ),
        Vector(0 -> "second", 1 -> "first")
      )
      assertEquals(
        model.complete(selectedBindings("alpha", "beta")).map(_.root),
        Right(completed)
      )
    }

  test("uses root-first ordinals with one fixed selected name between holes"):
    val root = TermShape.Select(
      TermShape.Select(
        TermShape.Select(ident("receiver"), firstTransport),
        "fixed"
      ),
      secondTransport
    )
    val model = TwoSelectedNameModel
      .createDistinct(
        root,
        Vector(
          ModelOccurrence("first", 2, firstTransport),
          ModelOccurrence("second", 0, secondTransport)
        )
      )
      .fold(error => fail(error.toString), identity)

    assertEquals(
      model.complete(selectedBindings("alpha", "beta")).map(_.root),
      Right(
        TermShape.Select(
          TermShape.Select(
            TermShape.Select(ident("receiver"), "alpha"),
            "fixed"
          ),
          "beta"
        )
      )
    )

  test("requires exactly the distinct typed selected-name binding set"):
    val model = distinctModel(selectedChain(firstTransport, secondTransport))

    assertEquals(
      model.complete(Map("first" -> selected("alpha"))),
      Left(ModelError.MissingBinding("second"))
    )
    assertEquals(
      model.complete(
        selectedBindings("alpha", "beta") + ("extra" -> selected("gamma"))
      ),
      Left(ModelError.ExtraBinding("extra"))
    )
    assertEquals(
      model.complete(
        Map("first" -> selected("alpha"), "second" -> null)
      ),
      Left(ModelError.NullBinding("second"))
    )
    assert(
      typeCheckErrors(
        """val bindings: Map[String, quasiquotes.construct.SelectedMemberName] = Map("first" -> "raw", "second" -> "raw")"""
      ).nonEmpty
    )

  test("preserves scalar repeated and Type categories without merging them"):
    val root = TermShape.Typed(
      TermShape.Apply(
        selectedChain(firstTransport, secondTransport),
        List(ident(scalarTransport), ident(repeatedTransport))
      ),
      typeTransport
    )
    val model = TwoSelectedNameModel
      .createDistinct(
        root,
        distinctOccurrences,
        scalarEntries = Vector("value" -> scalarTransport),
        scalarOccurrences = Vector(TermHoleOccurrence("value", 1)),
        repeatedEntries = Vector("args" -> repeatedTransport),
        repeatedOccurrences =
          Vector(RepeatedTermHoleOccurrence("args", 2)),
        typeEntries = Vector("first" -> typeTransport),
        ascriptions = Vector(TypeTemplate.TTHole("first"))
      )
      .fold(error => fail(error.toString), identity)
    val completed = model
      .complete(
        selectedBindings("invoke", "result"),
        scalarBindings =
          Map("value" -> constructed(TermShape.Literal("1"))),
        repeatedBindings = Map(
          "args" -> Vector(
            constructed(ident("left")),
            constructed(ident("right"))
          )
        ),
        typeBindings =
          Map("first" -> TypeNormalForm.STypeIdent("Int"))
      )
      .fold(error => fail(error.toString), identity)

    assertEquals(
      completed.root,
      TermShape.Typed(
        TermShape.Apply(
          selectedChain("invoke", "result"),
          List(TermShape.Literal("1"), ident("left"), ident("right"))
        ),
        "Int"
      )
    )
    assertEquals(
      completed.ascriptionTypes,
      Vector(TypeNormalForm.STypeIdent("Int"))
    )

    val scalarConflict = TwoSelectedNameModel.createDistinct(
      TermShape.Apply(
        selectedChain(firstTransport, secondTransport),
        List(ident(scalarTransport))
      ),
      distinctOccurrences,
      scalarEntries = Vector("first" -> scalarTransport),
      scalarOccurrences = Vector(TermHoleOccurrence("first", 1))
    )
    assertEquals(
      scalarConflict,
      Left(ModelError.CategoryConflict("first"))
    )

    val repeatedConflict = TwoSelectedNameModel.createDistinct(
      TermShape.Apply(
        selectedChain(firstTransport, secondTransport),
        List(ident(repeatedTransport))
      ),
      distinctOccurrences,
      repeatedEntries = Vector("second" -> repeatedTransport),
      repeatedOccurrences =
        Vector(RepeatedTermHoleOccurrence("second", 1))
    )
    assertEquals(
      repeatedConflict,
      Left(ModelError.CategoryConflict("second"))
    )

  test("rejects duplicate addresses transports and invalid ordinals"):
    val root = selectedChain(firstTransport, secondTransport)
    Vector(
      Vector.empty,
      Vector(ModelOccurrence("first", 1, firstTransport)),
      Vector(
        ModelOccurrence("first", 1, firstTransport),
        ModelOccurrence("second", 0, secondTransport),
        ModelOccurrence("third", 2, "__n067_third")
      )
    ).foreach { occurrences =>
      assertEquals(
        TwoSelectedNameModel.createDistinct(root, occurrences),
        Left(ModelError.Cardinality(occurrences.size))
      )
    }
    assertEquals(
      TwoSelectedNameModel.createDistinct(
        root,
        Vector(
          ModelOccurrence("first", 0, firstTransport),
          ModelOccurrence("second", 0, secondTransport)
        )
      ),
      Left(ModelError.DuplicateOrdinal(0))
    )
    assertEquals(
      TwoSelectedNameModel.createDistinct(
        TermShape.Select(
          TermShape.Select(ident("receiver"), firstTransport),
          firstTransport
        ),
        Vector(
          ModelOccurrence("first", 1, firstTransport),
          ModelOccurrence("second", 0, firstTransport)
        )
      ),
      Left(ModelError.DuplicateTransport(firstTransport))
    )
    assertEquals(
      TwoSelectedNameModel.createDistinct(
        root,
        Vector(
          ModelOccurrence("first", 3, firstTransport),
          ModelOccurrence("second", 0, secondTransport)
        )
      ),
      Left(ModelError.InvalidOccurrence("first", 3))
    )
    assertEquals(
      TwoSelectedNameModel.createDistinct(
        root,
        Vector(
          ModelOccurrence("first", 0, firstTransport),
          ModelOccurrence("second", 1, secondTransport)
        )
      ),
      Left(ModelError.InvalidOccurrence("first", 0))
    )

  test("preserves binder identity scope and normal ascription sidecars"):
    val binder = BinderId(70067)
    val root = TermShape.Lambda1(
      binder,
      "value",
      "Int",
      TermShape.Typed(
        TermShape.Select(
          TermShape.Select(
            TermShape.BoundReference(binder, "value"),
            firstTransport
          ),
          secondTransport
        ),
        typeTransport
      )
    )
    val model = TwoSelectedNameModel
      .createDistinct(
        root,
        distinctOccurrences,
        typeEntries = Vector("result" -> typeTransport),
        ascriptions = Vector(
          TypeTemplate.TTIdent("Int"),
          TypeTemplate.TTHole("result")
        )
      )
      .fold(error => fail(error.toString), identity)
    val completed = model
      .complete(
        selectedBindings("inner", "outer"),
        typeBindings =
          Map("result" -> TypeNormalForm.STypeIdent("String"))
      )
      .fold(error => fail(error.toString), identity)

    assertEquals(
      completed.root,
      TermShape.Lambda1(
        binder,
        "value",
        "Int",
        TermShape.Typed(
          TermShape.Select(
            TermShape.Select(
              TermShape.BoundReference(binder, "value"),
              "inner"
            ),
            "outer"
          ),
          "String"
        )
      )
    )
    assertEquals(
      completed.ascriptionTypes,
      Vector(
        TypeNormalForm.STypeIdent("Int"),
        TypeNormalForm.STypeIdent("String")
      )
    )

  test("semantic identity ignores generated spelling but retains logical position"):
    val first = distinctModel(
      selectedChain("__first_a", "__second_a"),
      Vector(
        ModelOccurrence("first", 1, "__first_a"),
        ModelOccurrence("second", 0, "__second_a")
      )
    )
    val renamed = distinctModel(
      selectedChain("__first_b", "__second_b"),
      Vector(
        ModelOccurrence("first", 1, "__first_b"),
        ModelOccurrence("second", 0, "__second_b")
      )
    )
    val moved = distinctModel(
      selectedChain("__first_c", "__second_c"),
      Vector(
        ModelOccurrence("first", 0, "__second_c"),
        ModelOccurrence("second", 1, "__first_c")
      )
    )
    val nestedQualifier = distinctModel(
      TermShape.Select(
        TermShape.Select(
          TermShape.Select(ident("root"), "nested"),
          "__first_d"
        ),
        "__second_d"
      ),
      Vector(
        ModelOccurrence("first", 1, "__first_d"),
        ModelOccurrence("second", 0, "__second_d")
      )
    )

    assertEquals(first, renamed)
    assertEquals(first.hashCode, renamed.hashCode)
    assertNotEquals(first, moved)
    assertNotEquals(first, nestedQualifier)

  test("repeated logical names need an occurrence-transport identity gate"):
    val generatedIndexFailure = intercept[IllegalArgumentException] {
      GeneratedHoleIndex.fromOccurrences(
        Vector(
          occurrence("member", firstTransport, 0),
          occurrence("member", secondTransport, 1)
        )
      )
    }
    assert(
      generatedIndexFailure.getMessage.contains(
        "A semantic hole name must reuse exactly one generated identifier"
      )
    )

    val sharedTransportIndex =
      GeneratedHoleIndex.fromOccurrences(
        Vector(
          occurrence("member", firstTransport, 0),
          occurrence("member", firstTransport, 1)
        )
      )
    val current = TermTemplate.createWithSelectedNameHole(
      TermShape.Select(
        TermShape.Select(ident("receiver"), firstTransport),
        firstTransport
      ),
      GeneratedHoleIndex.empty,
      Vector.empty,
      GeneratedHoleIndex.empty,
      Vector.empty,
      sharedTransportIndex,
      Vector(
        SelectedNameHoleOccurrence("member", 1),
        SelectedNameHoleOccurrence("member", 0)
      ),
      GeneratedHoleIndex.empty,
      Vector.empty
    )
    assertEquals(current, Left(TermConstructionError.DuplicateSelectedNameHole()))

    val occurrenceAware = TwoSelectedNameModel
      .createOccurrenceAware(
        selectedChain(firstTransport, secondTransport),
        Vector(
          ModelOccurrence("member", 1, firstTransport),
          ModelOccurrence("member", 0, secondTransport)
        )
      )
      .fold(error => fail(error.toString), identity)
    assertEquals(occurrenceAware.requiredBindings, Vector("member"))
    assertEquals(
      occurrenceAware
        .complete(Map("member" -> selected("same")))
        .map(_.root),
      Right(selectedChain("same", "same"))
    )

  test("retains the exact neutral plain non-keyword lexical intersection"):
    val model = distinctModel(selectedChain(firstTransport, secondTransport))
    Vector("ordinary", "member2", "_privateLike").foreach { value =>
      assert(
        model
          .complete(
            Map("first" -> selected(value), "second" -> selected("fixed"))
          )
          .isRight
      )
    }
    Vector("+", "type", "two words", "_").foreach { invalid =>
      assertEquals(
        model.complete(
          Map(
            "first" -> selected(invalid),
            "second" -> selected("fixed")
          )
        ),
        Left(ModelError.LexicalUnsupported(invalid))
      )
    }
    Vector("naïve", "$internal").foreach { invalid =>
      assert(SelectedMemberName.from(invalid).isLeft)
    }

  private def distinctModel(
      root: TermShape,
      occurrences: Vector[ModelOccurrence] = distinctOccurrences
  ): TwoSelectedNameModel =
    TwoSelectedNameModel
      .createDistinct(root, occurrences)
      .fold(error => fail(error.toString), identity)

  private def distinctOccurrences: Vector[ModelOccurrence] =
    Vector(
      ModelOccurrence("first", 1, firstTransport),
      ModelOccurrence("second", 0, secondTransport)
    )

  private def selectedBindings(
      first: String,
      second: String
  ): Map[String, SelectedMemberName] =
    Map("first" -> selected(first), "second" -> selected(second))

  private def selectedChain(first: String, second: String): TermShape =
    TermShape.Select(
      TermShape.Select(ident("receiver"), first),
      second
    )

  private def selected(value: String): SelectedMemberName =
    SelectedMemberName.from(value).fold(error => fail(error.message), identity)

  private def constructed(shape: TermShape): ConstructedTerm =
    ConstructedTerm.fromShape(shape).fold(error => fail(error.message), identity)

  private def occurrence(
      semantic: String,
      generated: String,
      offset: Int
  ): HoleOccurrence =
    HoleOccurrence(
      semantic,
      generated,
      SourceSpan(offset, offset + 1),
      SourceSpan(offset, offset + 1),
      HoleRole.TermTemplate
    )

private final case class ModelOccurrence(
    name: String,
    selectOrdinal: Int,
    transport: String
) derives CanEqual

private sealed trait ModelError derives CanEqual

private object ModelError:
  final case class Cardinality(actual: Int) extends ModelError
  final case class DuplicateLogicalName(name: String) extends ModelError
  final case class DuplicateOrdinal(ordinal: Int) extends ModelError
  final case class DuplicateTransport(transport: String) extends ModelError
  final case class InvalidOccurrence(name: String, ordinal: Int)
      extends ModelError
  final case class CategoryConflict(name: String) extends ModelError
  final case class MissingBinding(name: String) extends ModelError
  final case class ExtraBinding(name: String) extends ModelError
  final case class NullBinding(name: String) extends ModelError
  final case class LexicalUnsupported(name: String) extends ModelError
  final case class Base(error: TermConstructionError) extends ModelError

private final class TwoSelectedNameModel private (
    val root: TermShape,
    val occurrences: Vector[ModelOccurrence],
    val requiredBindings: Vector[String],
    scalarIndex: GeneratedHoleIndex,
    scalarOccurrences: Vector[TermHoleOccurrence],
    repeatedIndex: GeneratedHoleIndex,
    repeatedOccurrences: Vector[RepeatedTermHoleOccurrence],
    typeIndex: GeneratedHoleIndex,
    ascriptions: Vector[TypeTemplate],
    private val semanticRoot: TermShape
):
  def complete(
      selectedBindings: Map[String, SelectedMemberName],
      scalarBindings: Map[String, ConstructedTerm] = Map.empty,
      repeatedBindings: Map[String, Vector[ConstructedTerm]] = Map.empty,
      typeBindings: Map[String, TypeNormalForm] = Map.empty
  ): Either[ModelError, ConstructedTerm] =
    for
      _ <-
        if selectedBindings == null then
          Left(ModelError.NullBinding("<bindings>"))
        else Right(())
      _ <- requiredBindings
        .find(!selectedBindings.contains(_))
        .map(ModelError.MissingBinding.apply)
        .toLeft(())
      _ <- selectedBindings.keys.toVector.sorted
        .find(!requiredBindings.contains(_))
        .map(ModelError.ExtraBinding.apply)
        .toLeft(())
      _ <- selectedBindings.toVector.sortBy(_._1).collectFirst {
        case (name, null) => ModelError.NullBinding(name)
        case (_, selected)
            if DefinitionName.plain(selected.decoded).isLeft =>
          ModelError.LexicalUnsupported(selected.decoded)
      }.toLeft(())
      selectedRoot = TwoSelectedNameModel.rewriteSelectedFields(
        root,
        occurrences.map(occurrence =>
          occurrence.selectOrdinal ->
            selectedBindings(occurrence.name).decoded
        ).toMap
      )
      selectedTemplate <- TermTemplate
        .createWithRepeatedTerms(
          selectedRoot,
          scalarIndex,
          scalarOccurrences,
          repeatedIndex,
          repeatedOccurrences,
          typeIndex,
          ascriptions
        )
        .left
        .map(ModelError.Base.apply)
      completed <- selectedTemplate
        .completeWithRepeatedTerms(
          scalarBindings,
          repeatedBindings,
          typeBindings
        )
        .left
        .map(ModelError.Base.apply)
    yield completed

  override def equals(other: Any): Boolean =
    other match
      case that: TwoSelectedNameModel =>
        semanticRoot == that.semanticRoot &&
        requiredBindings == that.requiredBindings
      case _ => false

  override def hashCode: Int =
    (semanticRoot, requiredBindings).hashCode

private object TwoSelectedNameModel:
  def createDistinct(
      root: TermShape,
      occurrences: Vector[ModelOccurrence],
      scalarEntries: Vector[(String, String)] = Vector.empty,
      scalarOccurrences: Vector[TermHoleOccurrence] = Vector.empty,
      repeatedEntries: Vector[(String, String)] = Vector.empty,
      repeatedOccurrences: Vector[RepeatedTermHoleOccurrence] = Vector.empty,
      typeEntries: Vector[(String, String)] = Vector.empty,
      ascriptions: Vector[TypeTemplate] = Vector.empty
  ): Either[ModelError, TwoSelectedNameModel] =
    create(
      root,
      occurrences,
      allowRepeatedLogicalName = false,
      scalarEntries,
      scalarOccurrences,
      repeatedEntries,
      repeatedOccurrences,
      typeEntries,
      ascriptions
    )

  def createOccurrenceAware(
      root: TermShape,
      occurrences: Vector[ModelOccurrence]
  ): Either[ModelError, TwoSelectedNameModel] =
    create(
      root,
      occurrences,
      allowRepeatedLogicalName = true,
      Vector.empty,
      Vector.empty,
      Vector.empty,
      Vector.empty,
      Vector.empty,
      Vector.empty
    )

  private def create(
      root: TermShape,
      occurrences: Vector[ModelOccurrence],
      allowRepeatedLogicalName: Boolean,
      scalarEntries: Vector[(String, String)],
      scalarOccurrences: Vector[TermHoleOccurrence],
      repeatedEntries: Vector[(String, String)],
      repeatedOccurrences: Vector[RepeatedTermHoleOccurrence],
      typeEntries: Vector[(String, String)],
      ascriptions: Vector[TypeTemplate]
  ): Either[ModelError, TwoSelectedNameModel] =
    val scalarIndex = TermCoreTestFixtures.index(scalarEntries*)
    val repeatedIndex = TermCoreTestFixtures.index(repeatedEntries*)
    val typeIndex = TermCoreTestFixtures.index(typeEntries*)
    val sorted = occurrences.sortBy(_.selectOrdinal)
    val required = sorted.map(_.name).distinct
    val selectedNames = occurrences.map(_.name).toSet
    val termCategoryNames =
      scalarIndex.semanticNames ++ repeatedIndex.semanticNames
    val selects = TermShapeTraversal.selectEntries(root)

    for
      _ <-
        if occurrences.size == 2 then Right(())
        else Left(ModelError.Cardinality(occurrences.size))
      _ <-
        if allowRepeatedLogicalName || required.size == 2 then Right(())
        else Left(ModelError.DuplicateLogicalName(required.head))
      _ <- duplicate(occurrences.map(_.selectOrdinal))
        .map(ModelError.DuplicateOrdinal.apply)
        .toLeft(())
      _ <- duplicate(occurrences.map(_.transport))
        .map(ModelError.DuplicateTransport.apply)
        .toLeft(())
      _ <- occurrences.find { occurrence =>
        occurrence.selectOrdinal < 0 ||
        !selects
          .lift(occurrence.selectOrdinal)
          .exists(_.name == occurrence.transport) ||
        selects.count(_.name == occurrence.transport) != 1
      }.map(occurrence =>
        ModelError.InvalidOccurrence(
          occurrence.name,
          occurrence.selectOrdinal
        )
      ).toLeft(())
      _ <- (selectedNames intersect termCategoryNames).toVector.sorted.headOption
        .map(ModelError.CategoryConflict.apply)
        .toLeft(())
      _ <- TermTemplate
        .createWithRepeatedTerms(
          root,
          scalarIndex,
          scalarOccurrences,
          repeatedIndex,
          repeatedOccurrences,
          typeIndex,
          ascriptions
        )
        .left
        .map(ModelError.Base.apply)
      semanticRoot = rewriteSelectedFields(
        root,
        occurrences.map(occurrence =>
          occurrence.selectOrdinal ->
            ("__logical_selected_" + occurrence.name)
        ).toMap
      )
    yield new TwoSelectedNameModel(
      root,
      sorted,
      required,
      scalarIndex,
      scalarOccurrences,
      repeatedIndex,
      repeatedOccurrences,
      typeIndex,
      ascriptions,
      semanticRoot
    )

  private def duplicate[A](values: Vector[A]): Option[A] =
    values.groupBy(identity).collectFirst { case (value, matches)
        if matches.size > 1 =>
      value
    }

  def rewriteSelectedFields(
      root: TermShape,
      replacements: Map[Int, String]
  ): TermShape =
    var nextSelectOrdinal = 0

    def loop(shape: TermShape): TermShape =
      shape match
        case TermShape.Select(qualifier, name) =>
          val ordinal = nextSelectOrdinal
          nextSelectOrdinal += 1
          TermShape.Select(
            loop(qualifier),
            replacements.getOrElse(ordinal, name)
          )
        case TermShape.Lambda1(binderId, displayName, parameterType, body) =>
          TermShape.Lambda1(
            binderId,
            displayName,
            parameterType,
            loop(body)
          )
        case TermShape.Apply(function, arguments) =>
          TermShape.Apply(loop(function), arguments.map(loop))
        case TermShape.New(constructor, arguments) =>
          TermShape.New(constructor, arguments.map(loop))
        case TermShape.Infix(left, operator, right) =>
          TermShape.Infix(loop(left), operator, loop(right))
        case TermShape.Unary(operator, operand) =>
          TermShape.Unary(operator, loop(operand))
        case TermShape.InterpolatedString(prefix, parts, arguments) =>
          TermShape.InterpolatedString(prefix, parts, arguments.map(loop))
        case TermShape.Typed(expression, typeName) =>
          TermShape.Typed(loop(expression), typeName)
        case TermShape.Tuple(elements) =>
          TermShape.Tuple(elements.map(loop))
        case TermShape.If(condition, thenBranch, elseBranch) =>
          TermShape.If(loop(condition), loop(thenBranch), loop(elseBranch))
        case TermShape.Block(statements, result) =>
          TermShape.Block(
            statements.map {
              case BlockStatement.LocalVal(
                    binderId,
                    displayName,
                    declaredType,
                    initializer
                  ) =>
                BlockStatement.LocalVal(
                  binderId,
                  displayName,
                  declaredType,
                  loop(initializer)
                )
              case BlockStatement.LocalDef(
                    methodBinderId,
                    methodDisplayName,
                    parameterBinderId,
                    parameterDisplayName,
                    parameterType,
                    resultType,
                    body
                  ) =>
                BlockStatement.LocalDef(
                  methodBinderId,
                  methodDisplayName,
                  parameterBinderId,
                  parameterDisplayName,
                  parameterType,
                  resultType,
                  loop(body)
                )
              case term: TermShape => loop(term)
            },
            loop(result)
          )
        case TermShape.Parenthesized(expression) =>
          TermShape.Parenthesized(loop(expression))
        case unchanged => unchanged

    val result = loop(root)
    require(
      nextSelectOrdinal == TermShapeTraversal.selectEntries(root).size,
      "Select preorder must be consumed exactly once"
    )
    result
