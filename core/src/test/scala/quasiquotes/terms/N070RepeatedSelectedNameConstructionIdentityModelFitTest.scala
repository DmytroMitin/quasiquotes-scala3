package quasiquotes.terms

import quasiquotes.construct.SelectedMemberName
import quasiquotes.definitions.DefinitionName
import quasiquotes.matching.{
  SelectedNameSourceNormalizationError,
  SelectedNameSourceNormalizer,
  SelectedNameSourcePosition
}
import quasiquotes.parser.{BinderId, BlockStatement, TermShape}
import quasiquotes.source.*
import quasiquotes.types.{TypeNormalForm, TypeTemplate}

import scala.compiletime.testing.typeCheckErrors

final class N070RepeatedSelectedNameConstructionIdentityModelFitTest
    extends munit.FunSuite:
  import TermCoreTestFixtures.*

  private val innerTransport = "__n070_inner"
  private val outerTransport = "__n070_outer"
  private val scalarTransport = "__n070_scalar"
  private val repeatedTransport = "__n070_repeated"
  private val typeTransport = "__n070_type"

  test("one logical binding can address two occurrence-scoped selected positions"):
    val model = RepeatedSelectedNameConstructionModel.create(
      TermShape.Select(
        TermShape.Select(ident("receiver"), "__n070_inner"),
        "__n070_outer"
      ),
      Vector(
        SelectedNameHoleOccurrenceWithTransport(
          "member",
          1,
          "__n070_inner"
        ),
        SelectedNameHoleOccurrenceWithTransport(
          "member",
          0,
          "__n070_outer"
        )
      )
    )

    val accepted = model.fold(error => fail(error.toString), identity)
    assertEquals(accepted.requiredBindings, Vector("member"))
    assertEquals(
      accepted.complete(Map("member" -> selected("same"))).map(_.root),
      Right(selectedChain("same", "same"))
    )

  test("current indexes and constructors reject repeated construction at separate guards"):
    val generatedIndexFailure = intercept[IllegalArgumentException] {
      GeneratedHoleIndex.fromOccurrences(
        Vector(
          occurrence("member", innerTransport, 0),
          occurrence("member", outerTransport, 1)
        )
      )
    }
    assert(
      generatedIndexFailure.getMessage.contains(
        "A semantic hole name must reuse exactly one generated identifier"
      )
    )

    val sharedIndex = index("member" -> innerTransport)
    val sharedRoot = selectedChain(innerTransport, innerTransport)
    val repeatedOccurrences = Vector(
      SelectedNameHoleOccurrence("member", 1),
      SelectedNameHoleOccurrence("member", 0)
    )
    assertEquals(
      TermTemplate.createWithSelectedNameHole(
        sharedRoot,
        GeneratedHoleIndex.empty,
        Vector.empty,
        GeneratedHoleIndex.empty,
        Vector.empty,
        sharedIndex,
        repeatedOccurrences,
        GeneratedHoleIndex.empty,
        Vector.empty
      ),
      Left(TermConstructionError.DuplicateSelectedNameHole())
    )
    assertEquals(
      TermTemplate.createWithTwoSelectedNameHoles(
        sharedRoot,
        GeneratedHoleIndex.empty,
        Vector.empty,
        GeneratedHoleIndex.empty,
        Vector.empty,
        sharedIndex,
        repeatedOccurrences,
        GeneratedHoleIndex.empty,
        Vector.empty
      ),
      Left(TermConstructionError.SelectedNameHoleCardinalityMismatch(2, 1, 2))
    )

  test("completes repeated selected names in direct nullary and unary envelopes"):
    val direct = selectedChain(innerTransport, outerTransport)
    val cases = Vector(
      direct -> selectedChain("same", "same"),
      TermShape.Apply(direct, Nil) ->
        TermShape.Apply(selectedChain("same", "same"), Nil),
      TermShape.Apply(direct, List(TermShape.Literal("1"))) ->
        TermShape.Apply(
          selectedChain("same", "same"),
          List(TermShape.Literal("1"))
        )
    )

    cases.foreach { case (root, expected) =>
      assertEquals(
        repeatedModel(root)
          .complete(Map("member" -> selected("same")))
          .map(_.root),
        Right(expected)
      )
    }

  test("uses exact root-first ordinals with fixed and nested selected names"):
    val root = TermShape.Select(
      TermShape.Select(
        TermShape.Select(
          TermShape.Select(ident("receiver"), "namespace"),
          innerTransport
        ),
        "fixed"
      ),
      outerTransport
    )
    val model = repeatedModel(
      root,
      Vector(
        SelectedNameHoleOccurrenceWithTransport("member", 2, innerTransport),
        SelectedNameHoleOccurrenceWithTransport("member", 0, outerTransport)
      )
    )

    assertEquals(model.occurrences.map(_.selectOrdinal), Vector(0, 2))
    assertEquals(
      model.complete(Map("member" -> selected("same"))).map(_.root),
      Right(
        TermShape.Select(
          TermShape.Select(
            TermShape.Select(
              TermShape.Select(ident("receiver"), "namespace"),
              "same"
            ),
            "fixed"
          ),
          "same"
        )
      )
    )

  test("preserves scalar repeated and Type sidecars with one selected binding"):
    val root = TermShape.Typed(
      TermShape.Apply(
        selectedChain(innerTransport, outerTransport),
        List(ident(scalarTransport), ident(repeatedTransport))
      ),
      typeTransport
    )
    val model = RepeatedSelectedNameConstructionModel
      .create(
        root,
        repeatedOccurrences,
        scalarEntries = Vector("valueArg" -> scalarTransport),
        scalarOccurrences = Vector(TermHoleOccurrence("valueArg", 1)),
        repeatedEntries = Vector("args" -> repeatedTransport),
        repeatedTermOccurrences =
          Vector(RepeatedTermHoleOccurrence("args", 2)),
        typeEntries = Vector("member" -> typeTransport),
        ascriptions = Vector(TypeTemplate.TTHole("member"))
      )
      .fold(error => fail(error.toString), identity)

    val completed = model
      .complete(
        Map("member" -> selected("same")),
        scalarBindings =
          Map("valueArg" -> constructed(TermShape.Literal("1"))),
        repeatedBindings = Map(
          "args" -> Vector(constructed(ident("left")), constructed(ident("right")))
        ),
        typeBindings = Map("member" -> TypeNormalForm.STypeIdent("String"))
      )
      .fold(error => fail(error.toString), identity)

    assertEquals(model.requiredBindings, Vector("member"))
    assertEquals(
      completed.root,
      TermShape.Typed(
        TermShape.Apply(
          selectedChain("same", "same"),
          List(TermShape.Literal("1"), ident("left"), ident("right"))
        ),
        "String"
      )
    )
    assertEquals(
      completed.ascriptionTypes,
      Vector(TypeNormalForm.STypeIdent("String"))
    )

  test("preserves binder identity scope and Type sidecars"):
    val binder = BinderId(70070)
    val root = TermShape.Lambda1(
      binder,
      "value",
      "Int",
      TermShape.Typed(
        TermShape.Select(
          TermShape.Select(
            TermShape.BoundReference(binder, "value"),
            innerTransport
          ),
          outerTransport
        ),
        typeTransport
      )
    )
    val model = RepeatedSelectedNameConstructionModel
      .create(
        root,
        repeatedOccurrences,
        typeEntries = Vector("member" -> typeTransport),
        ascriptions = Vector(
          TypeTemplate.TTIdent("Int"),
          TypeTemplate.TTHole("member")
        )
      )
      .fold(error => fail(error.toString), identity)
    val completed = model
      .complete(
        Map("member" -> selected("same")),
        typeBindings = Map("member" -> TypeNormalForm.STypeIdent("String"))
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
              "same"
            ),
            "same"
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

  test("requires one exact typed binding and preserves the neutral lexical boundary"):
    val model = repeatedModel(selectedChain(innerTransport, outerTransport))
    assertEquals(
      model.complete(Map.empty),
      Left(RepeatedSelectedNameModelError.MissingBinding("member"))
    )
    assertEquals(
      model.complete(
        Map("member" -> selected("same"), "extra" -> selected("extra"))
      ),
      Left(RepeatedSelectedNameModelError.ExtraBinding("extra"))
    )
    assertEquals(
      model.complete(null),
      Left(RepeatedSelectedNameModelError.NullBinding("<bindings>"))
    )
    assertEquals(
      model.complete(Map("member" -> null)),
      Left(RepeatedSelectedNameModelError.NullBinding("member"))
    )
    Vector("ordinary", "member2", "_privateLike").foreach { value =>
      assert(model.complete(Map("member" -> selected(value))).isRight)
    }
    Vector("+", "type", "two words", "_").foreach { invalid =>
      assertEquals(
        model.complete(Map("member" -> selected(invalid))),
        Left(RepeatedSelectedNameModelError.LexicalUnsupported(invalid))
      )
    }
    Vector("naïve", "$internal").foreach { invalid =>
      assert(SelectedMemberName.from(invalid).isLeft)
    }
    assert(
      typeCheckErrors(
        """val bindings: Map[String, quasiquotes.construct.SelectedMemberName] = Map("member" -> "raw")"""
      ).nonEmpty
    )

  test("rejects stale duplicated extra and cross-category occurrence metadata"):
    val root = selectedChain(innerTransport, outerTransport)
    assertEquals(
      RepeatedSelectedNameConstructionModel.create(root, Vector.empty),
      Left(RepeatedSelectedNameModelError.Cardinality(0))
    )
    assertEquals(
      RepeatedSelectedNameConstructionModel.create(
        TermShape.Select(root, "__n070_third"),
        repeatedOccurrences :+
          SelectedNameHoleOccurrenceWithTransport(
            "member",
            0,
            "__n070_third"
          )
      ),
      Left(RepeatedSelectedNameModelError.Cardinality(3))
    )
    assertEquals(
      RepeatedSelectedNameConstructionModel.create(
        root,
        Vector(
          SelectedNameHoleOccurrenceWithTransport("member", 0, innerTransport),
          SelectedNameHoleOccurrenceWithTransport("member", 0, outerTransport)
        )
      ),
      Left(RepeatedSelectedNameModelError.DuplicateOrdinal(0))
    )
    assertEquals(
      RepeatedSelectedNameConstructionModel.create(
        selectedChain(innerTransport, innerTransport),
        Vector(
          SelectedNameHoleOccurrenceWithTransport("member", 1, innerTransport),
          SelectedNameHoleOccurrenceWithTransport("member", 0, innerTransport)
        )
      ),
      Left(RepeatedSelectedNameModelError.DuplicateTransport(innerTransport))
    )
    assertEquals(
      RepeatedSelectedNameConstructionModel.create(
        root,
        Vector(
          SelectedNameHoleOccurrenceWithTransport("member", 0, innerTransport),
          SelectedNameHoleOccurrenceWithTransport("member", 1, outerTransport)
        )
      ),
      Left(RepeatedSelectedNameModelError.InvalidOccurrence("member", 0))
    )
    assertEquals(
      RepeatedSelectedNameConstructionModel.create(
        TermShape.Apply(root, List(ident(innerTransport))),
        repeatedOccurrences
      ),
      Left(RepeatedSelectedNameModelError.TransportReuse(innerTransport))
    )
    assertEquals(
      RepeatedSelectedNameConstructionModel.create(
        TermShape.Apply(root, List(ident(scalarTransport))),
        repeatedOccurrences,
        scalarEntries = Vector("member" -> scalarTransport),
        scalarOccurrences = Vector(TermHoleOccurrence("member", 1))
      ),
      Left(RepeatedSelectedNameModelError.CategoryConflict("member"))
    )
    assertEquals(
      RepeatedSelectedNameConstructionModel.create(
        root,
        repeatedOccurrences,
        typeEntries = Vector("typeValue" -> innerTransport)
      ),
      Left(
        RepeatedSelectedNameModelError.GeneratedCategoryConflict(innerTransport)
      )
    )

  test("semantic equality and hash ignore allocated transports but retain positions"):
    val original = repeatedModel(selectedChain(innerTransport, outerTransport))
    val renamed = repeatedModel(
      selectedChain("__renamed_inner", "__renamed_outer"),
      Vector(
        SelectedNameHoleOccurrenceWithTransport("member", 1, "__renamed_inner"),
        SelectedNameHoleOccurrenceWithTransport("member", 0, "__renamed_outer")
      )
    )
    val moved = repeatedModel(
      TermShape.Select(
        TermShape.Select(
          TermShape.Select(ident("receiver"), innerTransport),
          "fixed"
        ),
        outerTransport
      ),
      Vector(
        SelectedNameHoleOccurrenceWithTransport("member", 2, innerTransport),
        SelectedNameHoleOccurrenceWithTransport("member", 0, outerTransport)
      )
    )

    assertEquals(original, renamed)
    assertEquals(original.hashCode, renamed.hashCode)
    assertNotEquals(original, moved)

  test("N068 normalization can supply the private occurrence vector without becoming an API"):
    val normalized = SelectedNameSourceNormalizer
      .normalize(
        "$receiver.$member.$member",
        Vector(
          SelectedNameSourcePosition(1, 1),
          SelectedNameSourcePosition(2, 0)
        )
      )
      .fold(error => fail(error.message), identity)
    val byOrdinal = normalized.selectedOccurrences.map(occurrence =>
      occurrence.selectOrdinal -> occurrence
    ).toMap
    val root = selectedChain(
      byOrdinal(1).transport,
      byOrdinal(0).transport
    )
    val model = RepeatedSelectedNameConstructionModel
      .create(
        root,
        normalized.selectedOccurrences.map(occurrence =>
          SelectedNameHoleOccurrenceWithTransport(
            occurrence.name,
            occurrence.selectOrdinal,
            occurrence.transport
          )
        )
      )
      .fold(error => fail(error.toString), identity)

    assertEquals(model.requiredBindings, Vector("member"))
    assertEquals(
      model.complete(Map("member" -> selected("same"))).map(_.root),
      Right(selectedChain("same", "same"))
    )
    assertEquals(
      SelectedNameSourceNormalizer.normalize(
        "$member.$member.$member",
        Vector(
          SelectedNameSourcePosition(1, 1),
          SelectedNameSourcePosition(2, 0)
        )
      ),
      Left(SelectedNameSourceNormalizationError.CaptureCategoryConflict("member"))
    )

  test("one distinct-two and repeated-two constructions remain separate identities"):
    val one = TermTemplate
      .createWithSelectedNameHole(
        TermShape.Select(ident("receiver"), innerTransport),
        GeneratedHoleIndex.empty,
        Vector.empty,
        GeneratedHoleIndex.empty,
        Vector.empty,
        index("member" -> innerTransport),
        Vector(SelectedNameHoleOccurrence("member", 0)),
        GeneratedHoleIndex.empty,
        Vector.empty
      )
      .fold(error => fail(error.message), identity)
    val distinct = TermTemplate
      .createWithTwoSelectedNameHoles(
        selectedChain(innerTransport, outerTransport),
        GeneratedHoleIndex.empty,
        Vector.empty,
        GeneratedHoleIndex.empty,
        Vector.empty,
        index("inner" -> innerTransport, "outer" -> outerTransport),
        Vector(
          SelectedNameHoleOccurrence("inner", 1),
          SelectedNameHoleOccurrence("outer", 0)
        ),
        GeneratedHoleIndex.empty,
        Vector.empty
      )
      .fold(error => fail(error.message), identity)
    val repeated = repeatedModel(selectedChain(innerTransport, outerTransport))

    assertEquals(one.requiredSelectedNameBindings, Vector("member"))
    assertEquals(distinct.requiredSelectedNameBindings, Vector("outer", "inner"))
    assertEquals(repeated.requiredBindings, Vector("member"))
    assertNotEquals(one.render, distinct.render)
    assertEquals(
      repeated.complete(Map("member" -> selected("same"))).map(_.root),
      Right(selectedChain("same", "same"))
    )

  private def repeatedModel(
      root: TermShape,
      occurrences: Vector[SelectedNameHoleOccurrenceWithTransport] =
        repeatedOccurrences
  ): RepeatedSelectedNameConstructionModel =
    RepeatedSelectedNameConstructionModel
      .create(root, occurrences)
      .fold(error => fail(error.toString), identity)

  private def repeatedOccurrences
      : Vector[SelectedNameHoleOccurrenceWithTransport] =
    Vector(
      SelectedNameHoleOccurrenceWithTransport("member", 1, innerTransport),
      SelectedNameHoleOccurrenceWithTransport("member", 0, outerTransport)
    )

  private def selectedChain(inner: String, outer: String): TermShape =
    TermShape.Select(TermShape.Select(ident("receiver"), inner), outer)

  private def selected(value: String): SelectedMemberName =
    SelectedMemberName.from(value).fold(error => fail(error.message), identity)

  private def constructed(shape: TermShape): ConstructedTerm =
    ConstructedTerm.fromShape(shape).fold(
      error => fail(error.message),
      identity
    )

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

private sealed trait RepeatedSelectedNameModelError derives CanEqual

private object RepeatedSelectedNameModelError:
  final case class Cardinality(actual: Int)
      extends RepeatedSelectedNameModelError
  final case class LogicalNameCardinality(actual: Int)
      extends RepeatedSelectedNameModelError
  final case class DuplicateOrdinal(ordinal: Int)
      extends RepeatedSelectedNameModelError
  final case class DuplicateTransport(transport: String)
      extends RepeatedSelectedNameModelError
  final case class InvalidOccurrence(name: String, ordinal: Int)
      extends RepeatedSelectedNameModelError
  final case class TransportReuse(transport: String)
      extends RepeatedSelectedNameModelError
  final case class CategoryConflict(name: String)
      extends RepeatedSelectedNameModelError
  final case class GeneratedCategoryConflict(transport: String)
      extends RepeatedSelectedNameModelError
  final case class MissingBinding(name: String)
      extends RepeatedSelectedNameModelError
  final case class ExtraBinding(name: String)
      extends RepeatedSelectedNameModelError
  final case class NullBinding(name: String)
      extends RepeatedSelectedNameModelError
  final case class LexicalUnsupported(name: String)
      extends RepeatedSelectedNameModelError
  final case class Base(error: TermConstructionError)
      extends RepeatedSelectedNameModelError

private final class RepeatedSelectedNameConstructionModel private (
    val root: TermShape,
    val occurrences: Vector[SelectedNameHoleOccurrenceWithTransport],
    val requiredBindings: Vector[String],
    scalarIndex: GeneratedHoleIndex,
    scalarOccurrences: Vector[TermHoleOccurrence],
    repeatedIndex: GeneratedHoleIndex,
    repeatedOccurrences: Vector[RepeatedTermHoleOccurrence],
    typeIndex: GeneratedHoleIndex,
    ascriptions: Vector[TypeTemplate],
    val semanticTemplate: TermTemplate
):
  import RepeatedSelectedNameModelError.*

  def complete(
      selectedBindings: Map[String, SelectedMemberName],
      scalarBindings: Map[String, ConstructedTerm] = Map.empty,
      repeatedBindings: Map[String, Vector[ConstructedTerm]] = Map.empty,
      typeBindings: Map[String, TypeNormalForm] = Map.empty
  ): Either[RepeatedSelectedNameModelError, ConstructedTerm] =
    for
      _ <-
        if selectedBindings == null then Left(NullBinding("<bindings>"))
        else Right(())
      _ <- requiredBindings
        .find(!selectedBindings.contains(_))
        .map(MissingBinding.apply)
        .toLeft(())
      _ <- selectedBindings.keys.toVector.sorted
        .find(!requiredBindings.contains(_))
        .map(ExtraBinding.apply)
        .toLeft(())
      _ <- selectedBindings.toVector.sortBy(_._1).collectFirst {
        case (name, null) => NullBinding(name)
        case (_, selected)
            if DefinitionName.plain(selected.decoded).isLeft =>
          LexicalUnsupported(selected.decoded)
      }.toLeft(())
      completedRoot = RepeatedSelectedNameConstructionModel.rewriteSelectedFields(
        root,
        occurrences.map(occurrence =>
          occurrence.selectOrdinal ->
            selectedBindings(occurrence.name).decoded
        ).toMap
      )
      template <- TermTemplate
        .createWithRepeatedTerms(
          completedRoot,
          scalarIndex,
          scalarOccurrences,
          repeatedIndex,
          repeatedOccurrences,
          typeIndex,
          ascriptions
        )
        .left
        .map(Base.apply)
      completed <- template
        .completeWithRepeatedTerms(
          scalarBindings,
          repeatedBindings,
          typeBindings
        )
        .left
        .map(Base.apply)
    yield completed

  override def equals(other: Any): Boolean =
    other match
      case that: RepeatedSelectedNameConstructionModel =>
        semanticTemplate == that.semanticTemplate &&
        requiredBindings == that.requiredBindings
      case _ => false

  override def hashCode: Int =
    (semanticTemplate, requiredBindings).hashCode

private object RepeatedSelectedNameConstructionModel:
  import RepeatedSelectedNameModelError.*

  def create(
      root: TermShape,
      occurrences: Vector[SelectedNameHoleOccurrenceWithTransport],
      scalarEntries: Vector[(String, String)] = Vector.empty,
      scalarOccurrences: Vector[TermHoleOccurrence] = Vector.empty,
      repeatedEntries: Vector[(String, String)] = Vector.empty,
      repeatedTermOccurrences: Vector[RepeatedTermHoleOccurrence] = Vector.empty,
      typeEntries: Vector[(String, String)] = Vector.empty,
      ascriptions: Vector[TypeTemplate] = Vector.empty
  ): Either[RepeatedSelectedNameModelError, RepeatedSelectedNameConstructionModel] =
    val scalarIndex = TermCoreTestFixtures.index(scalarEntries*)
    val repeatedIndex = TermCoreTestFixtures.index(repeatedEntries*)
    val typeIndex = TermCoreTestFixtures.index(typeEntries*)
    val sorted = Option(occurrences).getOrElse(Vector.empty).sortBy(_.selectOrdinal)
    val nonNullOccurrences = sorted.filter(_ != null)
    val logicalNames = nonNullOccurrences.map(_.name).distinct
    val selects = Option(root).toVector.flatMap(TermShapeTraversal.selectEntries)
    val selectedTransports = nonNullOccurrences.map(_.transport).toSet
    val termCategoryNames =
      scalarIndex.semanticNames ++ repeatedIndex.semanticNames
    val generatedCategoryNames =
      scalarIndex.generatedNames ++ repeatedIndex.generatedNames ++
        typeIndex.generatedNames

    for
      _ <- Either.cond(occurrences != null, (), Cardinality(0))
      _ <- Either.cond(sorted.size == 2, (), Cardinality(sorted.size))
      _ <- Either.cond(
        sorted.forall(_ != null),
        (),
        InvalidOccurrence("<null>", -1)
      )
      _ <- Either.cond(
        logicalNames.size == 1,
        (),
        LogicalNameCardinality(logicalNames.size)
      )
      _ <- duplicate(sorted.map(_.selectOrdinal))
        .map(DuplicateOrdinal.apply)
        .toLeft(())
      _ <- duplicate(sorted.map(_.transport))
        .map(DuplicateTransport.apply)
        .toLeft(())
      _ <- sorted.find { occurrence =>
        occurrence.selectOrdinal < 0 ||
        !selects
          .lift(occurrence.selectOrdinal)
          .exists(_.name == occurrence.transport)
      }.map(occurrence =>
        InvalidOccurrence(occurrence.name, occurrence.selectOrdinal)
      ).toLeft(())
      _ <- sorted.find { occurrence =>
        val fieldCount =
          TermShapeTraversal.identifierEntries(root).count(
            _.name == occurrence.transport
          ) +
            TermShapeTraversal.nonIdentifierFields(root).count(
              _ == occurrence.transport
            )
        fieldCount != 1
      }.map(occurrence => TransportReuse(occurrence.transport)).toLeft(())
      _ <- (logicalNames.toSet intersect termCategoryNames).toVector.sorted.headOption
        .map(CategoryConflict.apply)
        .toLeft(())
      _ <- (selectedTransports intersect generatedCategoryNames).toVector.sorted.headOption
        .map(GeneratedCategoryConflict.apply)
        .toLeft(())
      _ <- TermTemplate
        .createWithRepeatedTerms(
          root,
          scalarIndex,
          scalarOccurrences,
          repeatedIndex,
          repeatedTermOccurrences,
          typeIndex,
          ascriptions
        )
        .left
        .map(Base.apply)
      semanticRoot = rewriteSelectedFields(
        root,
        sorted.map(occurrence =>
          occurrence.selectOrdinal ->
            ("__logical_repeated_selected_" + occurrence.name)
        ).toMap
      )
      semanticTemplate <- TermTemplate
        .createWithRepeatedTerms(
          semanticRoot,
          scalarIndex,
          scalarOccurrences,
          repeatedIndex,
          repeatedTermOccurrences,
          typeIndex,
          ascriptions
        )
        .left
        .map(Base.apply)
    yield new RepeatedSelectedNameConstructionModel(
      root,
      sorted,
      logicalNames,
      scalarIndex,
      scalarOccurrences,
      repeatedIndex,
      repeatedTermOccurrences,
      typeIndex,
      ascriptions,
      semanticTemplate
    )

  private def duplicate[A](values: Vector[A]): Option[A] =
    values.groupBy(identity).collectFirst { case (value, matches)
        if matches.size > 1 =>
      value
    }

  private def rewriteSelectedFields(
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
