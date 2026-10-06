package quasiquotes.neutral

import _root_.quasiquotes.parser.TermShape
import _root_.quasiquotes.source.*
import _root_.quasiquotes.terms.*

import scala.annotation.nowarn
import scala.meta.*

/** Characterization of completed nested structure and the current flat repeated-hole boundary. */
@nowarn("cat=deprecation")
final class ScalametaRank3TermSequenceModelFitCharacterizationTest
    extends munit.FunSuite:
  private val repeatedGenerated = "__rank3_term_sequence_transport"

  test("fresh multi-clause applications are nested ordinary Apply nodes with no rank marker"):
    val cases = List(
      List(List(Lit.Int(1): Term)),
      List(Nil, List(Term.Name("second"))),
      List(List(Term.Name("first")), Nil, List(Lit.Int(3): Term, Term.Name("fourth")))
    )

    cases.zipWithIndex.foreach { (clauses, index) =>
      val fresh = freshApplication(clauses)

      assertEquals(metaClauseValues(fresh), clauses, clues(index, fresh.structure))
      assertEquals(countMetaApplies(fresh), clauses.size, clues(index, fresh.structure))
      assert(allTrees(fresh).forall(_.pos == Position.None), clues(index, fresh.structure))

      fresh match
        case outer: Term.Apply =>
          assertEquals(outer.children.last.productPrefix, "Term.ArgClause")
          assertEquals(
            outer.children.head.productPrefix,
            if clauses.size == 1 then "Term.Name" else "Term.Apply"
          )
        case other => fail(s"expected an Apply, got ${other.structure}")
    }

  test("neutral projection admits one clause and rejects the nested multi-clause boundary"):
    val one = freshApplication(List(List(Lit.Int(1): Term)))
    val two = freshApplication(List(Nil, List(Term.Name("second"))))
    val three = freshApplication(
      List(List(Term.Name("first")), Nil, List(Lit.Int(3): Term))
    )

    assertEquals(
      project(one),
      TermShape.Apply(identifier("f"), List(TermShape.Literal("1")))
    )
    List(two, three).foreach { term =>
      assertProjectionCode(term, "NEUTRAL_APPLY_MULTIPLE_LISTS_UNSUPPORTED")
    }

  test("completed Core nested Apply values erase literal versus runtime nested-list origin"):
    val literalClauses = List(
      List(identifier("first")),
      Nil,
      List(identifier("third"), TermShape.Literal("4"))
    )
    val runtimeClauses =
      Vector(
        Vector("first"),
        Vector.empty,
        Vector("third", "4")
      ).zipWithIndex.map { (clause, clauseIndex) =>
        clause.zipWithIndex.map { (value, valueIndex) =>
          if clauseIndex == 2 && valueIndex == 1 then TermShape.Literal(value)
          else identifier(value)
        }.toList
      }.toList
    val literal = nestedShape(literalClauses)
    val runtime = nestedShape(runtimeClauses)

    assertEquals(runtime, literal)
    assertEquals(runtime.render, literal.render)
    assertEquals(
      TermShapeTraversal.identifierEntries(runtime).map(_.name),
      Vector("f", "first", "third")
    )
    assertEquals(shapeClauseValues(runtime), literalClauses)
    assertEquals(countShapeApplies(runtime), 3)
    assertAuthoringCode(literal, "NEUTRAL_TERM_AUTHORING_STRUCTURE_UNSUPPORTED")
    assertAuthoringCode(runtime, "NEUTRAL_TERM_AUTHORING_STRUCTURE_UNSUPPORTED")

  test("N049 repeated completion changes one existing clause but cannot vary clause topology"):
    val root = TermShape.Apply(
      TermShape.Apply(identifier("f"), List(identifier("fixed"))),
      List(TermShape.Identifier(repeatedGenerated, isPlaceholder = true))
    )
    val template = repeatedTemplate(root, identifierOrdinal = 2)
    val values = Vector(constructed(identifier("left")), constructed(identifier("right")))
    val empty = completeRepeated(template, Vector.empty)
    val two = completeRepeated(template, values)

    assertEquals(
      empty.root,
      TermShape.Apply(
        TermShape.Apply(identifier("f"), List(identifier("fixed"))),
        Nil
      )
    )
    assertEquals(
      two.root,
      TermShape.Apply(
        TermShape.Apply(identifier("f"), List(identifier("fixed"))),
        List(identifier("left"), identifier("right"))
      )
    )
    assertEquals(countShapeApplies(empty.root), 2)
    assertEquals(countShapeApplies(two.root), 2)
    assertEquals(template.requiredRepeatedTermBindings, Vector("arguments"))

    val clausePositionAttempt = TermTemplate.createWithRepeatedTerms(
      TermShape.Apply(
        TermShape.Identifier(repeatedGenerated, isPlaceholder = true),
        Nil
      ),
      GeneratedHoleIndex.empty,
      Vector.empty,
      repeatedIndex,
      Vector(RepeatedTermHoleOccurrence("arguments", identifierOrdinal = 0)),
      GeneratedHoleIndex.empty,
      Vector.empty
    )
    assertEquals(
      clausePositionAttempt,
      Left(TermConstructionError.InvalidRepeatedTermHolePosition("arguments"))
    )

  test("completed Block statement sequences are structural while repeated statement holes stay excluded"):
    val literal = TermShape.Block(
      List(identifier("before"), identifier("middle"), identifier("after")),
      identifier("result")
    )
    val runtimeStatements =
      Vector("before", "middle", "after").iterator.map(identifier).toList
    val runtime = TermShape.Block(runtimeStatements, identifier("result"))

    assertEquals(runtime, literal)
    assertEquals(runtime.render, literal.render)
    val authored = author(runtime)
    assertEquals(project(authored), runtime)
    assert(allTrees(authored).forall(_.pos == Position.None))

    val repeatedStatementAttempt = TermTemplate.createWithRepeatedTerms(
      TermShape.Block(
        List(TermShape.Identifier(repeatedGenerated, isPlaceholder = true)),
        TermShape.Literal("0")
      ),
      GeneratedHoleIndex.empty,
      Vector.empty,
      repeatedIndex,
      Vector(RepeatedTermHoleOccurrence("arguments", identifierOrdinal = 0)),
      GeneratedHoleIndex.empty,
      Vector.empty
    )
    assertEquals(
      repeatedStatementAttempt,
      Left(TermConstructionError.InvalidRepeatedTermHolePosition("arguments"))
    )

  private def freshApplication(clauses: List[List[Term]]): Term =
    clauses.foldLeft(Term.Name("f"): Term) { (function, arguments) =>
      Term.Apply(function, Term.ArgClause(arguments))
    }

  private def metaClauseValues(term: Term): List[List[Term]] =
    term match
      case application: Term.Apply =>
        metaClauseValues(application.fun) :+ application.argClause.values
      case Term.Name("f") => Nil
      case other => fail(s"unexpected multi-clause function root: ${other.structure}")

  private def countMetaApplies(term: Term): Int =
    term match
      case application: Term.Apply => 1 + countMetaApplies(application.fun)
      case _ => 0

  private def nestedShape(clauses: List[List[TermShape]]): TermShape =
    clauses.foldLeft(identifier("f"): TermShape)(TermShape.Apply.apply)

  private def shapeClauseValues(shape: TermShape): List[List[TermShape]] =
    shape match
      case TermShape.Apply(function, arguments) =>
        shapeClauseValues(function) :+ arguments
      case TermShape.Identifier("f", false) => Nil
      case other => fail(s"unexpected completed function root: ${other.render}")

  private def countShapeApplies(shape: TermShape): Int =
    shape match
      case TermShape.Apply(function, _) => 1 + countShapeApplies(function)
      case _ => 0

  private def repeatedTemplate(root: TermShape, identifierOrdinal: Int): TermTemplate =
    TermTemplate
      .createWithRepeatedTerms(
        root,
        GeneratedHoleIndex.empty,
        Vector.empty,
        repeatedIndex,
        Vector(RepeatedTermHoleOccurrence("arguments", identifierOrdinal)),
        GeneratedHoleIndex.empty,
        Vector.empty
      )
      .fold(error => fail(error.message), identity)

  private def repeatedIndex: GeneratedHoleIndex =
    GeneratedHoleIndex.fromOccurrences(
      Vector(
        HoleOccurrence(
          "arguments",
          repeatedGenerated,
          SourceSpan(0, 1),
          SourceSpan(0, 1),
          HoleRole.TermTemplate
        )
      )
    )

  private def constructed(shape: TermShape): ConstructedTerm =
    ConstructedTerm.fromShape(shape).fold(error => fail(error.message), identity)

  private def completeRepeated(
      template: TermTemplate,
      values: Vector[ConstructedTerm]
  ): ConstructedTerm =
    template
      .completeWithRepeatedTerms(Map.empty, Map("arguments" -> values), Map.empty)
      .fold(error => fail(error.message), identity)

  private def identifier(name: String): TermShape =
    TermShape.Identifier(name, isPlaceholder = false)

  private def author(shape: TermShape): Term =
    ScalametaTermShapeAuthoring.author(shape) match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def project(term: Term): TermShape =
    ScalametaTermProjection.project(term) match
      case Right(value) => value.shape
      case Left(error) => fail(error.message)

  private def assertProjectionCode(term: Term, expected: String): Unit =
    assertEquals(
      ScalametaTermProjection.project(term).left.toOption.map(_.code),
      Some(expected),
      clues(term.structure)
    )

  private def assertAuthoringCode(shape: TermShape, expected: String): Unit =
    assertEquals(
      ScalametaTermShapeAuthoring.author(shape).left.toOption.map(_.code),
      Some(expected),
      clues(shape.render)
    )

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
