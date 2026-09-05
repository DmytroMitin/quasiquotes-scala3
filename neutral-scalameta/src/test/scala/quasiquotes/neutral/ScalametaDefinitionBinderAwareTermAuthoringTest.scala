package quasiquotes.neutral

import _root_.quasiquotes.definitions.{DefinitionName, DefinitionNameSpelling}
import _root_.quasiquotes.parser.{BinderId, TermShape}
import _root_.quasiquotes.terms.TermShapeTraversal

import scala.meta.*

final class ScalametaDefinitionBinderAwareTermAuthoringTest extends munit.FunSuite:
  private type DefinitionBinder = ScalametaTermShapeAuthoring.DefinitionBinder

  private val id0 = BinderId(0)
  private val xName = plainName("x")
  private val xBinder = binder(id0, xName)

  test("authors the existing generic family with externally resolved bound references"):
    val bound = TermShape.BoundReference(id0, "x")
    val fixtures = List(
      bound,
      TermShape.Select(bound, "field"),
      TermShape.Apply(TermShape.Identifier("f", false), List(bound)),
      TermShape.Infix(bound, "+", TermShape.Literal("1")),
      TermShape.Unary("-", bound),
      TermShape.Tuple(List(bound, TermShape.Identifier("y", false))),
      TermShape.If(
        TermShape.Identifier("cond", false),
        bound,
        TermShape.Identifier("y", false)
      ),
      TermShape.InterpolatedString("s", List("value=", ""), List(bound)),
      TermShape.New("synthetic.unresolved.Widget", List(bound)),
      TermShape.Block(List(TermShape.Literal("1")), bound)
    )

    fixtures.foreach { shape =>
      val authored = author(shape, Vector(xBinder))
      assertSeededRoundTrip(shape, authored, Vector(xBinder))
      assert(allTrees(authored).forall(_.pos == Position.None), clues(shape))
    }

  test("resolves arbitrary BinderIds by identity and canonicalizes stale display names"):
    val id7 = BinderId(7)
    val valueBinder = binder(id7, plainName("value"))
    val stale = TermShape.BoundReference(id7, "stale")
    val authored = author(stale, Vector(valueBinder))

    assertEquals(authored.productPrefix, "Term.Name")
    assertEquals(authored.asInstanceOf[Term.Name].value, "value")
    assertEquals(
      project(authored, Vector(valueBinder)).shape,
      TermShape.BoundReference(id7, "value")
    )
    assertSeededRoundTrip(stale, authored, Vector(valueBinder))

  test("resolves two external binders by ID independently of reference order"):
    val id7 = BinderId(7)
    val id2 = BinderId(2)
    val binders = Vector(
      binder(id7, plainName("left")),
      binder(id2, plainName("right"))
    )
    val shape = TermShape.Tuple(
      List(
        TermShape.BoundReference(id2, "second-stale"),
        TermShape.BoundReference(id7, "first-stale")
      )
    )
    val authored = author(shape, binders).asInstanceOf[Term.Tuple]

    assertEquals(authored.args.map(_.asInstanceOf[Term.Name].value), List("right", "left"))
    assertSeededRoundTrip(shape, authored, binders)

  test("authors a Core-admitted backticked Definition binder exactly"):
    val id3 = BinderId(3)
    val name = DefinitionName.backticked("`match`").toOption.get
    val binders = Vector(binder(id3, name))
    val shape = TermShape.BoundReference(id3, "ignored")
    val authored = author(shape, binders).asInstanceOf[Term.Name]
    val projected = project(authored, binders)

    assertEquals(authored.value, "match")
    assertEquals(authored.tokens.map(_.text).mkString, "`match`")
    assertEquals(projected.shape, TermShape.BoundReference(id3, "match"))
    assertEquals(name.spelling, DefinitionNameSpelling.BacktickedKeyword)
    assertSeededRoundTrip(shape, authored, binders)

  test("keeps a selected field matching the binder name outside capture"):
    val shape = TermShape.Select(TermShape.Identifier("service", false), "x")
    val authored = author(shape, Vector(xBinder))

    assertSeededRoundTrip(shape, authored, Vector(xBinder))
    assertEquals(project(authored, Vector(xBinder)).shape, shape)

  test("accepts a present empty binder environment for binder-free seeded reuse"):
    val shape = TermShape.Apply(
      TermShape.Identifier("f", false),
      List(TermShape.Literal("1"))
    )

    assertSeededRoundTrip(shape, author(shape, Vector.empty), Vector.empty)

  test("preserves the public binder-free BoundReference failure exactly"):
    assertEquals(
      ScalametaTermShapeAuthoring.author(TermShape.BoundReference(id0, "x")),
      Left(
        ScalametaTermShapeAuthoring.Error(
          "NEUTRAL_TERM_AUTHORING_FAMILY_UNSUPPORTED",
          "this TermShape family is outside binder-free N013-N015/N019 authoring."
        )
      )
    )

  test("rejects missing malformed duplicate and overflowing binder environments"):
    val id1 = BinderId(1)
    val yName = plainName("y")
    val invalid = List(
      Vector(null.asInstanceOf[DefinitionBinder]),
      Vector(binder(null, xName)),
      Vector(binder(id0, null)),
      Vector(xBinder, binder(id0, yName)),
      Vector(xBinder, binder(id1, xName)),
      Vector(binder(BinderId(Int.MaxValue), xName))
    )

    assertEquals(
      ScalametaTermShapeAuthoring.authorWithDefinitionBinders(
        TermShape.Literal("1"),
        null.asInstanceOf[Vector[DefinitionBinder]]
      ),
      Left(
        ScalametaTermShapeAuthoring.Error(
          "NEUTRAL_TERM_AUTHORING_STRUCTURE_UNSUPPORTED",
          "definition binders require present distinct BinderIds, distinct authorable names, and non-overflowing BinderIds."
        )
      )
    )

    invalid.foreach(binders =>
      assertErrorCode(
        TermShape.Literal("1"),
        binders,
        "NEUTRAL_TERM_AUTHORING_STRUCTURE_UNSUPPORTED"
      )
    )

  test("rejects an unknown BoundReference without converting it to a free identifier"):
    assertEquals(
      ScalametaTermShapeAuthoring.authorWithDefinitionBinders(
        TermShape.BoundReference(BinderId(9), "x"),
        Vector(xBinder)
      ),
      Left(
        ScalametaTermShapeAuthoring.Error(
          "NEUTRAL_TERM_AUTHORING_STRUCTURE_UNSUPPORTED",
          "bound references must resolve to a supplied definition binder."
        )
      )
    )

  test("rejects direct and nested free-identifier capture at seeded round trip"):
    assertEquals(
      ScalametaTermShapeAuthoring.authorWithDefinitionBinders(
        TermShape.Identifier("x", false),
        Vector(xBinder)
      ),
      Left(
        ScalametaTermShapeAuthoring.Error(
          "NEUTRAL_TERM_AUTHORING_ROUND_TRIP_REJECTED",
          "the authored definition body did not preserve its scoped neutral meaning."
        )
      )
    )

    val nestedCollisions = List(
      TermShape.Tuple(List(TermShape.Literal("1"), TermShape.Identifier("x", false))),
      TermShape.Apply(
        TermShape.Identifier("f", false),
        List(TermShape.Identifier("x", false))
      )
    )

    nestedCollisions.foreach(shape =>
      assertErrorCode(
        shape,
        Vector(xBinder),
        "NEUTRAL_TERM_AUTHORING_ROUND_TRIP_REJECTED"
      )
    )

  test("retains the Parenthesized unsupported family under seeded authoring"):
    assertErrorCode(
      TermShape.Parenthesized(TermShape.BoundReference(id0, "x")),
      Vector(xBinder),
      "NEUTRAL_TERM_AUTHORING_FAMILY_UNSUPPORTED"
    )

  private def binder(id: BinderId, name: DefinitionName): DefinitionBinder =
    ScalametaTermShapeAuthoring.DefinitionBinder(id, name)

  private def plainName(value: String): DefinitionName =
    DefinitionName.plain(value).toOption.get

  private def author(shape: TermShape, binders: Vector[DefinitionBinder]): Term =
    ScalametaTermShapeAuthoring.authorWithDefinitionBinders(shape, binders) match
      case Right(value) => value
      case Left(problem) => fail(problem.message)

  private def project(
      term: Term,
      binders: Vector[DefinitionBinder]
  ): ProjectedTermShape =
    val seeds = binders.map(binder =>
      ScalametaTermProjection.DefinitionBinder(binder.name.decoded, binder.binderId)
    )
    ScalametaTermProjection.projectWithDefinitionBinders(term, seeds) match
      case Right(value) => value
      case Left(problem) => fail(problem.message)

  private def assertSeededRoundTrip(
      expected: TermShape,
      authored: Term,
      binders: Vector[DefinitionBinder]
  ): Unit =
    val projected = project(authored, binders)
    val ids = binders.map(_.binderId)
    assertEquals(
      TermShapeTraversal.alphaNormalizeInScope(projected.shape, ids),
      TermShapeTraversal.alphaNormalizeInScope(expected, ids)
    )
    assertEquals(projected.sourceSpan, None)

  private def assertErrorCode(
      shape: TermShape,
      binders: Vector[DefinitionBinder],
      expected: String
  ): Unit =
    assertEquals(
      ScalametaTermShapeAuthoring
        .authorWithDefinitionBinders(shape, binders)
        .left
        .toOption
        .map(_.code),
      Some(expected)
    )

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
