package quasiquotes.neutral

import _root_.quasiquotes.parser.TermShape
import _root_.quasiquotes.source.*
import _root_.quasiquotes.terms.*

import scala.annotation.nowarn
import scala.meta.*

/** Characterization of ordinary List cardinality, not a new sequence feature. */
@nowarn("cat=deprecation")
final class ScalametaRank2TermArgumentSequenceModelFitCharacterizationTest
    extends munit.FunSuite:
  private val constructor = "synthetic.unresolved.Widget"

  test("completed Apply and New erase host collection origin while preserving ordered children"):
    val literalArguments = List(identifier("before"), identifier("left"), identifier("right"), identifier("after"))
    val runtimeArguments =
      identifier("before") ::
        (1 to 2).iterator.map(index => identifier(if index == 1 then "left" else "right")).toList :::
        List(identifier("after"))

    val literalApply = TermShape.Apply(identifier("f"), literalArguments)
    val runtimeApply = TermShape.Apply(identifier("f"), runtimeArguments)
    val literalNew = TermShape.New(constructor, literalArguments)
    val runtimeNew = TermShape.New(constructor, runtimeArguments)

    assertEquals(runtimeApply, literalApply)
    assertEquals(runtimeApply.render, literalApply.render)
    assertEquals(runtimeNew, literalNew)
    assertEquals(runtimeNew.render, literalNew.render)
    assertEquals(identifierNames(runtimeApply), Vector("f", "before", "left", "right", "after"))
    assertEquals(identifierNames(runtimeNew), Vector("before", "left", "right", "after"))

    List(literalApply -> runtimeApply, literalNew -> runtimeNew).foreach { (literal, runtime) =>
      val literalAuthored = author(literal)
      val runtimeAuthored = author(runtime)
      assertEquals(runtimeAuthored.structure, literalAuthored.structure)
      assertEquals(project(runtimeAuthored), runtime)
      assert(allTrees(runtimeAuthored).forall(_.pos == Position.None))
    }

  test("fresh Apply and New expose only ordinary ordered argument fields with no origin metadata"):
    val literalArguments = List(Lit.Int(1), Term.Name("middle"), Lit.Boolean(true))
    val runtimeArguments =
      (1 to 3).iterator.map {
        case 1 => Lit.Int(1): Term
        case 2 => Term.Name("middle"): Term
        case _ => Lit.Boolean(true): Term
      }.toList
    val literalApply = Term.Apply(Term.Name("f"), Term.ArgClause(literalArguments))
    val runtimeApply = Term.Apply(Term.Name("f"), Term.ArgClause(runtimeArguments))
    val literalNew = freshNew(literalArguments)
    val runtimeNew = freshNew(runtimeArguments)

    assertEquals(runtimeApply.structure, literalApply.structure)
    assertEquals(runtimeApply.fun.structure, "Term.Name(\"f\")")
    assertEquals(runtimeApply.argClause.values.map(_.structure), literalArguments.map(_.structure))
    assertEquals(runtimeApply.productIterator.toList.map(productSnapshot), List("Term.Name", "Term.ArgClause"))
    assertEquals(runtimeApply.children.map(_.productPrefix), List("Term.Name", "Term.ArgClause"))

    assertEquals(runtimeNew.structure, literalNew.structure)
    assertEquals(runtimeNew.productIterator.toList.map(productSnapshot), List("Init"))
    assertEquals(runtimeNew.children.map(_.productPrefix), List("Init"))
    assertEquals(runtimeNew.init.argClauses.size, 1)
    assertEquals(runtimeNew.init.argClauses.head.mod, None)
    assertEquals(runtimeNew.init.argClauses.head.values.map(_.structure), literalArguments.map(_.structure))

    val expectedArguments =
      List(TermShape.Literal("1"), identifier("middle"), TermShape.Literal("true"))
    assertEquals(project(runtimeApply), TermShape.Apply(identifier("f"), expectedArguments))
    assertEquals(project(runtimeNew), TermShape.New(constructor, expectedArguments))
    assert(allTrees(runtimeApply).forall(_.pos == Position.None))
    assert(allTrees(runtimeNew).forall(_.pos == Position.None))

  test("projection and authoring traverse Apply and New cardinalities zero through three"):
    (0 to 3).foreach { cardinality =>
      val metaArguments = runtimeMetaArguments(cardinality)
      val termArguments = shapeArguments(cardinality)
      val applyShape = TermShape.Apply(identifier("f"), termArguments)
      val newShape = TermShape.New(constructor, termArguments)

      val freshApply = Term.Apply(Term.Name("f"), Term.ArgClause(metaArguments))
      val freshConstructor = freshNew(metaArguments)
      assertEquals(project(freshApply), applyShape, clues(cardinality))
      assertEquals(project(freshConstructor), newShape, clues(cardinality))

      val authoredApply = author(applyShape)
      val authoredNew = author(newShape)
      assertEquals(project(authoredApply), applyShape, clues(cardinality))
      assertEquals(project(authoredNew), newShape, clues(cardinality))
      assertEquals(authoredApply.asInstanceOf[Term.Apply].argClause.values.size, cardinality)
      assertEquals(authoredNew.asInstanceOf[Term.New].init.argClauses.head.values.size, cardinality)
    }

  test("nearby exclusions remain family rules rather than sequence-rank failures"):
    val named = Term.Assign(Term.Name("value"), Lit.Int(1))
    val repeated = Term.Repeated(Term.Name("values"))

    List(named, repeated).foreach { argument =>
      assertProjectionCode(
        Term.Apply(Term.Name("f"), Term.ArgClause(List(argument))),
        "NEUTRAL_APPLY_ARGUMENT_UNSUPPORTED"
      )
      assertProjectionCode(freshNew(List(argument)), "NEUTRAL_NEW_ARGUMENT_UNSUPPORTED")
    }

    assertProjectionCode(
      Term.Apply(Term.Name("f"), Term.ArgClause(List(Lit.Int(1)), Some(Mod.Using()))),
      "NEUTRAL_APPLY_ARGUMENT_CLAUSE_UNSUPPORTED"
    )
    val nestedApply = Term.Apply(
      Term.Apply(Term.Name("f"), Term.ArgClause(List(Lit.Int(1)))),
      Term.ArgClause(List(Lit.Int(2)))
    )
    val nestedShape = TermShape.Apply(
      TermShape.Apply(identifier("f"), List(TermShape.Literal("1"))),
      List(TermShape.Literal("2"))
    )
    assertEquals(project(nestedApply), nestedShape)
    val authoredNested = author(nestedShape)
    assertEquals(project(authoredNested), nestedShape)
    assert(allTrees(authoredNested).forall(_.pos == Position.None))
    assertProjectionCode(
      Term.New(
        Init(
          constructorType,
          Name.Anonymous(),
          List(Term.ArgClause(List(Lit.Int(1))), Term.ArgClause(List(Lit.Int(2))))
        )
      ),
      "NEUTRAL_NEW_ARGUMENT_LIST_UNSUPPORTED"
    )
    assertProjectionCode(
      Term.New(
        Init(
          Type.Apply(constructorType, List(Type.Name("Int"))),
          Name.Anonymous(),
          List(Term.ArgClause(Nil))
        )
      ),
      "NEUTRAL_NEW_CONSTRUCTOR_TYPE_UNSUPPORTED"
    )

  test("a scalar TermTemplate hole completes to exactly one argument child"):
    val generatedName = "__term_transport"
    val holeIndex = GeneratedHoleIndex.fromOccurrences(
      Vector(
        HoleOccurrence(
          "value",
          generatedName,
          SourceSpan(0, 1),
          SourceSpan(0, 1),
          HoleRole.TermTemplate
        )
      )
    )
    val template = TermTemplate
      .create(
        TermShape.Apply(
          identifier("f"),
          List(TermShape.Identifier(generatedName, isPlaceholder = true))
        ),
        holeIndex,
        Vector(TermHoleOccurrence("value", identifierOrdinal = 1)),
        GeneratedHoleIndex.empty,
        Vector.empty
      )
      .toOption
      .get
    val tuple = TermShape.Tuple(List(TermShape.Literal("1"), TermShape.Literal("2")))
    val binding = ConstructedTerm.fromShape(tuple).toOption.get
    val completed = template.complete(Map("value" -> binding), Map.empty).toOption.get

    assertEquals(completed.root, TermShape.Apply(identifier("f"), List(tuple)))
    completed.root match
      case TermShape.Apply(_, arguments) =>
        assertEquals(arguments.size, 1)
        assertEquals(arguments.head, tuple)
      case other => fail(s"expected completed Apply, got ${other.render}")
    assertEquals(project(author(completed.root)), completed.root)
    assertEquals(template.requiredTermBindings, Vector("value"))

  private def identifier(name: String): TermShape =
    TermShape.Identifier(name, isPlaceholder = false)

  private def identifierNames(shape: TermShape): Vector[String] =
    TermShapeTraversal.identifierEntries(shape).map(_.name)

  private def shapeArguments(cardinality: Int): List[TermShape] =
    (1 to cardinality).iterator.map(index => TermShape.Literal(index.toString)).toList

  private def runtimeMetaArguments(cardinality: Int): List[Term] =
    (1 to cardinality).iterator.map(index => Lit.Int(index): Term).toList

  private def constructorType: Type =
    Type.Select(
      Term.Select(Term.Name("synthetic"), Term.Name("unresolved")),
      Type.Name("Widget")
    )

  private def freshNew(arguments: List[Term]): Term.New =
    Term.New(
      Init(
        constructorType,
        Name.Anonymous(),
        List(Term.ArgClause(arguments))
      )
    )

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
      clues(shape)
    )

  private def productSnapshot(value: Any): String =
    value match
      case product: Product => product.productPrefix
      case other => other.getClass.getSimpleName

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
