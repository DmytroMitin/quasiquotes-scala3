package quasiquotes.neutral

import quasiquotes.parser.TypeShape
import quasiquotes.types.*
import quasiquotes.types.TypeNormalForm.*

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

/** Characterization of source-path information, not a resolved Type authorer. */
@nowarn("cat=deprecation")
final class ScalametaResolvedTypeRepresentabilityCharacterizationTest extends munit.FunSuite:
  private val paths = List(
    List("scala", "Option"),
    List("scala", "collection", "immutable", "List"),
    List("scala", "util", "Either"),
    List("p", "Outer", "T")
  )

  private val collisions = for
    first <- ResolvedTypeOwnerKind.values.toList
    second <- ResolvedTypeOwnerKind.values.toList
  yield ResolvedTypeNameId(
    Vector(ResolvedTypeOwnerSegment(first, "p"), ResolvedTypeOwnerSegment(second, "Outer")),
    "T"
  )

  private val standards = List(
    StandardResolvedTypeNames.ListId -> "List",
    StandardResolvedTypeNames.OptionId -> "Option",
    StandardResolvedTypeNames.EitherId -> "Either",
    StandardResolvedTypeNames.IntId -> "Int",
    StandardResolvedTypeNames.BooleanId -> "Boolean",
    StandardResolvedTypeNames.AnyValId -> "AnyVal"
  )

  private val resolvedUnsupported = ScalametaTypeNormalFormAuthoring.Error(
    "NEUTRAL_TYPE_AUTHORING_RESOLVED_UNSUPPORTED",
    "STypeResolved is outside the unresolved N002 authoring family."
  )

  test("direct fresh one-owner and multi-owner selected paths have exact source topology"):
    val snapshots = List(
      "Type.Select(Term.Name(scala),Type.Name(Option))",
      "Type.Select(Term.Select(Term.Select(Term.Name(scala),Term.Name(collection)),Term.Name(immutable)),Type.Name(List))",
      "Type.Select(Term.Select(Term.Name(scala),Term.Name(util)),Type.Name(Either))",
      "Type.Select(Term.Select(Term.Name(p),Term.Name(Outer)),Type.Name(T))"
    )
    paths.zip(snapshots).foreach { (names, expected) =>
      val tree = freshPath(names)
      assertEquals(snapshot(tree), expected)
      assertEquals(pathNames(tree), names)
      assertEquals(tree.name.value, names.last)
      assert(allNodes(tree).forall(_.pos == Position.None))
      val second = freshPath(names)
      assert(!allNodes(tree).exists(node => allNodes(second).exists(_ eq node)))
    }

  test("selected source trees retain only name values and qualifier-name product fields"):
    paths.foreach { names =>
      allNodes(freshPath(names)).foreach {
        case selected: Type.Select =>
          assertEquals(selected.productArity, 2)
          assertEquals(selected.productIterator.toList, List(selected.qual, selected.name))
          assertEquals(selected.children, List(selected.qual, selected.name))
        case selected: Term.Select =>
          assertEquals(selected.productArity, 2)
          assertEquals(selected.productIterator.toList, List(selected.qual, selected.name))
          assertEquals(selected.children, List(selected.qual, selected.name))
        case name: Term.Name =>
          assertEquals(name.productIterator.toList, List(name.value))
          assertEquals(name.children, Nil)
        case name: Type.Name =>
          assertEquals(name.productIterator.toList, List(name.value))
          assertEquals(name.children, Nil)
        case other => fail(s"unexpected path node: ${other.productPrefix}")
      }
    }

  test("all nine legal two-owner kind graphs collide in canonicalSource and fresh topology"):
    assertEquals(collisions.distinct.size, 9)
    assertEquals(collisions.map(_.render).distinct.size, 9)
    assertEquals(collisions.map(_.canonicalSource).distinct, List("p.Outer.T"))
    val trees = collisions.map(id => freshPath(id.owners.map(_.name).toList :+ id.terminalName))
    assertEquals(trees.map(snapshot).distinct.size, 1)
    assertEquals(trees.map(_.structure).distinct.size, 1)
    assert(trees.flatMap(allNodes).forall(_.pos == Position.None))
    // The same spelling and the same Term/Type syntax classes cover Package,
    // Type and Module at either owner slot; capitalization cannot recover kind.

  test("public selected projection reaches environment-free conversion and pins its detail"):
    paths.foreach { names =>
      val expected = s"Selected type syntax `${names.mkString(".")}` is not supported; use unqualified `${names.last}` in the current experimental surface."
      assertEquals(TypeNormalForm.fromShape(pathShape(names)), Left(TypeQuasiquoteError(expected)))
      val rejection = Left(NeutralProjectionError("NEUTRAL_TYPE_NORMAL_FORM_REJECTED", expected))
      assertEquals(ScalametaTypeNormalFormProjection.project(freshPath(names)), rejection)
      // Even the package-private validated-shape seam runs normal-form admission.
      assertEquals(ScalametaTypeNormalFormProjection.projectValidatedShape(freshPath(names)), rejection)
    }

  test("public selected constructor projection has the distinct Core constructor detail"):
    val tree = Type.Apply(freshPath(List("scala", "Option")), List(Type.Name("Int")))
    val detail = "Selected type constructor syntax `scala.Option[...]` is not supported; use unqualified `Option[...]` in the current experimental surface."
    assertEquals(
      TypeNormalForm.fromShape(TypeShape.Apply(pathShape(List("scala", "Option")), List(TypeShape.Identifier("Int")))),
      Left(TypeQuasiquoteError(detail))
    )
    assertEquals(ScalametaTypeNormalFormProjection.project(tree),
      Left(NeutralProjectionError("NEUTRAL_TYPE_NORMAL_FORM_REJECTED", detail)))

  test("unsupported this qualifier fails structurally before normal-form conversion"):
    val tree = Type.Select(Term.This(Name.Anonymous()), Type.Name("T"))
    assertEquals(ScalametaTypeNormalFormProjection.project(tree), Left(NeutralProjectionError(
      "NEUTRAL_TYPE_STRUCTURE_UNSUPPORTED", "unsupported Scalameta type node: Term.This."
    )))

  test("resolved roots constructors apply arguments tuples and both function positions reject identically"):
    (collisions ++ standards.map(_._1)).foreach { id =>
      resolvedPositions(STypeResolved(id)).foreach { value =>
        assertEquals(ScalametaTypeNormalFormAuthoring.author(value), Left(resolvedUnsupported))
      }
    }

  test("a null identity inside a constructible resolved value retains the existing authoring rejection"):
    resolvedPositions(STypeResolved(null)).foreach { value =>
      assertEquals(ScalametaTypeNormalFormAuthoring.author(value), Left(resolvedUnsupported))
    }

  test("the identical fresh path recovers each different identity only with its matching environment"):
    val tree = freshPath(List("p", "Outer", "T"))
    val shape = pathShape(pathNames(tree))
    val recovered = collisions.map { id =>
      val environment = env(List(id))
      assert(environment.contains(id))
      collisions.filterNot(_ == id).foreach(other => assert(!environment.contains(other)))
      val result = TypeNormalForm.fromShapeResolved(shape, environment)
      assertEquals(result, Right(STypeResolved(id)))
      result.toOption.get
    }
    assertEquals(recovered.distinct.size, 9)

  test("every distinct colliding pair is rejected as ambiguous and identical duplicates are also rejected"):
    val expected = "TYPE_NAME_RESOLUTION_AMBIGUOUS: more than one binding was supplied for canonical global Type `p.Outer.T`."
    for
      left <- collisions
      right <- collisions
    do assertEquals(ResolvedTypeEnvironment.fromIds(List(left, right)).left.toOption.map(_.message), Some(expected))

  test("an absent selected path fails with the exact unresolved environment diagnostic"):
    assertEquals(TypeNormalForm.fromShapeResolved(pathShape(List("p", "Outer", "T")), env(Nil)),
      Left(TypeQuasiquoteError("TYPE_NAME_RESOLUTION_UNRESOLVED: no canonical global Type binding exists for `p.Outer.T`.")))

  test("non-path Core qualifier is rejected by the environment control"):
    val shape = TypeShape.Select(TypeShape.Apply(TypeShape.Identifier("List"), List(TypeShape.Identifier("Int"))), "T")
    assertEquals(TypeNormalForm.fromShapeResolved(shape, env(Nil)), Left(TypeQuasiquoteError(
      "TYPE_NAME_RESOLUTION_UNSUPPORTED_QUALIFIER: `TypeSelect(TypeApply(TypeIdent(List), [TypeIdent(Int)]), T)` is not a canonical package/type/module selected path."
    )))

  test("all six standard identities have fixed short names but canonical selected paths need an environment"):
    val canonical = List("scala.collection.immutable.List", "scala.Option", "scala.util.Either", "scala.Int", "scala.Boolean", "scala.AnyVal")
    standards.zip(canonical).foreach { case ((id, short), source) =>
      assertEquals(id.canonicalSource, source)
      assertEquals(StandardResolvedTypeNames.fixedSourceName(id), Some(short))
      assert(id.owners.forall(_.kind == ResolvedTypeOwnerKind.Package))
      assert(STypeIdent(short) != STypeResolved(id))
      val tree = freshPath(id.owners.map(_.name).toList :+ id.terminalName)
      assertEquals(TypeNormalForm.fromShapeResolved(pathShape(pathNames(tree)), env(List(id))), Right(STypeResolved(id)))
      assertEquals(ScalametaTypeNormalFormProjection.project(tree).left.toOption.map(_.code), Some("NEUTRAL_TYPE_NORMAL_FORM_REJECTED"))
    }

  test("environment-assisted List Option Either application recovery obeys exact identity and arity"):
    val constructors = List(StandardResolvedTypeNames.ListId -> 1, StandardResolvedTypeNames.OptionId -> 1, StandardResolvedTypeNames.EitherId -> 2)
    constructors.foreach { (id, arity) =>
      val names = id.owners.map(_.name).toList :+ id.terminalName
      val fresh = Type.Apply(freshPath(names), List.fill(arity)(Type.Name("Int")))
      val shape = TypeShape.Apply(pathShape(pathNames(fresh.tpe.asInstanceOf[Type.Select])), List.fill(arity)(TypeShape.Identifier("Int")))
      assertEquals(AppliedTypeConstructorPolicy.forResolved(id, arity).map(_.name), Some(id.terminalName))
      assertEquals(TypeNormalForm.fromShapeResolved(shape, env(List(id))),
        Right(STypeApply(STypeResolved(id), List.fill(arity)(STypeIdent("Int")))))
      List(0, 1, 2, 3).filterNot(_ == arity).foreach { wrongArity =>
        assertPolicyRejected(id, wrongArity)
      }
      val changedOwnerKind = id.copy(owners = id.owners.updated(0, id.owners.head.copy(kind = ResolvedTypeOwnerKind.Module)))
      assertEquals(changedOwnerKind.canonicalSource, id.canonicalSource)
      assertEquals(StandardResolvedTypeNames.fixedSourceName(changedOwnerKind), None)
      assertPolicyRejected(changedOwnerKind, arity)
    }
    List(StandardResolvedTypeNames.IntId, StandardResolvedTypeNames.BooleanId, StandardResolvedTypeNames.AnyValId, collisions.head)
      .foreach(id => assertPolicyRejected(id, 1))

  test("unresolved public authoring and reprojection keep primitive and fixed-constructor semantics"):
    val forms = List("Int", "String", "Boolean", "AnyVal").map(STypeIdent(_)) ++ List(
      STypeApply(STypeIdent("List"), List(STypeIdent("Int"))),
      STypeApply(STypeIdent("Option"), List(STypeIdent("AnyVal"))),
      STypeApply(STypeIdent("Either"), List(STypeIdent("Int"), STypeIdent("Boolean")))
    )
    forms.foreach { form =>
      val tree = ScalametaTypeNormalFormAuthoring.author(form).toOption.get
      assert(allNodes(tree).forall(_.pos == Position.None))
      assertEquals(ScalametaTypeNormalFormProjection.project(tree), Right(ProjectedTypeNormalForm(form, None)))
    }
    List("List", "Option", "Either").foreach { short =>
      val detail = s"Unsupported type identifier `$short`; supported identifiers are Int, String, Boolean, AnyVal."
      assertEquals(ScalametaTypeNormalFormAuthoring.author(STypeIdent(short)),
        Left(ScalametaTypeNormalFormAuthoring.Error("NEUTRAL_TYPE_AUTHORING_NORMAL_FORM_REJECTED", detail)))
    }

  test("empty owners empty names and compiler module encodings remain constructor rejected"):
    intercept[IllegalArgumentException](ResolvedTypeNameId(Vector.empty, "T"))
    intercept[IllegalArgumentException](ResolvedTypeNameId(collisions.head.owners, ""))
    intercept[IllegalArgumentException](ResolvedTypeNameId(collisions.head.owners, "T$"))
    ResolvedTypeOwnerKind.values.foreach { kind =>
      intercept[IllegalArgumentException](ResolvedTypeOwnerSegment(kind, ""))
      intercept[IllegalArgumentException](ResolvedTypeOwnerSegment(kind, "Outer$"))
    }

  test("positioned parser oracle adds source provenance but cannot choose an owner-kind graph"):
    // Parsing is an oracle only. The fresh-path helper never calls it.
    val source = "p.Outer.T"
    val parsed = Input.String(source).parse[Type].get.asInstanceOf[Type.Select]
    val fresh = freshPath(List("p", "Outer", "T"))
    assert(parsed.pos != Position.None)
    assertEquals((parsed.pos.start, parsed.pos.end), (0, source.length))
    assertEquals(parsed.tokens.map(_.text).mkString, source)
    assertEquals(parsed.structure, fresh.structure)
    assertEquals(snapshot(parsed), snapshot(fresh))
    assertEquals(ScalametaTypeNormalFormProjection.project(parsed), ScalametaTypeNormalFormProjection.project(fresh))
    collisions.foreach { id =>
      assertEquals(TypeNormalForm.fromShapeResolved(pathShape(pathNames(parsed)), env(List(id))), Right(STypeResolved(id)))
    }

  private def freshPath(names: List[String]): Type.Select =
    val qualifier = names.init.tail.foldLeft[Term.Ref](Term.Name(names.head)) { (prefix, name) =>
      Term.Select(prefix, Term.Name(name))
    }
    Type.Select(qualifier, Type.Name(names.last))

  private def pathNames(tree: Type.Select): List[String] =
    def qualifierNames(term: Term): List[String] = term match
      case name: Term.Name => List(name.value)
      case selected: Term.Select => qualifierNames(selected.qual) :+ selected.name.value
      case other => fail(s"unexpected qualifier: ${other.productPrefix}")
    qualifierNames(tree.qual) :+ tree.name.value

  private def pathShape(names: List[String]): TypeShape =
    names.tail.foldLeft[TypeShape](TypeShape.Identifier(names.head))(TypeShape.Select(_, _))

  // Test-only field snapshot: neither parser nor renderer supplies constructor input.
  private def snapshot(tree: Tree): String = tree match
    case name: Term.Name => s"Term.Name(${name.value})"
    case name: Type.Name => s"Type.Name(${name.value})"
    case selected: Term.Select => s"Term.Select(${snapshot(selected.qual)},${snapshot(selected.name)})"
    case selected: Type.Select => s"Type.Select(${snapshot(selected.qual)},${snapshot(selected.name)})"
    case other => fail(s"unexpected snapshot node: ${other.productPrefix}")

  private def allNodes(tree: Tree): List[Tree] = tree :: tree.children.flatMap(allNodes)

  private def env(ids: List[ResolvedTypeNameId]): ResolvedTypeEnvironment =
    ResolvedTypeEnvironment.fromIds(ids).toOption.get

  private def resolvedPositions(value: TypeNormalForm): List[TypeNormalForm] = List(
    value,
    STypeApply(value, List(STypeIdent("Int"))),
    STypeApply(STypeIdent("List"), List(value)),
    STypeTuple(List(STypeIdent("Int"), value)),
    STypeFunction(List(value), STypeIdent("Int")),
    STypeFunction(List(STypeIdent("Int")), value)
  )

  private def assertPolicyRejected(id: ResolvedTypeNameId, arity: Int): Unit =
    assertEquals(AppliedTypeConstructorPolicy.forResolved(id, arity), None)
    val shape = TypeShape.Apply(pathShape(id.owners.map(_.name).toList :+ id.terminalName), List.fill(arity)(TypeShape.Identifier("Int")))
    assertEquals(TypeNormalForm.fromShapeResolved(shape, env(List(id))), Left(TypeQuasiquoteError(
      s"TYPE_NAME_RESOLUTION_CONSTRUCTOR_POLICY_MISMATCH: `${id.canonicalSource}`/$arity is not one of the exact admitted List/1, Option/1, Either/2 declarations."
    )))
