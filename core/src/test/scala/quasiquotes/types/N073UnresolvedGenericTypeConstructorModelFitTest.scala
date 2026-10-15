package quasiquotes.types

import quasiquotes.parser.TypeShape
import quasiquotes.types.TypeNormalForm.*

/** Test-only model-fit oracle. It is not production admission policy. */
final class N073UnresolvedGenericTypeConstructorModelFitTest extends munit.FunSuite:
  private val candidateNames = Set("F", "Box", "Pair")
  private val atoms = Vector("Int", "String", "Boolean", "AnyVal")

  test("current public normal-form policy retains primitive atoms and exact fixed constructors"):
    atoms.foreach { name =>
      assertEquals(TypeNormalForm.fromShape(TypeShape.Identifier(name)), Right(STypeIdent(name)))
    }
    assertEquals(
      TypeNormalForm.fromShape(apply("List", ident("Int"))),
      Right(STypeApply(STypeIdent("List"), List(STypeIdent("Int"))))
    )
    assertEquals(
      TypeNormalForm.fromShape(apply("Option", ident("String"))),
      Right(STypeApply(STypeIdent("Option"), List(STypeIdent("String"))))
    )
    assertEquals(
      TypeNormalForm.fromShape(apply("Either", ident("Int"), ident("String"))),
      Right(
        STypeApply(
          STypeIdent("Either"),
          List(STypeIdent("Int"), STypeIdent("String"))
        )
      )
    )

  test("current public normal form and TypeTemplate reject unresolved applied names exactly"):
    val detail =
      "Unsupported applied type constructor `F`; supported constructors are List/1, Option/1, Either/2."
    val shape = apply("F", ident("Int"))

    assertEquals(TypeNormalForm.fromShape(shape), Left(TypeQuasiquoteError(detail)))
    assertEquals(TypeTemplate.fromShape(shape), Left(TypeQuasiquoteError(detail)))
    assertEquals(
      TypeTemplate.validateConstructed(
        STypeApply(STypeIdent("F"), List(STypeIdent("Int")))
      ),
      Left(
        TypeQuasiquoteError(
          "Unsupported constructed applied type `F[Int]`; supported constructors are List/1, Option/1, Either/2."
        )
      )
    )

  test("existing carriers preserve simple constructor spelling argument order and nesting"):
    val fixtures = List(
      apply("F", ident("Int")) ->
        STypeApply(STypeIdent("F"), List(STypeIdent("Int"))),
      apply("Box", ident("String")) ->
        STypeApply(STypeIdent("Box"), List(STypeIdent("String"))),
      apply("Pair", ident("Int"), ident("String")) ->
        STypeApply(
          STypeIdent("Pair"),
          List(STypeIdent("Int"), STypeIdent("String"))
        ),
      apply("Option", apply("F", ident("Int"))) ->
        STypeApply(
          STypeIdent("Option"),
          List(STypeApply(STypeIdent("F"), List(STypeIdent("Int"))))
        )
    )

    fixtures.foreach { (shape, expected) =>
      assertEquals(candidate(shape), Right(expected), clues(shape.render))
    }

    assertNotEquals(fixtures(0)._2, fixtures(1)._2)
    assertNotEquals(
      fixtures(2)._2,
      STypeApply(
        STypeIdent("Pair"),
        List(STypeIdent("String"), STypeIdent("Int"))
      )
    )

  test("syntactic application cardinality 0 1 2 3 and N does not prove declaration arity"):
    val decisionRows = List(
      0 -> candidate(ident("F")),
      1 -> candidate(apply("F", ident("Int"))),
      2 -> candidate(apply("F", ident("Int"), ident("String"))),
      3 -> candidate(apply("F", ident("Int"), ident("String"), ident("Boolean"))),
      7 -> candidate(TypeShape.Apply(ident("F"), runtimeShapes(7)))
    )

    assert(decisionRows.head._2.isLeft)
    decisionRows.tail.foreach { (actualArgumentCount, result) =>
      val form = result.toOption.get
      assertEquals(form.asInstanceOf[STypeApply].arguments.size, actualArgumentCount)
      assertEquals(form.asInstanceOf[STypeApply].constructor, STypeIdent("F"))
    }

    (1 to 3).foreach { arity =>
      assertEquals(
        TypeNormalForm.fromShape(TypeShape.Apply(ident("F"), runtimeShapes(arity)))
          .left.toOption.map(_.message),
        Some(
          "Unsupported applied type constructor `F`; supported constructors are List/1, Option/1, Either/2."
        )
      )
    }

  test("runtime-provided argument Lists erase into the same existing structural values"):
    val runtimeArguments = runtimeShapes(6)
    val literalArguments = List(
      ident("Int"),
      ident("String"),
      ident("Boolean"),
      ident("AnyVal"),
      ident("Int"),
      ident("String")
    )
    val runtime = candidate(TypeShape.Apply(ident("F"), runtimeArguments)).toOption.get
    val literal = candidate(TypeShape.Apply(ident("F"), literalArguments)).toOption.get

    assertEquals(runtime, literal)
    assertEquals(
      runtime.render,
      "STypeApply(STypeIdent(F), [STypeIdent(Int), STypeIdent(String), STypeIdent(Boolean), STypeIdent(AnyVal), STypeIdent(Int), STypeIdent(String)])"
    )

  test("test-local admission is deliberately bounded and rejects malformed selected and empty applications"):
    val empty = TypeShape.Apply(ident("F"), Nil)
    val selected = TypeShape.Apply(TypeShape.Select(ident("pkg"), "F"), List(ident("Int")))

    assertEquals(candidate(null), Left("N073_CANDIDATE_MISSING_SHAPE"))
    assertEquals(candidate(TypeShape.Apply(ident("F"), null)), Left("N073_CANDIDATE_MISSING_ARGUMENTS"))
    assertEquals(candidate(empty), Left("N073_CANDIDATE_EMPTY_APPLICATION"))
    assert(candidate(apply("Map", ident("Int"), ident("String"))).isLeft)
    assert(candidate(apply("`F`", ident("Int"))).isLeft)
    assert(candidate(selected).isLeft)

  test("an unresolved simple name is not a resolved declaration identity"):
    val id = ResolvedTypeNameId(
      Vector(ResolvedTypeOwnerSegment(ResolvedTypeOwnerKind.Package, "example")),
      "F"
    )
    val environment = ResolvedTypeEnvironment.fromIds(List(id)).toOption.get
    val unresolved = STypeApply(STypeIdent("F"), List(STypeIdent("Int")))
    val resolved = STypeApply(STypeResolved(id), List(STypeIdent("Int")))

    assertNotEquals(unresolved, resolved)
    assertEquals(
      TypeNormalForm.fromShapeResolved(apply("F", ident("Int")), environment)
        .left.toOption.map(_.message),
      Some(
        "Unsupported applied type constructor `F`; supported constructors are List/1, Option/1, Either/2."
      )
    )
    assertEquals(
      TypeNormalForm.fromShapeResolved(
        TypeShape.Apply(TypeShape.Select(ident("example"), "F"), List(ident("Int"))),
        environment
      ).left.toOption.map(_.message),
      Some(
        "TYPE_NAME_RESOLUTION_CONSTRUCTOR_POLICY_MISMATCH: `example.F`/1 is not one of the exact admitted List/1, Option/1, Either/2 declarations."
      )
    )

  private def candidate(shape: TypeShape): Either[String, TypeNormalForm] =
    Option(shape).toRight("N073_CANDIDATE_MISSING_SHAPE").flatMap {
      case TypeShape.Identifier(name) =>
        TypeNormalForm.fromShape(TypeShape.Identifier(name)).left.map(_.message)
      case TypeShape.Parenthesized(inner) =>
        candidate(inner)
      case TypeShape.Apply(TypeShape.Identifier(name), arguments) =>
        Option(arguments).toRight("N073_CANDIDATE_MISSING_ARGUMENTS").flatMap { present =>
          if present.isEmpty then Left("N073_CANDIDATE_EMPTY_APPLICATION")
          else if candidateNames(name) || AppliedTypeConstructorPolicy
              .forNormalFormSource(name, present.size)
              .isDefined
          then traverse(present)(candidate).map(STypeApply(STypeIdent(name), _))
          else TypeNormalForm.fromShape(TypeShape.Apply(TypeShape.Identifier(name), present)).left.map(_.message)
        }
      case other =>
        TypeNormalForm.fromShape(other).left.map(_.message)
    }

  private def traverse[A, B](values: List[A])(convert: A => Either[String, B]): Either[String, List[B]] =
    values.foldRight(Right(Nil): Either[String, List[B]]) { (value, accumulated) =>
      for
        head <- convert(value)
        tail <- accumulated
      yield head :: tail
    }

  private def ident(name: String): TypeShape = TypeShape.Identifier(name)

  private def apply(name: String, arguments: TypeShape*): TypeShape =
    TypeShape.Apply(ident(name), arguments.toList)

  private def runtimeShapes(arity: Int): List[TypeShape] =
    (0 until arity).iterator.map(index => ident(atoms(index % atoms.size))).toList
