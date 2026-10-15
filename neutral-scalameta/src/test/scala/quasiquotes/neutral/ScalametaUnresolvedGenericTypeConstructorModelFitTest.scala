package quasiquotes.neutral

import quasiquotes.parser.TypeShape
import quasiquotes.types.*
import quasiquotes.types.TypeNormalForm.*

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

/** Characterization of syntax and carrier fit, not public generic admission. */
@nowarn("cat=deprecation")
final class ScalametaUnresolvedGenericTypeConstructorModelFitTest extends munit.FunSuite:
  private val candidateNames = Set("F", "Box", "Pair")

  test("parsed simple generic applications retain constructor spelling argument order and nesting"):
    val fixtures = List(
      "F[Int]" -> ("F", List("Int")),
      "Box[String]" -> ("Box", List("String")),
      "Pair[Int, String]" -> ("Pair", List("Int", "String"))
    )

    fixtures.foreach { case (source, (constructorName, argumentNames)) =>
      val applied = parseType(source).asInstanceOf[Type.Apply]
      assertEquals(applied.tpe.asInstanceOf[Type.Name].value, constructorName)
      assertEquals(applied.args.map(_.asInstanceOf[Type.Name].value), argumentNames)
      assertEquals(applied.pos.start, 0)
      assertEquals(applied.pos.end, source.length)
    }

    val nested = parseType("Option[F[Int]]").asInstanceOf[Type.Apply]
    val inner = nested.args.head.asInstanceOf[Type.Apply]
    assertEquals(nested.tpe.asInstanceOf[Type.Name].value, "Option")
    assertEquals(inner.tpe.asInstanceOf[Type.Name].value, "F")
    assertEquals(inner.args.map(_.asInstanceOf[Type.Name].value), List("Int"))

  test("fresh Type.Name and Type.Apply values carry no declaration identity arity or kind"):
    val one = Type.Apply(Type.Name("F"), List(Type.Name("Int")))
    val two = Type.Apply(Type.Name("F"), List(Type.Name("Int"), Type.Name("String")))
    val three = Type.Apply(
      Type.Name("F"),
      List(Type.Name("Int"), Type.Name("String"), Type.Name("Boolean"))
    )

    List(one, two, three).foreach { applied =>
      assertEquals(applied.tpe.productPrefix, "Type.Name")
      assertEquals(applied.tpe.asInstanceOf[Type.Name].productIterator.toList, List("F"))
      assertEquals(applied.productIterator.toList, List(applied.tpe, applied.argClause))
      assert(allNodes(applied).forall(_.pos == Position.None))
    }
    assertEquals(List(one, two, three).map(_.args.size), List(1, 2, 3))

  test("test-local conversion preserves the existing carriers without making public admission valid"):
    val fixtures = List(
      "F[Int]" -> STypeApply(STypeIdent("F"), List(STypeIdent("Int"))),
      "Box[String]" -> STypeApply(STypeIdent("Box"), List(STypeIdent("String"))),
      "Pair[Int, String]" ->
        STypeApply(STypeIdent("Pair"), List(STypeIdent("Int"), STypeIdent("String"))),
      "Option[F[Int]]" ->
        STypeApply(
          STypeIdent("Option"),
          List(STypeApply(STypeIdent("F"), List(STypeIdent("Int"))))
        )
    )

    fixtures.foreach { (source, expected) =>
      val sourceType = parseType(source)
      val shape = candidateShape(sourceType).toOption.get
      val form = candidateNormalForm(shape).toOption.get
      val fresh = candidateAuthor(form).toOption.get

      assertEquals(form, expected, clues(source))
      assertEquals(fresh.structure, sourceType.structure, clues(source))
      assertEquals(candidateNormalForm(candidateShape(fresh).toOption.get), Right(expected))
      assert(ScalametaTypeNormalFormProjection.project(sourceType).isLeft)
      assert(ScalametaTypeNormalFormAuthoring.author(expected).isLeft)
    }

  test("current public projection and authoring retain exact unresolved-constructor diagnostics"):
    List("F[Int]", "F[Int, String]", "F[Int, String, Boolean]").foreach { source =>
      val detail =
        "Unsupported applied type constructor `F`; supported constructors are List/1, Option/1, Either/2."
      assertEquals(
        ScalametaTypeNormalFormProjection.project(parseType(source)),
        Left(NeutralProjectionError("NEUTRAL_TYPE_NORMAL_FORM_REJECTED", detail))
      )
    }

    val form = STypeApply(STypeIdent("F"), List(STypeIdent("Int")))
    assertEquals(
      ScalametaTypeNormalFormAuthoring.author(form),
      Left(
        ScalametaTypeNormalFormAuthoring.Error(
          "NEUTRAL_TYPE_AUTHORING_NORMAL_FORM_REJECTED",
          "Unsupported applied type constructor `F`; supported constructors are List/1, Option/1, Either/2."
        )
      )
    )

  test("runtime-provided N arguments preserve ordinary List order with no separate sequence carrier"):
    val argumentNames = List("Int", "String", "Boolean", "AnyVal", "Int", "String")
    val runtimeArguments = argumentNames.map(Type.Name(_): Type)
    val tree = Type.Apply(Type.Name("F"), runtimeArguments)
    val form = candidateNormalForm(candidateShape(tree).toOption.get).toOption.get
    val applied = form.asInstanceOf[STypeApply]

    assertEquals(applied.constructor, STypeIdent("F"))
    assertEquals(applied.arguments, argumentNames.map(STypeIdent(_)))
    assertEquals(candidateAuthor(form).toOption.get.structure, tree.structure)

  test("zero arguments malformed input unknown names and selected constructors stay outside the candidate"):
    intercept[org.scalameta.invariants.InvariantFailedException] {
      Type.Apply(Type.Name("F"), Nil)
    }
    assertEquals(candidateShape(null), Left("N073_CANDIDATE_MISSING_TYPE"))
    assert(candidateShape(parseType("Map[Int, String]")).isLeft)
    assert(candidateShape(parseType("pkg.F[Int]")).isLeft)
    assert(candidateShape(parseType("[A] =>> F[A]")).isLeft)
    assert(candidateShape(parseType("F[Int] { type Out = String }")).isLeft)

  test("backticked lexical spelling decodes to the same Type.Name value while tokens retain the distinction"):
    val plain = parseType("F[Int]").asInstanceOf[Type.Apply]
    val backticked = parseType("`F`[Int]").asInstanceOf[Type.Apply]

    assertEquals(plain.tpe.asInstanceOf[Type.Name].value, "F")
    assertEquals(backticked.tpe.asInstanceOf[Type.Name].value, "F")
    assertEquals(backticked.structure, plain.structure)
    assertEquals(candidateShape(backticked), candidateShape(plain))
    assertEquals(plain.tokens.map(_.text).mkString, "F[Int]")
    assertEquals(backticked.tokens.map(_.text).mkString, "`F`[Int]")
    assertEquals(candidateAuthor(candidateNormalForm(candidateShape(backticked).toOption.get).toOption.get).toOption.get.syntax, "F[Int]")

  test("fixed-constructor arity and selected-resolved boundaries remain unchanged"):
    val wrongArity = parseType("Either[Int]")
    assertEquals(
      ScalametaTypeNormalFormProjection.project(wrongArity),
      Left(
        NeutralProjectionError(
          "NEUTRAL_TYPE_NORMAL_FORM_REJECTED",
          "Expected exactly 2 type arguments for `Either`, but found 1."
        )
      )
    )

    val selected = parseType("scala.Option[Int]")
    assertEquals(
      ScalametaTypeNormalFormProjection.project(selected),
      Left(
        NeutralProjectionError(
          "NEUTRAL_TYPE_NORMAL_FORM_REJECTED",
          "Selected type constructor syntax `scala.Option[...]` is not supported; use unqualified `Option[...]` in the current experimental surface."
        )
      )
    )

    val resolved = STypeApply(
      STypeResolved(StandardResolvedTypeNames.OptionId),
      List(STypeIdent("Int"))
    )
    assertEquals(
      ScalametaTypeNormalFormAuthoring.author(resolved).left.toOption.map(_.code),
      Some("NEUTRAL_TYPE_AUTHORING_RESOLVED_UNSUPPORTED")
    )
    assertNotEquals(
      STypeApply(STypeIdent("Option"), List(STypeIdent("Int"))),
      resolved
    )

  private def candidateShape(sourceType: Type): Either[String, TypeShape] =
    Option(sourceType).toRight("N073_CANDIDATE_MISSING_TYPE").flatMap {
      case name: Type.Name =>
        Right(TypeShape.Identifier(name.value))
      case applied: Type.Apply =>
        for
          constructor <- candidateShape(applied.tpe)
          arguments <- traverse(applied.args)(candidateShape)
          _ <- Either.cond(arguments.nonEmpty, (), "N073_CANDIDATE_EMPTY_APPLICATION")
          _ <- constructor match
            case TypeShape.Identifier(name)
                if candidateNames(name) || AppliedTypeConstructorPolicy
                  .forNormalFormSource(name, arguments.size)
                  .isDefined => Right(())
            case _ => Left("N073_CANDIDATE_CONSTRUCTOR_UNSUPPORTED")
        yield TypeShape.Apply(constructor, arguments)
      case _ =>
        Left("N073_CANDIDATE_STRUCTURE_UNSUPPORTED")
    }

  private def candidateNormalForm(shape: TypeShape): Either[String, TypeNormalForm] =
    Option(shape).toRight("N073_CANDIDATE_MISSING_SHAPE").flatMap {
      case TypeShape.Identifier(name) =>
        TypeNormalForm.fromShape(TypeShape.Identifier(name)).left.map(_.message)
      case TypeShape.Apply(TypeShape.Identifier(name), arguments)
          if candidateNames(name) || AppliedTypeConstructorPolicy
            .forNormalFormSource(name, arguments.size)
            .isDefined =>
        traverse(arguments)(candidateNormalForm).map(STypeApply(STypeIdent(name), _))
      case _ =>
        Left("N073_CANDIDATE_SHAPE_UNSUPPORTED")
    }

  private def candidateAuthor(normalForm: TypeNormalForm): Either[String, Type] =
    Option(normalForm).toRight("N073_CANDIDATE_MISSING_FORM").flatMap {
      case STypeIdent(name) =>
        if Set("Int", "String", "Boolean", "AnyVal")(name) then Right(Type.Name(name))
        else Left("N073_CANDIDATE_ATOM_UNSUPPORTED")
      case STypeApply(STypeIdent(name), arguments)
          if candidateNames(name) || AppliedTypeConstructorPolicy
            .forNormalFormSource(name, arguments.size)
            .isDefined =>
        traverse(arguments)(candidateAuthor).map(Type.Apply(Type.Name(name), _))
      case _ =>
        Left("N073_CANDIDATE_FORM_UNSUPPORTED")
    }

  private def traverse[A, B](values: List[A])(convert: A => Either[String, B]): Either[String, List[B]] =
    values.foldRight(Right(Nil): Either[String, List[B]]) { (value, accumulated) =>
      for
        head <- convert(value)
        tail <- accumulated
      yield head :: tail
    }

  private def parseType(source: String): Type = Input.String(source).parse[Type].get

  private def allNodes(tree: Tree): List[Tree] = tree :: tree.children.flatMap(allNodes)
