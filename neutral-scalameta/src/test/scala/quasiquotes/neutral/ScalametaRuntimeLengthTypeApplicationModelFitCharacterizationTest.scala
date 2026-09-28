package quasiquotes.neutral

import quasiquotes.parser.TypeShape
import quasiquotes.types.*
import quasiquotes.types.TypeNormalForm.*

import scala.annotation.nowarn
import scala.meta.*

/** Characterization of ordinary List cardinality, not a new sequence feature. */
@nowarn("cat=deprecation")
final class ScalametaRuntimeLengthTypeApplicationModelFitCharacterizationTest
    extends munit.FunSuite:
  private final case class ConstructorCase(name: String, requiredArity: Int)

  private val constructors = List(
    ConstructorCase("List", 1),
    ConstructorCase("Option", 1),
    ConstructorCase("Either", 2)
  )

  private val argumentNames = Vector("Int", "String", "Boolean")

  test("host-sequence origin erases into the existing normal-form and Scalameta List fields"):
    val literalForm = STypeApply(
      STypeIdent("Either"),
      List(STypeIdent("Int"), STypeIdent("String"))
    )
    val runtimeForm = STypeApply(STypeIdent("Either"), runtimeForms(2))

    assertEquals(runtimeForm, literalForm)
    assertEquals(runtimeForm.render, literalForm.render)

    val literalAuthored = author(literalForm)
    val runtimeAuthored = author(runtimeForm)
    assertEquals(runtimeAuthored.structure, literalAuthored.structure)
    assertEquals(project(runtimeAuthored), literalForm)

    val literalTree = Type.Apply(
      Type.Name("Either"),
      List(Type.Name("Int"), Type.Name("String"))
    )
    val runtimeTree = Type.Apply(Type.Name("Either"), runtimeTypes(2))

    assertEquals(runtimeTree.tpe.structure, literalTree.tpe.structure)
    assertEquals(runtimeTree.args.map(_.structure), literalTree.args.map(_.structure))
    assertEquals(runtimeTree.structure, literalTree.structure)
    assertEquals(project(runtimeTree), project(literalTree))
    assertEquals(project(runtimeTree), literalForm)

  test("List Option and Either cardinalities 0 through 3 are decided by fixed constructor policy"):
    constructors.foreach { constructor =>
      (0 to 3).foreach { arity =>
        val shapeArguments = runtimeShapes(arity)
        val normalArguments = runtimeForms(arity)
        val shape = TypeShape.Apply(TypeShape.Identifier(constructor.name), shapeArguments)
        val normalForm = STypeApply(STypeIdent(constructor.name), normalArguments)

        if arity == constructor.requiredArity then
          val sourceType = Type.Apply(Type.Name(constructor.name), runtimeTypes(arity))
          val expected = Right(normalForm)
          assertEquals(TypeNormalForm.fromShape(shape), expected, clues(constructor, arity))
          assertEquals(
            ScalametaTypeNormalFormProjection.project(sourceType),
            Right(ProjectedTypeNormalForm(normalForm, None)),
            clues(constructor, arity)
          )
          val authored = author(normalForm)
          assertEquals(authored.structure, sourceType.structure, clues(constructor, arity))
          assertEquals(project(authored), normalForm, clues(constructor, arity))
        else
          val detail = wrongArityDetail(constructor, arity)
          assertEquals(
            TypeNormalForm.fromShape(shape),
            Left(TypeQuasiquoteError(detail)),
            clues(constructor, arity)
          )
          if arity == 0 then
            intercept[org.scalameta.invariants.InvariantFailedException] {
              Type.Apply(Type.Name(constructor.name), runtimeTypes(arity))
            }
          else
            val sourceType = Type.Apply(Type.Name(constructor.name), runtimeTypes(arity))
            assertEquals(
              ScalametaTypeNormalFormProjection.project(sourceType),
              Left(NeutralProjectionError("NEUTRAL_TYPE_NORMAL_FORM_REJECTED", detail)),
              clues(constructor, arity)
            )
          assertEquals(
            ScalametaTypeNormalFormAuthoring.author(normalForm),
            Left(
              ScalametaTypeNormalFormAuthoring.Error(
                "NEUTRAL_TYPE_AUTHORING_NORMAL_FORM_REJECTED",
                detail
              )
            ),
            clues(constructor, arity)
          )
      }
    }

  test("fresh generic F applications retain spelling and cardinality but fail current admission"):
    (1 to 3).foreach { arity =>
      val detail =
        "Unsupported applied type constructor `F`; supported constructors are List/1, Option/1, Either/2."
      val shape = TypeShape.Apply(TypeShape.Identifier("F"), runtimeShapes(arity))
      val sourceType = Type.Apply(Type.Name("F"), runtimeTypes(arity))
      val normalForm = STypeApply(STypeIdent("F"), runtimeForms(arity))

      assertEquals(sourceType.tpe.asInstanceOf[Type.Name].value, "F")
      assertEquals(sourceType.args.map(_.asInstanceOf[Type.Name].value), argumentNames.take(arity).toList)
      assertEquals(TypeNormalForm.fromShape(shape), Left(TypeQuasiquoteError(detail)))
      assertEquals(
        ScalametaTypeNormalFormProjection.project(sourceType),
        Left(NeutralProjectionError("NEUTRAL_TYPE_NORMAL_FORM_REJECTED", detail))
      )
      assertEquals(normalForm.render, s"STypeApply(STypeIdent(F), [${runtimeForms(arity).map(_.render).mkString(", ")}])")
      assertEquals(
        ScalametaTypeNormalFormAuthoring.author(normalForm),
        Left(
          ScalametaTypeNormalFormAuthoring.Error(
            "NEUTRAL_TYPE_AUTHORING_NORMAL_FORM_REJECTED",
            detail
          )
        )
      )
    }

    assertEquals(
      ScalametaTypeNormalFormAuthoring.author(STypeIdent("F")),
      Left(
        ScalametaTypeNormalFormAuthoring.Error(
          "NEUTRAL_TYPE_AUTHORING_NORMAL_FORM_REJECTED",
          "Unsupported type identifier `F`; supported identifiers are Int, String, Boolean, AnyVal."
        )
      )
    )

  test("a runtime-sized argument List changes neither selected nor resolved constructor facts"):
    val selectedNames = List("scala", "Option")
    val selectedShape = TypeShape.Apply(pathShape(selectedNames), runtimeShapes(1))
    val selectedTree = Type.Apply(freshPath(selectedNames), runtimeTypes(1))
    val selectedDetail =
      "Selected type constructor syntax `scala.Option[...]` is not supported; use unqualified `Option[...]` in the current experimental surface."

    assertEquals(TypeNormalForm.fromShape(selectedShape), Left(TypeQuasiquoteError(selectedDetail)))
    assertEquals(
      ScalametaTypeNormalFormProjection.project(selectedTree),
      Left(NeutralProjectionError("NEUTRAL_TYPE_NORMAL_FORM_REJECTED", selectedDetail))
    )

    val optionId = StandardResolvedTypeNames.OptionId
    val environment = ResolvedTypeEnvironment.fromIds(List(optionId)).toOption.get
    val resolvedForm = STypeApply(STypeResolved(optionId), runtimeForms(1))

    assertEquals(
      TypeNormalForm.fromShapeResolved(selectedShape, environment),
      Right(resolvedForm)
    )
    assertEquals(AppliedTypeConstructorPolicy.forResolved(optionId, 1).map(_.requiredArity), Some(1))
    List(0, 2, 3).foreach { arity =>
      val mismatch = TypeShape.Apply(pathShape(selectedNames), runtimeShapes(arity))
      assertEquals(AppliedTypeConstructorPolicy.forResolved(optionId, arity), None)
      assertEquals(
        TypeNormalForm.fromShapeResolved(mismatch, environment),
        Left(
          TypeQuasiquoteError(
            s"TYPE_NAME_RESOLUTION_CONSTRUCTOR_POLICY_MISMATCH: `scala.Option`/$arity is not one of the exact admitted List/1, Option/1, Either/2 declarations."
          )
        )
      )
    }
    assertEquals(
      ScalametaTypeNormalFormAuthoring.author(resolvedForm),
      Left(
        ScalametaTypeNormalFormAuthoring.Error(
          "NEUTRAL_TYPE_AUTHORING_RESOLVED_UNSUPPORTED",
          "STypeResolved is outside the unresolved N002 authoring family."
        )
      )
    )

  test("fresh application fields contain no host-sequence provenance channel"):
    val literal = Type.Apply(Type.Name("List"), List(Type.Name("Int")))
    val runtime = Type.Apply(Type.Name("List"), runtimeTypes(1))

    List(literal, runtime).foreach { applied =>
      assertEquals(applied.productArity, 2)
      assertEquals(applied.productIterator.toList, List(applied.tpe, applied.argClause))
      assertEquals(applied.args.map(_.structure), List("Type.Name(\"Int\")"))
      assert(allNodes(applied).forall(_.pos == Position.None))
    }
    assertEquals(runtime.structure, literal.structure)
    assert(!allNodes(runtime).exists(runtimeNode => allNodes(literal).exists(_ eq runtimeNode)))

  private def runtimeForms(arity: Int): List[TypeNormalForm] =
    (0 until arity).iterator.map(index => STypeIdent(argumentNames(index))).toList

  private def runtimeShapes(arity: Int): List[TypeShape] =
    (0 until arity).iterator.map(index => TypeShape.Identifier(argumentNames(index))).toList

  private def runtimeTypes(arity: Int): List[Type] =
    (0 until arity).iterator.map(index => Type.Name(argumentNames(index)): Type).toList

  private def wrongArityDetail(constructor: ConstructorCase, actualArity: Int): String =
    val noun = if constructor.requiredArity == 1 then "argument" else "arguments"
    s"Expected exactly ${constructor.requiredArity} type $noun for `${constructor.name}`, but found $actualArity."

  private def freshPath(names: List[String]): Type.Select =
    val qualifier = names.init.tail.foldLeft[Term.Ref](Term.Name(names.head)) { (prefix, name) =>
      Term.Select(prefix, Term.Name(name))
    }
    Type.Select(qualifier, Type.Name(names.last))

  private def pathShape(names: List[String]): TypeShape =
    names.tail.foldLeft[TypeShape](TypeShape.Identifier(names.head))(TypeShape.Select(_, _))

  private def author(normalForm: TypeNormalForm): Type =
    ScalametaTypeNormalFormAuthoring.author(normalForm) match
      case Right(value) => value
      case Left(error) => fail(error.message)

  private def project(sourceType: Type): TypeNormalForm =
    ScalametaTypeNormalFormProjection.project(sourceType) match
      case Right(value) => value.normalForm
      case Left(error) => fail(error.message)

  private def allNodes(tree: Tree): List[Tree] =
    tree :: tree.children.flatMap(allNodes)
