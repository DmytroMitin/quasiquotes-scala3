package quasiquotes.construct

import scala.quoted.staging.{Compiler, withQuotes}

import quasiquotes.definitions.DefinitionName
import quasiquotes.matching.DefinitionPattern
import quasiquotes.publicapi.CompletedType
import quasiquotes.types.{TargetTypeReprInspector, TypeNormalForm}

trait Q009RefinedBase:
  type Member

class Q009TypedDefinitionRecursiveTypeFeasibilityTest extends munit.FunSuite:
  private final case class LayerEvidence(
      label: String,
      inspected: String,
      completedKind: String,
      completedSource: String,
      lowererSucceeded: Boolean,
      publicDqrSucceeded: Boolean,
      ownerBinderAndIdentity: Boolean,
      publicDqqOriginalRhs: Boolean
  )

  test("required families locate the first loss across inspector conversion lowerer and public hosts"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val evidence = withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*

      val methodName = DefinitionName.plain("identity").toOption.get
      val parameterName = DefinitionName.plain("value").toOption.get
      val converter = TypedSingleParameterDefinitionLowerer.getClass
        .getDeclaredMethod("toCompletedType", classOf[TypeNormalForm])
      converter.setAccessible(true)

      def exactCompleted(normalForm: TypeNormalForm): CompletedType =
        converter
          .invoke(TypedSingleParameterDefinitionLowerer, normalForm)
          .asInstanceOf[Either[String, CompletedType]]
          .toOption
          .get

      def independentDefinition(tpe: TypeRepr): DefDef =
        val methodType = MethodType(List("value"))(_ => List(tpe), _ => tpe)
        val symbol = Symbol.newMethod(Symbol.spliceOwner, "identity", methodType)
        DefDef(symbol, clauses =>
          clauses match
            case List(List(parameter)) => Some(Ref(parameter.symbol))
            case _ => report.errorAndAbort("Q009 independent Definition fixture lost its parameter")
        )

      def currentDqr(tpe: TypeRepr): Either[String, DefDef] =
        try
          Right(
            Quasiquotes.dqr(
              StringContext("def identity(value: ", "): ", " = value")
            )(using q)(tpe, tpe)
          )
        catch
          case failure: Throwable => Left(Option(failure.getMessage).getOrElse(failure.getClass.getName))

      def ownerBinderAndIdentity(definition: DefDef, tpe: TypeRepr): Boolean =
        val parameter = definition.paramss.head.asInstanceOf[TermParamClause].params.head
        definition.symbol.owner == Symbol.spliceOwner &&
          parameter.symbol.owner == definition.symbol &&
          (parameter.tpt.tpe.asInstanceOf[AnyRef] eq tpe.asInstanceOf[AnyRef]) &&
          (definition.returnTpt.tpe.asInstanceOf[AnyRef] eq tpe.asInstanceOf[AnyRef]) &&
          (definition.rhs.get match
            case reference: Ref => reference.symbol == parameter.symbol
            case _ => false)

      val families = List(
        ("Int", TypeRepr.of[Int], "Int"),
        ("String", TypeRepr.of[String], "String"),
        ("Boolean", TypeRepr.of[Boolean], "Boolean"),
        ("List[Int]", TypeRepr.of[List[Int]], "List[Int]"),
        ("Option[String]", TypeRepr.of[Option[String]], "Option[String]"),
        ("Either[Int, String]", TypeRepr.of[Either[Int, String]], "Either[Int, String]"),
        ("List[Option[Int]]", TypeRepr.of[List[Option[Int]]], "List[Option[Int]]"),
        (
          "Either[List[Int], Option[String]]",
          TypeRepr.of[Either[List[Int], Option[String]]],
          "Either[List[Int], Option[String]]"
        ),
        ("(Int, String)", TypeRepr.of[(Int, String)], "(Int, String)"),
        (
          "(Int, String, Boolean)",
          TypeRepr.of[(Int, String, Boolean)],
          "(Int, String, Boolean)"
        ),
        ("Int => String", TypeRepr.of[Int => String], "Int => String"),
        (
          "(Int, Boolean) => String",
          TypeRepr.of[(Int, Boolean) => String],
          "(Int, Boolean) => String"
        )
      )

      families.map { (label, tpe, source) =>
        val normalForm = TargetTypeReprInspector.inspect(using q)(tpe).toOption.get
        val completed = exactCompleted(normalForm)
        val lowered = TypedSingleParameterDefinitionLowerer.lower(using q)(
          methodName,
          parameterName,
          tpe,
          tpe
        )
        val public = currentDqr(tpe)
        val target = independentDefinition(tpe)
        val pattern = DefinitionPattern.dqq(
          StringContext(s"def identity(value: $source): $source = ", "")
        )(using q)
        val captured = pattern.unapply(using q)(target)

        LayerEvidence(
          label,
          normalForm.render,
          completed.kindCode,
          completed.source,
          lowered.isRight,
          public.isRight,
          lowered.toOption.exists(ownerBinderAndIdentity(_, tpe)) &&
            public.toOption.exists(ownerBinderAndIdentity(_, tpe)),
          captured.exists(_ eq target.rhs.get)
        )
      }

    val admitted = evidence.take(8)
    val tupleAndFunction = evidence.drop(8)
    admitted.foreach { value =>
      assert(value.lowererSucceeded, value)
      assert(value.publicDqrSucceeded, value)
      assert(value.ownerBinderAndIdentity, value)
      assert(value.publicDqqOriginalRhs, value)
      assertEquals(
        value.completedKind,
        if value.label == "Int" || value.label == "String" || value.label == "Boolean" then "named" else "applied",
        value.label
      )
    }
    tupleAndFunction.foreach { value =>
      assert(!value.lowererSucceeded, value)
      assert(!value.publicDqrSucceeded, value)
      assert(!value.ownerBinderAndIdentity, value)
      assert(value.publicDqqOriginalRhs, value)
      assertEquals(value.completedKind, "applied", value.label)
    }
    assertEquals(evidence.map(_.label), List(
      "Int",
      "String",
      "Boolean",
      "List[Int]",
      "Option[String]",
      "Either[Int, String]",
      "List[Option[Int]]",
      "Either[List[Int], Option[String]]",
      "(Int, String)",
      "(Int, String, Boolean)",
      "Int => String",
      "(Int, Boolean) => String"
    ))
    assertEquals(evidence.map(_.inspected), List(
      "STypeIdent(Int)",
      "STypeIdent(String)",
      "STypeIdent(Boolean)",
      "STypeApply(STypeIdent(List), [STypeIdent(Int)])",
      "STypeApply(STypeIdent(Option), [STypeIdent(String)])",
      "STypeApply(STypeIdent(Either), [STypeIdent(Int), STypeIdent(String)])",
      "STypeApply(STypeIdent(List), [STypeApply(STypeIdent(Option), [STypeIdent(Int)])])",
      "STypeApply(STypeIdent(Either), [STypeApply(STypeIdent(List), [STypeIdent(Int)]), STypeApply(STypeIdent(Option), [STypeIdent(String)])])",
      "STypeTuple([STypeIdent(Int), STypeIdent(String)])",
      "STypeTuple([STypeIdent(Int), STypeIdent(String), STypeIdent(Boolean)])",
      "STypeFunction([STypeIdent(Int)], STypeIdent(String))",
      "STypeFunction([STypeIdent(Int), STypeIdent(Boolean)], STypeIdent(String))"
    ))
    assertEquals(evidence.map(_.completedSource), List(
      "Int",
      "String",
      "Boolean",
      "List[Int]",
      "Option[String]",
      "Either[Int, String]",
      "List[Option[Int]]",
      "Either[List[Int], Option[String]]",
      "Tuple2[Int, String]",
      "Tuple3[Int, String, Boolean]",
      "Function1[Int, String]",
      "Function2[Int, Boolean, String]"
    ))

  test("negative controls reject unequal and refined Types without widening"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val evidence = withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*

      val unequal = TypedSingleParameterDefinitionLowerer.lower(using q)(
        DefinitionName.plain("identity").toOption.get,
        DefinitionName.plain("value").toOption.get,
        TypeRepr.of[Int],
        TypeRepr.of[String]
      )
      val refined = TargetTypeReprInspector.inspect(using q)(
        TypeRepr.of[Q009RefinedBase { type Member = Int }]
      )
      (unequal.left.toOption, refined.left.toOption.map(_.message))

    assert(evidence._1.exists(_.contains("must have equal normalized types")))
    assert(evidence._2.exists(_.contains("Unsupported target type representation")))
