package quasiquotes.hybrid

import scala.quoted.staging.{Compiler, withQuotes}

class Q009TypedScalametaDefinitionRecursiveTypeParityTest extends munit.FunSuite:
  private final case class ParityEvidence(
      label: String,
      currentConstruction: Boolean,
      scalametaConstruction: Boolean,
      currentOwnerBinderAndIdentity: Boolean,
      scalametaOwnerBinderAndIdentity: Boolean,
      currentPatternOriginalRhs: Boolean,
      scalametaPatternOriginalRhs: Boolean
  )

  test("current-Dotty and typed-Scalameta Definition routes agree for every required family"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val evidence = withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*

      def attempt(operation: => DefDef): Either[String, DefDef] =
        try Right(operation)
        catch
          case failure: Throwable => Left(Option(failure.getMessage).getOrElse(failure.getClass.getName))

      def independentDefinition(tpe: TypeRepr): DefDef =
        val methodType = MethodType(List("value"))(_ => List(tpe), _ => tpe)
        val symbol = Symbol.newMethod(Symbol.spliceOwner, "identity", methodType)
        DefDef(symbol, clauses =>
          clauses match
            case List(List(parameter)) => Some(Ref(parameter.symbol))
            case _ => report.errorAndAbort("Q009 parity fixture lost its parameter")
        )

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
        val current = attempt(
          quasiquotes.construct.Quasiquotes.dqr(
            StringContext("def identity(value: ", "): ", " = value")
          )(using q)(tpe, tpe)
        )
        val scalameta = attempt(
          quasiquotes.scalameta.ScalametaQuasiquotes.dqr(
            StringContext("def identity(value: ", "): ", " = value")
          )(using q)(tpe, tpe)
        )
        val target = independentDefinition(tpe)
        val currentPattern = quasiquotes.matching.DefinitionPattern.dqq(
          StringContext(s"def identity(value: $source): $source = ", "")
        )(using q)
        val scalametaPattern = quasiquotes.scalameta.ScalametaQuasiPattern.dqq(
          StringContext(s"def identity(value: $source): $source = ", "")
        )(using q)

        ParityEvidence(
          label,
          current.isRight,
          scalameta.isRight,
          current.toOption.exists(ownerBinderAndIdentity(_, tpe)),
          scalameta.toOption.exists(ownerBinderAndIdentity(_, tpe)),
          currentPattern.unapply(using q)(target).exists(_ eq target.rhs.get),
          scalametaPattern.unapply(using q)(target).exists(_ eq target.rhs.get)
        )
      }

    evidence.take(8).foreach { value =>
      assert(value.currentConstruction, value)
      assert(value.scalametaConstruction, value)
      assert(value.currentOwnerBinderAndIdentity, value)
      assert(value.scalametaOwnerBinderAndIdentity, value)
      assert(value.currentPatternOriginalRhs, value)
      assert(value.scalametaPatternOriginalRhs, value)
    }
    evidence.drop(8).foreach { value =>
      assert(!value.currentConstruction, value)
      assert(!value.scalametaConstruction, value)
      assert(!value.currentOwnerBinderAndIdentity, value)
      assert(!value.scalametaOwnerBinderAndIdentity, value)
      assert(value.currentPatternOriginalRhs, value)
      assert(value.scalametaPatternOriginalRhs, value)
    }

  test("both public construction hosts retain unequal-Type rejection"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val evidence = withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*

      def message(operation: => DefDef): String =
        try
          operation
          "<no-abort>"
        catch
          case failure: Throwable => Option(failure.getMessage).getOrElse(failure.getClass.getName)

      val current = message(
        quasiquotes.construct.Quasiquotes.dqr(
          StringContext("def identity(value: ", "): ", " = value")
        )(using q)(TypeRepr.of[Int], TypeRepr.of[String])
      )
      val scalameta = message(
        quasiquotes.scalameta.ScalametaQuasiquotes.dqr(
          StringContext("def identity(value: ", "): ", " = value")
        )(using q)(TypeRepr.of[Int], TypeRepr.of[String])
      )
      (current, scalameta)

    assert(evidence._1.contains("must have equal normalized types"))
    assert(evidence._2.contains("must have equal normalized types"))
