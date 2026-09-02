package quasiquotes.hybrid

import scala.quoted.staging.{Compiler, withQuotes}

final class Q010TypedScalametaFixedAppliedDefinitionConstructionTest extends munit.FunSuite:
  private final case class Evidence(
      label: String,
      directConstruction: Boolean,
      umbrellaConstruction: Boolean,
      directOwnerBinderAndIdentity: Boolean,
      umbrellaOwnerBinderAndIdentity: Boolean,
      directOriginalRhs: Boolean,
      umbrellaOriginalRhs: Boolean
  )

  test("typed-Scalameta direct and umbrella Definition hosts admit the fixed applied family"):
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
            case _ => report.errorAndAbort("Q010 Scalameta fixture lost its parameter")
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
        ("List[Int]", TypeRepr.of[List[Int]]),
        ("Option[String]", TypeRepr.of[Option[String]]),
        ("Either[Int, String]", TypeRepr.of[Either[Int, String]]),
        ("List[Option[Int]]", TypeRepr.of[List[Option[Int]]]),
        ("Either[List[Int], Option[String]]", TypeRepr.of[Either[List[Int], Option[String]]])
      )

      families.map { (source, tpe) =>
        val direct = attempt(
          quasiquotes.scalameta.ScalametaQuasiquotes.dqr(
            StringContext("def identity(value: ", "): ", " = value")
          )(using q)(tpe, tpe)
        )
        val umbrella = attempt {
          import quasiquotes.scalameta.Quasiquotes.dqr
          dqr(StringContext("def identity(value: ", "): ", " = value"))(using q)(tpe, tpe)
        }
        val target = independentDefinition(tpe)
        val directPattern = quasiquotes.scalameta.ScalametaQuasiPattern.dqq(
          StringContext(s"def identity(value: $source): $source = ", "")
        )(using q)
        val umbrellaPattern =
          import quasiquotes.scalameta.Quasiquotes.dqq
          dqq(StringContext(s"def identity(value: $source): $source = ", ""))(using q)

        Evidence(
          source,
          direct.isRight,
          umbrella.isRight,
          direct.toOption.exists(ownerBinderAndIdentity(_, tpe)),
          umbrella.toOption.exists(ownerBinderAndIdentity(_, tpe)),
          directPattern.unapply(using q)(target).exists(_ eq target.rhs.get),
          umbrellaPattern.unapply(using q)(target).exists(_ eq target.rhs.get)
        )
      }

    evidence.foreach { value =>
      assert(value.directConstruction, value)
      assert(value.umbrellaConstruction, value)
      assert(value.directOwnerBinderAndIdentity, value)
      assert(value.umbrellaOwnerBinderAndIdentity, value)
      assert(value.directOriginalRhs, value)
      assert(value.umbrellaOriginalRhs, value)
    }
