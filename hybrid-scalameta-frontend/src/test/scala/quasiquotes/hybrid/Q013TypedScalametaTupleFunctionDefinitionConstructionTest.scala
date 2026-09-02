package quasiquotes.hybrid

import scala.quoted.staging.{Compiler, withQuotes}

final class Q013TypedScalametaTupleFunctionDefinitionConstructionTest extends munit.FunSuite:
  private final case class Evidence(
      label: String,
      directConstruction: Boolean,
      umbrellaConstruction: Boolean,
      directOwnerBinderAndIdentity: Boolean,
      umbrellaOwnerBinderAndIdentity: Boolean,
      directOriginalRhs: Boolean,
      umbrellaOriginalRhs: Boolean
  )

  test("typed-Scalameta direct and umbrella Definition hosts admit tuple function and nested families"):
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
            case _ => report.errorAndAbort("Q013 Scalameta fixture lost its parameter")
        )

      def ownerBinderAndIdentity(definition: DefDef, tpe: TypeRepr): Boolean =
        definition.paramss match
          case List(clause: TermParamClause) if clause.params.size == 1 =>
            val parameter = clause.params.head
            definition.symbol.owner == Symbol.spliceOwner &&
              parameter.symbol.owner == definition.symbol &&
              (parameter.tpt.tpe.asInstanceOf[AnyRef] eq tpe.asInstanceOf[AnyRef]) &&
              (definition.returnTpt.tpe.asInstanceOf[AnyRef] eq tpe.asInstanceOf[AnyRef]) &&
              (definition.rhs.get match
                case reference: Ref => reference.symbol == parameter.symbol
                case _ => false)
          case _ => false

      val families = List(
        ("(Int, String)", TypeRepr.of[(Int, String)]),
        ("(Int, String, Boolean)", TypeRepr.of[(Int, String, Boolean)]),
        ("Int => String", TypeRepr.of[Int => String]),
        ("(Int, Boolean) => String", TypeRepr.of[(Int, Boolean) => String]),
        ("Option[(Int, String)]", TypeRepr.of[Option[(Int, String)]]),
        ("List[Int => String]", TypeRepr.of[List[Int => String]]),
        (
          "Either[(Int, String), Boolean => Int]",
          TypeRepr.of[Either[(Int, String), Boolean => Int]]
        )
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

    val failures = evidence.filterNot(value =>
      value.directConstruction &&
        value.umbrellaConstruction &&
        value.directOwnerBinderAndIdentity &&
        value.umbrellaOwnerBinderAndIdentity &&
        value.directOriginalRhs &&
        value.umbrellaOriginalRhs
    )
    assertEquals(failures, Nil)

  test("typed-Scalameta construction retains unequal-Type rejection"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val message = withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*
      try
        quasiquotes.scalameta.ScalametaQuasiquotes.dqr(
          StringContext("def identity(value: ", "): ", " = value")
        )(using q)(TypeRepr.of[(Int, String)], TypeRepr.of[(Int, Boolean)])
        "<no-abort>"
      catch
        case failure: Throwable => Option(failure.getMessage).getOrElse(failure.getClass.getName)

    assert(message.contains("must have equal normalized types"))
