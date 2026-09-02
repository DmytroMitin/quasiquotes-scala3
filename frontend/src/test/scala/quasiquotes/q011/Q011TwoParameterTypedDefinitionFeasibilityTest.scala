package quasiquotes.q011

import scala.compiletime.testing.typeCheckErrors
import scala.quoted.staging.{Compiler, withQuotes}

import quasiquotes.construct.Quasiquotes
import quasiquotes.construct.TypedTwoParameterDefinitionLowerer
import quasiquotes.definitions.DefinitionName
import quasiquotes.publicapi.{CompletedTerm, CompletedType, DefinitionConstruction}

final class Q011TwoParameterTypedDefinitionFeasibilityTest extends munit.FunSuite:
  private final case class ConstructionEvidence(
      label: String,
      coreSource: String,
      owner: Boolean,
      oneOrderedClause: Boolean,
      parameterOwners: Boolean,
      paramSymssIdentity: Boolean,
      typeIdentity: Boolean,
      selectedBinder: Boolean
  )

  test("exact-two lowerer reuses Core and preserves owner binder order and caller Type identity"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val evidence = withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*

      val methodName = DefinitionName.plain("choose").toOption.get
      val leftName = DefinitionName.plain("left").toOption.get
      val rightName = DefinitionName.plain("right").toOption.get

      def completed(tpe: TypeRepr): CompletedType =
        val name =
          if tpe =:= TypeRepr.of[Int] then "Int"
          else if tpe =:= TypeRepr.of[String] then "String"
          else "Boolean"
        CompletedType.named(name).toOption.get

      def row(
          label: String,
          firstType: TypeRepr,
          secondType: TypeRepr,
          resultType: TypeRepr,
          selected: DefinitionName
      ): ConstructionEvidence =
        val core = DefinitionConstruction.twoParameterMethod(
          "choose",
          "left",
          completed(firstType),
          "right",
          completed(secondType),
          completed(resultType),
          CompletedTerm.definitionParameterReference(selected.decoded).toOption.get
        ).toOption.get
        val definition = TypedTwoParameterDefinitionLowerer.lower(using q)(
          methodName,
          leftName,
          firstType,
          rightName,
          secondType,
          resultType,
          selected
        ).toOption.get
        val clause = definition.paramss.head.asInstanceOf[TermParamClause]
        val List(first, second) = clause.params: @unchecked
        val selectedSymbol = if selected == leftName then first.symbol else second.symbol

        ConstructionEvidence(
          label,
          core.source,
          definition.symbol.owner == Symbol.spliceOwner,
          definition.paramss.size == 1 && !clause.isImplicit && !clause.isGiven &&
            !clause.isErased && clause.params.map(_.name) == List("left", "right"),
          first.symbol.owner == definition.symbol && second.symbol.owner == definition.symbol,
          definition.symbol.paramSymss == List(List(first.symbol, second.symbol)),
          (first.tpt.tpe.asInstanceOf[AnyRef] eq firstType.asInstanceOf[AnyRef]) &&
            (second.tpt.tpe.asInstanceOf[AnyRef] eq secondType.asInstanceOf[AnyRef]) &&
            (definition.returnTpt.tpe.asInstanceOf[AnyRef] eq resultType.asInstanceOf[AnyRef]),
          definition.rhs.exists {
            case reference: Ref => reference.symbol == selectedSymbol
            case _ => false
          }
        )

      List(
        row("Int/String -> first", TypeRepr.of[Int], TypeRepr.of[String], TypeRepr.of[Int], leftName),
        row("Int/String -> second", TypeRepr.of[Int], TypeRepr.of[String], TypeRepr.of[String], rightName),
        row("Boolean/Int -> first", TypeRepr.of[Boolean], TypeRepr.of[Int], TypeRepr.of[Boolean], leftName),
        row("String/Boolean -> second", TypeRepr.of[String], TypeRepr.of[Boolean], TypeRepr.of[Boolean], rightName)
      )

    evidence.foreach { row =>
      assert(row.coreSource.endsWith(if row.label.endsWith("first") then "= left" else "= right"), row)
      assert(row.owner, row)
      assert(row.oneOrderedClause, row)
      assert(row.parameterOwners, row)
      assert(row.paramSymssIdentity, row)
      assert(row.typeIdentity, row)
      assert(row.selectedBinder, row)
    }

  test("Core equality and the fixed-applied two-parameter boundary remain closed"):
    val intType = CompletedType.named("Int").toOption.get
    val stringType = CompletedType.named("String").toOption.get
    val listInt = CompletedType
      .applied(CompletedType.named("List").toOption.get, Vector(intType))
      .toOption
      .get
    val left = CompletedTerm.definitionParameterReference("left").toOption.get

    val unequal = DefinitionConstruction.twoParameterMethod(
      "choose", "left", intType, "right", stringType, stringType, left
    )
    val applied = DefinitionConstruction.twoParameterMethod(
      "choose", "left", listInt, "right", stringType, listInt, left
    )

    assert(unequal.left.toOption.exists(_.message.contains("result type to equal")))
    assert(applied.left.toOption.exists(_.message.contains("`List`")))

  test("matching admits arbitrary RHS with original identity and rejects every bounded mismatch"):
    val evidence = Q011DefinitionPatternFixtureProbe.evidence
    assertEquals(evidence.head, ("success-arbitrary-rhs", true, true))
    evidence.tail.foreach { row =>
      assert(!row._2, row)
      assert(!row._3, row)
    }
    assertEquals(
      evidence.map(_._1),
      List(
        "success-arbitrary-rhs",
        "method-name",
        "first-parameter-name",
        "second-parameter-name",
        "first-parameter-type",
        "second-parameter-type",
        "result-type",
        "one-parameter",
        "three-parameters",
        "two-clauses",
        "default",
        "contextual",
        "foreign-owner"
      )
    )

  test("existing dqr signature now constructs exact-two templates and single templates stay unambiguous"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val evidence = withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*

      val intType = TypeRepr.of[Int]
      val stringType = TypeRepr.of[String]
      val single = Quasiquotes.dqr(
        StringContext("def identity(value: ", "): ", " = value")
      )(using q)(intType, intType)
      val exactTwo = Quasiquotes.dqr(
        StringContext("def first(left: ", ", right: ", "): ", " = left")
      )(using q)(intType, stringType, intType)

      (single.name, exactTwo.name)

    assertEquals(evidence, ("identity", "first"))

  test("the production same-spelling selector preserves exact Term binders through direct and umbrella imports"):
    assert(Q011DefinitionPatternApiTypingProbe.verify())

  test("explicit current single-pattern typing and matchDefinition remain source-valid"):
    val errors = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.matching.{
          DefinitionPattern,
          SingleParameterDefinitionMatch,
          SingleParameterDefinitionPattern
        }

        def current(using q: Quotes): Unit =
          val pattern: SingleParameterDefinitionPattern =
            DefinitionPattern.dqq(StringContext("def identity(value: Int): Int = ", ""))(using q)
          val _: q.reflect.DefDef => Option[
            SingleParameterDefinitionMatch[q.reflect.TypeRepr, q.reflect.Term]
          ] = pattern.matchDefinition(using q)
      }"""
    )
    assertEquals(errors, Nil)
