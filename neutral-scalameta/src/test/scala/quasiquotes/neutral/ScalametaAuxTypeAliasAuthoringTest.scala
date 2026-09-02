package quasiquotes.neutral

import quasiquotes.definitions.*
import quasiquotes.definitions.ScopedType.*
import quasiquotes.parser.BinderId

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

@nowarn("cat=deprecation")
final class ScalametaAuxTypeAliasAuthoringTest extends munit.FunSuite:
  private val Canonical =
    "type Aux[N <: Nat, M <: Nat, Out0 <: Nat] = Add[N, M] { type Out = Out0 }"

  test("authors the canonical N001 plan to the exact bounded Defn.Type topology"):
    val authored = author(project(parseAlias(Canonical), canonicalExpectation))

    assertDefinition(
      authored,
      aliasName = "Aux",
      parameterNames = List("N", "M", "Out0"),
      upperBounds = List("Nat", "Nat", "Nat"),
      targetName = "Add",
      targetArguments = List("N", "M"),
      memberName = "Out",
      outputName = "Out0"
    )

  test("preserves fully renamed source-facing names and binder roles"):
    val source =
      "type Evidence[Left <: Domain, Right <: Domain, Result0 <: Domain] = Combine[Left, Right] { type Result = Result0 }"
    val expected = renamedExpectation
    val authored = author(project(parseAlias(source), expected))

    assertDefinition(
      authored,
      aliasName = "Evidence",
      parameterNames = List("Left", "Right", "Result0"),
      upperBounds = List("Domain", "Domain", "Domain"),
      targetName = "Combine",
      targetArguments = List("Left", "Right"),
      memberName = "Result",
      outputName = "Result0"
    )

  test("authors fresh unpositioned roots recursive children bounds and clauses"):
    val positioned = parseAlias(Canonical)
    assertNotEquals(positioned.pos, Position.None)

    val authored = author(project(positioned, canonicalExpectation))
    assert(allConstructedTrees(authored).forall(_.pos == Position.None))

  test("N001 to N012 to N001 preserves bounded source semantics and canonical binder positions"):
    val fixtures = List(
      Canonical -> canonicalExpectation,
      "type Evidence[Left <: Domain, Right <: Domain, Result0 <: Domain] = Combine[Left, Right] { type Result = Result0 }" -> renamedExpectation
    )

    fixtures.foreach { case (source, expected) =>
      val initial = project(parseAlias(source), expected)
      val reprojected = project(author(initial), expected)

      assertEquals(reprojected.aliasDisplayName, initial.aliasDisplayName)
      assertEquals(reprojected.typeParameters.map(_.displayName), initial.typeParameters.map(_.displayName))
      assertEquals(reprojected.typeParameters.map(_.upperBound), initial.typeParameters.map(_.upperBound))
      assertEquals(reprojected.appliedBase.constructor, initial.appliedBase.constructor)
      assertEquals(reprojected.argumentBinderPositions, Vector(0, 1))
      assertEquals(reprojected.refinementMember.memberName, initial.refinementMember.memberName)
      assertEquals(reprojected.outputReference.binderId, BinderId(2))
      assertEquals(reprojected.outputReference.displayName, initial.outputReference.displayName)
    }

  test("noncanonical BinderIds author by declaration references rather than numeric identity"):
    val plan = createPlan(renamedExpectation, Vector(10, 20, 30).map(BinderId(_)))
    val authored = author(plan)

    assertDefinition(
      authored,
      aliasName = "Evidence",
      parameterNames = List("Left", "Right", "Result0"),
      upperBounds = List("Domain", "Domain", "Domain"),
      targetName = "Combine",
      targetArguments = List("Left", "Right"),
      memberName = "Result",
      outputName = "Result0"
    )

    val reprojected = project(authored, renamedExpectation)
    assertEquals(reprojected.typeParameters.map(_.binderId), Vector(0, 1, 2).map(BinderId(_)))
    assertEquals(reprojected.argumentBinderPositions, Vector(0, 1))
    assertEquals(reprojected.outputReference.binderId, BinderId(2))

  test("rejects a missing plan with one stable bounded authoring error"):
    assertEquals(
      ScalametaAuxTypeAliasAuthoring.author(null),
      Left(
        ScalametaAuxTypeAliasAuthoring.Error(
          "NEUTRAL_AUX_AUTHORING_MISSING",
          "the AuxTypeAliasPlan must be present."
        )
      )
    )

  test("malformed topology is stopped by the reused Core plan boundary"):
    val parameters = declarations(canonicalExpectation, Vector(0, 1, 2).map(BinderId(_)))
    val malformed = Refinement(
      Applied(
        SourceName("Add"),
        Vector(
          TypeParameterReference(parameters(1).binderId, parameters(1).displayName),
          TypeParameterReference(parameters(0).binderId, parameters(0).displayName)
        )
      ),
      Vector(ScopedTypeAlias("Out", TypeParameterReference(parameters(2).binderId, "Out0")))
    )

    assertEquals(
      AuxTypeAliasPlan
        .create("Aux", parameters, malformed, canonicalExpectation)
        .left
        .toOption
        .map(_.code),
      Some("TARGET_BINDER_REFERENCE_MISMATCH")
    )

  private val canonicalExpectation = AuxTypeAliasExpectation(
    aliasName = "Aux",
    firstParameter = AuxTypeParameterExpectation("N", "Nat"),
    secondParameter = AuxTypeParameterExpectation("M", "Nat"),
    outputParameter = AuxTypeParameterExpectation("Out0", "Nat"),
    targetName = "Add",
    refinementMemberName = "Out"
  )

  private val renamedExpectation = AuxTypeAliasExpectation(
    aliasName = "Evidence",
    firstParameter = AuxTypeParameterExpectation("Left", "Domain"),
    secondParameter = AuxTypeParameterExpectation("Right", "Domain"),
    outputParameter = AuxTypeParameterExpectation("Result0", "Domain"),
    targetName = "Combine",
    refinementMemberName = "Result"
  )

  private def createPlan(
      expected: AuxTypeAliasExpectation,
      binderIds: Vector[BinderId]
  ): AuxTypeAliasPlan =
    val parameters = declarations(expected, binderIds)
    val rhs = Refinement(
      Applied(
        SourceName(expected.targetName),
        Vector(
          TypeParameterReference(parameters(0).binderId, parameters(0).displayName),
          TypeParameterReference(parameters(1).binderId, parameters(1).displayName)
        )
      ),
      Vector(
        ScopedTypeAlias(
          expected.refinementMemberName,
          TypeParameterReference(parameters(2).binderId, parameters(2).displayName)
        )
      )
    )
    AuxTypeAliasPlan
      .create(expected.aliasName, parameters, rhs, expected)
      .fold(problem => fail(problem.message), identity)

  private def declarations(
      expected: AuxTypeAliasExpectation,
      binderIds: Vector[BinderId]
  ): Vector[ScopedTypeParameter] =
    expected.parameters.zip(binderIds).map { case (parameter, binderId) =>
      ScopedTypeParameter(
        binderId,
        parameter.displayName,
        SourceName(parameter.upperBoundName)
      )
    }

  private def parseAlias(source: String): Defn.Type =
    Scala3(source).parse[Stat].get.asInstanceOf[Defn.Type]

  private def project(
      definition: Defn.Type,
      expected: AuxTypeAliasExpectation
  ): AuxTypeAliasPlan =
    ScalametaAuxTypeAliasProjection
      .project(definition, expected)
      .fold(problem => fail(problem.message), _.plan)

  private def author(plan: AuxTypeAliasPlan): Defn.Type =
    ScalametaAuxTypeAliasAuthoring
      .author(plan)
      .fold(problem => fail(problem.message), identity)

  private def assertDefinition(
      definition: Defn.Type,
      aliasName: String,
      parameterNames: List[String],
      upperBounds: List[String],
      targetName: String,
      targetArguments: List[String],
      memberName: String,
      outputName: String
  ): Unit =
    assertEquals(definition.mods, Nil)
    assertEquals(definition.name.value, aliasName)
    assertEquals(definition.tparamClause.values.map(_.name.value), parameterNames)
    assertEquals(
      definition.tparamClause.values.map(_.bounds.hi.get.asInstanceOf[Type.Name].value),
      upperBounds
    )
    definition.tparamClause.values.foreach { parameter =>
      assertEquals(parameter.mods, Nil)
      assertEquals(parameter.tparamClause.values, Nil)
      assertEquals(parameter.bounds.lo, None)
      assertEquals(parameter.bounds.context, Nil)
      assertEquals(parameter.bounds.view, Nil)
    }
    assertEmptyBounds(definition.bounds)

    val refinement = definition.body.asInstanceOf[Type.Refine]
    val applied = refinement.tpe.get.asInstanceOf[Type.Apply]
    assertEquals(applied.tpe.asInstanceOf[Type.Name].value, targetName)
    assertEquals(applied.argClause.values.map(_.asInstanceOf[Type.Name].value), targetArguments)
    assertEquals(refinement.stats.size, 1)

    val member = refinement.stats.head.asInstanceOf[Defn.Type]
    assertEquals(member.mods, Nil)
    assertEquals(member.name.value, memberName)
    assertEquals(member.tparamClause.values, Nil)
    assertEmptyBounds(member.bounds)
    assertEquals(member.body.asInstanceOf[Type.Name].value, outputName)

  private def assertEmptyBounds(bounds: Type.Bounds): Unit =
    assertEquals(bounds.lo, None)
    assertEquals(bounds.hi, None)
    assertEquals(bounds.context, Nil)
    assertEquals(bounds.view, Nil)

  private def allConstructedTrees(definition: Defn.Type): List[Tree] =
    val parameters = definition.tparamClause.values
    val refinement = definition.body.asInstanceOf[Type.Refine]
    val applied = refinement.tpe.get.asInstanceOf[Type.Apply]
    val member = refinement.stats.head.asInstanceOf[Defn.Type]
    List(
      definition,
      definition.name,
      definition.tparamClause,
      definition.bounds,
      refinement,
      refinement.body,
      applied,
      applied.tpe,
      applied.argClause,
      member,
      member.name,
      member.tparamClause,
      member.bounds,
      member.body
    ) ++ parameters.flatMap(parameter =>
      List(
        parameter,
        parameter.name,
        parameter.tparamClause,
        parameter.bounds,
        parameter.bounds.hi.get
      )
    ) ++ applied.argClause.values
