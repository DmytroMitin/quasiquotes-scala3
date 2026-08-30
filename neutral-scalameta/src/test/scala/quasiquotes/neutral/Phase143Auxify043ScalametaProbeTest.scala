package quasiquotes.neutral

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

import _root_.quasiquotes.definitions.DelegatedForwardingMethodPlan.*
import _root_.quasiquotes.definitions.ScopedType.*
import _root_.quasiquotes.parser.BinderId

/** Production projection contract for the exact AUXify input-043 definition. */
@nowarn("cat=deprecation")
class Phase143Auxify043ScalametaProbeTest extends munit.FunSuite:
  test("canonical and renamed definitions project three binder roles and one method identity") {
    val canonical = authoredDefinition(
      methodName = "show",
      typeParameterName = "A",
      valueParameterName = "a",
      contextualParameterName = "inst",
      traitName = "Show",
      resultTypeName = "String"
    )
    val renamed = authoredDefinition(
      methodName = "render",
      typeParameterName = "Element",
      valueParameterName = "value",
      contextualParameterName = "evidence",
      traitName = "Display",
      resultTypeName = "Text"
    )

    val canonicalPlan = project(canonical)
    val renamedPlan = project(renamed)

    assertPlan(canonicalPlan, "show", "A", "a", "inst", "Show", "String")
    assertPlan(
      renamedPlan,
      "render",
      "Element",
      "value",
      "evidence",
      "Display",
      "Text"
    )
    assertEquals(
      canonical.syntax,
      "def show[A](a: A)(using inst: Show[A]): String = inst.show(a)"
    )
  }

  test("malformed neighboring shapes retain the earliest exact 043 diagnostic") {
    val rows = List(
      "private def show[A](a: A)(using inst: Show[A]): String = inst.show(a)" ->
        "DEFINITION_TOPOLOGY_UNSUPPORTED",
      "def show(a: A)(using inst: Show[A]): String = inst.show(a)" ->
        "TYPE_PARAMETER_TOPOLOGY_UNSUPPORTED",
      "def show[A, B](a: A)(using inst: Show[A]): String = inst.show(a)" ->
        "TYPE_PARAMETER_TOPOLOGY_UNSUPPORTED",
      "def show[A](a: A): String = a.toString" ->
        "VALUE_CLAUSE_TOPOLOGY_UNSUPPORTED",
      "def show[A](using inst: Show[A])(a: A): String = inst.show(a)" ->
        "ORDINARY_CLAUSE_UNSUPPORTED",
      "def show[A](a: A, b: A)(using inst: Show[A]): String = inst.show(a)" ->
        "ORDINARY_PARAMETER_UNSUPPORTED",
      "def show[A](a: A = ???)(using inst: Show[A]): String = inst.show(a)" ->
        "ORDINARY_PARAMETER_UNSUPPORTED",
      "def show[A](a: => A)(using inst: Show[A]): String = inst.show(a)" ->
        "ORDINARY_PARAMETER_UNSUPPORTED",
      "def show[A](a: A*)(using inst: Show[A]): String = inst.show(a)" ->
        "ORDINARY_PARAMETER_UNSUPPORTED",
      "def show[A](a: A)(inst: Show[A]): String = inst.show(a)" ->
        "CONTEXTUAL_CLAUSE_UNSUPPORTED",
      "def show[A](a: A)(using first: Show[A], second: Show[A]): String = first.show(a)" ->
        "CONTEXTUAL_PARAMETER_UNSUPPORTED",
      "def show[A](a: A)(using inst: Show[A] = ???): String = inst.show(a)" ->
        "CONTEXTUAL_PARAMETER_UNSUPPORTED",
      "def show[A](a: String)(using inst: Show[A]): String = inst.show(a)" ->
        "ORDINARY_PARAMETER_TYPE_BINDER_MISMATCH",
      "def show[A](a: A)(using inst: pkg.Show[A]): String = inst.show(a)" ->
        "CONTEXTUAL_PARAMETER_TYPE_UNSUPPORTED",
      "def show[A](a: A)(using inst: Show[String]): String = inst.show(a)" ->
        "CONTEXTUAL_PARAMETER_TYPE_BINDER_MISMATCH",
      "def show[A](a: A)(using inst: Show[A]): Box[String] = inst.show(a)" ->
        "RESULT_TYPE_UNSUPPORTED",
      "def show[A](a: A)(using inst: Show[A]): String = inst" ->
        "BODY_APPLICATION_UNSUPPORTED",
      "def show[A](a: A)(using inst: Show[A]): String = show(a)" ->
        "BODY_SELECTION_UNSUPPORTED",
      "def show[A](a: A)(using inst: Show[A]): String = inst.render(a)" ->
        "BODY_SELECTED_METHOD_MISMATCH",
      "def show[A](a: A)(using inst: Show[A]): String = inst.show(other)" ->
        "BODY_ARGUMENT_BINDER_MISMATCH",
      "def show[A](a: A)(using inst: Show[A]): String = other.show(a)" ->
        "BODY_RECEIVER_BINDER_MISMATCH",
      "def show[A](a: A)(using inst: Show[A]): String = inst.show(a, a)" ->
        "BODY_ARGUMENT_TOPOLOGY_UNSUPPORTED",
      "def show[A <: Bound](a: A)(using inst: Show[A]): String = inst.show(a)" ->
        "TYPE_PARAMETER_TOPOLOGY_UNSUPPORTED"
    )

    rows.foreach { case (source, expected) =>
      val definition = Scala3(source).parse[Stat].get.asInstanceOf[Defn.Def]
      assertEquals(
        ScalametaDelegatedForwardingMethodProjection
          .project(definition)
          .left
          .toOption
          .map(_.code),
        Some(expected),
        clues(source)
      )
    }
  }

  private def project(definition: Defn.Def): Plan =
    ScalametaDelegatedForwardingMethodProjection
      .project(definition)
      .fold(problem => fail(s"${problem.code}: ${problem.detail}"), _.plan)

  private def assertPlan(
      plan: Plan,
      methodName: String,
      typeName: String,
      ordinaryName: String,
      contextualName: String,
      constructorName: String,
      resultName: String
  ): Unit =
    assertEquals(plan.methodIdentity.sourceName, methodName)
    assertEquals(plan.typeParameter, TypeParameter(BinderId(0), typeName))
    assertEquals(
      plan.ordinaryParameter,
      OrdinaryParameter(
        BinderId(1),
        ordinaryName,
        TypeParameterReference(BinderId(0), typeName)
      )
    )
    assertEquals(
      plan.contextualParameter,
      ContextualParameter(
        BinderId(2),
        contextualName,
        Applied(
          SourceName(constructorName),
          Vector(TypeParameterReference(BinderId(0), typeName))
        )
      )
    )
    assertEquals(plan.resultType, SourceName(resultName))
    assert(plan.methodIdentity eq plan.body.selectedMethodIdentity)
    assertEquals(plan.body.receiver, ContextualReference(BinderId(2)))
    assertEquals(plan.body.argument, OrdinaryReference(BinderId(1)))

  private def authoredDefinition(
      methodName: String,
      typeParameterName: String,
      valueParameterName: String,
      contextualParameterName: String,
      traitName: String,
      resultTypeName: String
  ): Defn.Def =
    val method = Term.Name(methodName)
    val typeParameterNameTree = Type.Name(typeParameterName)
    val typeParameter: Type.Param = tparam"$typeParameterNameTree"
    val valueParameter = Term.Name(valueParameterName)
    val contextualParameter = Term.Name(contextualParameterName)
    val traitNameTree = Type.Name(traitName)
    val contextualType: Type = t"$traitNameTree[$typeParameterNameTree]"
    val resultType = Type.Name(resultTypeName)
    val definition: Defn.Def =
      q"def $method[..${List(typeParameter)}]($valueParameter: $typeParameterNameTree)(using $contextualParameter: $contextualType): $resultType = $contextualParameter.$method($valueParameter)"
    definition
