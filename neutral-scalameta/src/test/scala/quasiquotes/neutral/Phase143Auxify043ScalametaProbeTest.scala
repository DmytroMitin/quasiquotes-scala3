package quasiquotes.neutral

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

/** Test-only Scalameta 4.17.3 characterization for AUXify input 043. */
@nowarn("cat=deprecation")
class Phase143Auxify043ScalametaProbeTest extends munit.FunSuite:
  test("canonical and renamed 043 definitions have one coherent Scalameta topology") {
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

    assertEquals(
      TestOnly043ScalametaShape.characterize(canonical),
      Right(
        TestOnly043ScalametaShape.Summary(
          "show",
          "A",
          "a",
          "inst",
          "Show",
          "String"
        )
      )
    )
    assertEquals(
      TestOnly043ScalametaShape.characterize(renamed),
      Right(
        TestOnly043ScalametaShape.Summary(
          "render",
          "Element",
          "value",
          "evidence",
          "Display",
          "Text"
        )
      )
    )
    assertEquals(
      canonical.syntax,
      "def show[A](a: A)(using inst: Show[A]): String = inst.show(a)"
    )
  }

  test("malformed neighboring shapes remain independently distinguishable") {
    val rows = List(
      "def show[A, B](a: A)(using inst: Show[A]): String = inst.show(a)" ->
        "TYPE_PARAMETER_TOPOLOGY_UNSUPPORTED",
      "def show[A](using inst: Show[A])(a: A): String = inst.show(a)" ->
        "ORDINARY_CLAUSE_UNSUPPORTED",
      "def show[A](a: A)(inst: Show[A]): String = inst.show(a)" ->
        "CONTEXTUAL_CLAUSE_UNSUPPORTED",
      "def show[A](a: A)(using inst: Show[A]): String = inst.render(a)" ->
        "BODY_SELECTED_METHOD_MISMATCH",
      "def show[A](a: A)(using inst: Show[A]): String = inst.show(other)" ->
        "BODY_ARGUMENT_BINDER_MISMATCH",
      "def show[A](a: A)(using inst: Show[A]): String = other.show(a)" ->
        "BODY_RECEIVER_BINDER_MISMATCH",
      "def show[A](a: A)(using inst: Show[A]): Box[String] = inst.show(a)" ->
        "RESULT_TYPE_UNSUPPORTED",
      "def show[A <: Bound](a: A)(using inst: Show[A]): String = inst.show(a)" ->
        "TYPE_PARAMETER_TOPOLOGY_UNSUPPORTED"
    )

    rows.foreach { case (source, expected) =>
      val definition = Scala3(source).parse[Stat].get.asInstanceOf[Defn.Def]
      assertEquals(
        TestOnly043ScalametaShape.characterize(definition).left.toOption,
        Some(expected),
        clues(source)
      )
    }
  }

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

@nowarn("cat=deprecation")
private object TestOnly043ScalametaShape:
  final case class Summary(
      methodName: String,
      typeParameterName: String,
      valueParameterName: String,
      contextualParameterName: String,
      traitName: String,
      resultTypeName: String
  ) derives CanEqual

  def characterize(definition: Defn.Def): Either[String, Summary] =
    for
      group <- definition.paramClauseGroups match
        case value :: Nil => Right(value)
        case _ => Left("PARAMETER_GROUP_TOPOLOGY_UNSUPPORTED")
      typeParameter <- group.tparamClause.values match
        case value :: Nil
            if value.mods.isEmpty &&
              value.tparamClause.values.isEmpty &&
              value.bounds.lo.isEmpty &&
              value.bounds.hi.isEmpty &&
              value.bounds.context.isEmpty &&
              value.bounds.view.isEmpty =>
          Right(value)
        case _ => Left("TYPE_PARAMETER_TOPOLOGY_UNSUPPORTED")
      clauses <- group.paramClauses match
        case ordinary :: contextual :: Nil => Right(ordinary -> contextual)
        case _ => Left("VALUE_CLAUSE_TOPOLOGY_UNSUPPORTED")
      ordinary <- clauses._1 match
        case clause if clause.mod.isEmpty =>
          clause.values match
            case parameter :: Nil
                if parameter.mods.isEmpty && parameter.default.isEmpty =>
              Right(parameter)
            case _ => Left("ORDINARY_PARAMETER_UNSUPPORTED")
        case _ => Left("ORDINARY_CLAUSE_UNSUPPORTED")
      contextual <- clauses._2 match
        case clause if clause.mod.exists(_.isInstanceOf[Mod.Using]) =>
          clause.values match
            case parameter :: Nil
                if parameter.mods.forall(_.isInstanceOf[Mod.Using]) &&
                  parameter.default.isEmpty =>
              Right(parameter)
            case _ => Left("CONTEXTUAL_PARAMETER_UNSUPPORTED")
        case _ => Left("CONTEXTUAL_CLAUSE_UNSUPPORTED")
      ordinaryType <- ordinary.decltpe match
        case Some(name: Type.Name) if name.value == typeParameter.name.value =>
          Right(name)
        case _ => Left("ORDINARY_PARAMETER_TYPE_BINDER_MISMATCH")
      traitName <- contextual.decltpe match
        case Some(applied: Type.Apply) =>
          (applied.tpe, applied.argClause.values) match
            case (constructor: Type.Name, List(reference: Type.Name))
                if reference.value == typeParameter.name.value =>
              Right(constructor)
            case _ => Left("CONTEXTUAL_TYPE_UNSUPPORTED")
        case _ => Left("CONTEXTUAL_TYPE_UNSUPPORTED")
      resultType <- definition.decltpe match
        case Some(name: Type.Name) => Right(name)
        case _ => Left("RESULT_TYPE_UNSUPPORTED")
      application <- definition.body match
        case value: Term.Apply => Right(value)
        case _ => Left("BODY_APPLICATION_UNSUPPORTED")
      selection <- application.fun match
        case value: Term.Select => Right(value)
        case _ => Left("BODY_SELECTION_UNSUPPORTED")
      _ <- selection.qual match
        case name: Term.Name if name.value == contextual.name.value => Right(())
        case _ => Left("BODY_RECEIVER_BINDER_MISMATCH")
      _ <- Either.cond(
        selection.name.value == definition.name.value,
        (),
        "BODY_SELECTED_METHOD_MISMATCH"
      )
      _ <- application.args match
        case List(name: Term.Name) if name.value == ordinary.name.value => Right(())
        case _ => Left("BODY_ARGUMENT_BINDER_MISMATCH")
    yield Summary(
      definition.name.value,
      typeParameter.name.value,
      ordinary.name.value,
      contextual.name.value,
      traitName.value,
      resultType.value
    )
