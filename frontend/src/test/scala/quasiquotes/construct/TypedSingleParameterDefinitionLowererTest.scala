package quasiquotes.construct

import scala.quoted.staging.{Compiler, withQuotes}

import quasiquotes.definitions.DefinitionName
import quasiquotes.matching.DefinitionPattern
import quasiquotes.types.TypeNormalForm

class TypedSingleParameterDefinitionLowererTest extends munit.FunSuite:
  test("structured lowerer preserves caller Types and the current owner/binder shape"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val evidence = withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*

      val callerType = TypeRepr.of[Int]
      val methodName = DefinitionName.plain("identity").toOption.get
      val parameterName = DefinitionName.plain("value").toOption.get
      val definition = TypedSingleParameterDefinitionLowerer
        .lower(using q)(methodName, parameterName, callerType, callerType)
        .toOption
        .get
      val parameter = definition.paramss.head.asInstanceOf[TermParamClause].params.head
      val body = definition.rhs.get

      (
        definition.name,
        parameter.name,
        parameter.tpt.tpe.asInstanceOf[AnyRef].eq(callerType.asInstanceOf[AnyRef]),
        definition.returnTpt.tpe.asInstanceOf[AnyRef].eq(callerType.asInstanceOf[AnyRef]),
        definition.symbol.owner == Symbol.spliceOwner,
        parameter.symbol.owner == definition.symbol,
        body match
          case reference: Ref => reference.symbol == parameter.symbol
          case _ => false
      )

    assertEquals(evidence, ("identity", "value", true, true, true, true, true))

  test("structured lowerer rejects unequal normalized Types recoverably"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val result = withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*

      TypedSingleParameterDefinitionLowerer.lower(using q)(
        DefinitionName.plain("identity").toOption.get,
        DefinitionName.plain("value").toOption.get,
        TypeRepr.of[Int],
        TypeRepr.of[String]
      )

    assert(result.isLeft)

  test("structured pattern factory and source parser share the existing matcher"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val evidence = withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*

      val callerType = TypeRepr.of[Int]
      val methodName = DefinitionName.plain("identity").toOption.get
      val parameterName = DefinitionName.plain("value").toOption.get
      val definition = TypedSingleParameterDefinitionLowerer
        .lower(using q)(methodName, parameterName, callerType, callerType)
        .toOption
        .get
      val body = definition.rhs.get
      val structured = DefinitionPattern.singleParameterStructured(
        methodName,
        parameterName,
        TypeNormalForm.STypeIdent("Int"),
        TypeNormalForm.STypeIdent("Int")
      )
      val parsed = DefinitionPattern
        .singleParameter("def identity(value: Int): Int = $body")
        .toOption
        .get

      (
        structured
          .unapply(using q)(definition)
          .exists(_.asInstanceOf[AnyRef].eq(body.asInstanceOf[AnyRef])),
        parsed
          .unapply(using q)(definition)
          .exists(_.asInstanceOf[AnyRef].eq(body.asInstanceOf[AnyRef]))
      )

    assertEquals(evidence, (true, true))
