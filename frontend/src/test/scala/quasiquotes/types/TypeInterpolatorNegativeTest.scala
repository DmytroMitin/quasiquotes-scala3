package quasiquotes.types

import scala.compiletime.testing.typeCheckErrors

class TypeInterpolatorNegativeTest extends munit.FunSuite:
  private inline def messages(inline source: String): List[String] =
    typeCheckErrors(source).map(_.message)

  test("malformed tqr and tqq templates use controlled public prefixes"):
    assert(messages("quasiquotes.types.TypeInterpolatorMacros.malformedTqr").exists(_.contains("Invalid tqr type template:")))
    assert(messages("quasiquotes.types.TypeInterpolatorMacros.malformedTqq").exists(_.contains("Invalid tqq type-pattern template:")))

  test("unsupported templates and splices are rejected without backend fallback"):
    assert(messages("quasiquotes.types.TypeInterpolatorMacros.unsupportedTqrTemplate").exists(_.contains("Invalid tqr type template:")))
    assert(messages("quasiquotes.types.TypeInterpolatorMacros.unsupportedTqrSplice").exists(_.contains("Invalid tqr type template: unsupported TypeRepr splice")))
    assert(messages("quasiquotes.types.TypeInterpolatorMacros.unsupportedTqqTemplate").exists(_.contains("Invalid tqq type-pattern template:")))

  test("constructor holes remain outside the bounded tqq policy"):
    val errors = messages("quasiquotes.types.TypeInterpolatorMacros.constructorHoleTqq")
    assert(errors.exists(_.contains("Invalid tqq type-pattern template:")))
    assert(errors.exists(_.contains("Type-constructor hole")))

  test("hostile arity and null contexts fail with controlled diagnostics"):
    assert(messages("quasiquotes.types.TypeInterpolatorMacros.hostileTqrArity").exists(_.contains("Invalid tqr type template: expected 1 TypeRepr splice(s), but received 0.")))
    assert(messages("quasiquotes.types.TypeInterpolatorMacros.nullTqrContext").exists(_.contains("Invalid tqr type template: StringContext must not be null.")))
    assert(messages("quasiquotes.types.TypeInterpolatorMacros.nullTqqContext").exists(_.contains("Invalid tqq type-pattern template: StringContext must not be null.")))
