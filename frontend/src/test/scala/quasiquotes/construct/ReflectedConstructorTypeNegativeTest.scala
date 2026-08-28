package quasiquotes.construct

import scala.compiletime.testing.typeCheckErrors

final class ReflectedConstructorTypeNegativeTest extends munit.FunSuite:
  private inline def messages(inline call: String): List[String] =
    typeCheckErrors(call).map(_.message)

  test("reflected Types fail closed outside the complete constructor type"):
    val failures = Vector(
      messages("quasiquotes.construct.ReflectedConstructorTypeNegativeMacros.reflectedInTermPosition"),
      messages("quasiquotes.construct.ReflectedConstructorTypeNegativeMacros.reflectedInAscriptionPosition"),
      messages("quasiquotes.construct.ReflectedConstructorTypeNegativeMacros.reflectedInMethodTypePosition"),
      messages("quasiquotes.construct.ReflectedConstructorTypeNegativeMacros.reflectedInPartialConstructorPath"),
      messages("quasiquotes.construct.ReflectedConstructorTypeNegativeMacros.reflectedInAppliedConstructorType")
    )

    assert(failures.forall(_.nonEmpty))
    assert(failures(0).exists(_.contains("Reflected-Type splice")), failures(0).mkString(" | "))
    assert(
      failures(1).exists(
        _.toLowerCase(java.util.Locale.ROOT)
          .contains("only the complete type of a constructor expression")
      ),
      failures(1).mkString(" | ")
    )
    assert(failures(2).exists(_.contains("method type arguments")), failures(2).mkString(" | "))
    assert(failures(3).exists(_.contains("partial or applied constructor type syntax")), failures(3).mkString(" | "))
    assert(failures(4).exists(_.contains("partial or applied constructor type syntax")), failures(4).mkString(" | "))

  test("other payload categories remain invalid in constructor type position"):
    val term = messages(
      "quasiquotes.construct.ReflectedConstructorTypeNegativeMacros.termInConstructorTypePosition"
    )
    val compilerFree = messages(
      "quasiquotes.construct.ReflectedConstructorTypeNegativeMacros.constructedTypeInConstructorTypePosition"
    )

    assert(term.exists(_.contains("Term splice")))
    assert(term.exists(_.contains("complete type of a constructor expression")))
    assert(compilerFree.exists(_.contains("Constructed-type splice")), compilerFree.mkString(" | "))
    assert(compilerFree.exists(_.contains("complete type of an expression ascription")), compilerFree.mkString(" | "))

  test("the exact compiler rejects a non-instantiable reflected Type"):
    val errors = messages(
      "quasiquotes.construct.ReflectedConstructorTypeNegativeMacros.nonInstantiableReflectedType"
    )

    assert(errors.nonEmpty)
    assert(errors.exists(message =>
      message.contains("Could not apply constructor") ||
        message.toLowerCase(java.util.Locale.ROOT).contains("trait") ||
        message.toLowerCase(java.util.Locale.ROOT).contains("abstract")
    ))
