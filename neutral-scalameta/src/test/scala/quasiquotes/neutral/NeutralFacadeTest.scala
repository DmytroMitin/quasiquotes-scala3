package quasiquotes.neutral

import scala.annotation.nowarn
import scala.meta.*
import scala.meta.dialects.Scala3

import scala.meta.quasiquotes.{q as nqr}
import scala.meta.quasiquotes.{q as nqq}
import scala.meta.quasiquotes.{t as ntqr}
import scala.meta.quasiquotes.{t as ntqq}
import scala.meta.quasiquotes.{q as ndqr}
import scala.meta.quasiquotes.{q as ndqq}

/** Consumer-local aliases characterize direct Scalameta use, not a façade. */
@nowarn("cat=deprecation")
class DirectScalametaAuthoringTest extends munit.FunSuite:
  test("nqr and nqq delegate term construction, matching, and repeated splices") {
    val callee: Term = Term.Name("combine")
    val arguments: List[Term] = List(Lit.Int(1), Lit.String("two"))
    val call: Term = nqr"$callee(..$arguments)"

    val matched = call match
      case nqq"$function(..$capturedArguments)" =>
        function.structure == callee.structure &&
          capturedArguments.map(_.structure) == arguments.map(_.structure)
      case _ => false

    assert(matched)

    val receiver: Term = Term.Name("Show")
    val typeArgument: Type = Type.Name("A")
    val valueArgument: Term = Term.Name("inst")
    val selectedApplication: Term =
      nqr"$receiver.apply[$typeArgument]($valueArgument)"
    assertEquals(selectedApplication.syntax, "Show.apply[A](inst)")
  }

  test("ntqr and ntqq delegate applied-type construction and matching") {
    val argument: Type = Type.Name("A")
    val showOfA: Type = ntqr"Show[$argument]"

    val captured = showOfA match
      case ntqq"Show[$value]" => Some(value)
      case _ => None

    assertEquals(captured.map(_.structure), Some(argument.structure))
  }

  test("ndqr and ndqq delegate definition and repeated member mechanics") {
    val method: Stat =
      ndqr"def apply[A](using inst: Show[A]): Show[A] = inst"
    val members: List[Stat] = List(method)
    val companion: Stat = ndqr"object Show { ..$members }"
    val traitDefinition: Stat =
      ndqr"trait Show[A] { def show(a: A): String }"

    val methodMatched = method match
      case ndqq"def $name[..$tparams](...$paramss): $result = $body" =>
        name.value == "apply" &&
          tparams.map(_.name.value) == List("A") &&
          paramss.map(_.size) == List(1) &&
          result.exists(_.syntax == "Show[A]") &&
          body.syntax == "inst"
      case _ => false

    val companionMatched = companion match
      case ndqq"object $name { ..$capturedMembers }" =>
        name.value == "Show" && capturedMembers.map(_.structure) == members.map(_.structure)
      case _ => false

    val traitMatched = traitDefinition match
      case ndqq"trait $name[..$tparams] { ..$capturedMembers }" =>
        name.value == "Show" &&
          tparams.map(_.name.value) == List("A") &&
          capturedMembers.size == 1
      case _ => false

    assert(methodMatched)
    assert(companionMatched)
    assert(traitMatched)
    assert(method.pos != Position.None)
  }
