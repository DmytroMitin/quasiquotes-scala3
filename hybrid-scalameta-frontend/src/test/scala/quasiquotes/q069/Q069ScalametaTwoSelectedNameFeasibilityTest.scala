package quasiquotes.q069

import scala.compiletime.testing.typeCheckErrors

import quasiquotes.construct.hybrid.ScalametaTermFrontend
import quasiquotes.matching.{SelectedMemberPatternSource, TermPattern}
import quasiquotes.scalameta.TermFrontend

import scala.meta.Term

final class Q069ScalametaTwoSelectedNameFeasibilityTest extends munit.FunSuite:
  private inline def messages(inline source: String): List[String] =
    typeCheckErrors(source).map(_.message)

  test("inline source helper loses provenance and Q075 candidates fail closed"):
    val direct = messages(
      """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.qq
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$first.$second" => (receiver, first, second)
          case _ => ()"""
    )
    val unary = messages(
      """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.qq
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$first.$second($argument)" => (receiver, first, second, argument)
          case _ => ()"""
    )
    val repeated = messages(
      """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.qq
        def probe(using q: Quotes)(target: q.reflect.Term) = target match
          case qq"$receiver.$same.$same" => (receiver, same)
          case _ => ()"""
    )
    Vector(direct, unary).foreach { errors =>
      assert(errors.exists(_.contains("Invalid Scalameta qq term-pattern template")), clues(errors))
      assert(errors.exists(_.contains("Selected source occurrence index")), clues(errors))
      assert(!errors.exists(_.contains("selected-name capture is supported only")), clues(errors))
    }
    assert(repeated.exists(_.contains("duplicate pattern variable: same")), clues(repeated))
    assert(repeated.exists(_.contains("Selected source occurrence index")), clues(repeated))

  test("Scalameta parser and exact validator can compile the ordinary chain independently of selected-role typing"):
    val source = "$receiver.$first.$second($argument)"
    val classified = SelectedMemberPatternSource.classifyIncludingRanked(
      List("", ".", ".", "(", ")")
    )
    assert(classified.isLeft)
    val generated = "__qqhole_receiver.__qqhole_first.__qqhole_second(__qqhole_argument)"
    val tree = ScalametaTermFrontend.parse(generated).toOption.get
    tree match
      case Term.Apply(Term.Select(Term.Select(_, first), second), _) =>
        assertEquals(first.value, "__qqhole_first")
        assertEquals(second.value, "__qqhole_second")
      case other => fail(s"unexpected selected Scalameta tree: ${other.productPrefix}")
    assertEquals(ScalametaTermFrontend.validateExactCompiler(generated), Right(()))
    val compiled = TermFrontend.compile(source).toOption.get
    assertEquals(compiled.engine, TermFrontend.Engine.Scalameta)
    assertEquals(compiled.primaryFailure, None)
    assertEquals(compiled.pattern,
      TermPattern.Apply(
        TermPattern.Select(
          TermPattern.Select(TermPattern.Hole("receiver"), "__qqhole_first"),
          "__qqhole_second"
        ),
        List(TermPattern.Hole("argument"))
      ))
