package quasiquotes.matching

import scala.compiletime.testing.typeCheckErrors

final class Q077RecursiveSelectedNameChainFeasibilityTest extends munit.FunSuite:
  test("public standard nested one-name composition agrees with flat two-name matching"):
    assertEquals(
      Q077StandardChainProbe.publicCompositionObservations,
      List(
        "distinct:alpha:beta:true:true:true",
        "equal:same:same:true:true:true",
        "invalid-apply:true",
        "invalid-unicode:true"
      )
    )

  test("public standard one-name matching peels fixed depths recursively"):
    assertEquals(
      Q077StandardChainProbe.recursivePeelObservations,
      List(
        "depth-0::true:true",
        "depth-1:alpha:true:true",
        "depth-2:alpha,beta:true:true",
        "depth-3:alpha,beta,gamma:true:true",
        "depth-4:alpha,beta,gamma,delta:true:true",
        "depth-8:alpha,beta,gamma,delta,epsilon,zeta,eta,theta:true:true",
        "short:true",
        "long-arbitrary-prefix:true",
        "long-fixed-root-rejected:true",
        "lexical:+,type,safe spaced name:true",
        "wrapped:true",
        "apply:true",
        "null:true"
      )
    )

  test("generic standard flat plan compiles N=1 through N=4 and statically types N=3/N=4"):
    assertEquals(
      Q077StandardChainProbe.flatModelObservations,
      List(
        "n1:alpha:true",
        "n2:alpha,beta:true",
        "n3:alpha,beta,gamma:true",
        "n4:alpha,beta,gamma,delta:true",
        "composition-n2:true",
        "composition-n3:true",
        "composition-n4:true"
      )
    )

    val three = Q077StandardFlatModel.plan(3).fold(fail(_), identity)
    val four = Q077StandardFlatModel.plan(4).fold(fail(_), identity)
    assertEquals(three.selectedOccurrences.map(_.selectOrdinal), Vector(2, 1, 0))
    assertEquals(four.selectedOccurrences.map(_.selectOrdinal), Vector(3, 2, 1, 0))
    assertEquals(three.selectedOccurrences.map(_.transport).distinct.size, 3)
    assertEquals(four.selectedOccurrences.map(_.transport).distinct.size, 4)

  test("public standard nested composition is equivalent to the Q076 private rank-2 model"):
    assertEquals(
      Q077StandardChainProbe.rankedCompositionObservations,
      List(
        "zero:first:second:0:true:true",
        "one:first:second:1:true:true",
        "many:first:second:3:true:true",
        "non-apply:true",
        "curried:true"
      )
    )

  test("standard public flat N=1/N=2 remain positive and N=3/N=4 remain unsupported"):
    assertEquals(typeCheckErrors(Q077StandardCompileCases.flatOne), Nil)
    assertEquals(typeCheckErrors(Q077StandardCompileCases.flatTwo), Nil)
    assert(typeCheckErrors(Q077StandardCompileCases.flatThree).nonEmpty)
    assert(typeCheckErrors(Q077StandardCompileCases.flatFour).nonEmpty)

  test("standard negative and source-role matrix fails atomically"):
    assertEquals(
      Q077StandardChainProbe.negativeObservations,
      List(
        "bad-ordinal:true",
        "duplicate-transport:true",
        "malformed-transport:true",
        "category-conflict:true",
        "missing-source:true",
        "source-spans:true",
        "unicode-source:true",
        "native-e005:true",
        "repeat-marker:true",
        "rank3:true",
        "multiple-rank2:true",
        "right-associative:true",
        "typeapply:true",
        "named-apply:true",
        "contextual-multiclause:true",
        "extra-scalar-rank2:true",
        "foreign-quotes:true"
      )
    )

private object Q077StandardCompileCases:
  inline val flatOne =
    """{
      import scala.quoted.*
      import quasiquotes.matching.QuasiPattern.*
      def probe(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$first" => (receiver, first)
        case _ => ()
    }"""

  inline val flatTwo =
    """{
      import scala.quoted.*
      import quasiquotes.matching.QuasiPattern.*
      def probe(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$first.$second" => (receiver, first, second)
        case _ => ()
    }"""

  inline val flatThree =
    """{
      import scala.quoted.*
      import quasiquotes.matching.QuasiPattern.*
      def probe(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$first.$second.$third" => (receiver, first, second, third)
        case _ => ()
    }"""

  inline val flatFour =
    """{
      import scala.quoted.*
      import quasiquotes.matching.QuasiPattern.*
      def probe(using q: Quotes)(target: q.reflect.Term) = target match
        case qq"$receiver.$first.$second.$third.$fourth" =>
          (receiver, first, second, third, fourth)
        case _ => ()
    }"""
