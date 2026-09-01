package quasiquotes.q003

import scala.compiletime.testing.typeCheckErrors

import quasiquotes.matching.RankedPatternSource

class Q003SequenceNewDiagnosticTest extends munit.FunSuite:
  private inline def messages(inline source: String): List[String] =
    typeCheckErrors(source).map(_.message)

  private def assertInvalid(errors: List[String]): Unit =
    assert(errors.exists(_.contains("Invalid qq term-pattern template")), errors.mkString(" | "))

  test("New rejects multiple, rank-3, malformed, and constructor-position rank markers"):
    val multiple = messages(
      """import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def attempt(using q: Quotes)(term: q.reflect.Term) = term match
          case qq"new quasiquotes.q003.Q003Constructor(..$left, ..$right)" => ()
          case _ => ()
      """
    )
    val rankThree = messages(
      """import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def attempt(using q: Quotes)(term: q.reflect.Term) = term match
          case qq"new quasiquotes.q003.Q003Constructor(...$arguments)" => ()
          case _ => ()
      """
    )
    val constructorPosition = messages(
      """import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def attempt(using q: Quotes)(term: q.reflect.Term) = term match
          case qq"new ..$constructor(1)" => ()
          case _ => ()
      """
    )

    assert(multiple.exists(_.contains("only one rank-2 sequence-Term capture")))
    assert(rankThree.exists(_.contains("unsupported rank-3 `...` capture marker")))
    assertInvalid(constructorPosition)

  test("New keeps dynamic, type-applied, multi-clause, named, and anonymous boundaries closed"):
    val dynamic = messages(
      """import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def attempt(using q: Quotes)(term: q.reflect.Term) = term match
          case qq"new $constructor(..$arguments)" => ()
          case _ => ()
      """
    )
    val typeApplied = messages(
      """import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def attempt(using q: Quotes)(term: q.reflect.Term) = term match
          case qq"new java.util.ArrayList[Int](..$arguments)" => ()
          case _ => ()
      """
    )
    val multiClause = messages(
      """import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def attempt(using q: Quotes)(term: q.reflect.Term) = term match
          case qq"new quasiquotes.q003.Q003Constructor(..$arguments)(1)" => ()
          case _ => ()
      """
    )
    val named = messages(
      """import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def attempt(using q: Quotes)(term: q.reflect.Term) = term match
          case qq"new quasiquotes.q003.Q003Constructor(first = ..$arguments)" => ()
          case _ => ()
      """
    )
    val anonymous = messages(
      """import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def attempt(using q: Quotes)(term: q.reflect.Term) = term match
          case qq"new quasiquotes.q003.Q003Constructor(..$arguments) { }" => ()
          case _ => ()
      """
    )

    List(dynamic, typeApplied, multiClause, named, anonymous).foreach(assertInvalid)

  test("ranked New does not loosen non-list positions or scalar-sequence role reuse"):
    val tuple = messages(
      """import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def attempt(using q: Quotes)(term: q.reflect.Term) = term match
          case qq"(..$arguments, 1)" => ()
          case _ => ()
      """
    )
    val interpolation = RankedPatternSource.compile(List("s\"${..", "}\""), sequenceIndex = 0)
    val block = messages(
      """import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def attempt(using q: Quotes)(term: q.reflect.Term) = term match
          case qq"{ ..$arguments; 1 }" => ()
          case _ => ()
      """
    )
    val lambda = messages(
      """import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def attempt(using q: Quotes)(term: q.reflect.Term) = term match
          case qq"(..$arguments) => 1" => ()
          case _ => ()
      """
    )
    val typed = messages(
      """import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def attempt(using q: Quotes)(term: q.reflect.Term) = term match
          case qq"(..$arguments: Int)" => ()
          case _ => ()
      """
    )
    List(tuple, block, typed).foreach { errors =>
      assert(
        errors.exists(_.contains("direct ordinary Apply or fixed one-list New argument")),
        errors.mkString(" | ")
      )
    }
    assert(
      interpolation.left.exists(_.contains("direct ordinary Apply or fixed one-list New argument")),
      interpolation.toString
    )
    assertInvalid(lambda)

    val mixedRole = messages(
      """import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def attempt(using q: Quotes)(term: q.reflect.Term) = term match
          case qq"new quasiquotes.q003.Q003Constructor(..$same, $same)" => ()
          case _ => ()
      """
    )
    assert(mixedRole.exists(_.contains("duplicate pattern variable: same")), mixedRole.mkString(" | "))
