package quasiquotes.q050

import scala.compiletime.testing.typeCheckErrors
import scala.quoted.staging.{Compiler, withQuotes}

private object Q050Evidence:
  val applyEmpty = Q050ScalametaQqSequenceCaptureMacros.applyArguments(Q050Targets.empty())
  val applyOne = Q050ScalametaQqSequenceCaptureMacros.applyArguments(Q050Targets.one(1))
  val applyMany = Q050ScalametaQqSequenceCaptureMacros.applyArguments(Q050Targets.many(1, 2, 3))
  val applyTail = Q050ScalametaQqSequenceCaptureMacros.applyTail(Q050Targets.many(1, 2, 3))
  val applyInit = Q050ScalametaQqSequenceCaptureMacros.applyInit(Q050Targets.many(1, 2, 3))
  val applyEmptyMiddle = Q050ScalametaQqSequenceCaptureMacros.applyMiddle(Q050Targets.two(1, 9))
  val applyOneMiddle = Q050ScalametaQqSequenceCaptureMacros.applyMiddle(Q050Targets.many(1, 2, 9))
  val applyManyMiddle = Q050ScalametaQqSequenceCaptureMacros.applyMiddle(Q050Targets.four(1, 2, 3, 9))
  val applyInsufficient = Q050ScalametaQqSequenceCaptureMacros.applyMiddle(Q050Targets.one(1))
  val applyPrefixMismatch =
    Q050ScalametaQqSequenceCaptureMacros.applyFixedEndsMatch(Q050Targets.many(2, 3, 9))
  val applySuffixMismatch =
    Q050ScalametaQqSequenceCaptureMacros.applyFixedEndsMatch(Q050Targets.many(1, 3, 8))

  val newEmpty = Q050ScalametaQqSequenceCaptureMacros.newArguments(new Q050Constructor())
  val newOne = Q050ScalametaQqSequenceCaptureMacros.newArguments(new Q050Constructor(1))
  val newMany = Q050ScalametaQqSequenceCaptureMacros.newArguments(new Q050Constructor(1, 2, 3))
  val newTail = Q050ScalametaQqSequenceCaptureMacros.newTail(new Q050Constructor(1, 2, 3))
  val newInit = Q050ScalametaQqSequenceCaptureMacros.newInit(new Q050Constructor(1, 2, 3))
  val newEmptyMiddle =
    Q050ScalametaQqSequenceCaptureMacros.newMiddle(new Q050Constructor(1, 9))
  val newOneMiddle =
    Q050ScalametaQqSequenceCaptureMacros.newMiddle(new Q050Constructor(1, 2, 9))
  val newManyMiddle =
    Q050ScalametaQqSequenceCaptureMacros.newMiddle(new Q050Constructor(1, 2, 3, 9))
  val newInsufficient =
    Q050ScalametaQqSequenceCaptureMacros.newMiddle(new Q050Constructor(1))
  val newPrefixMismatch =
    Q050ScalametaQqSequenceCaptureMacros.newFixedEndsMatch(new Q050Constructor(2, 3, 9))
  val newSuffixMismatch =
    Q050ScalametaQqSequenceCaptureMacros.newFixedEndsMatch(new Q050Constructor(1, 3, 8))

  val scalarDirect = Q050ScalametaQqSequenceCaptureMacros.scalarDirect(20, 22)
  val scalarUmbrella = Q050ScalametaQqSequenceCaptureMacros.scalarUmbrella(20, 22)
  val dynamicScalar = Q050ScalametaQqSequenceCaptureMacros.dynamicScalarFallback(20, 22)
  val identityAndEngine = Q050ScalametaQqSequenceCaptureMacros.rankedIdentityAndEngine

class Q050ScalametaQqSequenceCaptureTest extends munit.FunSuite:
  private inline def messages(inline source: String): List[String] =
    typeCheckErrors(source).map(_.message)

  private def stagedAbortMessage(operation: scala.quoted.Quotes ?=> Unit): String =
    given Compiler = Compiler.make(getClass.getClassLoader)
    withQuotes:
      try
        operation
        "<no-abort>"
      catch
        case error: Throwable =>
          Option(error.getMessage).getOrElse(error.getClass.getName)

  test("Scalameta qq binds Apply zero one and many arguments as an exact Term sequence"):
    assertEquals(Q050Evidence.applyEmpty, Nil)
    assertEquals(Q050Evidence.applyOne, List(1))
    assertEquals(Q050Evidence.applyMany, List(1, 2, 3))

  test("Scalameta qq preserves Apply prefix suffix middle and atomic mismatch behavior"):
    assertEquals(Q050Evidence.applyTail, (1, List(2, 3)))
    assertEquals(Q050Evidence.applyInit, (List(1, 2), 3))
    assertEquals(Q050Evidence.applyEmptyMiddle, Some((1, Nil, 9)))
    assertEquals(Q050Evidence.applyOneMiddle, Some((1, List(2), 9)))
    assertEquals(Q050Evidence.applyManyMiddle, Some((1, List(2, 3), 9)))
    assertEquals(Q050Evidence.applyInsufficient, None)
    assert(!Q050Evidence.applyPrefixMismatch)
    assert(!Q050Evidence.applySuffixMismatch)

  test("Scalameta qq binds fixed one-list New zero one and many arguments"):
    assertEquals(Q050Evidence.newEmpty, Nil)
    assertEquals(Q050Evidence.newOne, List(1))
    assertEquals(Q050Evidence.newMany, List(1, 2, 3))

  test("Scalameta qq preserves New prefix suffix middle and atomic mismatch behavior"):
    assertEquals(Q050Evidence.newTail, (1, List(2, 3)))
    assertEquals(Q050Evidence.newInit, (List(1, 2), 3))
    assertEquals(Q050Evidence.newEmptyMiddle, Some((1, Nil, 9)))
    assertEquals(Q050Evidence.newOneMiddle, Some((1, List(2), 9)))
    assertEquals(Q050Evidence.newManyMiddle, Some((1, List(2, 3), 9)))
    assertEquals(Q050Evidence.newInsufficient, None)
    assert(!Q050Evidence.newPrefixMismatch)
    assert(!Q050Evidence.newSuffixMismatch)

  test("ranked captures preserve original reflected identity order and Scalameta engine truth"):
    assertEquals(Q050Evidence.identityAndEngine, (true, true, "Scalameta"))

  test("scalar direct and umbrella imports keep exact Term binder behavior"):
    assertEquals(Q050Evidence.scalarDirect, (20, 22))
    assertEquals(Q050Evidence.scalarUmbrella, (20, 22))
    assertEquals(Q050Evidence.dynamicScalar, (20, 22))

  test("dots inside a guest string stay scalar lexical content"):
    val errors = messages(
      """import scala.quoted.*
        import quasiquotes.scalameta.ScalametaQuasiPattern.qq
        def attempt(using q: Quotes)(term: q.reflect.Term) = term match
          case qq"s\"..$insideString\"" => val _: q.reflect.Term = insideString
          case _ => ()
      """
    )
    assertEquals(errors, Nil)

  test("ranked and scalar binder types are exact and reject reverse assignments"):
    val directPositive = messages(
      """import scala.quoted.*
        import quasiquotes.scalameta.ScalametaQuasiPattern.qq
        def attempt(using q: Quotes)(term: q.reflect.Term) = term match
          case qq"$function(..$arguments)" =>
            val _: q.reflect.Term = function
            val _: Seq[q.reflect.Term] = arguments
          case _ => ()
      """
    )
    val umbrellaPositive = messages(
      """import scala.quoted.*
        import quasiquotes.scalameta.Quasiquotes.qq
        def attempt(using q: Quotes)(term: q.reflect.Term) = term match
          case qq"$function($head, ..$middle, $tail)" =>
            val _: q.reflect.Term = function
            val _: q.reflect.Term = head
            val _: Seq[q.reflect.Term] = middle
            val _: q.reflect.Term = tail
          case _ => ()
      """
    )
    val sequenceAsScalar = messages(
      """import scala.quoted.*
        import quasiquotes.scalameta.ScalametaQuasiPattern.qq
        def attempt(using q: Quotes)(term: q.reflect.Term) = term match
          case qq"$function(..$arguments)" => val _: q.reflect.Term = arguments
          case _ => ()
      """
    )
    val scalarAsSequence = messages(
      """import scala.quoted.*
        import quasiquotes.scalameta.ScalametaQuasiPattern.qq
        def attempt(using q: Quotes)(term: q.reflect.Term) = term match
          case qq"$left + $right" => val _: Seq[q.reflect.Term] = left
          case _ => ()
      """
    )
    assertEquals(directPositive, Nil)
    assertEquals(umbrellaPositive, Nil)
    assert(sequenceAsScalar.exists(_.contains("q.reflect.Term")), sequenceAsScalar.mkString(" | "))
    assert(scalarAsSequence.exists(_.contains("Seq")), scalarAsSequence.mkString(" | "))

  test("rank diagnostics reject unsupported multiplicity rank spelling positions and names"):
    val cases = List(
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.qq
          def attempt(using q: Quotes)(term: q.reflect.Term) = term match
            case qq"$function(..$left, ..$right)" => ()
            case _ => ()"""
      ) -> "only one rank-2 sequence-Term capture",
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.qq
          def attempt(using q: Quotes)(term: q.reflect.Term) = term match
            case qq"$function(...$arguments)" => ()
            case _ => ()"""
      ) -> "unsupported rank-3",
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.qq
          def attempt(using q: Quotes)(term: q.reflect.Term) = term match
            case qq"$function(. .$arguments)" => ()
            case _ => ()"""
      ) -> "orphan or malformed",
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.qq
          def attempt(using q: Quotes)(term: q.reflect.Term) = term match
            case qq"..$arguments" => ()
            case _ => ()"""
      ) -> "direct ordinary Apply or fixed one-list New argument",
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.qq
          def attempt(using q: Quotes)(term: q.reflect.Term) = term match
            case qq"(..$arguments, 1)" => ()
            case _ => ()"""
      ) -> "direct ordinary Apply or fixed one-list New argument",
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.qq
          def attempt(using q: Quotes)(term: q.reflect.Term) = term match
            case qq"{ ..$arguments; 1 }" => ()
            case _ => ()"""
      ) -> "direct ordinary Apply or fixed one-list New argument",
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.qq
          def attempt(using q: Quotes)(term: q.reflect.Term) = term match
            case qq"(x: Int) => ..$arguments" => ()
            case _ => ()"""
      ) -> "direct ordinary Apply or fixed one-list New argument",
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.qq
          def attempt(using q: Quotes)(term: q.reflect.Term) = term match
            case qq"(..$arguments: Int)" => ()
            case _ => ()"""
      ) -> "direct ordinary Apply or fixed one-list New argument",
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.qq
          def attempt(using q: Quotes)(term: q.reflect.Term) = term match
            case qq"$function(..$same, $same)" => ()
            case _ => ()"""
      ) -> "duplicate pattern variable: same",
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.qq
          def attempt(using q: Quotes)(term: q.reflect.Term) = term match
            case qq"new $constructor(..$arguments)" => ()
            case _ => ()"""
      ) -> "Invalid Scalameta qq term-pattern template",
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.qq
          def attempt(using q: Quotes)(term: q.reflect.Term) = term match
            case qq"new quasiquotes.q050.Q050Constructor(..$arguments)(1)" => ()
            case _ => ()"""
      ) -> "Invalid Scalameta qq term-pattern template",
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.qq
          def attempt(using q: Quotes)(term: q.reflect.Term) = term match
            case qq"new quasiquotes.q050.Q050Constructor(first = ..$arguments)" => ()
            case _ => ()"""
      ) -> "Invalid Scalameta qq term-pattern template"
    )
    cases.foreach { case (errors, expected) =>
      assert(errors.exists(_.contains(expected)), s"$expected: ${errors.mkString(" | ")}")
    }

  test("Type rank admission does not widen Definition rank"):
    val typeRank = stagedAbortMessage:
      val q = summon[scala.quoted.Quotes]
      quasiquotes.scalameta.ScalametaQuasiPattern.tqq(
        StringContext("List[..", "]")
      )(using q)
    val definitionRank = messages(
      """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.dqq
        def attempt(using q: Quotes)(definition: q.reflect.DefDef) = definition match
          case dqq"def f(x: Int): Int = ..$body" => ()
          case _ => ()"""
    )
    assertEquals(typeRank, "<no-abort>")
    assert(definitionRank.nonEmpty)
