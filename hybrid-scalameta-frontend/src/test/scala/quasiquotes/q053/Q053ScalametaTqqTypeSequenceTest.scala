package quasiquotes.q053

import scala.compiletime.testing.typeCheckErrors

class Q053ScalametaTqqTypeSequenceTest extends munit.FunSuite:
  private inline def messages(inline source: String): List[String] =
    typeCheckErrors(source).map(_.message)

  test("hybrid fixed constructors expose exact zero one and many Type captures"):
    assertEquals(
      Q053ScalametaTqqTypeSequenceMacros.allArguments,
      (List("STypeIdent(Int)", "STypeIdent(String)"), true)
    )
    assertEquals(
      Q053ScalametaTqqTypeSequenceMacros.tailArgument,
      ("STypeIdent(Int)", List("STypeIdent(String)"), true)
    )
    assertEquals(
      Q053ScalametaTqqTypeSequenceMacros.initArgument,
      (List("STypeIdent(Int)"), "STypeIdent(String)", true)
    )
    assertEquals(
      Q053ScalametaTqqTypeSequenceMacros.emptyMiddle,
      ("STypeIdent(Int)", Nil, "STypeIdent(String)", true)
    )

  test("hybrid ranked matching preserves boundaries and original identity"):
    assert(Q053ScalametaTqqTypeSequenceMacros.boundaries)
    assert(Q053ScalametaTqqTypeSequenceMacros.nestedIdentity)
    assert(Q053ScalametaTqqTypeSequenceMacros.repeatedSemanticIdentity)

  test("hybrid scalar direct dynamic and umbrella routes remain exact"):
    assert(Q053ScalametaTqqTypeSequenceMacros.scalarDirect)
    assert(Q053ScalametaTqqTypeSequenceMacros.scalarUmbrella)
    assert(Q053ScalametaTqqTypeSequenceMacros.dynamicScalarFallback)

  test("rank-stripped hybrid Type patterns compile through Scalameta"):
    assertEquals(Q053ScalametaTqqTypeSequenceMacros.rankedEngine, ("Scalameta", true))

  test("hybrid selector exposes exact scalar and sequence static types"):
    val sequenceAsScalar = messages(
      """import scala.quoted.*
        import quasiquotes.scalameta.ScalametaQuasiPattern.tqq
        def attempt(using q: Quotes)(target: q.reflect.TypeRepr) = target match
          case tqq"Either[..$arguments]" =>
            val _: q.reflect.TypeRepr = arguments
          case _ => ()
      """
    )
    val scalarAsSequence = messages(
      """import scala.quoted.*
        import quasiquotes.scalameta.ScalametaQuasiPattern.tqq
        def attempt(using q: Quotes)(target: q.reflect.TypeRepr) = target match
          case tqq"Either[$head, ..$tail]" =>
            val _: Seq[q.reflect.TypeRepr] = head
            val _: Seq[q.reflect.TypeRepr] = tail
          case _ => ()
      """
    )
    val rankedPositive = messages(
      """import scala.quoted.*
        import quasiquotes.scalameta.Quasiquotes.tqq
        def attempt(using q: Quotes)(target: q.reflect.TypeRepr) = target match
          case tqq"Either[$head, ..$tail]" =>
            val _: q.reflect.TypeRepr = head
            val _: Seq[q.reflect.TypeRepr] = tail
          case _ => ()
      """
    )
    assert(sequenceAsScalar.exists(_.contains("TypeRepr")))
    assert(scalarAsSequence.exists(_.contains("Seq")))
    assertEquals(rankedPositive, Nil)

  test("unsupported hybrid ranked forms keep deliberate Type diagnostics"):
    val cases = List(
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.tqq
          def attempt(using q: Quotes)(target: q.reflect.TypeRepr) = target match
            case tqq"Either[..$left, ..$right]" => ()
            case _ => ()"""
      ) -> "only one rank-2",
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.tqq
          def attempt(using q: Quotes)(target: q.reflect.TypeRepr) = target match
            case tqq"Either[...$arguments]" => ()
            case _ => ()"""
      ) -> "rank-3",
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.tqq
          def attempt(using q: Quotes)(target: q.reflect.TypeRepr) = target match
            case tqq"Either[.. $arguments]" => ()
            case _ => ()"""
      ) -> "orphan or malformed",
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.tqq
          def attempt(using q: Quotes)(target: q.reflect.TypeRepr) = target match
            case tqq"Either[. .$arguments]" => ()
            case _ => ()"""
      ) -> "orphan or malformed",
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.tqq
          def attempt(using q: Quotes)(target: q.reflect.TypeRepr) = target match
            case tqq"..$arguments" => ()
            case _ => ()"""
      ) -> "direct fixed Type constructor argument list",
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.tqq
          def attempt(using q: Quotes)(target: q.reflect.TypeRepr) = target match
            case tqq"(..$arguments, Int)" => ()
            case _ => ()"""
      ) -> "direct fixed Type constructor argument list",
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.tqq
          def attempt(using q: Quotes)(target: q.reflect.TypeRepr) = target match
            case tqq"(..$arguments) => Int" => ()
            case _ => ()"""
      ) -> "direct fixed Type constructor argument list",
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.tqq
          def attempt(using q: Quotes)(target: q.reflect.TypeRepr) = target match
            case tqq"Map[..$arguments]" => ()
            case _ => ()"""
      ) -> "Unsupported applied Type constructor",
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.tqq
          def attempt(using q: Quotes)(target: q.reflect.TypeRepr) = target match
            case tqq"scala.Either[..$arguments]" => ()
            case _ => ()"""
      ) -> "dynamic or selected Type constructors",
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.tqq
          def attempt(using q: Quotes)(target: q.reflect.TypeRepr) = target match
            case tqq"Either[Int, ..$middle, String, Boolean]" => ()
            case _ => ()"""
      ) -> "exceed"
    )
    cases.foreach { case (errors, expected) =>
      assert(errors.exists(_.contains(expected)), s"$expected: ${errors.mkString(" | ")}")
    }

  test("one hybrid binder name cannot occupy scalar and sequence roles"):
    val errors = messages(
      """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.tqq
        def attempt(using q: Quotes)(target: q.reflect.TypeRepr) = target match
          case tqq"Either[$same, ..$same]" => ()
          case _ => ()"""
    )
    assert(errors.nonEmpty)
