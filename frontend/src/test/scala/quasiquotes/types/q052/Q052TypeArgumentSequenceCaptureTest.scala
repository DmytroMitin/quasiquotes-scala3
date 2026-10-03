package quasiquotes.types.q052

import scala.compiletime.testing.typeCheckErrors

class Q052TypeArgumentSequenceCaptureTest extends munit.FunSuite:
  private inline def messages(inline source: String): List[String] =
    typeCheckErrors(source).map(_.message)

  test("fixed constructors expose zero one and many original Type arguments"):
    assertEquals(
      Q052TypeArgumentSequenceCaptureMacros.allArguments,
      (List("STypeIdent(Int)", "STypeIdent(String)"), true)
    )
    assertEquals(
      Q052TypeArgumentSequenceCaptureMacros.tailArgument,
      ("STypeIdent(Int)", List("STypeIdent(String)"), true)
    )
    assertEquals(
      Q052TypeArgumentSequenceCaptureMacros.initArgument,
      (List("STypeIdent(Int)"), "STypeIdent(String)", true)
    )
    assertEquals(
      Q052TypeArgumentSequenceCaptureMacros.emptyMiddle,
      ("STypeIdent(Int)", Nil, "STypeIdent(String)", true)
    )

  test("fixed sides fail atomically and scalar tqq remains exact"):
    assert(!Q052TypeArgumentSequenceCaptureMacros.prefixMismatch)
    assert(!Q052TypeArgumentSequenceCaptureMacros.suffixMismatch)
    assert(Q052TypeArgumentSequenceCaptureMacros.unaryFixedConstructors)
    assert(Q052TypeArgumentSequenceCaptureMacros.targetBoundaryFailures)
    assert(Q052TypeArgumentSequenceCaptureMacros.scalarCompatibility)

  test("sequence and nested scalar captures preserve exact original identities"):
    assert(Q052TypeArgumentSequenceCaptureMacros.nestedIdentity)
    assert(Q052TypeArgumentSequenceCaptureMacros.repeatedSemanticIdentity)

  test("production selector exposes exact scalar and sequence static types"):
    val sequenceAsScalar = messages(
      """import scala.quoted.*
        import quasiquotes.types.QuasiTypequotes.*
        def attempt(using q: Quotes)(target: q.reflect.TypeRepr) =
          target match
            case tqq"Either[..$arguments]" =>
              val _: q.reflect.TypeRepr = arguments
            case _ => ()
      """
    )
    val scalarAsSequence = messages(
      """import scala.quoted.*
        import quasiquotes.types.QuasiTypequotes.*
        def attempt(using q: Quotes)(target: q.reflect.TypeRepr) =
          target match
            case tqq"Either[$left, ..$arguments]" =>
              val _: Seq[q.reflect.TypeRepr] = left
              val _: Seq[q.reflect.TypeRepr] = arguments
            case _ => ()
      """
    )
    assert(sequenceAsScalar.exists(_.contains("TypeRepr")))
    assert(scalarAsSequence.exists(_.contains("Seq")))

    val dynamicScalarFallback = messages(
      """import scala.quoted.*
        import quasiquotes.types.{QuasiTypequotes, TypePatternExtractor}
        import quasiquotes.types.QuasiTypequotes.*
        def extractor(using q: Quotes)(context: StringContext): TypePatternExtractor[q.reflect.TypeRepr] =
          context.tqq
      """
    )
    assertEquals(dynamicScalarFallback, Nil)

  test("one semantic name cannot occupy scalar and sequence roles"):
    val errors = messages(
      """import scala.quoted.*
        import quasiquotes.types.QuasiTypequotes.*
        def attempt(using q: Quotes)(target: q.reflect.TypeRepr) =
          target match
            case tqq"Either[$same, ..$same]" => ()
            case _ => ()
      """
    )
    assert(errors.nonEmpty)


  test("unsupported ranked forms fail with deliberate production diagnostics"):
    val multiple = messages(
      """import scala.quoted.*
        import quasiquotes.types.QuasiTypequotes.*
        def attempt(using q: Quotes)(target: q.reflect.TypeRepr) =
          target match
            case tqq"Either[..$left, ..$right]" => ()
            case _ => ()
      """
    )
    val rank3 = messages(
      """import scala.quoted.*
        import quasiquotes.types.QuasiTypequotes.*
        def attempt(using q: Quotes)(target: q.reflect.TypeRepr) =
          target match
            case tqq"Either[...$arguments]" => ()
            case _ => ()
      """
    )
    val orphan = messages(
      """import scala.quoted.*
        import quasiquotes.types.QuasiTypequotes.*
        def attempt(using q: Quotes)(target: q.reflect.TypeRepr) =
          target match
            case tqq"Either[.. $arguments]" => ()
            case _ => ()
      """
    )
    val root = messages(
      """import scala.quoted.*
        import quasiquotes.types.QuasiTypequotes.*
        def attempt(using q: Quotes)(target: q.reflect.TypeRepr) =
          target match
            case tqq"..$arguments" => ()
            case _ => ()
      """
    )
    val tuple = messages(
      """import scala.quoted.*
        import quasiquotes.types.QuasiTypequotes.*
        def attempt(using q: Quotes)(target: q.reflect.TypeRepr) =
          target match
            case tqq"(..$arguments, Int)" => ()
            case _ => ()
      """
    )
    val function = messages(
      """import scala.quoted.*
        import quasiquotes.types.QuasiTypequotes.*
        def attempt(using q: Quotes)(target: q.reflect.TypeRepr) =
          target match
            case tqq"(..$arguments) => Int" => ()
            case _ => ()
      """
    )
    val unsupported = messages(
      """import scala.quoted.*
        import quasiquotes.types.QuasiTypequotes.*
        def attempt(using q: Quotes)(target: q.reflect.TypeRepr) =
          target match
            case tqq"Map[..$arguments]" => ()
            case _ => ()
      """
    )
    val selected = messages(
      """import scala.quoted.*
        import quasiquotes.types.QuasiTypequotes.*
        def attempt(using q: Quotes)(target: q.reflect.TypeRepr) =
          target match
            case tqq"scala.Either[..$arguments]" => ()
            case _ => ()
      """
    )
    val dynamic = messages(
      """import scala.quoted.*
        import quasiquotes.types.QuasiTypequotes.*
        def attempt(using q: Quotes)(target: q.reflect.TypeRepr) =
          target match
            case tqq"$constructor[..$arguments]" => ()
            case _ => ()
      """
    )
    val exceedsArity = messages(
      """import scala.quoted.*
        import quasiquotes.types.QuasiTypequotes.*
        def attempt(using q: Quotes)(target: q.reflect.TypeRepr) =
          target match
            case tqq"Either[Int, ..$middle, String, Boolean]" => ()
            case _ => ()
      """
    )

    assert(multiple.exists(_.contains("only one rank-2")))
    assert(rank3.exists(_.contains("rank-3")))
    assert(orphan.exists(_.contains("orphan or malformed")))
    assert(root.exists(_.contains("direct fixed Type constructor argument list")))
    assert(tuple.exists(_.contains("direct fixed Type constructor argument list")))
    assert(function.exists(_.contains("direct fixed Type constructor argument list")))
    assert(unsupported.exists(_.contains("Unsupported applied Type constructor")))
    assert(selected.exists(_.contains("dynamic or selected Type constructors")))
    assert(dynamic.exists(_.contains("dynamic Type-constructor capture")))
    assert(exceedsArity.exists(_.contains("exceed")))
