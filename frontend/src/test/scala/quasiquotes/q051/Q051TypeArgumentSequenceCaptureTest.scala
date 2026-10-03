package quasiquotes.types.q051

import scala.compiletime.testing.typeCheckErrors

class Q051TypeArgumentSequenceCaptureTest extends munit.FunSuite:
  private inline def messages(inline source: String): List[String] =
    typeCheckErrors(source).map(_.message)

  test("fixed constructors expose zero one and many original Type arguments"):
    assertEquals(
      Q051TypeArgumentSequenceCaptureMacros.allArguments,
      (List("STypeIdent(Int)", "STypeIdent(String)"), true)
    )
    assertEquals(
      Q051TypeArgumentSequenceCaptureMacros.tailArgument,
      ("STypeIdent(Int)", List("STypeIdent(String)"), true)
    )
    assertEquals(
      Q051TypeArgumentSequenceCaptureMacros.initArgument,
      (List("STypeIdent(Int)"), "STypeIdent(String)", true)
    )
    assertEquals(
      Q051TypeArgumentSequenceCaptureMacros.emptyMiddle,
      ("STypeIdent(Int)", Nil, "STypeIdent(String)", true)
    )

  test("fixed sides fail atomically and scalar tqq remains exact"):
    assert(!Q051TypeArgumentSequenceCaptureMacros.prefixMismatch)
    assert(!Q051TypeArgumentSequenceCaptureMacros.suffixMismatch)
    assert(Q051TypeArgumentSequenceCaptureMacros.unaryFixedConstructors)
    assert(Q051TypeArgumentSequenceCaptureMacros.targetBoundaryFailures)
    assert(Q051TypeArgumentSequenceCaptureMacros.scalarCompatibility)

  test("sequence and nested scalar captures preserve exact original identities"):
    assert(Q051TypeArgumentSequenceCaptureMacros.nestedIdentity)
    assert(Q051TypeArgumentSequenceCaptureMacros.repeatedSemanticIdentity)

  test("candidate selector exposes exact scalar and sequence static types"):
    val sequenceAsScalar = messages(
      """import scala.quoted.*
        import quasiquotes.types.q051.Q051TypePatternProbe.*
        def attempt(using q: Quotes)(target: q.reflect.TypeRepr) =
          target match
            case tqq"Either[..$arguments]" =>
              val _: q.reflect.TypeRepr = arguments
            case _ => ()
      """
    )
    val scalarAsSequence = messages(
      """import scala.quoted.*
        import quasiquotes.types.q051.Q051TypePatternProbe.*
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

  test("one semantic name cannot occupy scalar and sequence roles"):
    val errors = messages(
      """import scala.quoted.*
        import quasiquotes.types.q051.Q051TypePatternProbe.*
        def attempt(using q: Quotes)(target: q.reflect.TypeRepr) =
          target match
            case tqq"Either[$same, ..$same]" => ()
            case _ => ()
      """
    )
    assert(errors.nonEmpty)

  test("unsupported ranked forms fail with deliberate diagnostics"):
    assert(Q051TypeArgumentSequenceCaptureMacros.diagnostic("multiple").contains("only one rank-2"))
    assert(Q051TypeArgumentSequenceCaptureMacros.diagnostic("rank3").contains("rank-3"))
    assert(Q051TypeArgumentSequenceCaptureMacros.diagnostic("orphan").contains("orphan or malformed"))
    assert(Q051TypeArgumentSequenceCaptureMacros.diagnostic("root").contains("direct fixed Type constructor argument list"))
    assert(Q051TypeArgumentSequenceCaptureMacros.diagnostic("tuple").contains("direct fixed Type constructor argument list"))
    assert(Q051TypeArgumentSequenceCaptureMacros.diagnostic("function").contains("direct fixed Type constructor argument list"))
    assert(Q051TypeArgumentSequenceCaptureMacros.diagnostic("unsupported").contains("Unsupported applied Type constructor"))
    assert(Q051TypeArgumentSequenceCaptureMacros.diagnostic("selected").contains("dynamic or selected Type constructors"))
    assert(Q051TypeArgumentSequenceCaptureMacros.diagnostic("dynamic").contains("dynamic Type-constructor capture"))
    assert(Q051TypeArgumentSequenceCaptureMacros.diagnostic("exceeds-arity").contains("exceed"))
