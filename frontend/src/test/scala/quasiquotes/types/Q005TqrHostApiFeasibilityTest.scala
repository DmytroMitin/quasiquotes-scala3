package quasiquotes.types

import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

/** Test-only candidate proving that a direct `Seq[TypeRepr]` overload can be
  * additive beside the current scalar varargs overload.
  */
private object Q005DirectSeqTqrCandidate:
  extension (sc: StringContext)
    def tqr(using q: Quotes)(arguments: q.reflect.TypeRepr*): q.reflect.TypeRepr =
      QuasiTypequotes.tqr(sc)(using q)(arguments*)

    def tqr(using q: Quotes)(
        constructor: q.reflect.TypeRepr,
        arguments: Seq[q.reflect.TypeRepr],
        additionalArgumentSequences: Seq[q.reflect.TypeRepr]*
    ): q.reflect.TypeRepr =
      import q.reflect.*

      if additionalArgumentSequences.nonEmpty then
        report.errorAndAbort(
          "Q005 candidate multiple sequence Type splices: exactly one is supported."
        )
      if sc.parts != Seq("", "[..", "]") then
        report.errorAndAbort(
          "Q005 candidate rank mismatch: the direct Type sequence overload requires exactly `tqr\"$constructor[..$arguments]\"`."
        )
      if constructor == null then
        report.errorAndAbort("Q005 candidate invalid constructor: TypeRepr must not be null.")
      if arguments == null then
        report.errorAndAbort("Q005 candidate invalid sequence: Seq[TypeRepr] must not be null.")
      if arguments.exists(_ == null) then
        report.errorAndAbort("Q005 candidate invalid sequence: TypeRepr elements must not be null.")

      val expectedArity =
        val symbol = constructor.typeSymbol
        if symbol == Symbol.noSymbol then None
        else
          val typeParameters =
            symbol.primaryConstructor.paramSymss.flatten.filter(_.isTypeParam)
          Option.when(typeParameters.nonEmpty)(typeParameters.size)

      expectedArity match
        case None =>
          report.errorAndAbort(
            s"Q005 candidate invalid constructor: `${constructor.show}` is not an unapplied class type constructor."
          )
        case Some(expected) if expected != arguments.size =>
          report.errorAndAbort(
            s"Q005 candidate wrong argument count: constructor expects $expected Type argument(s), received ${arguments.size}."
          )
        case Some(_) =>
          AppliedType(constructor, arguments.toList)

private object Q005TypeRankShape:
  def classify(parts: Seq[String], argumentKinds: Seq[String]): Either[String, Unit] =
    if parts == Seq("", "[..", "]") && argumentKinds == Seq("scalar", "sequence") then Right(())
    else if argumentKinds.count(_ == "sequence") > 1 then Left("MULTIPLE_SEQUENCE_TYPE_SPLICES")
    else if parts.exists(_.endsWith("...")) then Left("UNSUPPORTED_TYPE_SPLICE_RANK")
    else if parts.exists(part => part.contains(". .") || part.endsWith("..")) then
      Left("SEQUENCE_CARRIER_RANK_MISMATCH")
    else if argumentKinds.contains("sequence") then Left("UNSUPPORTED_SEQUENCE_TYPE_POSITION")
    else Left("SEQUENCE_CARRIER_RANK_MISMATCH")

class Q005TqrHostApiFeasibilityTest extends munit.FunSuite:
  test("a direct Seq overload is additive and source-unambiguous beside scalar tqr"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val evidence = withQuotes:
      def run(using q: Quotes): (Boolean, Boolean, Boolean, Boolean) =
        import q.reflect.*
        import Q005DirectSeqTqrCandidate.*

        def constructorOf(applied: TypeRepr): TypeRepr =
          applied match
            case AppliedType(constructor, _) => constructor
            case other => report.errorAndAbort(s"Q005 expected an AppliedType, obtained ${other.show}")

        val intType = TypeRepr.of[Int]
        val stringType = TypeRepr.of[String]
        val scalarUnary = tqr"List[$intType]"
        val scalarBinary = tqr"Either[$intType, $stringType]"

        val eitherConstructor = constructorOf(TypeRepr.of[Either[Any, Any]])
        val dynamicArguments: Seq[TypeRepr] = List(intType, stringType)
        val dynamic = tqr"$eitherConstructor[..$dynamicArguments]"
        val returnedArguments = dynamic match
          case AppliedType(_, arguments) => arguments
          case other => report.errorAndAbort(s"Q005 expected a dynamic AppliedType, obtained ${other.show}")

        (
          scalarUnary =:= TypeRepr.of[List[Int]],
          scalarBinary =:= TypeRepr.of[Either[Int, String]],
          dynamic =:= TypeRepr.of[Either[Int, String]],
          returnedArguments.zip(dynamicArguments).forall { case (returned, original) =>
            returned.asInstanceOf[AnyRef] eq original.asInstanceOf[AnyRef]
          }
        )

      run(using summon[Quotes])

    assertEquals(evidence, (true, true, true, true))

  test("the first slice has an exact fail-closed source and host-rank shape"):
    assertEquals(
      Q005TypeRankShape.classify(Seq("", "[..", "]"), Seq("scalar", "sequence")),
      Right(())
    )
    assertEquals(
      Q005TypeRankShape.classify(Seq("", "[", "]"), Seq("scalar", "sequence")),
      Left("UNSUPPORTED_SEQUENCE_TYPE_POSITION")
    )
    assertEquals(
      Q005TypeRankShape.classify(Seq("", "[..", ", ..", "]"), Seq("scalar", "sequence", "sequence")),
      Left("MULTIPLE_SEQUENCE_TYPE_SPLICES")
    )
    assertEquals(
      Q005TypeRankShape.classify(Seq("", "[...", "]"), Seq("scalar", "sequence")),
      Left("UNSUPPORTED_TYPE_SPLICE_RANK")
    )
    assertEquals(
      Q005TypeRankShape.classify(Seq("", "[. .", "]"), Seq("scalar", "sequence")),
      Left("SEQUENCE_CARRIER_RANK_MISMATCH")
    )
    assertEquals(
      Q005TypeRankShape.classify(Seq("", "[..", "]"), Seq("scalar", "scalar")),
      Left("SEQUENCE_CARRIER_RANK_MISMATCH")
    )
    assertEquals(
      Q005TypeRankShape.classify(Seq("`..`", ""), Seq("sequence")),
      Left("UNSUPPORTED_SEQUENCE_TYPE_POSITION")
    )
    assertEquals(
      Q005TypeRankShape.classify(Seq("/* .. */", ""), Seq("sequence")),
      Left("UNSUPPORTED_SEQUENCE_TYPE_POSITION")
    )
