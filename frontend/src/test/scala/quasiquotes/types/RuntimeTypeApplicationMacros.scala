package quasiquotes.types

import scala.quoted.*

final class RuntimeTypeApplicationOuter:
  final class Inner[A]
  object Module:
    final class Inner[A]

final class RuntimeTypeApplicationBound[A <: java.lang.Number]

object RuntimeTypeApplicationFixture:
  val outer = new RuntimeTypeApplicationOuter

object RuntimeTypeApplicationMacros:
  inline def rejected0: Unit = ${ rejectedImpl(0) }
  inline def rejected1: Unit = ${ rejectedImpl(1) }
  inline def rejected2: Unit = ${ rejectedImpl(2) }
  inline def rejected3: Unit = ${ rejectedImpl(3) }
  inline def rejected4: Unit = ${ rejectedImpl(4) }
  inline def rejected5: Unit = ${ rejectedImpl(5) }
  inline def rejected6: Unit = ${ rejectedImpl(6) }
  inline def rejected7: Unit = ${ rejectedImpl(7) }
  inline def rejected8: Unit = ${ rejectedImpl(8) }
  inline def rejected9: Unit = ${ rejectedImpl(9) }
  inline def rejected10: Unit = ${ rejectedImpl(10) }
  inline def rejected11: Unit = ${ rejectedImpl(11) }
  inline def rejected12: Unit = ${ rejectedImpl(12) }
  inline def rejected13: Unit = ${ rejectedImpl(13) }
  inline def rejected14: Unit = ${ rejectedImpl(14) }
  inline def rejected15: Unit = ${ rejectedImpl(15) }
  inline def rejected16: Unit = ${ rejectedImpl(16) }
  inline def rejected17: Unit = ${ rejectedImpl(17) }
  inline def rejected18: Unit = ${ rejectedImpl(18) }
  inline def rejected19: Unit = ${ rejectedImpl(19) }
  inline def rejected20: Unit = ${ rejectedImpl(20) }
  inline def rejected21: Unit = ${ rejectedImpl(21) }
  inline def rejected22: Unit = ${ rejectedImpl(22) }
  inline def rejected23: Unit = ${ rejectedImpl(23) }
  inline def rejected24: Unit = ${ rejectedImpl(24) }
  inline def rejected25: Unit = ${ rejectedImpl(25) }
  inline def rejected26: Unit = ${ rejectedImpl(26) }
  inline def rejected27: Unit = ${ rejectedImpl(27) }
  inline def rejected28: Unit = ${ rejectedImpl(28) }

  inline def rejected29: Unit = ${ rejectedImpl(29) }
  inline def rejected30: Unit = ${ rejectedImpl(30) }
  inline def rejected31: Unit = ${ rejectedImpl(31) }
  inline def rejected32: Unit = ${ rejectedImpl(32) }
  inline def rejected33: Unit = ${ rejectedImpl(33) }

  inline def rejected34: Unit = ${ rejectedImpl(34) }
  inline def rejected35: Unit = ${ rejectedImpl(35) }

  private def rejectedImpl(scenario: Int)(using q: Quotes): Expr[Unit] =
    import q.reflect.*
    import QuasiTypequotes.*
    val list = TypeRepr.of[List[Int]] match
      case AppliedType(c, _) => c
    val either = TypeRepr.of[Either[Int, String]] match
      case AppliedType(c, _) => c
    val box = TypeRepr.of[Q005HigherKindedBox[List]] match
      case AppliedType(c, _) => c
    val int = TypeRepr.of[Int]
    val args: Seq[TypeRepr] = Seq(int)
    scenario match
      case 0 => tqr"$list[${Seq.empty[TypeRepr]}]"
      case 1 => tqr"$list[..${Seq.empty[TypeRepr]}]"
      case 2 => tqr"$either[..$args]"
      case 3 => tqr"$list[..${Seq(int, int)}]"
      case 4 => tqr"$int[..$args]"
      case 5 => tqr"$list[..${Seq(list)}]"
      case 6 => tqr"$box[..$args]"
      case 7 => tqr"$list[..$args, ..$args]"
      case 8 => tqr"$list[...$args]"
      case 9 => tqr"$list[. .$args]"
      case 10 => tqr"$list[..$int]"
      case 11 => tqr"List[$list[..$args]]"
      case 12 => tqr"$list[Int, ..$args]"
      case 13 => tqr"${null.asInstanceOf[TypeRepr]}[..$args]"
      case 14 => tqr"$list[..${null: Seq[TypeRepr]}]"
      case 15 => tqr"$list[..${Seq(null.asInstanceOf[TypeRepr])}]"
      case 16 => StringContext("", "[/* .. */", "]").tqr(list, args)
      case 17 => StringContext("", "[`..`", "]").tqr(list, args)
      case 18 => StringContext("", "[\"..\"", "]").tqr(list, args)
      case 19 => StringContext("", "[s\"${ .. }\"", "]").tqr(list, args)
      case 20 => StringContext("", "[// ..\n", "]").tqr(list, args)
      case 21 => StringContext("", "[/* outer /* .. */ .. */", "]").tqr(list, args)
      case 22 => StringContext("", "['.'", "]").tqr(list, args)
      case 23 => StringContext("", "[.. ", "]").tqr(list, args)
      case 24 => StringContext("..", "[..", "]").tqr(list, args)
      case 25 => StringContext("", null, "]").tqr(list, args)
      case 26 => StringContext("", "[.", "]").tqr(list, args)
      case 27 => StringContext("", "[.", ".]").tqr(list, args)
      case 28 =>
        val inner = TypeRepr.of[RuntimeTypeApplicationFixture.outer.Inner[Int]] match
          case AppliedType(c, _) => c
        tqr"$inner[..$args]"
      case 29 => tqr"$list[..${Seq(TypeRepr.of[RuntimeTypeApplicationFixture.outer.Inner[Int]])}]"
      case 30 =>
        val lambda = TypeRepr.of[[A] =>> Either[A, A]]
        tqr"$lambda[..$args]"
      case 31 => tqr"$box[..${Seq(TypeRepr.of[[A] =>> Either[A, A]])}]"
      case 32 => tqr"$list[..${Seq(TypeRepr.of[AnyRef { type Value = Int }])}]"
      case 33 =>
        val bounded = TypeRepr.of[RuntimeTypeApplicationBound[java.lang.Integer]] match
          case AppliedType(c, _) => c
        tqr"$bounded[..${Seq(TypeRepr.of[java.lang.Integer])}]"
      case 34 => StringContext("", "[. ", "]").tqr(list, args)
      case 35 =>
        val inner = TypeRepr.of[RuntimeTypeApplicationFixture.outer.Module.Inner[Int]] match
          case AppliedType(c, _) => c
        tqr"$inner[..$args]"
      case _ => report.errorAndAbort("Unknown runtime Type application test scenario")
    '{ () }
