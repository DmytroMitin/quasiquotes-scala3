package external.consumer

import scala.quoted.*

object Q047RuntimeTypeApplicationNegativeMacros:
  inline def rejected0: Unit = ${ rejectedImpl(0) }
  inline def rejected1: Unit = ${ rejectedImpl(1) }
  inline def rejected2: Unit = ${ rejectedImpl(2) }
  inline def rejected3: Unit = ${ rejectedImpl(3) }
  inline def rejected4: Unit = ${ rejectedImpl(4) }
  inline def rejected5: Unit = ${ rejectedImpl(5) }
  inline def rejected6: Unit = ${ rejectedImpl(6) }
  inline def rejected7: Unit = ${ rejectedImpl(7) }

  private def rejectedImpl(scenario: Int)(using q: Quotes): Expr[Unit] =
    import q.reflect.*
    import quasiquotes.scalameta.ScalametaQuasiquotes.tqr

    val list = TypeRepr.of[List[Int]] match
      case AppliedType(constructor, _) => constructor
    val int = TypeRepr.of[Int]
    val arguments: Seq[TypeRepr] = Seq(int)

    scenario match
      case 0 => tqr"$list[${Seq.empty[TypeRepr]}]"
      case 1 => tqr"List[$list[..$arguments]]"
      case 2 => tqr"$list[..$arguments, ..$arguments]"
      case 3 => tqr"$int[..$arguments]"
      case 4 => tqr"$list[..${Seq.empty[TypeRepr]}]"
      case 5 => tqr"$list[..${Seq(list)}]"
      case 6 => tqr"$list[..${null: Seq[TypeRepr]}]"
      case 7 => tqr"$list[..${Seq(null.asInstanceOf[TypeRepr])}]"
      case _ => report.errorAndAbort("Unknown Q047 runtime Type application test scenario")
    '{ () }
