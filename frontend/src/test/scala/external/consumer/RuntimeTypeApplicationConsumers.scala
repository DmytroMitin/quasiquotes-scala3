package external.consumer

import scala.quoted.*

object RuntimeTypeApplicationConsumers:
  inline def direct: Boolean = ${ directImpl }
  inline def umbrella: Boolean = ${ umbrellaImpl }

  private def directImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import quasiquotes.types.QuasiTypequotes.tqr
    val constructor = TypeRepr.of[List[Int]] match
      case AppliedType(c, _) => c
    val arguments: Seq[TypeRepr] = Vector(TypeRepr.of[Int])
    val result: q.reflect.TypeRepr = tqr"$constructor[..$arguments]"
    Expr(result =:= TypeRepr.of[List[Int]])

  private def umbrellaImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import quasiquotes.Quasiquotes.tqr
    val constructor = TypeRepr.of[Either[Int, String]] match
      case AppliedType(c, _) => c
    val arguments: Seq[TypeRepr] = Vector(TypeRepr.of[Int], TypeRepr.of[String])
    val result: q.reflect.TypeRepr = tqr"$constructor[..$arguments]"
    Expr(result =:= TypeRepr.of[Either[Int, String]])
