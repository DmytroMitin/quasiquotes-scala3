package quasiquotes

import scala.quoted.*

object FacadeImportProbeMacros:
  inline def umbrellaWorks: Boolean = ${ umbrellaWorksImpl }

  inline def selectiveAndLegacyWork: Boolean = ${ selectiveAndLegacyWorkImpl }

  inline def plainExportsWork: Boolean = ${ plainExportsWorkImpl }

  private def umbrellaWorksImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import FacadeProbe.*

    val term = qr"1 + 2"
    val termMatched = term match
      case qq"$left + $right" => left.show == "1" && right.show == "2"
      case _ => false

    val argument = TypeRepr.of[Int]
    val applied = tqr"List[$argument]"
    val typeMatched = applied match
      case tqq"List[$captured]" => captured =:= argument
      case _ => false

    val definition = dqr"def identity(x: $argument): $argument = x"
    val definitionMatched = definition match
      case dqq"def identity(x: Int): Int = $body" => body.show == "x"
      case _ => false

    Expr(termMatched && typeMatched && definitionMatched)

  private def selectiveAndLegacyWorkImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import domains.Terms.*
    import domains.Types.tqr
    import quasiquotes.construct.Quasiquotes.{qr as legacyQr}

    val facadeTerm = qr"3 + 4"
    val legacyTerm = legacyQr"5 + 6"
    val facadeType = tqr"Option[Int]"

    Expr(
      facadeTerm.show == "3.+(4)" &&
        legacyTerm.show == "5.+(6)" &&
        facadeType =:= TypeRepr.of[Option[Int]]
    )

  private def plainExportsWorkImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import PlainExportFacadeProbe.*

    val term = qr"7 + 8"
    val termMatched = term match
      case qq"$left + $right" => left.show == "7" && right.show == "8"
      case _ => false
    val intType = TypeRepr.of[Int]
    val typ = tqr"List[$intType]"
    val typeMatched = typ match
      case tqq"List[$captured]" => captured =:= intType
      case _ => false
    val definition = dqr"def exported(x: $intType): $intType = x"
    val definitionMatched = definition match
      case dqq"def exported(x: Int): Int = $body" => body.show == "x"
      case _ => false

    Expr(termMatched && typeMatched && definitionMatched)
