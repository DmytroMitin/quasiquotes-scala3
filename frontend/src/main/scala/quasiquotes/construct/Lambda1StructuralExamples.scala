package quasiquotes.construct

import scala.quoted.*

private[quasiquotes] object Lambda1StructuralExamples:
  inline def identity: Int => Any = ${ identityImpl }
  inline def addOne: Int => Any = ${ addOneImpl }
  inline def stringIdentity: String => Any = ${ stringIdentityImpl }
  inline def call: Int => Any = ${ callImpl }
  inline def preserveOuter(inline external: Int): Int => Any = ${ preserveOuterImpl('external) }
  inline def unsafeSpliceMessage(inline external: Int): String =
    ${ unsafeSpliceMessageImpl('external) }

  private def identityImpl(using Quotes): Expr[Int => Any] =
    import Quasiquotes.*
    qr"(x: Int) => x".asExprOf[Int => Any]

  private def addOneImpl(using Quotes): Expr[Int => Any] =
    import Quasiquotes.*
    qr"(x: Int) => x + 1".asExprOf[Int => Any]

  private def stringIdentityImpl(using Quotes): Expr[String => Any] =
    import Quasiquotes.*
    qr"(value: String) => value".asExprOf[String => Any]

  private def callImpl(using Quotes): Expr[Int => Any] =
    import Quasiquotes.*
    qr"(x: Int) => double(x)".asExprOf[Int => Any]

  private def preserveOuterImpl(external: Expr[Int])(using Quotes): Expr[Int => Any] =
    import quotes.reflect.*
    import Quasiquotes.*
    val externalTerm = external.asTerm
    qr"(x: Int) => x + $externalTerm".asExprOf[Int => Any]

  private def unsafeSpliceMessageImpl(external: Expr[Int])(using Quotes): Expr[String] =
    import quotes.reflect.*
    QuasiquoteBuilder.build(
      Seq("(x: Int) => ", ""),
      Seq(external.asTerm)
    ) match
      case Left(error) => Expr(error.message)
      case Right(_) => Expr("accepted")
