package quasiquotes.construct

import scala.quoted.*

object QuasiquoteMacroExamples:
  private object demo:
    def bar(x: Int): Int = x + 1

  inline def emitIntLiteral: Int = ${ emitIntLiteralImpl }

  inline def emitStringLiteral: String = ${ emitStringLiteralImpl }

  inline def callSelectedMethodViaHole(x: Int): Int = ${ callSelectedMethodViaHoleImpl('x) }

  inline def callFunctionHole(x: Int): Int = ${ callFunctionHoleImpl('x) }

  inline def stringLength(value: String): Int = ${ stringLengthImpl('value) }

  inline def unsupportedSyntaxMessage: String = ${ unsupportedSyntaxMessageImpl }

  private def emitIntLiteralImpl(using Quotes): Expr[Int] =
    import quotes.reflect.*
    import quasiquotes.construct.Quasiquotes.*
    qr"1".asExprOf[Int]

  private def emitStringLiteralImpl(using Quotes): Expr[String] =
    import quotes.reflect.*
    import quasiquotes.construct.Quasiquotes.*
    qr""""abc"""".asExprOf[String]

  private def callSelectedMethodViaHoleImpl(x: Expr[Int])(using Quotes): Expr[Int] =
    import quotes.reflect.*
    import quasiquotes.construct.Quasiquotes.*
    val demoTerm = '{ demo }.asTerm
    val xTerm = x.asTerm
    qr"$demoTerm.bar($xTerm)".asExprOf[Int]

  private def callFunctionHoleImpl(x: Expr[Int])(using Quotes): Expr[Int] =
    import quotes.reflect.*
    import quasiquotes.construct.Quasiquotes.*
    val functionTerm = Select.unique('{ (n: Int) => n + 1 }.asTerm, "apply")
    val xTerm = x.asTerm
    qr"$functionTerm($xTerm)".asExprOf[Int]

  private def stringLengthImpl(value: Expr[String])(using Quotes): Expr[Int] =
    import quotes.reflect.*
    import quasiquotes.construct.Quasiquotes.*
    val valueTerm = value.asTerm
    qr"$valueTerm.length".asExprOf[Int]

  private def unsupportedSyntaxMessageImpl(using Quotes): Expr[String] =
    import quotes.reflect.*
    QuasiquoteBuilder.build(Seq("1 + 2"), Nil) match
      case Left(error) => Expr(error.message)
      case Right(term) => Expr(term.show)
