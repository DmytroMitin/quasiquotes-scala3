package quasiquotes.construct

import scala.quoted.*

private[quasiquotes] object ConstructorNewExamples:
  inline def emptyBuilderLength: Int =
    ${ emptyBuilderLengthImpl }

  inline def literalBuilderCapacity: Int =
    ${ literalBuilderCapacityImpl }

  inline def stringBuilderCapacity(capacity: Int): Int =
    ${ stringBuilderCapacityImpl('capacity) }

  inline def exceptionMessage(message: String): String =
    ${ exceptionMessageImpl('message) }

  inline def treeStructure(capacity: Int): String =
    ${ treeStructureImpl('capacity) }

  inline def failureMessage(inline source: String): String =
    ${ failureMessageImpl('source) }

  private def emptyBuilderLengthImpl(using Quotes): Expr[Int] =
    import quotes.reflect.*
    import Quasiquotes.*
    val result = qr"new java.lang.StringBuilder()"
    '{ ${ result.asExprOf[java.lang.StringBuilder] }.length() }

  private def literalBuilderCapacityImpl(using Quotes): Expr[Int] =
    import quotes.reflect.*
    import Quasiquotes.*
    val result = qr"new java.lang.StringBuilder(24)"
    '{ ${ result.asExprOf[java.lang.StringBuilder] }.capacity() }

  private def stringBuilderCapacityImpl(capacity: Expr[Int])(using Quotes): Expr[Int] =
    import quotes.reflect.*
    import Quasiquotes.*
    val result = qr"new java.lang.StringBuilder(${capacity.asTerm})"
    '{ ${ result.asExprOf[java.lang.StringBuilder] }.capacity() }

  private def exceptionMessageImpl(message: Expr[String])(using Quotes): Expr[String] =
    import quotes.reflect.*
    import Quasiquotes.*
    val result = qr"new java.lang.RuntimeException(${message.asTerm})"
    '{ ${ result.asExprOf[java.lang.RuntimeException] }.getMessage() }

  private def treeStructureImpl(capacity: Expr[Int])(using Quotes): Expr[String] =
    import quotes.reflect.*
    import Quasiquotes.*
    val result = qr"new java.lang.StringBuilder(${capacity.asTerm})"
    Expr(result.show(using Printer.TreeStructure))

  private def failureMessageImpl(source: Expr[String])(using Quotes): Expr[String] =
    val text = source.valueOrAbort
    QuasiquoteBuilder.build(Seq(text), Nil) match
      case Left(error) => Expr(error.message)
      case Right(_) => Expr("unexpected success")
