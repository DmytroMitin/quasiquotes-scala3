package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Constants.*
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter

final class ExistingUntpdOrdinaryMethodRhsTermProjectionCharacterizationTest
    extends munit.FunSuite:
  test("parser RHS topology is characterized before semantic projection") {
    withContext {
      val methods = parseMethods(
        """class Characterized:
          |  def identifier(x: Int): Int = x
          |  def integer(x: Int): Int = 1
          |  def boolean(flag: Boolean): Boolean = true
          |  def string(x: Int): String = "text"
          |  def selected(x: Int): Int = holder.x
          |  def applied(x: Int): Int = helper(x)
          |  def selectedApplied(x: Int, y: Int): Int = Math.max(x, y)
          |  def infix(x: Int, y: Int): Int = x + y
          |  def unary(x: Int): Int = -x
          |  def tuple(x: Int, y: Int): (Int, Int) = (x, y)
          |  def conditional(flag: Boolean, x: Int): Int = if flag then x else 0
          |  def parenthesized(x: Int): Int = ((x))
          |  def opaque(x: Int): Int = x match { case value => value }
          |""".stripMargin
      )

      assertEquals(
        methods.map(method => method.name.toString -> topology(method.rhs)),
        List(
          "identifier" -> "Ident(x)",
          "integer" -> "Number(1,Whole(10))",
          "boolean" -> "Literal(Boolean(true))",
          "string" -> "Literal(String(text))",
          "selected" -> "Select(Ident(holder),x)",
          "applied" -> "Apply(Ident(helper),[Ident(x)])",
          "selectedApplied" -> "Apply(Select(Ident(Math),max),[Ident(x),Ident(y)])",
          "infix" -> "InfixOp(Ident(x),Ident(+),Ident(y))",
          "unary" -> "PrefixOp(Ident(-),Ident(x))",
          "tuple" -> "Tuple([Ident(x),Ident(y)])",
          "conditional" -> "If(Ident(flag),Ident(x),Number(0,Whole(10)))",
          "parenthesized" -> "Parens(Parens(Ident(x)))",
          "opaque" -> "Match"
        )
      )
    }
  }

  private def topology(tree: untpd.Tree): String =
    tree match
      case untpd.Ident(name) => s"Ident($name)"
      case untpd.Number(digits, kind) => s"Number($digits,$kind)"
      case untpd.Literal(constant) =>
        constant.tag match
          case BooleanTag => s"Literal(Boolean(${constant.booleanValue}))"
          case StringTag => s"Literal(String(${constant.value.asInstanceOf[String]}))"
          case other => s"Literal($other)"
      case untpd.Select(qualifier, name) => s"Select(${topology(qualifier)},$name)"
      case untpd.Apply(function, arguments) =>
        s"Apply(${topology(function)},[${arguments.map(topology).mkString(",")}])"
      case untpd.InfixOp(left, operator, right) =>
        s"InfixOp(${topology(left)},${topology(operator)},${topology(right)})"
      case untpd.PrefixOp(operator, operand) =>
        s"PrefixOp(${topology(operator)},${topology(operand)})"
      case untpd.Tuple(elements) => s"Tuple([${elements.map(topology).mkString(",")}])"
      case untpd.If(condition, thenBranch, elseBranch) =>
        s"If(${topology(condition)},${topology(thenBranch)},${topology(elseBranch)})"
      case untpd.Parens(inner) => s"Parens(${topology(inner)})"
      case _: untpd.Match => "Match"
      case other => other.getClass.getSimpleName

  private def parseMethods(source: String)(using outer: Context): List[untpd.DefDef] =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("RhsTermProjectionCharacterization.scala", source)
    given Context = outer.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed match
      case packageDef: untpd.PackageDef =>
        packageDef.stats match
          case (root: untpd.TypeDef) :: Nil =>
            root.rhs.asInstanceOf[untpd.Template].body.map {
              case method: untpd.DefDef => method
              case other => fail(s"expected DefDef, found ${other.getClass.getSimpleName}")
            }
          case other => fail(s"expected one TypeDef, found $other")
      case other => fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
