package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter

final class ExistingUntpdOrdinaryMethodTypeSlotProjectionCharacterizationTest
    extends munit.FunSuite:
  test("parser Type-slot topology is characterized before semantic projection") {
    withContext {
      val method = parseMethod(
        """class Characterized:
          |  def f(
          |    primitive: Int,
          |    recursive: Either[List[Int], Option[(String, Boolean)]],
          |    tuple3: (Int, String, Boolean),
          |    function1: Int => String,
          |    function2: (Int, String) => Boolean,
          |    parenthesized: ((Int))
          |  ): Option[(Int, String) => Either[Boolean, List[Int]]] = ???
          |""".stripMargin
      )

      val parameterTopologies = method.paramss.head.map {
        case parameter: untpd.ValDef => topology(parameter.tpt)
        case other => fail(s"expected ValDef parameter, found ${other.getClass.getSimpleName}")
      }
      val resultTopology = topology(method.tpt)

      assertEquals(
        parameterTopologies :+ resultTopology,
        List(
          "Ident(Int)",
          "Applied(Ident(Either),[Applied(Ident(List),[Ident(Int)]),Applied(Ident(Option),[Tuple([Ident(String),Ident(Boolean)])])])",
          "Tuple([Ident(Int),Ident(String),Ident(Boolean)])",
          "Function([Ident(Int)],Ident(String))",
          "Function([Ident(Int),Ident(String)],Ident(Boolean))",
          "Parens(Parens(Ident(Int)))",
          "Applied(Ident(Option),[Function([Ident(Int),Ident(String)],Applied(Ident(Either),[Ident(Boolean),Applied(Ident(List),[Ident(Int)])]))])"
        )
      )
    }
  }

  private def topology(tree: untpd.Tree): String =
    tree match
      case untpd.Ident(name) => s"Ident($name)"
      case untpd.AppliedTypeTree(constructor, arguments) =>
        s"Applied(${topology(constructor)},[${arguments.map(topology).mkString(",")}])"
      case untpd.Tuple(elements) =>
        s"Tuple([${elements.map(topology).mkString(",")}])"
      case untpd.Function(arguments, result) =>
        s"Function([${arguments.map(topology).mkString(",")}],${topology(result)})"
      case untpd.Parens(inner) => s"Parens(${topology(inner)})"
      case other => s"${other.getClass.getSimpleName}(${other.productIterator.mkString(",")})"

  private def parseMethod(source: String)(using outer: Context): untpd.DefDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("TypeSlotProjectionCharacterization.scala", source)
    given Context = outer.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed match
      case packageDef: untpd.PackageDef =>
        packageDef.stats match
          case (root: untpd.TypeDef) :: Nil =>
            root.rhs.asInstanceOf[untpd.Template].body match
              case (method: untpd.DefDef) :: Nil => method
              case other => fail(s"expected one method, found $other")
          case other => fail(s"expected one TypeDef, found $other")
      case other => fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")

  private def withContext[A](run: Context ?=> A): A =
    val base = new ContextBase
    run(using base.initialCtx)
