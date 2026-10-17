package quasiquotes.definitions.dotty

import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.{Context, ContextBase}
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter

final class CanonicalOptionConstructorAuthorityParserTopologyTest extends munit.FunSuite:
  test("U072 parser topology probe for unqualified, qualified and rooted Option[String]"):
    withContext:
      val observed = Vector(
        "unqualified" -> "Option[String]",
        "qualified" -> "scala.Option[String]",
        "rooted" -> "_root_.scala.Option[String]",
        "rooted-pair" -> "_root_.scala.Option[_root_.java.lang.String]"
      ).map { (label, syntax) =>
        val method = parseMethod(s"class Probe:\n  def echo(value: $syntax): $syntax = value\n")
        val parameter = method.paramss.head.head.asInstanceOf[untpd.ValDef]
        s"$label parameter=${topology(parameter.tpt)} result=${topology(method.tpt)}"
      }

      assertEquals(
        observed,
        Vector(
          "unqualified parameter=Applied(Ident(Option),[Ident(String)]) result=Applied(Ident(Option),[Ident(String)])",
          "qualified parameter=Applied(Select(Ident(scala),Option),[Ident(String)]) result=Applied(Select(Ident(scala),Option),[Ident(String)])",
          "rooted parameter=Applied(Select(Select(Ident(_root_),scala),Option),[Ident(String)]) result=Applied(Select(Select(Ident(_root_),scala),Option),[Ident(String)])",
          "rooted-pair parameter=Applied(Select(Select(Ident(_root_),scala),Option),[Select(Select(Select(Ident(_root_),java),lang),String)]) result=Applied(Select(Select(Ident(_root_),scala),Option),[Select(Select(Select(Ident(_root_),java),lang),String)])"
        )
      )

  private def topology(tree: untpd.Tree): String = tree match
    case untpd.Ident(name) => s"Ident($name)"
    case untpd.Select(qualifier, name) => s"Select(${topology(qualifier)},$name)"
    case untpd.AppliedTypeTree(constructor, arguments) =>
      s"Applied(${topology(constructor)},[${arguments.map(topology).mkString(",")}])"
    case other => s"${other.getClass.getSimpleName}(${other.productIterator.mkString(",")})"

  private def parseMethod(source: String)(using outer: Context): untpd.DefDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("U072ParserTopology.scala", source)
    given Context = outer.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parsed = new Parsers.Parser(unit.source).parse()
    assertEquals(reporter.pendingMessages.toList, Nil)
    parsed match
      case packageDef: untpd.PackageDef =>
        packageDef.stats.collect { case root: untpd.TypeDef => root } match
          case root :: Nil =>
            root.rhs.asInstanceOf[untpd.Template].body.collect { case method: untpd.DefDef => method } match
              case method :: Nil => method
              case other => fail(s"expected one method, found $other")
          case other => fail(s"expected one TypeDef, found $other")
      case other => fail(s"expected PackageDef, found ${other.getClass.getSimpleName}")

  private def withContext[A](run: Context ?=> A): A =
    run(using (new ContextBase).initialCtx)
