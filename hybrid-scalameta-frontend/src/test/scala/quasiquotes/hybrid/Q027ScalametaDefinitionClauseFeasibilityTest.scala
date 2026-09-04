package quasiquotes.hybrid

import scala.language.experimental.erasedDefinitions
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

import quasiquotes.hybrid.q027.Q027ScalametaDefinitionClauseSyntax

final class Q027ScalametaDefinitionClauseFeasibilityTest extends munit.FunSuite:
  test("Q027 Scalameta probe is usable from an external package"):
    assert(external.consumer.Q027ExternalScalametaClauseConsumer.namedUsing.isRight)

  test("Scalameta preserves authored clause modes and context bounds independently"):
    val rows = List(
      "ordinary" -> "def ordinary(value: Int): Int = value",
      "named-using" -> "def named(using ordering: Ordering[Int]): Int = 1",
      "anonymous-using" -> "def anonymous(using Ordering[Int]): Int = 1",
      "scala2-implicit" -> "def old(implicit ordering: Ordering[Int]): Int = 1",
      "ordinary-then-using" -> "def mixed(value: Int)(using ordering: Ordering[Int]): Int = value",
      "multiple-using" -> "def multiple(using first: Ordering[Int])(using second: Numeric[Int]): Int = 1",
      "erased" -> "def erasedClause(erased token: Int): Int = 1",
      "context-bound" -> "def bounded[A: Ordering]: Int = 1",
      "context-bound-ordinary" -> "def boundedOrdinary[A: Ordering](value: A): A = value",
      "context-bound-explicit-using" -> "def boundedUsing[A: Ordering](value: A)(using marker: Marker): A = value"
    ).map((label, source) => label -> Q027ScalametaDefinitionClauseSyntax.inspect(source))

    rows.foreach(row => println(s"Q027_SCALAMETA_STRUCTURE ${TermQ3DialectPolicy.compilerVersion} $row"))
    assert(rows.forall(_._2.isRight), rows)
    val summaries = rows.map((label, result) => label -> result.toOption.get).toMap
    assertEquals(summaries("ordinary").clauseModes, List("ordinary"))
    assertEquals(summaries("named-using").clauseModes, List("using"))
    assertEquals(summaries("anonymous-using").clauseModes, List("using"))
    assertEquals(summaries("scala2-implicit").clauseModes, List("implicit"))
    assertEquals(summaries("ordinary-then-using").clauseModes, List("ordinary", "using"))
    assertEquals(summaries("multiple-using").clauseModes, List("using", "using"))
    assertEquals(summaries("erased").clauseModes, List("ordinary"))
    assertEquals(summaries("erased").parameterModifiers, List(List(List("erased"))))
    assertEquals(summaries("context-bound").clauseModes, Nil)
    assertEquals(summaries("context-bound").contextBounds, List(List("Ordering")))
    assertEquals(summaries("context-bound-ordinary").clauseModes, List("ordinary"))
    assertEquals(summaries("context-bound-ordinary").contextBounds, List(List("Ordering")))
    assertEquals(summaries("context-bound-explicit-using").clauseModes, List("ordinary", "using"))
    assertEquals(summaries("context-bound-explicit-using").contextBounds, List(List("Ordering")))

  test("typed-Scalameta production selectors remain closed for nonordinary and context-bound targets"):
    import quasiquotes.scalameta.ScalametaQuasiPattern.dqq

    given Compiler = Compiler.make(getClass.getClassLoader)
    val result = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*

      def definition(expression: Expr[Any], expectedName: String): DefDef =
        val definitions = scala.collection.mutable.ListBuffer.empty[DefDef]
        val traversal = new TreeTraverser:
          override def traverseTree(tree: Tree)(owner: Symbol): Unit =
            tree match
              case value: DefDef if value.name == expectedName => definitions += value
              case _ => ()
            super.traverseTree(tree)(owner)
        traversal.traverseTree(expression.asTerm)(Symbol.spliceOwner)
        definitions.head

      def q020(target: DefDef): Boolean = target match
        case dqq"def $name(...$paramss): $result = $body" => true
        case _ => false
      def q026(target: DefDef): Boolean = target match
        case dqq"$mods def $name(...$paramss): $result = $body" => true
        case _ => false
      def q022(target: DefDef): Boolean = target match
        case dqq"def $name[..$tparams](...$paramss): $result = $body" => true
        case _ => false
      def q025(target: DefDef): Boolean = target match
        case dqq"$mods def $name[..$tparams](...$paramss): $result = $body" => true
        case _ => false

      val nongeneric = List(
        definition('{ def named(using ordering: Ordering[Int]): Int = 1; () }, "named"),
        definition('{ def erasedClause(erased token: Int): Int = 1; () }, "erasedClause")
      )
      val generic = List(
        definition('{ def genericUsing[A](value: A)(using Ordering[A]): A = value; () }, "genericUsing"),
        definition('{ def bounded[A: Ordering](value: A): A = value; () }, "bounded")
      )
      (
        nongeneric.forall(target => !q020(target) && !q026(target)),
        generic.forall(target => !q022(target) && !q025(target))
      )

    assert(result._1, result)
    assert(result._2, result)
