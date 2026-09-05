package quasiquotes.hybrid

import scala.compiletime.testing.typeCheckErrors
import scala.language.experimental.erasedDefinitions
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

import quasiquotes.hybrid.q030.Q030ScalametaDefinitionSyntax
import quasiquotes.matching.Q030GenericNamedUsingCandidateFactory
import quasiquotes.q030.Q030Marker

final class Q030ScalametaGenericNamedUsingDefinitionClauseFeasibilityTest extends munit.FunSuite:
  test("Q030 typed-Scalameta probe is externally usable with the exact candidate type"):
    assert(external.consumer.Q030ExternalScalametaGenericNamedUsingDefinitionClauseConsumer.source.isRight)
    val _ = external.consumer.Q030ExternalScalametaGenericNamedUsingDefinitionClauseConsumer

  test("Scalameta keeps context bounds separate from authored using clauses"):
    val rows = List(
      "generic-one" -> "def genericOne[A](using ordering: Ordering[A]): Int = 1",
      "generic-two" -> "def genericTwo[A, B](using ev: Conversion[A, B]): Int = 1",
      "upper-bound" -> "def upperBound[A <: AnyRef](using ordering: Ordering[A]): Int = 1",
      "dependent-bound" -> "def dependentBound[A, B <: List[A]](using marker: Numeric[Int]): Int = 1",
      "authored-evidence-name" -> "def authoredEvidenceName[A](using evidence$1: Ordering[A]): Int = 1",
      "context-bound" -> "def contextBound[A: Ordering]: Int = 1",
      "context-bound-using" -> "def contextBoundUsing[A: Ordering](using marker: Marker): Int = 1",
      "context-bound-ordinary" -> "def contextBoundOrdinary[A: Ordering](value: A): A = value",
      "context-bound-ordinary-using" -> "def contextBoundOrdinaryUsing[A: Ordering](value: A)(using marker: Marker): A = value",
      "multiple-context-bounds-using" -> "def multipleContextBounds[A: Ordering : Numeric](using marker: Marker): Int = 1",
      "anonymous-using" -> "def anonymousUsing[A](using Ordering[A]): Int = 1",
      "scala2-implicit" -> "def scala2Implicit[A](implicit ordering: Ordering[A]): Int = 1",
      "erased" -> "def erasedParameter[A](erased erasedToken: Ordering[A]): Int = 1"
    ).map((label, source) => label -> Q030ScalametaDefinitionSyntax.inspect(source))

    rows.foreach(row => println(s"Q030_SCALAMETA_SOURCE ${row._1} ${row._2}"))
    assert(rows.forall(_._2.isRight), rows)
    val summaries = rows.map((label, result) => label -> result.toOption.get).toMap
    assertEquals(summaries("generic-one").typeParameters.map(_.name), List("A"))
    assertEquals(summaries("generic-one").typeParameters.flatMap(_.contextBounds), Nil)
    assertEquals(summaries("generic-one").clauseModes, List("using"))
    assertEquals(summaries("generic-one").parameterClauses.flatMap(_.map(_.name)), List("ordering"))
    assertEquals(summaries("generic-two").typeParameters.map(_.name), List("A", "B"))
    assertEquals(summaries("upper-bound").typeParameters.head.upperBound, Some("AnyRef"))
    assertEquals(summaries("dependent-bound").typeParameters(1).upperBound, Some("List[A]"))
    assertEquals(
      summaries("authored-evidence-name").parameterClauses.flatten.map(_.name),
      List("evidence$1")
    )

    assertEquals(summaries("context-bound").typeParameters.head.contextBounds, List("Ordering"))
    assertEquals(summaries("context-bound").clauseModes, Nil)
    assertEquals(summaries("context-bound-using").typeParameters.head.contextBounds, List("Ordering"))
    assertEquals(summaries("context-bound-using").clauseModes, List("using"))
    assertEquals(summaries("context-bound-using").parameterClauses.flatMap(_.map(_.name)), List("marker"))
    assertEquals(summaries("context-bound-ordinary").clauseModes, List("ordinary"))
    assertEquals(summaries("context-bound-ordinary-using").clauseModes, List("ordinary", "using"))
    assertEquals(
      summaries("multiple-context-bounds-using").typeParameters.head.contextBounds,
      List("Ordering", "Numeric")
    )
    assertEquals(summaries("multiple-context-bounds-using").clauseModes, List("using"))
    assertEquals(summaries("anonymous-using").parameterClauses.flatten.map(_.name), List(""))
    assertEquals(summaries("scala2-implicit").clauseModes, List("implicit"))
    assertEquals(summaries("erased").clauseModes, List("ordinary"))
    assert(summaries("erased").parameterClauses.flatten.head.modifiers.contains("erased"))

  test("richer Scalameta source evidence cannot make the shared Quotes target predicate safe"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val result = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*
      def definition(expression: Expr[Any], expectedName: String): DefDef =
        val found = scala.collection.mutable.ListBuffer.empty[DefDef]
        val traversal = new TreeTraverser:
          override def traverseTree(tree: Tree)(owner: Symbol): Unit =
            tree match
              case value: DefDef if value.name == expectedName => found += value
              case _ => ()
            super.traverseTree(tree)(owner)
        traversal.traverseTree(expression.asTerm)(Symbol.spliceOwner)
        found.head

      val ordinary = definition('{ def ordinary[A](using ordering: Ordering[A]): Int = 1; () }, "ordinary")
      val bounded = definition('{ def bounded[A: Ordering]: Int = 1; () }, "bounded")
      val merged = definition('{ def merged[A: Ordering](using marker: Q030Marker): Int = 1; () }, "merged")
      val candidate = Q030GenericNamedUsingCandidateFactory.capturedModifiers(using q)
      (
        candidate.unapply(ordinary).nonEmpty,
        candidate.unapply(bounded).nonEmpty,
        candidate.unapply(merged).nonEmpty
      )

    val compilerLine = dotty.tools.dotc.config.Properties.versionNumberString
    if compilerLine.startsWith("3.3") then assertEquals(result, (true, false, false))
    else assertEquals(result, (true, true, true))

  test("typed-Scalameta production remains closed for generic named using grammar"):
    val errors = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.dqq
         def f(using q: Quotes)(d: q.reflect.DefDef) = d match
           case dqq"$mods def $name[..$tparams](using ..$params): $result = $body" => ()
           case _ => ()"""
    ).map(_.message)
    assert(errors.nonEmpty, errors)
    assert(errors.exists(_.contains("Invalid Scalameta dqq definition-pattern template")), errors)
