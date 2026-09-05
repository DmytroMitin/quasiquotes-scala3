package quasiquotes.hybrid

import scala.compiletime.testing.typeCheckErrors
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

import quasiquotes.hybrid.q033.{Q033ScalametaDefinitionPattern, Q033ScalametaMixedClauseSyntax}
import quasiquotes.q033.Q033MixedClauseCandidateFactory

final class Q033ScalametaMixedOrdinaryNamedUsingFeasibilityTest extends munit.FunSuite:
  test("typed-Scalameta Q033 grammar exposes the exact external candidate type"):
    val _ = external.consumer.Q033ExternalScalametaMixedOrdinaryNamedUsingConsumer

  test("Scalameta source grammar distinguishes absence empty ordinary mixed and context-bound forms"):
    val rows = List(
      "no-clauses" -> "def noClauses: Int = 0",
      "empty-ordinary" -> "def emptyOrdinary(): Int = 0",
      "ordinary" -> "def ordinary(x: Int): Int = x",
      "named-using" -> "def namedUsing(using ord: Ordering[Int]): Int = 1",
      "mixed" -> "def mixed(x: Int)(using ord: Ordering[Int]): Int = x",
      "mixed-many" -> "def mixedMany(x: Int, y: String)(using ord: Ordering[Int], num: Numeric[Int]): Int = x",
      "empty-then-using" -> "def emptyThenUsing()(using ord: Ordering[Int]): Int = 1",
      "context-bound" -> "def cb[A: Ordering](value: A)(using marker: Marker): A = value",
      "context-bound-using" -> "def cbUsing[A: Ordering](using marker: Marker): Int = 1"
    ).map((label, source) => label -> Q033ScalametaMixedClauseSyntax.inspect(source))

    rows.foreach(row => println(s"Q033_SCALAMETA_SOURCE ${TermQ3DialectPolicy.compilerVersion} $row"))
    assert(rows.forall(_._2.isRight), rows)
    val summaries = rows.map((label, result) => label -> result.toOption.get).toMap
    assertEquals(summaries("no-clauses").clauseModes, Nil)
    assertEquals(summaries("empty-ordinary").clauseModes, List("ordinary"))
    assertEquals(summaries("empty-ordinary").parameterNames, List(Nil))
    assertEquals(summaries("mixed").clauseModes, List("ordinary", "using"))
    assertEquals(summaries("mixed-many").parameterNames.map(_.size), List(2, 2))
    assertEquals(summaries("empty-then-using").parameterNames.map(_.size), List(0, 1))
    assertEquals(summaries("context-bound").typeParameterCount, 1)
    assertEquals(summaries("context-bound").contextBounds, List(List("Ordering")))
    assertEquals(summaries("context-bound").parameterNames.flatten, List("value", "marker"))
    assertEquals(summaries("context-bound-using").contextBounds, List(List("Ordering")))
    assertEquals(summaries("context-bound-using").parameterNames.flatten, List("marker"))

  test("typed-Scalameta static recognition delegates target semantics to the shared Quotes candidate"):
    import Q033ScalametaDefinitionPattern.dqq

    given Compiler = Compiler.make(getClass.getClassLoader)
    val result = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*
      val found = scala.collection.mutable.ListBuffer.empty[DefDef]
      val traversal = new TreeTraverser:
        override def traverseTree(tree: Tree)(owner: Symbol): Unit =
          tree match
            case value: DefDef if value.name == "mixed" => found += value
            case _ => ()
          super.traverseTree(tree)(owner)
      traversal.traverseTree('{
        class Fixture:
          final def mixed(x: Int, y: String)(using ord: Ordering[Int], num: Numeric[Int]): List[Option[Int]] = List(Some(x))
        ()
      }.asTerm)(Symbol.spliceOwner)
      val target = found.head
      val hybrid = dqq(StringContext("", " def ", "(..", ")(using ..", "): ", " = ", ""))(using q)
      val shared = Q033MixedClauseCandidateFactory.capturedModifiers(using q)
      val hybridResult = hybrid.unapply(target).get
      val sharedResult = shared.unapply(target).get
      (
        hybridResult._1.flags == sharedResult._1.flags,
        hybridResult._2 == sharedResult._2,
        hybridResult._3.zip(sharedResult._3).forall((left, right) => left eq right),
        hybridResult._4.zip(sharedResult._4).forall((left, right) => left eq right),
        hybridResult._5 =:= sharedResult._5,
        hybridResult._6 eq sharedResult._6
      )

    assertEquals(result, (true, true, true, true, true, true))

  test("typed-Scalameta test grammar rejects unselected layouts and production now owns Q033"):
    inline def messages(inline source: String): List[String] = typeCheckErrors(source).map(_.message)
    inline def candidateMessages(inline pattern: String): List[String] =
      messages(
        """import scala.quoted.*; import quasiquotes.hybrid.q033.Q033ScalametaDefinitionPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             """ + pattern + """
             case _ => ()"""
      )

    val accepted = candidateMessages("""case dqq"$mods def $name(..$params)(using ..$usingParams): $result = $body" => ()""")
    val rejected = List(
      candidateMessages("""case dqq"$mods def $name(using ..$usingParams)(..$params): $result = $body" => ()"""),
      candidateMessages("""case dqq"$mods def $name(..$first)(..$second): $result = $body" => ()"""),
      candidateMessages("""case dqq"$mods def $name(...$paramss)(using ..$usingParams): $result = $body" => ()"""),
      candidateMessages("""case dqq"$mods def $name[..$tparams](..$params)(using ..$usingParams): $result = $body" => ()"""),
      candidateMessages("""case dqq"def $name(..$params)(using ..$usingParams): $result = $body" => ()"""),
      candidateMessages("""case dqq"$mods def $name(..$params)(using ..$usingParams): Int = $body" => ()"""),
      candidateMessages("""case dqq"$mods def $name(..$params)(using ..$usingParams): $result = $left + $right" => ()""")
    )
    assertEquals(accepted, Nil)
    assert(rejected.forall(_.nonEmpty), rejected)
    assert(rejected.flatten.forall(_.contains("Invalid Q033 typed-Scalameta dqq")), rejected)

    val production = messages(
      """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.dqq
         def f(using q: Quotes)(d: q.reflect.DefDef) = d match
           case dqq"$mods def $name(..$params)(using ..$usingParams): $result = $body" => ()
           case _ => ()"""
    )
    assertEquals(production, Nil)
