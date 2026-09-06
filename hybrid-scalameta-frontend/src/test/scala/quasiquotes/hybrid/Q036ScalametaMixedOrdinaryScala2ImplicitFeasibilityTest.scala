package quasiquotes.hybrid

import scala.compiletime.testing.typeCheckErrors
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

import quasiquotes.hybrid.q036.{Q036ScalametaDefinitionPattern, Q036ScalametaMixedClauseSyntax}
import quasiquotes.q036.Q036MixedClauseCandidateFactory

final class Q036ScalametaMixedOrdinaryScala2ImplicitFeasibilityTest extends munit.FunSuite:
  test("typed-Scalameta Q036 grammar exposes the exact external candidate type"):
    val _ = external.consumer.Q036ExternalScalametaMixedOrdinaryScala2ImplicitConsumer

  test("Scalameta source grammar distinguishes absence empty ordinary mixed and context-bound forms"):
    val rows = List(
      "no-clauses" -> "def noClauses: Int = 0",
      "empty-ordinary" -> "def emptyOrdinary(): Int = 0",
      "ordinary" -> "def ordinary(x: Int): Int = x",
      "one-implicit" -> "def oneImplicit(implicit ord: Ordering[Int]): Int = 1",
      "named-using" -> "def namedUsing(using ord: Ordering[Int]): Int = 1",
      "mixed" -> "def mixed(x: Int)(implicit ord: Ordering[Int]): Int = x",
      "mixed-many" -> "def mixedMany(x: Int, y: String)(implicit ord: Ordering[Int], num: Numeric[Int]): Int = x",
      "empty-then-implicit" -> "def emptyThenImplicit()(implicit ord: Ordering[Int]): Int = 1",
      "context-bound" -> "def cb[A: Ordering](value: A)(implicit marker: Marker): A = value",
      "context-bound-using" -> "def cbUsing[A: Ordering](using marker: Marker): Int = 1"
    ).map((label, source) => label -> Q036ScalametaMixedClauseSyntax.inspect(source))

    rows.foreach(row => println(s"Q036_SCALAMETA_SOURCE ${TermQ3DialectPolicy.compilerVersion} $row"))
    assert(rows.forall(_._2.isRight), rows)
    val summaries = rows.map((label, result) => label -> result.toOption.get).toMap
    assertEquals(summaries("no-clauses").clauseModes, Nil)
    assertEquals(summaries("empty-ordinary").clauseModes, List("ordinary"))
    assertEquals(summaries("empty-ordinary").parameterNames, List(Nil))
    assertEquals(summaries("mixed").clauseModes, List("ordinary", "implicit"))
    assertEquals(summaries("mixed-many").parameterNames.map(_.size), List(2, 2))
    assertEquals(summaries("mixed-many").parameterModifiers, List(List(Nil, Nil), List(List("implicit"), List("implicit"))))
    assert(summaries("mixed-many").defaults.flatten.forall(!_))
    assertEquals(summaries("empty-then-implicit").parameterNames.map(_.size), List(0, 1))
    assertEquals(summaries("context-bound").typeParameterCount, 1)
    assertEquals(summaries("context-bound").contextBounds, List(List("Ordering")))
    assertEquals(summaries("context-bound").parameterNames.flatten, List("value", "marker"))
    assertEquals(summaries("context-bound-using").contextBounds, List(List("Ordering")))
    assertEquals(summaries("context-bound-using").parameterNames.flatten, List("marker"))
    assert(Q036ScalametaMixedClauseSyntax.inspect("def invalid()(implicit): Int = 1").isLeft)

  test("typed-Scalameta static recognition delegates target semantics to the shared Quotes candidate"):
    import Q036ScalametaDefinitionPattern.dqq

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
          final def mixed(x: Int, y: String)(implicit ord: Ordering[Int], num: Numeric[Int]): List[Option[Int]] = List(Some(x))
        ()
      }.asTerm)(Symbol.spliceOwner)
      val target = found.head
      val hybrid = dqq(StringContext("", " def ", "(..", ")(implicit ..", "): ", " = ", ""))(using q)
      val shared = Q036MixedClauseCandidateFactory.capturedModifiers(using q)
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

  test("typed-Scalameta test grammar rejects unselected layouts and production selects Q036 through Q037"):
    inline def messages(inline source: String): List[String] = typeCheckErrors(source).map(_.message)
    inline def candidateMessages(inline pattern: String): List[String] =
      messages(
        """import scala.quoted.*; import quasiquotes.hybrid.q036.Q036ScalametaDefinitionPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             """ + pattern + """
             case _ => ()"""
      )

    val accepted = candidateMessages("""case dqq"$mods def $name(..$params)(implicit ..$usingParams): $result = $body" => ()""")
    val rejected = List(
      candidateMessages("""case dqq"$mods def $name(using ..$usingParams)(..$params): $result = $body" => ()"""),
      candidateMessages("""case dqq"$mods def $name(..$first)(..$second): $result = $body" => ()"""),
      candidateMessages("""case dqq"$mods def $name(..$first)(implicit ..$second)(implicit ..$third): $result = $body" => ()"""),
      candidateMessages("""case dqq"$mods def $name(...$paramss)(implicit ..$usingParams): $result = $body" => ()"""),
      candidateMessages("""case dqq"$mods def $name[..$tparams](..$params)(implicit ..$usingParams): $result = $body" => ()"""),
      candidateMessages("""case dqq"$mods def fixed(..$params)(implicit ..$usingParams): $result = $body" => ()"""),
      candidateMessages("""case dqq"private def $name(..$params)(implicit ..$usingParams): $result = $body" => ()"""),
      candidateMessages("""case dqq"def $name(..$params)(implicit ..$usingParams): $result = $body" => ()"""),
      candidateMessages("""case dqq"$mods def $name(..$params)(implicit ..$usingParams): Int = $body" => ()"""),
      candidateMessages("""case dqq"$mods def $name(..$params)(implicit ..$usingParams): $result = $left + $right" => ()"""),
      candidateMessages("""case dqq"$mods def $name(.$params)(implicit ..$usingParams): $result = $body" => ()"""),
      candidateMessages("""case dqq"$mods def $name(..$params)(implicit .$usingParams): $result = $body" => ()"""),
      candidateMessages("""case dqq"$mods def $name(..$params)(implicit ..$usingParams): $result = $body trailing" => ()""")
    )
    assertEquals(accepted, Nil)
    assert(rejected.forall(_.nonEmpty), rejected)
    assert(rejected.flatten.forall(_.contains("Invalid Q036 typed-Scalameta dqq")), rejected)

    val production = messages(
      """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.dqq
         def f(using q: Quotes)(d: q.reflect.DefDef) = d match
           case dqq"$mods def $name(..$params)(implicit ..$usingParams): $result = $body" => ()
           case _ => ()"""
    )
    assertEquals(production, Nil)
