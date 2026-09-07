package quasiquotes.hybrid

import scala.compiletime.testing.typeCheckErrors
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

import quasiquotes.hybrid.q038.{Q038ScalametaDefaultSyntax, Q038ScalametaDefinitionPattern}
import quasiquotes.q038.Q038DefaultedOrdinaryCandidateFactory

final class Q038ScalametaDefaultedOrdinaryDefinitionFeasibilityTest extends munit.FunSuite:
  test("typed-Scalameta Q038 grammar exposes the exact external presence-only candidate type"):
    val _ = external.consumer.Q038ExternalScalametaDefaultedOrdinaryDefinitionConsumer

  test("Scalameta preserves authored default AST presence family syntax and clause structure"):
    val rows = List(
      "no-default" -> "def noDefault(x: Int): Int = x",
      "one-default" -> "def oneDefault(x: Int = 1): Int = x",
      "trailing-default" -> "def trailingDefault(x: Int, y: String = \"x\"): String = y",
      "two-defaults" -> "def twoDefaults(x: Int = 1, y: String = \"x\"): String = y",
      "nonliteral" -> "def nonliteral(x: Int = 1 + 2): Int = x",
      "selection" -> "def selection(x: Int = Defaults.stable): Int = x",
      "call" -> "def call(x: Int = Defaults.call()): Int = x",
      "multiple" -> "def multiple(x: Int = 1)(y: Int = 2): Int = x + y",
      "depends" -> "def depends(x: Int)(y: Int = x): Int = y",
      "generic" -> "def generic[A](x: A = null.asInstanceOf[A]): A = x"
    ).map((label, source) => label -> Q038ScalametaDefaultSyntax.inspect(source))

    rows.foreach(row => println(s"Q038_SCALAMETA_SOURCE ${TermQ3DialectPolicy.compilerVersion} $row"))
    assert(rows.forall(_._2.isRight), rows)
    val summaries = rows.map((label, result) => label -> result.toOption.get).toMap
    assertEquals(summaries("no-default").parameters.flatten.map(_.defaultPresent), List(false))
    assertEquals(summaries("one-default").parameters.flatten.map(_.defaultSyntax), List(Some("1")))
    assertEquals(summaries("trailing-default").parameters.flatten.map(_.defaultPresent), List(false, true))
    assertEquals(summaries("two-defaults").parameters.flatten.map(_.defaultPresent), List(true, true))
    assertEquals(summaries("nonliteral").parameters.flatten.head.defaultSyntax, Some("1 + 2"))
    assertEquals(summaries("selection").parameters.flatten.head.defaultFamily, Some("Term.Select"))
    assertEquals(summaries("call").parameters.flatten.head.defaultFamily, Some("Term.Apply"))
    assertEquals(summaries("multiple").clauseModes, List("ordinary", "ordinary"))
    assertEquals(summaries("multiple").parameters.map(_.size), List(1, 1))
    assertEquals(summaries("depends").parameters(1).head.defaultSyntax, Some("x"))
    assertEquals(summaries("generic").typeParameterCount, 1)
    assertEquals(summaries("generic").parameters.flatten.head.defaultFamily, Some("Term.ApplyType"))

  test("typed-Scalameta static recognition delegates all target semantics to the Quotes candidate"):
    import Q038ScalametaDefinitionPattern.dqq

    given Compiler = Compiler.make(getClass.getClassLoader)
    val result = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*
      val found = scala.collection.mutable.ListBuffer.empty[DefDef]
      val traversal = new TreeTraverser:
        override def traverseTree(tree: Tree)(owner: Symbol): Unit =
          tree match
            case value: DefDef if value.name == "selected" => found += value
            case _ => ()
          super.traverseTree(tree)(owner)
      traversal.traverseTree('{
        class Fixture:
          final def selected(x: Int, y: String = "x"): List[Option[Int]] = List(Some(x))
        ()
      }.asTerm)(Symbol.spliceOwner)
      val target = found.head
      val hybrid = dqq(StringContext("", " def ", "(..", "): ", " = ", ""))(using q)
      val shared = Q038DefaultedOrdinaryCandidateFactory.capturedModifiers(using q)
      val hybridResult = hybrid.unapply(target).get
      val sharedResult = shared.unapply(target).get
      (
        hybridResult._1.flags == sharedResult._1.flags,
        hybridResult._2 == sharedResult._2,
        hybridResult._3.zip(sharedResult._3).forall((left, right) => left eq right),
        hybridResult._4 =:= sharedResult._4,
        hybridResult._5.eq(sharedResult._5)
      )

    assertEquals(result, (true, true, true, true, true))

  test("typed-Scalameta test grammar rejects unselected layouts and production remains closed"):
    inline def messages(inline source: String): List[String] = typeCheckErrors(source).map(_.message)
    inline def candidateMessages(inline pattern: String): List[String] =
      messages(
        """import scala.quoted.*; import quasiquotes.hybrid.q038.Q038ScalametaDefinitionPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             """ + pattern + """
             case _ => ()"""
      )

    val accepted = candidateMessages("""case dqq"$mods def $name(..$params): $result = $body" => ()""")
    val rejected = List(
      candidateMessages("""case dqq"def $name(..$params): $result = $body" => ()"""),
      candidateMessages("""case dqq"$mods def fixed(..$params): $result = $body" => ()"""),
      candidateMessages("""case dqq"$mods def $name(...$paramss): $result = $body" => ()"""),
      candidateMessages("""case dqq"$mods def $name[..$tparams](..$params): $result = $body" => ()"""),
      candidateMessages("""case dqq"$mods def $name(..$params)(..$second): $result = $body" => ()"""),
      candidateMessages("""case dqq"$mods def $name(using ..$params): $result = $body" => ()"""),
      candidateMessages("""case dqq"$mods def $name(..$params): Int = $body" => ()"""),
      candidateMessages("""case dqq"$mods def $name(..$params): $result = $left + $right" => ()""")
    )
    assertEquals(accepted, Nil)
    assert(rejected.forall(_.nonEmpty), rejected)
    assert(rejected.flatten.forall(_.contains("Invalid Q038 typed-Scalameta dqq")), rejected)

    val production = messages(
      """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.dqq
         def f(using q: Quotes)(d: q.reflect.DefDef) = d match
           case dqq"$mods def $name(..$params): $result = $body" => ()
           case _ => ()"""
    )
    assertEquals(production, Nil)
