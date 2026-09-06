package quasiquotes.hybrid

import scala.compiletime.testing.typeCheckErrors
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

import quasiquotes.hybrid.q042.{Q042ScalametaByNameSyntax, Q042ScalametaDefinitionPattern}
import quasiquotes.q042.Q042ByNameOrdinaryCandidateFactory

final class Q042ScalametaByNameOrdinaryDefinitionFeasibilityTest extends munit.FunSuite:
  test("typed-Scalameta Q042 grammar exposes the exact external by-name parameter capture type"):
    val _ = external.consumer.Q042ExternalScalametaByNameOrdinaryDefinitionConsumer

  test("Scalameta preserves strict by-name Function0 and repeated source structures"):
    val rows = List(
      "strict" -> "def strict(x: Int): Int = x",
      "by-name" -> "def byName(x: => Int): Int = x",
      "function0" -> "def thunk(x: () => Int): Int = x()",
      "nested" -> "def nested(x: => List[Option[Int]]): Int = x.size",
      "two" -> "def two(a: => Int, b: => String): String = b",
      "many" -> "def many(a: => Int, b: String, c: => Long, d: Boolean, e: => Double): Double = e",
      "defaulted" -> "def defaulted(x: => Int = 1): Int = x",
      "generic" -> "def generic[A](x: => A): A = x",
      "multiple" -> "def multiple(a: Int)(b: => String): String = b",
      "using" -> "def contextual(using x: => Int): Int = x",
      "implicit" -> "def implicitByName(implicit x: => Int): Int = x",
      "erased" -> "def erasedByName(erased x: => Int): Int = 0",
      "annotated" -> "def annotated(@deprecated(\"q042\", \"\") x: => Int): Int = x",
      "repeated" -> "def repeated(xs: Int*): Int = xs.size"
    ).map((label, source) => label -> Q042ScalametaByNameSyntax.inspect(source))
    println(s"Q042_SCALAMETA_SOURCE ${TermQ3DialectPolicy.compilerVersion} $rows")
    assert(rows.forall(_._2.isRight), rows)
    val summaries = rows.map((label, result) => label -> result.toOption.get).toMap
    val strict = summaries("strict").parameters.flatten.head
    val byName = summaries("by-name").parameters.flatten.head
    val function0 = summaries("function0").parameters.flatten.head
    val repeated = summaries("repeated").parameters.flatten.head
    assertEquals(strict.typeFamily, Some("Type.Name"))
    assertEquals(strict.byNameElementSyntax, None)
    assertEquals(byName.typeFamily, Some("Type.ByName"))
    assertEquals(byName.byNameElementSyntax, Some("Int"))
    assertEquals(function0.typeFamily, Some("Type.Function"))
    assertEquals(function0.byNameElementSyntax, None)
    assertEquals(repeated.typeFamily, Some("Type.Repeated"))
    assertEquals(repeated.repeatedElementSyntax, Some("Int"))
    assertEquals(summaries("nested").parameters.flatten.head.byNameElementFamily, Some("Type.Apply"))
    assertEquals(summaries("nested").parameters.flatten.head.byNameElementSyntax, Some("List[Option[Int]]"))
    assertEquals(summaries("two").parameters.flatten.map(_.name), List("a", "b"))
    assertEquals(summaries("many").parameters.flatten.map(_.byNameElementSyntax), List(Some("Int"), None, Some("Long"), None, Some("Double")))
    assert(summaries("defaulted").parameters.flatten.head.defaultPresent)
    assertEquals(summaries("generic").typeParameterCount, 1)
    assertEquals(summaries("multiple").clauseModes, List("ordinary", "ordinary"))
    assertEquals(summaries("using").clauseModes, List("using"))
    assertEquals(summaries("implicit").clauseModes, List("implicit"))
    assert(summaries("annotated").parameters.flatten.head.modifiers.nonEmpty)

  test("typed-Scalameta recognition delegates target semantics to the shared Quotes candidate"):
    import Q042ScalametaDefinitionPattern.dqq
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
          final def selected(first: Int, delayed: => List[Option[Int]]): List[Option[Int]] = delayed
        ()
      }.asTerm)(Symbol.spliceOwner)
      val target = found.head
      val hybrid = dqq(StringContext("", " def ", "(..", "): ", " = ", ""))(using q)
      val shared = Q042ByNameOrdinaryCandidateFactory.capturedModifiers(using q)
      val hybridResult = hybrid.unapply(target).get
      val sharedResult = shared.unapply(target).get
      val delayed = hybridResult._3.last
      val treeElement = Q042ByNameOrdinaryCandidateFactory.valDefByNameElementType(using q)(delayed)
      val methodElement = Q042ByNameOrdinaryCandidateFactory.methodByNameElementType(using q)(target, 1)
      (
        hybridResult._1.flags == sharedResult._1.flags,
        hybridResult._2 == sharedResult._2,
        hybridResult._3.zip(sharedResult._3).forall((left, right) => left eq right),
        (treeElement, methodElement) match
          case (Some(a), Some(b)) => a =:= b && delayed.symbol.termRef.widen =:= a
          case _ => false,
        hybridResult._4 =:= sharedResult._4,
        hybridResult._5.eq(sharedResult._5)
      )
    assertEquals(result, (true, true, true, true, true, true))

  test("existing typed-Scalameta rank-3 production admits by-name targets while rank-2 stays closed"):
    import quasiquotes.scalameta.ScalametaQuasiPattern.dqq
    given Compiler = Compiler.make(getClass.getClassLoader)
    val row = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*
      val found = scala.collection.mutable.ListBuffer.empty[DefDef]
      val traversal = new TreeTraverser:
        override def traverseTree(tree: Tree)(owner: Symbol): Unit =
          tree match
            case value: DefDef if value.name == "byName" => found += value
            case _ => ()
          super.traverseTree(tree)(owner)
      traversal.traverseTree('{ def byName(value: => Int): Int = value; () }.asTerm)(Symbol.spliceOwner)
      val target = found.head
      val existing = dqq(StringContext("", " def ", "(...", "): ", " = ", ""))(using q)
      val captured = existing.unapply(target)
      val original = target.paramss match
        case List(clause: TermParamClause) => clause.params.head
        case _ => report.errorAndAbort("Q042 by-name fixture lost its ordinary clause")
      (
        captured.nonEmpty,
        captured.exists(_._3.flatten.head eq original)
      )
    assertEquals(row, (true, true))
    val rankTwo = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.dqq
         def f(using q: Quotes)(d: q.reflect.DefDef) = d match
           case dqq\"$mods def $name(..$params): $result = $body\" => ()
           case _ => ()"""
    )
    assert(rankTwo.nonEmpty, rankTwo)

  test("typed-Scalameta test grammar rejects unselected layouts and dynamic contexts"):
    inline def messages(inline source: String): List[String] = typeCheckErrors(source).map(_.message)
    inline def candidateMessages(inline pattern: String): List[String] =
      messages(
        """import scala.quoted.*; import quasiquotes.hybrid.q042.Q042ScalametaDefinitionPattern.dqq
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
      candidateMessages("""case dqq"$mods def $name(fixed: Int, ..$params): $result = $body" => ()"""),
      candidateMessages("""case dqq"$mods def $name(..$params)(..$second): $result = $body" => ()"""),
      candidateMessages("""case dqq"$mods def $name(using ..$params): $result = $body" => ()"""),
      candidateMessages("""case dqq"$mods def $name(..$params): Int = $body" => ()"""),
      candidateMessages("""case dqq"$mods def $name(..$params): $result = $left + $right" => ()"""),
      candidateMessages("""case dqq"$mods def $name(.$params): $result = $body" => ()"""),
      candidateMessages("""case dqq"$mods def $name(..$params): $result = $body trailing" => ()"""),
      candidateMessages("""case dqq"$mods def $name(..$params): $result = $params" => ()""")
    )
    val dynamic = messages(
      """import scala.quoted.*; import quasiquotes.hybrid.q042.Q042ScalametaDefinitionPattern.dqq
         def f(using q: Quotes)(context: StringContext) = context.dqq"""
    )
    assertEquals(accepted, Nil)
    assert(rejected.forall(_.nonEmpty), rejected)
    assert(rejected.dropRight(1).flatten.forall(_.contains("Invalid Q042 typed-Scalameta dqq")), rejected)
    assert(rejected.last.exists(_.contains("duplicate pattern variable")), rejected.last)
    assert(dynamic.nonEmpty, dynamic)
