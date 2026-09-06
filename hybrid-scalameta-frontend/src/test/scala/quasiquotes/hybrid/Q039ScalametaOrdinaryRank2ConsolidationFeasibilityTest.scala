package quasiquotes.hybrid

import scala.compiletime.testing.typeCheckErrors
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

import quasiquotes.hybrid.q042.Q042ScalametaByNameSyntax

final class Q039ScalametaOrdinaryRank2ConsolidationFeasibilityTest extends munit.FunSuite:
  private val sources = List(
    "empty" -> "def empty(): Int = 0",
    "strict" -> "def strict(a: Int, b: String): Int = a",
    "by-name-mix" -> "def byNameMix(a: Int, b: => String, c: Long): String = b",
    "repeated-mix" -> "def repeatedMix(a: Int, b: String*): Int = a + b.size",
    "by-name-repeated" -> "def byNameAndRepeated(a: => Int, b: String*): Int = a + b.size",
    "many" -> "def many(a: => Int, b: String, c: => Long, d: Boolean, e: => Double): Double = e",
    "default-strict" -> "def defaultStrict(a: Int = 1): Int = a",
    "strict-default" -> "def strictThenDefault(a: Int, b: String = \"x\"): String = b",
    "default-by-name" -> "def defaultThenByName(a: Int = 1, b: => String): String = b",
    "multiple" -> "def multiple(a: Int)(b: => String)(c: Long*): String = b"
  )

  test("typed Scalameta retains exact mixed parameter source forms"):
    val rows = sources.map((label, source) => label -> Q042ScalametaByNameSyntax.inspect(source))
    rows.foreach(row => println(s"Q039_SCALAMETA ${TermQ3DialectPolicy.compilerVersion} $row"))
    assert(rows.forall(_._2.isRight), rows)
    val summaries = rows.map((label, row) => label -> row.toOption.get).toMap
    def families(name: String) = summaries(name).parameters.flatten.map(_.typeFamily)
    def defaults(name: String) = summaries(name).parameters.flatten.map(_.defaultPresent)
    assertEquals(families("strict"), List(Some("Type.Name"), Some("Type.Name")))
    assertEquals(families("by-name-mix"), List(Some("Type.Name"), Some("Type.ByName"), Some("Type.Name")))
    assertEquals(families("repeated-mix"), List(Some("Type.Name"), Some("Type.Repeated")))
    assertEquals(families("by-name-repeated"), List(Some("Type.ByName"), Some("Type.Repeated")))
    assertEquals(families("many"), List(Some("Type.ByName"), Some("Type.Name"), Some("Type.ByName"), Some("Type.Name"), Some("Type.ByName")))
    assertEquals(defaults("default-strict"), List(true))
    assertEquals(defaults("strict-default"), List(false, true))
    assertEquals(defaults("default-by-name"), List(true, false))
    assertEquals(summaries("empty").parameters.map(_.size), List(0))
    assertEquals(summaries("multiple").parameters.map(_.size), List(1, 1, 1))
    assertEquals(summaries("by-name-repeated").parameters.flatten.last.repeatedElementSyntax, Some("String"))
    assertEquals(summaries("default-by-name").parameters.flatten.head.typeSyntax, Some("Int"))

  test("typed-Scalameta production matches rank-3 mixed recoverable modes and rejects defaults"):
    import quasiquotes.scalameta.ScalametaQuasiPattern.dqq

    given Compiler = Compiler.make(getClass.getClassLoader)
    val result = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*
      val found = scala.collection.mutable.Map.empty[String, DefDef]
      val traversal = new TreeTraverser:
        override def traverseTree(tree: Tree)(owner: Symbol): Unit =
          tree match
            case definition: DefDef => found.update(definition.name, definition)
            case _ => ()
          super.traverseTree(tree)(owner)
      traversal.traverseTree('{
        def strict(a: Int, b: String): Int = a
        def defaulted(a: Int = 1): Int = a
        def byName(a: Int, b: => String): String = b
        def repeated(a: Int, b: String*): Int = a + b.size
        def combined(a: => Int, b: String*): Int = a + b.size
        def multiple(a: Int)(b: => String)(c: Long*): String = b
        ()
      }.asTerm)(Symbol.spliceOwner)
      val extractor = dqq(StringContext("", " def ", "(...", "): ", " = ", ""))(using q)
      List("strict", "defaulted", "byName", "repeated", "combined", "multiple").map { name =>
        val target = found(name)
        name -> extractor.unapply(target).map(captured =>
          captured._3.flatten.zip(target.paramss.collect { case c: TermParamClause => c.params }.flatten)
            .forall((left, right) => left eq right) && captured._5.eq(target.rhs.get)
        )
      }
    println(s"Q039_SCALAMETA_PRODUCTION $result")
    assertEquals(result.toMap, Map(
      "strict" -> Some(true),
      "defaulted" -> None,
      "byName" -> Some(true),
      "repeated" -> Some(true),
      "combined" -> Some(true),
      "multiple" -> Some(true)
    ))

    val rankTwo = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.dqq
         def f(using q: Quotes)(d: q.reflect.DefDef) = d match
           case dqq\"$mods def $name(..$params): $result = $body\" => ()
           case _ => ()"""
    )
    assert(rankTwo.nonEmpty, rankTwo)
