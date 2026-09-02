package quasiquotes.q002

import scala.compiletime.testing.typeCheckErrors
import scala.quoted.staging.{Compiler, withQuotes}

import quasiquotes.matching.RankedPatternSource

class Q002RankDiagnosticTest extends munit.FunSuite:
  private inline def messages(inline source: String): List[String] =
    typeCheckErrors(source).map(_.message)

  private def stagedAbortMessage(operation: scala.quoted.Quotes ?=> Unit): String =
    given Compiler = Compiler.make(getClass.getClassLoader)
    withQuotes:
      try
        operation
        "<no-abort>"
      catch
        case error: Throwable => Option(error.getMessage).getOrElse(error.getClass.getName)

  test("qq rejects multiple, rank-3, orphan, and non-Apply sequence markers with Q002 diagnostics"):
    val multiple = messages(
      """import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def attempt(using q: Quotes)(term: q.reflect.Term) = term match
          case qq"$function(..$left, ..$right)" => ()
          case _ => ()
      """
    )
    val rankThree = messages(
      """import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def attempt(using q: Quotes)(term: q.reflect.Term) = term match
          case qq"$function(...$arguments)" => ()
          case _ => ()
      """
    )
    val orphan = messages(
      """import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def attempt(using q: Quotes)(term: q.reflect.Term) = term match
          case qq"$function(..)" => ()
          case _ => ()
      """
    )
    val root = messages(
      """import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def attempt(using q: Quotes)(term: q.reflect.Term) = term match
          case qq"..$arguments" => ()
          case _ => ()
      """
    )
    val tuple = messages(
      """import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def attempt(using q: Quotes)(term: q.reflect.Term) = term match
          case qq"(..$arguments, 1)" => ()
          case _ => ()
      """
    )
    val typed = messages(
      """import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def attempt(using q: Quotes)(term: q.reflect.Term) = term match
          case qq"(..$arguments: List[Int])" => ()
          case _ => ()
      """
    )
    val block = messages(
      """import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def attempt(using q: Quotes)(term: q.reflect.Term) = term match
          case qq"{ ..$statements; 1 }" => ()
          case _ => ()
      """
    )

    assert(multiple.exists(_.contains("only one rank-2 sequence-Term capture")))
    assert(rankThree.exists(_.contains("unsupported rank-3 `...` capture marker")))
    assert(orphan.exists(_.contains("orphan or malformed `..` rank marker")))
    assert(root.exists(_.contains("direct ordinary Apply or fixed one-list New argument")))
    assert(tuple.exists(_.contains("direct ordinary Apply or fixed one-list New argument")), tuple.mkString(" | "))
    assert(typed.exists(_.contains("direct ordinary Apply or fixed one-list New argument")), typed.mkString(" | "))
    assert(block.exists(_.contains("direct ordinary Apply or fixed one-list New argument")), block.mkString(" | "))

  test("split-dot and Type/Definition rank spellings fail with stable family diagnostics"):
    val splitDots = messages(
      """import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def attempt(using q: Quotes)(term: q.reflect.Term) = term match
          case qq"$function(. .$arguments)" => ()
          case _ => ()
      """
    )
    val typeRank = stagedAbortMessage:
      val q = summon[scala.quoted.Quotes]
      quasiquotes.types.QuasiTypequotes.tqq(StringContext("List[..", "]"))(using q)
    val definitionRank = messages(
      """import scala.quoted.*
        import quasiquotes.matching.DefinitionPattern.*
        def attempt(using q: Quotes)(definition: q.reflect.DefDef) = definition match
          case dqq"def f(x: Int): Int = ..$body" => ()
          case _ => ()
      """
    )

    assert(
      splitDots.exists(_.contains("orphan or malformed rank-marker spelling")),
      splitDots.mkString(" | ")
    )
    assert(
      typeRank.contains("rank-2 captures are not supported for Type patterns"),
      typeRank
    )
    assert(
      definitionRank.exists(_.contains("rank-2 captures are not supported for Definition patterns")),
      definitionRank.mkString(" | ")
    )

  test("rank text inside guest strings, comments, and backticks is not classified"):
    val errors = messages(
      """import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def attempt(using q: Quotes)(term: q.reflect.Term) =
          term match
            case qq"s\"..$insideString\"" => val _: q.reflect.Term = insideString
            case _ => ()
          term match
            case qq"identity(/* .. */ $insideComment)" => val _: q.reflect.Term = insideComment
            case _ => ()
          term match
            case qq"`..`($insideBacktick)" => val _: q.reflect.Term = insideBacktick
            case _ => ()
          term match
            case qq"identity('.', '.', $insideCharacters)" =>
              val _: q.reflect.Term = insideCharacters
            case _ => ()
          term match
            case qq"identity(\"\"\"a\" .. b\"\"\", $outsideTriple)" =>
              val _: q.reflect.Term = outsideTriple
            case _ => ()
      """
    )
    assertEquals(errors, Nil)

  test("rank text in a braced guest-string interpolation is classified then rejected by position"):
    val result = RankedPatternSource.compile(
      List("s\"${..", "}\""),
      sequenceIndex = 0
    )
    assert(
      result.left.exists(_.contains("direct ordinary Apply or fixed one-list New argument")),
      result.toString
    )

  test("a sequence binder cannot be used as a scalar Term and duplicate host binders fail closed"):
    val sequenceAsScalar = messages(
      """import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def attempt(using q: Quotes)(term: q.reflect.Term) = term match
          case qq"$function(..$arguments)" => val _: q.reflect.Term = arguments
          case _ => ()
      """
    )
    val repeated = messages(
      """import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def attempt(using q: Quotes)(term: q.reflect.Term) = term match
          case qq"$function(..$same, $same)" => ()
          case _ => ()
      """
    )
    val repeatedSequence = messages(
      """import scala.quoted.*
        import quasiquotes.matching.QuasiPattern.*
        def attempt(using q: Quotes)(term: q.reflect.Term) = term match
          case qq"$function(..$same, ..$same)" => ()
          case _ => ()
      """
    )

    assert(sequenceAsScalar.exists(_.contains("q.reflect.Term")))
    assert(
      repeated.exists(_.contains("duplicate pattern variable: same")),
      repeated.mkString(" | ")
    )
    assert(repeatedSequence.exists(_.contains("only one rank-2 sequence-Term capture")))
