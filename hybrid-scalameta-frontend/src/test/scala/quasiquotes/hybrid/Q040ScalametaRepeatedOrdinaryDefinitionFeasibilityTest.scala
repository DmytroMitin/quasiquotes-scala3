package quasiquotes.hybrid

import scala.compiletime.testing.typeCheckErrors
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

import quasiquotes.hybrid.q040.{Q040ScalametaDefinitionPattern, Q040ScalametaRepeatedSyntax}
import quasiquotes.q040.Q040RepeatedOrdinaryCandidateFactory

final class Q040ScalametaRepeatedOrdinaryDefinitionFeasibilityTest extends munit.FunSuite:
  test("typed-Scalameta Q040 grammar exposes the exact external repeated-parameter capture type"):
    val _ = external.consumer.Q040ExternalScalametaRepeatedOrdinaryDefinitionConsumer

  test("Scalameta preserves repeated source AST element Types order and clause structure"):
    inline def compilerMessages(inline source: String): List[String] =
      typeCheckErrors(source).map(_.message)

    val validRows = List(
      "plain-seq" -> "def plainSeq(xs: Seq[Int]): Int = xs.size",
      "repeated-only" -> "def repeatedOnly(xs: Int*): Int = xs.size",
      "prefix-repeated" -> "def prefixRepeated(head: Int, tail: String*): Int = head + tail.size",
      "nested-repeated" -> "def nestedRepeated(xs: List[Option[Int]]*): Int = xs.size",
      "generic" -> "def generic[A](xs: A*): Int = xs.size",
      "multiple" -> "def multiple(prefix: Int)(tail: Long*): Int = prefix + tail.size",
      "by-name" -> "def byName(value: => Seq[Int]): Int = value.size"
    ).map((label, source) => label -> Q040ScalametaRepeatedSyntax.inspect(source))
    val parserRejectedRows = List(
      "following-parameter" -> "def following(xs: Int*, y: Int): Int = y",
      "two-repeated" -> "def twice(xs: Int*, ys: String*): Int = 0"
    ).map((label, source) => label -> Q040ScalametaRepeatedSyntax.inspect(source))
    val compilerRejectedButScalametaAcceptedRows = List(
      "repeated-default" -> "def repeatedDefault(xs: Int* = Seq(1)): Int = xs.size",
      "using-repeated" -> "def usingRepeated(using xs: Int*): Int = xs.size",
      "implicit-repeated" -> "def implicitRepeated(implicit xs: Int*): Int = xs.size"
    ).map((label, source) => label -> Q040ScalametaRepeatedSyntax.inspect(source))

    println(
      s"Q040_SCALAMETA_SOURCE ${TermQ3DialectPolicy.compilerVersion} valid=$validRows parserRejected=$parserRejectedRows compilerRejectedButScalametaAccepted=$compilerRejectedButScalametaAcceptedRows"
    )
    assert(validRows.forall(_._2.isRight), validRows)
    assert(parserRejectedRows.forall(_._2.isLeft), parserRejectedRows)
    assert(
      compilerRejectedButScalametaAcceptedRows.forall(_._2.isRight),
      compilerRejectedButScalametaAcceptedRows
    )
    val repeatedDefaultCompilerErrors: List[String] =
      compilerMessages("def repeatedDefault(xs: Int* = Seq(1)): Int = xs.size")
    val contextualCompilerResults: List[List[String]] = List(
      compilerMessages("def usingRepeated(using xs: Int*): Int = xs.size"),
      compilerMessages("def implicitRepeated(implicit xs: Int*): Int = xs.size")
    )
    assert(repeatedDefaultCompilerErrors.nonEmpty, repeatedDefaultCompilerErrors)
    if TermQ3DialectPolicy.compilerVersion == "3.3.8" then
      assert(contextualCompilerResults.forall(_.isEmpty), contextualCompilerResults)
    else
      assert(contextualCompilerResults.forall(_.nonEmpty), contextualCompilerResults)
    val summaries = validRows.map((label, result) => label -> result.toOption.get).toMap
    assertEquals(summaries("plain-seq").parameters.flatten.head.typeFamily, Some("Type.Apply"))
    assertEquals(summaries("plain-seq").parameters.flatten.head.repeatedElementSyntax, None)
    assertEquals(summaries("repeated-only").parameters.flatten.head.typeFamily, Some("Type.Repeated"))
    assertEquals(summaries("repeated-only").parameters.flatten.head.repeatedElementSyntax, Some("Int"))
    assertEquals(summaries("prefix-repeated").parameters.flatten.map(_.name), List("head", "tail"))
    assertEquals(summaries("prefix-repeated").parameters.flatten.last.repeatedElementSyntax, Some("String"))
    assertEquals(summaries("nested-repeated").parameters.flatten.head.repeatedElementFamily, Some("Type.Apply"))
    assertEquals(summaries("nested-repeated").parameters.flatten.head.repeatedElementSyntax, Some("List[Option[Int]]"))
    assertEquals(summaries("generic").typeParameterCount, 1)
    assertEquals(summaries("generic").parameters.flatten.head.repeatedElementSyntax, Some("A"))
    assertEquals(summaries("multiple").clauseModes, List("ordinary", "ordinary"))
    assertEquals(summaries("multiple").parameters.map(_.size), List(1, 1))
    assertEquals(summaries("by-name").parameters.flatten.head.typeFamily, Some("Type.ByName"))

  test("typed-Scalameta recognition delegates exact target semantics to the Quotes candidate"):
    import Q040ScalametaDefinitionPattern.dqq

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
          final def selected(head: Int, tail: List[Option[Int]]*): List[Option[Int]] = tail.head
        ()
      }.asTerm)(Symbol.spliceOwner)
      val target = found.head
      val hybrid = dqq(StringContext("", " def ", "(..", "): ", " = ", ""))(using q)
      val shared = Q040RepeatedOrdinaryCandidateFactory.capturedModifiers(using q)
      val hybridResult = hybrid.unapply(target).get
      val sharedResult = shared.unapply(target).get
      (
        hybridResult._1.flags == sharedResult._1.flags,
        hybridResult._2 == sharedResult._2,
        hybridResult._3.zip(sharedResult._3).forall((left, right) => left eq right),
        hybridResult._3.last.tpt.tpe match
          case AnnotatedType(_, annotation) => annotation.tpe.typeSymbol == defn.RepeatedAnnot
          case _ => false,
        (target.symbol.termRef.widen match
          case method: MethodType =>
            method.paramTypes.lift(1).exists {
              case AppliedType(constructor, List(_)) =>
                constructor.typeSymbol == defn.RepeatedParamClass
              case _ => false
            }
          case _ => false),
        hybridResult._4 =:= sharedResult._4,
        hybridResult._5.eq(sharedResult._5)
      )

    assertEquals(result, (true, true, true, true, true, true, true))

  test("existing typed-Scalameta rank-3 production admits repeated targets while rank-2 stays closed"):
    import quasiquotes.scalameta.ScalametaQuasiPattern.dqq

    given Compiler = Compiler.make(getClass.getClassLoader)
    val row = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*
      val found = scala.collection.mutable.ListBuffer.empty[DefDef]
      val traversal = new TreeTraverser:
        override def traverseTree(tree: Tree)(owner: Symbol): Unit =
          tree match
            case value: DefDef if value.name == "repeated" => found += value
            case _ => ()
          super.traverseTree(tree)(owner)
      traversal.traverseTree('{ def repeated(values: Int*): Int = values.size; () }.asTerm)(Symbol.spliceOwner)
      val target = found.head
      val existing = dqq(StringContext("", " def ", "(...", "): ", " = ", ""))(using q)
      val captured = existing.unapply(target)
      (
        captured.nonEmpty,
        captured.exists(_._3.flatten.head eq target.paramss.head.asInstanceOf[TermParamClause].params.head)
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
        """import scala.quoted.*; import quasiquotes.hybrid.q040.Q040ScalametaDefinitionPattern.dqq
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
      candidateMessages("""case dqq"$mods def $name(..$params): $result = $params" => ()""")
    )
    val dynamic = messages(
      """import scala.quoted.*; import quasiquotes.hybrid.q040.Q040ScalametaDefinitionPattern.dqq
         def f(using q: Quotes)(context: StringContext) = context.dqq"""
    )
    assertEquals(accepted, Nil)
    assert(rejected.forall(_.nonEmpty), rejected)
    assert(rejected.dropRight(1).flatten.forall(_.contains("Invalid Q040 typed-Scalameta dqq")), rejected)
    assert(rejected.last.exists(_.contains("duplicate pattern variable")), rejected.last)
    assert(dynamic.nonEmpty, dynamic)
