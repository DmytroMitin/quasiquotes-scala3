package quasiquotes.q012r

import java.nio.file.{Files, Paths}

import scala.compiletime.testing.typeCheckErrors
import scala.jdk.CollectionConverters.*
import scala.language.experimental.erasedDefinitions
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

import quasiquotes.construct.{PublicDefinitionQuasiquote, Quasiquotes, TypedTwoParameterDefinitionLowerer}
import quasiquotes.definitions.DefinitionName
import quasiquotes.matching.SingleParameterDefinitionPattern

final class Q012RSameSpellingDefinitionTest extends munit.FunSuite:
  test("static one and two parameter dqq calls keep precise types and dynamic calls keep the single fallback"):
    assert(Q012RSameSpellingDefinitionProbe.verify)

  test("dqr exact-two construction preserves ownership order Type identity and selected binder"):
    Q012RDefinitionEvidence.construction.foreach { row =>
      assert(row._2, row)
      assert(row._3, row)
      assert(row._4, row)
      assert(row._5, row)
      assert(row._6, row)
    }

  test("exact-two matching returns original arbitrary RHS and rejects the complete bounded mismatch matrix"):
    val rows = Q012RDefinitionEvidence.mismatches
    rows.foreach(row => assert(row._2, row))
    assertEquals(rows.map(_._1), List(
      "success-arbitrary-rhs",
      "method-name",
      "first-name",
      "second-name",
      "first-type",
      "second-type",
      "result-type",
      "one-parameter",
      "three-parameters",
      "two-clauses",
      "default",
      "contextual-given",
      "implicit",
      "erased",
      "foreign-owner",
      "wrong-param-order",
      "null"
    ))

  test("static invalid dqq templates fail with controlled diagnostics"):
    inline def messages(inline source: String): List[String] =
      typeCheckErrors(source).map(_.message)

    val malformed = messages(
      """import scala.quoted.*; import quasiquotes.matching.DefinitionPattern.*
         def f(using q: Quotes)(d: q.reflect.DefDef) = d match
           case dqq"not a definition $body" => ()
           case _ => ()"""
    )
    val unsupportedType = messages(
      """import scala.quoted.*; import quasiquotes.matching.DefinitionPattern.*
         def f(using q: Quotes)(d: q.reflect.DefDef) = d match
           case dqq"def f(x: List[Int], y: String): String = $body" => ()
           case _ => ()"""
    )
    val twoBodies = messages(
      """import scala.quoted.*; import quasiquotes.matching.DefinitionPattern.*
         def f(using q: Quotes)(d: q.reflect.DefDef) = d match
           case dqq"def f(x: Int, y: String): Int = $left + $right" => ()
           case _ => ()"""
    )
    val rankTwo = messages(
      """import scala.quoted.*; import quasiquotes.matching.DefinitionPattern.*
         def f(using q: Quotes)(d: q.reflect.DefDef) = d match
           case dqq"def f(x: Int, y: String): Int = ..$body" => ()
           case _ => ()"""
    )
    val rankThree = messages(
      """import scala.quoted.*; import quasiquotes.matching.DefinitionPattern.*
         def f(using q: Quotes)(d: q.reflect.DefDef) = d match
           case dqq"def f(x: Int, y: String): Int = ...$body" => ()
           case _ => ()"""
    )
    val topology = messages(
      """import scala.quoted.*; import quasiquotes.matching.DefinitionPattern.*
         def f(using q: Quotes)(d: q.reflect.DefDef) = d match
           case dqq"def f(x: Int)(y: String): Int = $body" => ()
           case _ => ()"""
    )

    assert(malformed.exists(_.contains("Invalid dqq definition-pattern template")), malformed)
    assert(unsupportedType.exists(_.contains("standalone Int/String/Boolean")), unsupportedType)
    assert(twoBodies.exists(_.contains("exactly one body capture slot")), twoBodies)
    assert(rankTwo.exists(_.contains("rank-2 captures are not supported")), rankTwo)
    assert(rankThree.exists(_.contains("rank-3 captures are not supported")), rankThree)
    assert(topology.exists(_.contains("Invalid exact-two definition pattern")), topology)

  test("dqr exact-two exclusions fail with controlled diagnostics and one-parameter diagnostics remain stable"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val messages = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*

      def abortMessage(operation: => Any): String =
        try
          operation
          "<no-abort>"
        catch case error: Throwable => Option(error.getMessage).getOrElse(error.getClass.getName)

      val intType = TypeRepr.of[Int]
      val stringType = TypeRepr.of[String]
      val listInt = TypeRepr.of[List[Int]]
      val tuple = TypeRepr.of[(Int, String)]
      val function = TypeRepr.of[Int => String]
      val anyVal = TypeRepr.of[AnyVal]

      List(
        abortMessage(Quasiquotes.dqr(StringContext("def f(x: ", "): ", " = x"))(using q)(intType)),
        abortMessage(Quasiquotes.dqr(StringContext("def f(x: ", ", x: ", "): ", " = x"))(using q)(intType, stringType, intType)),
        abortMessage(Quasiquotes.dqr(StringContext("def f(x: ", ", y: ", "): ", " = z"))(using q)(intType, stringType, intType)),
        abortMessage(Quasiquotes.dqr(StringContext("def f(x: ", ", y: ", "): ", " = x"))(using q)(intType, stringType, stringType)),
        abortMessage(Quasiquotes.dqr(StringContext("def f(x: ", ", y: ", "): ", " = x"))(using q)(listInt, stringType, listInt)),
        abortMessage(Quasiquotes.dqr(StringContext("def f(x: ", ", y: ", "): ", " = x"))(using q)(tuple, stringType, tuple)),
        abortMessage(Quasiquotes.dqr(StringContext("def f(x: ", ", y: ", "): ", " = x"))(using q)(function, stringType, function)),
        abortMessage(Quasiquotes.dqr(StringContext("def f(x: ", ", y: ", "): ", " = x"))(using q)(anyVal, stringType, anyVal)),
        abortMessage(Quasiquotes.dqr(StringContext("def f[T](x: ", ", y: ", "): ", " = x"))(using q)(intType, stringType, intType)),
        abortMessage(Quasiquotes.dqr(StringContext("def f(x: ", ")(y: ", "): ", " = x"))(using q)(intType, stringType, intType)),
        abortMessage(Quasiquotes.dqr(StringContext("def f(using x: ", ", y: ", "): ", " = x"))(using q)(intType, stringType, intType)),
        abortMessage(Quasiquotes.dqr(StringContext("def f(x: ", " = 0, y: ", "): ", " = x"))(using q)(intType, stringType, intType)),
        abortMessage(Quasiquotes.dqr(StringContext("def f(x: ", ", y: ", ", z: ", "): ", " = x"))(using q)(intType, stringType, intType, intType)),
        abortMessage(Quasiquotes.dqr(StringContext("private def f(x: ", ", y: ", "): ", " = x"))(using q)(intType, stringType, intType)),
        abortMessage(PublicDefinitionQuasiquote.build(using q)(null, Seq(intType, stringType, intType))),
        abortMessage(PublicDefinitionQuasiquote.build(using q)(StringContext("def f(x: ", ", y: ", "): ", " = x"), null)),
        abortMessage(
          TypedTwoParameterDefinitionLowerer.lower(using q)(
            DefinitionName.plain("f").toOption.get,
            DefinitionName.plain("x").toOption.get,
            null.asInstanceOf[TypeRepr],
            DefinitionName.plain("y").toOption.get,
            stringType,
            intType,
            DefinitionName.plain("x").toOption.get
          ).fold(message => throw new IllegalArgumentException(message), identity)
        ),
        abortMessage(Quasiquotes.dqr(StringContext("def f(x: ", "): ", " = notX"))(using q)(intType, intType))
      )

    assert(messages.forall(_ != "<no-abort>"), messages.mkString(" | "))
    assert(messages.last.contains("body must reference the declared parameter name exactly"), messages.last)

  test("the old dqq JVM descriptor remains callable by an external Java consumer"):
    val method = Class
      .forName("quasiquotes.matching.DefinitionPattern$")
      .getMethods
      .find(method =>
        method.getName == "dqq" &&
          method.getParameterTypes.toList.map(_.getName) ==
            List("scala.StringContext", "scala.quoted.Quotes")
      )
    assert(method.nonEmpty)
    assertEquals(method.get.getReturnType.getName, "quasiquotes.matching.SingleParameterDefinitionPattern")

    val pattern: SingleParameterDefinitionPattern =
      external.consumer.Q012RLegacyDqqJvmConsumer.build(
        StringContext("def identity(value: Int): Int = ", ""),
        null
      )
    assert(pattern != null)

  test("production source contains one dqq spelling and no arity-numbered interpolator"):
    val sourceRoot = Paths.get(
      sys.props("user.dir"), "frontend", "src", "main", "scala"
    )
    val files = Files.walk(sourceRoot)
    val production =
      try
        files.iterator.asScala
          .filter(path => path.toString.endsWith(".scala"))
          .map(Files.readString)
          .mkString("\n")
      finally files.close()
    assert(!production.contains("def dqq2"))
    assert(!production.contains("def dqq3"))
    assert(!production.contains("def dqq4"))
