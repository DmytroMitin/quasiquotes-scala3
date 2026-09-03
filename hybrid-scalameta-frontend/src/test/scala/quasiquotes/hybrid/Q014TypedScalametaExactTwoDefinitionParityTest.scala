package quasiquotes.hybrid

import java.nio.file.{Files, Paths}

import scala.compiletime.testing.typeCheckErrors
import scala.jdk.CollectionConverters.*
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

import quasiquotes.definitions.hybrid.ScalametaDefinitionFrontend
import quasiquotes.matching.{DefinitionPatternExtractor, SingleParameterDefinitionPattern}

final class Q014TypedScalametaExactTwoDefinitionParityTest extends munit.FunSuite:
  test("static typed-Scalameta dqq selects precise exact-one and exact-two carriers through direct and umbrella routes"):
    val errors = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.matching.{DefinitionPatternExtractor, SingleParameterDefinitionPattern}

        def direct(using q: Quotes): Unit =
          val one: SingleParameterDefinitionPattern =
            quasiquotes.scalameta.ScalametaQuasiPattern.dqq(
              StringContext("def identity(value: Int): Int = ", "")
            )(using q)
          val two: DefinitionPatternExtractor =
            quasiquotes.scalameta.ScalametaQuasiPattern.dqq(
              StringContext("def first(left: Int, right: String): Int = ", "")
            )(using q)

        def umbrella(using q: Quotes): Unit =
          import quasiquotes.scalameta.Quasiquotes.dqq
          val two: DefinitionPatternExtractor = dqq(
            StringContext("def second(left: Int, right: String): String = ", "")
          )(using q)

        def dynamic(using q: Quotes)(context: StringContext): SingleParameterDefinitionPattern =
          quasiquotes.scalameta.ScalametaQuasiPattern.dqq(context)(using q)
      }"""
    )

    assertEquals(errors, Nil)

  test("direct and umbrella typed-Scalameta dqq pattern syntax preserve exact Term capture typing"):
    val directErrors = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.scalameta.ScalametaQuasiPattern.dqq

        def compile(using q: Quotes)(target: q.reflect.DefDef): Unit =
          target match
            case dqq"def first(left: Int, right: String): Int = $body" =>
              val _: q.reflect.Term = body
            case _ => ()
      }"""
    )
    val umbrellaErrors = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.scalameta.Quasiquotes.dqq

        def compile(using q: Quotes)(target: q.reflect.DefDef): Unit =
          target match
            case dqq"def first(left: Int, right: String): Int = $body" =>
              val _: q.reflect.Term = body
            case _ => ()
      }"""
    )

    assertEquals(directErrors, Nil)
    assertEquals(umbrellaErrors, Nil)

  test("typed-Scalameta exact-two dqr preserves owners order caller Types and selected binder"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val evidence = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*

      def attempt(operation: => DefDef): Either[String, DefDef] =
        try Right(operation)
        catch
          case failure: Throwable =>
            Left(Option(failure.getMessage).getOrElse(failure.getClass.getName))

      def valid(definition: DefDef, firstType: TypeRepr, secondType: TypeRepr, resultType: TypeRepr, selectSecond: Boolean): Boolean =
        definition.paramss match
          case List(clause: TermParamClause) if clause.params.size == 2 =>
            val List(first, second) = clause.params: @unchecked
            val selected = if selectSecond then second else first
            definition.symbol.owner == Symbol.spliceOwner &&
              first.symbol.owner == definition.symbol &&
              second.symbol.owner == definition.symbol &&
              definition.symbol.paramSymss == List(List(first.symbol, second.symbol)) &&
              (first.tpt.tpe.asInstanceOf[AnyRef] eq firstType.asInstanceOf[AnyRef]) &&
              (second.tpt.tpe.asInstanceOf[AnyRef] eq secondType.asInstanceOf[AnyRef]) &&
              (definition.returnTpt.tpe.asInstanceOf[AnyRef] eq resultType.asInstanceOf[AnyRef]) &&
              definition.rhs.exists {
                case reference: Ref => reference.symbol == selected.symbol
                case _ => false
              }
          case _ => false

      val intType = TypeRepr.of[Int]
      val stringType = TypeRepr.of[String]
      val direct = attempt(
        quasiquotes.scalameta.ScalametaQuasiquotes.dqr(
          StringContext("def first(left: ", ", right: ", "): ", " = left")
        )(using q)(intType, stringType, intType)
      )
      val umbrella = attempt {
        import quasiquotes.scalameta.Quasiquotes.dqr
        dqr(
          StringContext("def second(left: ", ", right: ", "): ", " = right")
        )(using q)(intType, stringType, stringType)
      }

      (
        direct.toOption.exists(valid(_, intType, stringType, intType, selectSecond = false)),
        umbrella.toOption.exists(valid(_, intType, stringType, stringType, selectSecond = true)),
        direct.left.toOption,
        umbrella.left.toOption
      )

    assert(evidence._1, evidence._3.getOrElse("direct exact-two construction failed"))
    assert(evidence._2, evidence._4.getOrElse("umbrella exact-two construction failed"))

  test("typed-Scalameta exact-two matching returns the original arbitrary RHS and rejects ordinary mismatch"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val evidence = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*

      def definition(expression: Expr[Any]): DefDef =
        expression.asTerm match
          case Inlined(_, _, Block(statements, _)) =>
            statements.collectFirst { case value: DefDef => value }.get
          case Block(statements, _) =>
            statements.collectFirst { case value: DefDef => value }.get
          case other => report.errorAndAbort(s"unexpected Q014 fixture: ${other.show}")

      val exact = definition('{
        def first(left: Int, right: String): Int = left + right.length
        ()
      })
      val mismatch = definition('{
        def first(left: Int, right: Boolean): Int = left
        ()
      })
      val wrongMethod = definition('{ def other(left: Int, right: String): Int = left; () })
      val wrongFirstName = definition('{ def first(value: Int, right: String): Int = value; () })
      val wrongSecondName = definition('{ def first(left: Int, value: String): Int = left; () })
      val wrongFirstType = definition('{ def first(left: Boolean, right: String): Int = 0; () })
      val wrongResult = definition('{ def first(left: Int, right: String): String = right; () })
      val one = definition('{ def first(left: Int): Int = left; () })
      val three = definition('{ def first(left: Int, right: String, third: Boolean): Int = left; () })
      val twoClauses = definition('{ def first(left: Int)(right: String): Int = left; () })
      val default = definition('{ def first(left: Int = 0, right: String): Int = left; () })
      val contextual = definition('{ def first(using left: Int, right: String): Int = left; () })
      val foreign = definition('{ def first(left: Int, right: String): Int = left; () })
      val direct =
        quasiquotes.scalameta.ScalametaQuasiPattern.dqq(
          StringContext("def first(left: Int, right: String): Int = ", "")
        )(using q)
      val umbrella =
        import quasiquotes.scalameta.Quasiquotes.dqq
        dqq(
          StringContext("def first(left: Int, right: String): Int = ", "")
        )(using q)

      val exactClause = exact.paramss.head.asInstanceOf[TermParamClause]
      val wrongOrder = DefDef.copy(exact)(
        exact.name,
        List(TermParamClause(exactClause.params.reverse)),
        exact.returnTpt,
        exact.rhs
      )
      val foreignOwner = DefDef.copy(exact)(exact.name, foreign.paramss, exact.returnTpt, exact.rhs)

      def rejected(label: String, target: DefDef): (String, Boolean) =
        (label, direct.unapply(using q)(target).isEmpty)

      List(
        ("direct-original-rhs", direct.unapply(using q)(exact).exists(body => exact.rhs.exists(_ eq body))),
        ("umbrella-original-rhs", umbrella.unapply(using q)(exact).exists(body => exact.rhs.exists(_ eq body))),
        rejected("method-name", wrongMethod),
        rejected("first-name", wrongFirstName),
        rejected("second-name", wrongSecondName),
        rejected("first-type", wrongFirstType),
        rejected("second-type", mismatch),
        rejected("result-type", wrongResult),
        rejected("one-parameter", one),
        rejected("three-parameters", three),
        rejected("two-clauses", twoClauses),
        rejected("default", default),
        rejected("contextual", contextual),
        rejected("wrong-order", wrongOrder),
        rejected("foreign-owner", foreignOwner),
        rejected("null", null.asInstanceOf[DefDef])
      )

    evidence.foreach(row => assert(row._2, row))

  test("exact-two construction and pattern templates keep the bounded Type and topology exclusions"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val construction = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*

      val intType = TypeRepr.of[Int]
      val stringType = TypeRepr.of[String]
      val rejectedTypes = List(
        TypeRepr.of[List[Int]],
        TypeRepr.of[Option[Int]],
        TypeRepr.of[Either[Int, String]],
        TypeRepr.of[(Int, String)],
        TypeRepr.of[Int => String]
      )
      val typeMessages = rejectedTypes.map { rejected =>
        ScalametaDefinitionFrontend.build(using q)(
          Seq("def first(left: ", ", right: ", "): ", " = left"),
          Seq(rejected, stringType, rejected)
        ).left.toOption.map(_.message).getOrElse("<accepted>")
      }
      val unequal = ScalametaDefinitionFrontend.build(using q)(
        Seq("def first(left: ", ", right: ", "): ", " = left"),
        Seq(intType, stringType, stringType)
      ).left.toOption.map(_.message).getOrElse("<accepted>")
      val duplicate = ScalametaDefinitionFrontend.build(using q)(
        Seq("def first(value: ", ", value: ", "): ", " = value"),
        Seq(intType, stringType, intType)
      ).left.toOption.map(_.message).getOrElse("<accepted>")
      val wrongBody = ScalametaDefinitionFrontend.build(using q)(
        Seq("def first(left: ", ", right: ", "): ", " = other"),
        Seq(intType, stringType, intType)
      ).left.toOption.map(_.message).getOrElse("<accepted>")

      (typeMessages, unequal, duplicate, wrongBody)

    assert(construction._1.forall(_ != "<accepted>"), construction._1)
    assertNotEquals(construction._2, "<accepted>")
    assertNotEquals(construction._3, "<accepted>")
    assertNotEquals(construction._4, "<accepted>")

    val unsupportedType = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.scalameta.ScalametaQuasiPattern.dqq
        def compile(using q: Quotes)(target: q.reflect.DefDef): Unit =
          target match
            case dqq"def first(left: List[Int], right: String): List[Int] = $body" => ()
            case _ => ()
      }"""
    )
    val multipleClauses = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.scalameta.ScalametaQuasiPattern.dqq
        def compile(using q: Quotes)(target: q.reflect.DefDef): Unit =
          target match
            case dqq"def first(left: Int)(right: String): Int = $body" => ()
            case _ => ()
      }"""
    )
    val ranked = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.scalameta.ScalametaQuasiPattern.dqq
        def compile(using q: Quotes)(target: q.reflect.DefDef): Unit =
          target match
            case dqq"def first(left: Int, right: String): Int = ..$body" => ()
            case _ => ()
      }"""
    )

    assert(unsupportedType.exists(_.message.contains("Invalid Scalameta dqq")), unsupportedType)
    assert(multipleClauses.exists(_.message.contains("Invalid Scalameta dqq")), multipleClauses)
    assert(ranked.exists(_.message.contains("rank-2 captures are not supported")), ranked)

  test("exact-two Scalameta source projection rejects the complete bounded topology and normalization matrix"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val construction = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*

      val types = Seq(TypeRepr.of[Int], TypeRepr.of[String], TypeRepr.of[Int])
      val rows = List(
        Seq("def f(x: ", "): ", " = x + ", ""),
        Seq("def f(a: Int, b: ", ", c: ", "): ", " = b"),
        Seq("def f(x: ", ")(y: ", "): ", " = x"),
        Seq("def f(using x: ", ", y: ", "): ", " = x"),
        Seq("def f(implicit x: ", ", y: ", "): ", " = x"),
        Seq("def f(erased x: ", ", erased y: ", "): ", " = x"),
        Seq("def f(x: ", " = 0, y: ", "): ", " = x"),
        Seq("private def f(x: ", ", y: ", "): ", " = x"),
        Seq("def f[A](x: ", ", y: ", "): ", " = x"),
        Seq("def `f`(x: ", ", y: ", "): ", " = x"),
        Seq("def f(/* comment */ x: ", ", y: ", "): ", " = x"),
        Seq("def f(", ",", "):", "")
      )

      (
        rows.map(parts => ScalametaDefinitionFrontend.build(using q)(parts, types).isLeft),
        ScalametaDefinitionFrontend.build(using q)(null, types).isLeft,
        ScalametaDefinitionFrontend
          .build(using q)(
            Seq("def f(x: ", ", y: ", "): ", " = x"),
            null.asInstanceOf[Seq[TypeRepr]]
          )
          .isLeft
      )

    assert(construction._1.forall(identity), construction._1)
    assert(construction._2)
    assert(construction._3)

    val rejectedPatterns = List(
      Seq("not a definition ", ""),
      Seq("def f(x: Int): Int = ", ""),
      Seq("def f(x: Int, y: String, z: Boolean): Int = ", ""),
      Seq("def f(x: Int)(y: String): Int = ", ""),
      Seq("def f(using x: Int, y: String): Int = ", ""),
      Seq("def f(x: Int = 0, y: String): Int = ", ""),
      Seq("private def f(x: Int, y: String): Int = ", ""),
      Seq("def f[A](x: Int, y: String): Int = ", ""),
      Seq("def `f`(x: Int, y: String): Int = ", ""),
      Seq("def f(/* comment */ x: Int, y: String): Int = ", ""),
      Seq("def f(x: (Int, String), y: String): (Int, String) = ", ""),
      Seq("def f(x: Int => String, y: String): Int => String = ", ""),
      Seq("def f(x: Int, y: String): Int = ", " + ", "")
    )
    rejectedPatterns.foreach(parts =>
      assert(ScalametaDefinitionFrontend.projectExactTwoPattern(parts).isLeft, parts)
    )

  test("the historical typed-Scalameta dqq JVM descriptor remains callable externally"):
    val method = Class
      .forName("quasiquotes.scalameta.ScalametaQuasiPattern$")
      .getMethods
      .find(method =>
        method.getName == "dqq" &&
          method.getParameterTypes.toList.map(_.getName) ==
            List("scala.StringContext", "scala.quoted.Quotes")
      )
    assert(method.nonEmpty)
    assertEquals(method.get.getReturnType.getName, "quasiquotes.matching.SingleParameterDefinitionPattern")

    val pattern = external.consumer.Q014LegacyScalametaDqqJvmConsumer.build(
      StringContext("def identity(value: Int): Int = ", ""),
      null
    )
    assert(pattern != null)

  test("typed-Scalameta production exposes no dqqN or arity-bound Definition-pattern family"):
    val sourceRoot = Paths.get(
      sys.props("user.dir"), "hybrid-scalameta-frontend", "src", "main", "scala"
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
    assert(!production.contains("TwoParameterDefinitionPattern"))
    assert(!production.contains("ThreeParameterDefinitionPattern"))
    assert(!production.contains("NParameterDefinitionPattern"))
