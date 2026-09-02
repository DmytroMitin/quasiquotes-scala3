package quasiquotes.hybrid

import scala.quoted.staging.{Compiler, withQuotes}

import quasiquotes.definitions.hybrid.ScalametaDefinitionFrontend

class Q007ScalametaDefinitionFrontendTest extends munit.FunSuite:
  test("applied tuple and function construction retain the current bounded rejection"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val evidence = withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*

      val callerTypes = List(
        TypeRepr.of[List[Int]],
        TypeRepr.of[(Int, String)],
        TypeRepr.of[Int => String]
      )
      callerTypes.map { callerType =>
        val scalameta = ScalametaDefinitionFrontend
          .build(using q)(
            Seq("def id(value: ", "): ", " = value"),
            Seq(callerType, callerType)
          )
        val current =
          try
            quasiquotes.construct.Quasiquotes.dqr(
              StringContext("def id(value: ", "): ", " = value")
            )(using q)(callerType, callerType)
            "<no-abort>"
          catch
            case error: Throwable => Option(error.getMessage).getOrElse(error.getClass.getName)

        scalameta.left.exists(_.detail.contains("Unsupported type-construction identifier")) &&
          current.contains("Unsupported type-construction identifier")
      }

    assertEquals(evidence, List(true, true, true))

  test("production projector builds the current owner/binder shape with original Types"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val evidence = withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*

      val callerType = TypeRepr.of[Int]
      val definition = ScalametaDefinitionFrontend
        .build(using q)(
          Seq("def id(value: ", "): ", " = value"),
          Seq(callerType, callerType)
        )
        .toOption
        .get
      val parameter = definition.paramss.head.asInstanceOf[TermParamClause].params.head
      val body = definition.rhs.get

      (
        definition.name == "id",
        parameter.name == "value",
        parameter.tpt.tpe.asInstanceOf[AnyRef].eq(callerType.asInstanceOf[AnyRef]),
        definition.returnTpt.tpe.asInstanceOf[AnyRef].eq(callerType.asInstanceOf[AnyRef]),
        definition.symbol.owner == Symbol.spliceOwner,
        parameter.symbol.owner == definition.symbol,
        body match
          case reference: Ref => reference.symbol == parameter.symbol
          case _ => false
      )

    assertEquals(evidence, (true, true, true, true, true, true, true))

  test("production pattern projector reuses the existing matcher and original RHS"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val evidence = withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*

      val callerType = TypeRepr.of[Int]
      val definition = ScalametaDefinitionFrontend
        .build(using q)(
          Seq("def id(value: ", "): ", " = value"),
          Seq(callerType, callerType)
        )
        .toOption
        .get
      val body = definition.rhs.get
      val pattern = ScalametaDefinitionFrontend
        .compilePattern(Seq("def id(value: Int): Int = ", ""))
        .toOption
        .get
      val captured = pattern.unapply(using q)(definition).get
      val mismatch = ScalametaDefinitionFrontend
        .compilePattern(Seq("def other(value: Int): Int = ", ""))
        .toOption
        .get
        .unapply(using q)(definition)
      val parameterMismatch = ScalametaDefinitionFrontend
        .compilePattern(Seq("def id(other: Int): Int = ", ""))
        .toOption
        .get
        .unapply(using q)(definition)
      val parameterTypeMismatch = ScalametaDefinitionFrontend
        .compilePattern(Seq("def id(value: String): Int = ", ""))
        .toOption
        .get
        .unapply(using q)(definition)
      val resultTypeMismatch = ScalametaDefinitionFrontend
        .compilePattern(Seq("def id(value: Int): String = ", ""))
        .toOption
        .get
        .unapply(using q)(definition)
      val unsupportedTarget = '{
        def id(value: Int, other: Int): Int = value
        ()
      }.asTerm match
        case Inlined(_, _, Block((target: DefDef) :: Nil, _)) =>
          pattern.unapply(using q)(target)
        case other =>
          report.errorAndAbort(s"unexpected staged unsupported target: ${other.show}")

      (
        captured.asInstanceOf[AnyRef].eq(body.asInstanceOf[AnyRef]),
        mismatch.isEmpty,
        parameterMismatch.isEmpty,
        parameterTypeMismatch.isEmpty,
        resultTypeMismatch.isEmpty,
        unsupportedTarget.isEmpty
      )

    assertEquals(evidence, (true, true, true, true, true, true))

  test("fixed applied tuple and function pattern Types reuse existing TypeNormalForm semantics"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val evidence = withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*

      def definition(term: Term): DefDef =
        term match
          case Inlined(_, _, body) => definition(body)
          case Block((value: DefDef) :: Nil, _) => value
          case other => report.errorAndAbort(s"unexpected staged Definition fixture: ${other.show}")

      val targets = List(
        definition('{ def id(value: List[Int]): List[Int] = value; () }.asTerm),
        definition('{ def id(value: (Int, String)): (Int, String) = value; () }.asTerm),
        definition('{ def id(value: Int => String): Int => String = value; () }.asTerm)
      )
      val patterns = List(
        "def id(value: List[Int]): List[Int] = ",
        "def id(value: (Int, String)): (Int, String) = ",
        "def id(value: Int => String): Int => String = "
      ).map(source =>
        ScalametaDefinitionFrontend.compilePattern(Seq(source, "")).toOption.get
      )

      targets.zip(patterns).map { (target, pattern) =>
        pattern
          .unapply(using q)(target)
          .exists(_.asInstanceOf[AnyRef].eq(target.rhs.get.asInstanceOf[AnyRef]))
      }

    assertEquals(evidence, List(true, true, true))

  test("construction and pattern sentinels are collision-free and field-confined"):
    val constructionParts = Seq(
      "def __qq_scmeta_definition_type_0(value: ",
      "): ",
      " = value"
    )
    val construction = ScalametaDefinitionFrontend
      .projectConstruction(constructionParts)
      .toOption
      .get
    val patternParts = Seq(
      "def __qq_scmeta_definition_body_0(value: Int): Int = ",
      ""
    )
    val pattern = ScalametaDefinitionFrontend
      .projectPattern(patternParts)
      .toOption
      .get

    assertNotEquals(construction.parameterTypePlaceholder, construction.resultTypePlaceholder)
    assert(!constructionParts.exists(_.contains(construction.parameterTypePlaceholder)))
    assert(!constructionParts.exists(_.contains(construction.resultTypePlaceholder)))
    assert(!patternParts.exists(_.contains(pattern.bodySentinel)))

  test("source normalization, malformed parsing, excluded shapes, and ranks fail closed"):
    val rejectedConstruction = List(
      Seq("def `id`(value: ", "): ", " = value"),
      Seq("def id(/* comment */ value: ", "): ", " = value"),
      Seq("def id[A](value: ", "): ", " = value"),
      Seq("def id(left: ", ", right: Int): ", " = left"),
      Seq("def id(value: ", ")(other: Int): ", " = value"),
      Seq("def id(using value: ", "): ", " = value"),
      Seq("def id(value: ", "): ", " = other"),
      Seq("def id(", "", "")
    )

    rejectedConstruction.foreach(parts =>
      assert(ScalametaDefinitionFrontend.projectConstruction(parts).isLeft, parts)
    )
    val malformed = ScalametaDefinitionFrontend.projectConstruction(Seq("def id(", "", ""))
    assertEquals(malformed.left.toOption.map(_.category), Some("SCALAMETA_PARSE_FAILURE"))

    val rank2 = ScalametaDefinitionFrontend.compilePattern(
      Seq("def id(value: Int): Int = ..", "")
    )
    val rank3 = ScalametaDefinitionFrontend.compilePattern(
      Seq("def id(value: Int): Int = ...", "")
    )
    assert(rank2.left.exists(_.detail.contains("rank-2 captures are not supported")))
    assert(rank3.left.exists(_.detail.contains("rank-3 captures are not supported")))

  test("public malformed and ranked templates report controlled Scalameta Definition diagnostics"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val messages = withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*

      def abortMessage(operation: => Any): String =
        try
          operation
          "<no-abort>"
        catch
          case error: Throwable => Option(error.getMessage).getOrElse(error.getClass.getName)

      val intType = TypeRepr.of[Int]
      val malformed = abortMessage(
        quasiquotes.scalameta.ScalametaQuasiquotes.dqr(
          StringContext("def id(", "", "")
        )(using q)(intType, intType)
      )
      val rank2 = abortMessage(
        quasiquotes.scalameta.ScalametaQuasiPattern.dqq(
          StringContext("def id(value: Int): Int = ..", "")
        )(using q)
      )
      val rank3 = abortMessage(
        quasiquotes.scalameta.ScalametaQuasiPattern.dqq(
          StringContext("def id(value: Int): Int = ...", "")
        )(using q)
      )
      (malformed, rank2, rank3)

    assert(messages._1.contains("Invalid Scalameta dqr definition template"))
    assert(messages._1.contains("SCALAMETA_PARSE_FAILURE"))
    assert(messages._2.contains("rank-2 captures are not supported for Definition patterns"))
    assert(messages._3.contains("rank-3 captures are not supported for Definition patterns"))
