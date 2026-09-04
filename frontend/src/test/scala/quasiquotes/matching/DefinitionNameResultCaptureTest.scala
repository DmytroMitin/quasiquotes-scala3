package quasiquotes.matching

import scala.compiletime.testing.typeCheckErrors
import scala.language.experimental.erasedDefinitions
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

final class DefinitionNameResultCaptureTest extends munit.FunSuite:
  test("external packages receive exact direct and umbrella Definition capture types"):
    val _ = external.consumer.Q020ExternalDefinitionCaptureConsumer

  test("production Definition capture preserves semantic names results paramss and bodies"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val rows = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*

      def definition(expression: Expr[Any], expectedName: String): DefDef =
        val definitions = scala.collection.mutable.ListBuffer.empty[DefDef]
        val traversal = new TreeTraverser:
          override def traverseTree(tree: Tree)(owner: Symbol): Unit =
            tree match
              case value: DefDef if value.name == expectedName => definitions += value
              case _ => ()
            super.traverseTree(tree)(owner)
        traversal.traverseTree(expression.asTerm)(Symbol.spliceOwner)
        definitions.head

      val targets = List(
        ("plain", Nil, TypeRepr.of[Int], definition('{ def plain: Int = 1; () }, "plain")),
        ("mixed_Name42", List(0), TypeRepr.of[String], definition('{ def mixed_Name42(): String = "ok"; () }, "mixed_Name42")),
        ("type", List(1), TypeRepr.of[List[Int]], definition('{ def `type`(value: Int): List[Int] = List(value); () }, "type")),
        ("++", List(1, 1), TypeRepr.of[Either[Int, String]], definition('{ def ++(left: Int)(right: String): Either[Int, String] = Left(left); () }, "++")),
        ("tupleResult", List(1, 0, 2), TypeRepr.of[(Int, String)], definition('{ def tupleResult(first: Int)()(second: String, third: Long): (Int, String) = (first, second); () }, "tupleResult")),
        ("functionResult", List(1), TypeRepr.of[Int => String], definition('{ def functionResult(prefix: String): Int => String = value => prefix + value; () }, "functionResult")),
        (
          "nestedResult",
          List(1, 0, 2, 1),
          TypeRepr.of[Map[String, List[Either[Int, (String, Long)]]]],
          definition('{
            def nestedResult(first: Int)()(second: String, third: Long)(fourth: Boolean): Map[String, List[Either[Int, (String, Long)]]] =
              Map(second -> List(if fourth then Left(first) else Right((second, third))))
            ()
          }, "nestedResult")
        )
      )
      val extractor = DefinitionPattern.dqq(
        StringContext("def ", "(...", "): ", " = ", "")
      )(using q)

      targets.map { (expectedName, expectedCounts, expectedResult, target) =>
        val originalClauses = target.paramss.collect { case clause: TermParamClause => clause }
        val capture = extractor.unapply(target).get
        val parameters = capture._2.flatten
        (
          expectedName,
          expectedCounts,
          capture._1,
          target.name,
          capture._2.map(_.size).toList,
          capture._2.zip(originalClauses).forall((captured, original) =>
            captured.zip(original.params).forall((left, right) => left eq right)
          ),
          parameters.map(_.symbol).distinct.size == parameters.size,
          parameters.forall(parameter =>
            parameter.symbol != Symbol.noSymbol && parameter.symbol.owner == target.symbol
          ),
          target.symbol.paramSymss == capture._2.map(_.map(_.symbol).toList).toList,
          capture._3 =:= expectedResult,
          capture._3 =:= target.returnTpt.tpe,
          target.rhs.exists(_ eq capture._4)
        )
      }

    rows.foreach { row =>
      assertEquals(row._3, row._1, row)
      assertEquals(row._4, row._1, row)
      assertEquals(row._5, row._2, row)
      assert(row._6, row)
      assert(row._7, row)
      assert(row._8, row)
      assert(row._9, row)
      assert(row._10, row)
      assert(row._11, row)
      assert(row._12, row)
    }

  test("semantic result capture is independent of explicit source annotation presence"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val result = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*

      def definition(expression: Expr[Any], expectedName: String): DefDef =
        val definitions = scala.collection.mutable.ListBuffer.empty[DefDef]
        val traversal = new TreeTraverser:
          override def traverseTree(tree: Tree)(owner: Symbol): Unit =
            tree match
              case value: DefDef if value.name == expectedName => definitions += value
              case _ => ()
            super.traverseTree(tree)(owner)
        traversal.traverseTree(expression.asTerm)(Symbol.spliceOwner)
        definitions.head

      val inferred = definition('{ def inferred = 1; () }, "inferred")
      val explicit = definition('{ def explicit: Int = 1; () }, "explicit")
      val extractor = DefinitionPattern.dqq(
        StringContext("def ", "(...", "): ", " = ", "")
      )(using q)
      val inferredCapture = extractor.unapply(inferred).get
      val explicitCapture = extractor.unapply(explicit).get
      (
        inferredCapture._3 =:= TypeRepr.of[Int],
        explicitCapture._3 =:= TypeRepr.of[Int],
        inferredCapture._3 =:= inferred.returnTpt.tpe,
        explicitCapture._3 =:= explicit.returnTpt.tpe
      )

    assert(result._1, result)
    assert(result._2, result)
    assert(result._3, result)
    assert(result._4, result)

  test("production Definition capture rejects the retained target boundary"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val rows = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*

      def definition(expression: Expr[Any], name: Option[String] = None): DefDef =
        val definitions = scala.collection.mutable.ListBuffer.empty[DefDef]
        val traversal = new TreeTraverser:
          override def traverseTree(tree: Tree)(owner: Symbol): Unit =
            tree match
              case value: DefDef => definitions += value
              case _ => ()
            super.traverseTree(tree)(owner)
        traversal.traverseTree(expression.asTerm)(Symbol.spliceOwner)
        name.fold(definitions.head)(selected => definitions.find(_.name == selected).get)

      val exact = definition('{ def exact(first: String)(second: Boolean, third: Int): Either[Int, String] = Left(third); () })
      val foreign = definition('{ def foreign(first: String)(second: Boolean, third: Int): Either[Int, String] = Left(third); () })
      val exactClauses = exact.paramss.collect { case clause: TermParamClause => clause }
      val foreignOwner = DefDef.copy(exact)(exact.name, foreign.paramss, exact.returnTpt, exact.rhs)
      val reversedAcross = DefDef.copy(exact)(exact.name, exactClauses.reverse.toList, exact.returnTpt, exact.rhs)
      val reversedWithin = DefDef.copy(exact)(
        exact.name,
        List(exactClauses.head, TermParamClause(exactClauses(1).params.reverse)),
        exact.returnTpt,
        exact.rhs
      )
      val duplicate = DefDef.copy(exact)(
        exact.name,
        List(TermParamClause(List(exactClauses.head.params.head, exactClauses.head.params.head))),
        exact.returnTpt,
        exact.rhs
      )
      val noRhs = DefDef.copy(exact)(exact.name, exact.paramss, exact.returnTpt, None)
      val constructor = definition('{ class Sample(value: Int); () }, Some("<init>"))
      val extension = definition('{
        extension (value: String) def expanded(flag: Boolean): String = value
        ()
      }, Some("expanded"))

      def flaggedAccessor(flags: Flags): DefDef =
        val symbol = Symbol.newMethod(
          Symbol.spliceOwner,
          "accessor",
          MethodType(Nil)(_ => Nil, _ => TypeRepr.of[String]),
          flags,
          Symbol.noSymbol
        )
        DefDef(symbol, _ => Some(Literal(StringConstant("value"))))

      val provided = definition('{
        given provided(using value: Int): String = value.toString
        ()
      })
      val targets = List(
        "missing-rhs" -> noRhs,
        "type-parameter-clause" -> definition('{ def generic[A](value: A): A = value; () }),
        "contextual-using" -> definition('{ def contextual(using value: String): String = value; () }),
        "anonymous-given" -> definition('{ def anonymous(using String): String = ""; () }),
        "implicit" -> definition('{ def implicitClause(implicit value: String): String = value; () }),
        "erased" -> definition('{ def erasedClause(erased value: Int): Int = 0; () }),
        "default" -> definition('{ def defaulted(value: String = ""): String = value; () }),
        "foreign-owner" -> foreignOwner,
        "paramSymss-across" -> reversedAcross,
        "paramSymss-within" -> reversedWithin,
        "duplicate-symbol" -> duplicate,
        "constructor" -> constructor,
        "extension" -> extension,
        "field-accessor" -> flaggedAccessor(Flags.FieldAccessor),
        "param-accessor" -> flaggedAccessor(Flags.ParamAccessor),
        "case-accessor" -> flaggedAccessor(Flags.CaseAccessor),
        "provided-given" -> provided,
        "null" -> null.asInstanceOf[DefDef]
      )
      val extractor = DefinitionPattern.dqq(
        StringContext("def ", "(...", "): ", " = ", "")
      )(using q)
      targets.map((label, target) => (label, extractor.unapply(target).isEmpty))

    rows.foreach(row => assert(row._2, row))

  test("production selector admits only the exact static four-capture layout"):
    inline def messages(inline source: String): List[String] =
      typeCheckErrors(source).map(_.message)

    inline def patternMessages(inline pattern: String): List[String] =
      messages(
        """import scala.quoted.*; import quasiquotes.matching.DefinitionPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             """ + pattern + """
             case _ => ()"""
      )

    val accepted = messages(
      """import scala.quoted.*; import quasiquotes.matching.DefinitionPattern.dqq
         def f(using q: Quotes)(d: q.reflect.DefDef) = d match
           case dqq"def $name(...$paramss): $result = $body" => ()
           case _ => ()"""
    )
    val cases = List(
      patternMessages("""case dqq"def fixed(...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(...$paramss) = $body" => ()"""),
      patternMessages("""case dqq"def $name(...$paramss): Int = $body" => ()"""),
      patternMessages("""case dqq"def $name(..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(first: Int)(...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(...$paramss)(last: Int): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(...$left)(...$right): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(..$params)(...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(...): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(...$paramss): $result = $body + 1" => ()"""),
      patternMessages("""case dqq"def $name(...$paramss): $result = $left + $right" => ()"""),
      patternMessages("""case dqq"private def $name(...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name[A](...$paramss): $result = $body" => ()""")
    )

    assertEquals(accepted, Nil)
    assert(cases.forall(_.nonEmpty), cases)
    assert(cases.flatten.forall(_.contains("Invalid dqq definition-pattern template")), cases)

  test("dynamic selection retains the historical exact-one fallback"):
    val errors = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.matching.{DefinitionPattern, RankedDefinitionPatternExtractor}
        def dynamic(using q: Quotes)(context: StringContext): RankedDefinitionPatternExtractor[
          q.reflect.DefDef,
          (String, Seq[Seq[q.reflect.ValDef]], q.reflect.TypeRepr, q.reflect.Term)
        ] = DefinitionPattern.dqq(context)(using q)
      }"""
    )
    assert(errors.nonEmpty, errors)
