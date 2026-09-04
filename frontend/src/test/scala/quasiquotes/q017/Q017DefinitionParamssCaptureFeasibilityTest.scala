package quasiquotes.q017

import scala.compiletime.testing.typeCheckErrors
import scala.language.experimental.erasedDefinitions
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

final class Q017DefinitionParamssCaptureFeasibilityTest extends munit.FunSuite:
  test("candidates A and B expose exact external-package binder types"):
    val _ = external.consumer.Q017ExternalDefinitionParamssConsumer

  test("both candidates preserve ordinary clause topology identity and zero-versus-empty"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val rows = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*

      def definition(expression: Expr[Any]): DefDef =
        expression.asTerm match
          case Inlined(_, _, Block(statements, _)) =>
            statements.collectFirst { case value: DefDef => value }.get
          case Block(statements, _) =>
            statements.collectFirst { case value: DefDef => value }.get
          case other => report.errorAndAbort(s"unexpected Q017 fixture: ${other.show}")

      val targets = List(
        "zero" -> definition('{ def collect: Int = 0; () }),
        "one-empty" -> definition('{ def collect(): Int = 0; () }),
        "one-nonempty" -> definition('{ def collect(first: String): Int = first.length; () }),
        "two" -> definition('{ def collect(first: String)(second: Boolean, third: Int): Int = if second then first.length + third else third; () }),
        "three-with-empty" -> definition('{ def collect(first: String)()(third: Int, fourth: Long): Int = first.length + third + fourth.toInt; () })
      )
      val expected = List(Nil, List(0), List(1), List(1, 2), List(1, 0, 2))
      val candidateA = Q017CandidateAFactory.extractor(using q)
      val candidateB = Q017CandidateBFactory.extractor(using q)

      targets.zip(expected).map { case ((label, target), expectedCounts) =>
        val original = target.paramss.collect { case clause: TermParamClause => clause }
        val a = candidateA.unapply(target).get
        val b = candidateB.unapply(target).get
        val aParams = a._1
        val bClauses = b._1
        val bParams = bClauses.map(_.params)
        val originalParams = original.map(_.params)
        val flattened = aParams.flatten
        (
          label,
          expectedCounts,
          aParams.map(_.size).toList,
          bClauses.map(_.params.size).toList,
          aParams.zip(originalParams).forall((captured, expected) =>
            captured.zip(expected).forall((left, right) => left eq right)
          ),
          bParams.zip(originalParams).forall((captured, expected) =>
            captured.zip(expected).forall((left, right) => left eq right)
          ),
          bClauses.zip(original).forall((captured, expected) => captured eq expected),
          flattened.map(_.symbol).distinct.size == flattened.size,
          flattened.forall(parameter =>
            parameter.symbol != Symbol.noSymbol && parameter.symbol.owner == target.symbol
          ),
          target.symbol.paramSymss == aParams.map(_.map(_.symbol).toList).toList,
          target.rhs.exists(_ eq a._2) && target.rhs.exists(_ eq b._2)
        )
      }

    rows.foreach { row =>
      assertEquals(row._3, row._2, row)
      assertEquals(row._4, row._2, row)
      assert(row._5, row)
      assert(row._6, row)
      assert(row._7, row)
      assert(row._8, row)
      assert(row._9, row)
      assert(row._10, row)
      assert(row._11, row)
    }

  test("clause modes expose the information retained by each candidate"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val observations = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*

      def definition(expression: Expr[Any]): DefDef =
        val definitions = scala.collection.mutable.ListBuffer.empty[DefDef]
        val traversal = new TreeTraverser:
          override def traverseTree(tree: Tree)(owner: Symbol): Unit =
            tree match
              case value: DefDef if value.name == "collect" => definitions += value
              case _ => ()
            super.traverseTree(tree)(owner)
        traversal.traverseTree(expression.asTerm)(Symbol.spliceOwner)
        definitions.head

      val targets = List(
        "ordinary" -> definition('{ def collect(a: Int)(b: String): Int = a; () }),
        "contextual" -> definition('{ def collect(a: Int)(using b: String): Int = a; () }),
        "given" -> definition('{ def collect(a: Int)(using String): Int = a; () }),
        "implicit" -> definition('{ def collect(a: Int)(implicit b: String): Int = a; () }),
        "erased" -> definition('{ def collect(a: Int)(erased b: Int): Int = a; () })
      )
      val candidateA = Q017CandidateAFactory.extractor(using q)
      val candidateB = Q017CandidateBFactory.extractor(using q)

      targets.map { (label, target) =>
        val a = candidateA.unapply(target).get._1
        val b = candidateB.unapply(target).get._1
        val modes = b.map(clause =>
          (clause.isImplicit, clause.isGiven, clause.isErased)
        )
        val parameterFlags = a.map(_.map(parameter =>
          (
            parameter.symbol.flags.is(Flags.Implicit),
            parameter.symbol.flags.is(Flags.Given),
            parameter.symbol.flags.is(Flags.Erased)
          )
        ))
        (label, modes, parameterFlags)
      }

    observations.foreach(observation => println(s"Q017_CLAUSE_MODE $observation"))
    assertEquals(observations.map(_._1), List("ordinary", "contextual", "given", "implicit", "erased"))
    assert(observations.forall(_._2.size == 2), observations)
    assert(observations.forall(_._3.forall(_.nonEmpty)), observations)
    assert(observations.find(_._1 == "ordinary").exists(_._2.forall(_ == (false, false, false))), observations)
    assert(observations.find(_._1 == "contextual").exists(_._2.last._2), observations)
    assert(observations.find(_._1 == "given").exists(_._2.last._2), observations)
    assert(observations.find(_._1 == "implicit").exists(_._2.last._1), observations)
    assert(observations.find(_._1 == "erased").exists(row => !row._2.last._3), observations)
    assert(observations.find(_._1 == "erased").exists(_._3.last.head._3), observations)

  test("both candidates reject the bounded negative target matrix"):
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

      val exact = definition('{ def collect(first: String)(second: Boolean, third: Int): Int = 0; () })
      val foreign = definition('{ def collect(first: String)(second: Boolean, third: Int): Int = 0; () })
      val exactClauses = exact.paramss.collect { case clause: TermParamClause => clause }
      val foreignOwner = DefDef.copy(exact)(exact.name, foreign.paramss, exact.returnTpt, exact.rhs)
      val reversedAcross = DefDef.copy(exact)(
        exact.name,
        exactClauses.reverse.toList,
        exact.returnTpt,
        exact.rhs
      )
      val reversedWithin = DefDef.copy(exact)(
        exact.name,
        List(exactClauses.head, TermParamClause(exactClauses(1).params.reverse)),
        exact.returnTpt,
        exact.rhs
      )
      val noRhs = DefDef.copy(exact)(exact.name, exact.paramss, exact.returnTpt, None)
      val constructor = definition('{ class Sample(value: Int); () }, Some("<init>"))
      val extension = definition('{
        extension (value: String) def collect(flag: Boolean): Int = value.length
        ()
      }, Some("collect"))
      val accessorSymbol = Symbol.newMethod(
        Symbol.spliceOwner,
        "collect",
        MethodType(Nil)(_ => Nil, _ => TypeRepr.of[Int]),
        Flags.FieldAccessor,
        Symbol.noSymbol
      )
      val accessor = DefDef(accessorSymbol, _ => Some(Literal(IntConstant(0))))
      val givenMethod = definition('{
        given collect(using value: Int): Int = value
        ()
      })

      val targets = List(
        "wrong-method" -> definition('{ def other(): Int = 0; () }),
        "wrong-result" -> definition('{ def collect(): String = ""; () }),
        "no-rhs" -> noRhs,
        "type-parameter-clause" -> definition('{ def collect[A](value: A): Int = 0; () }),
        "default" -> definition('{ def collect(first: String = ""): Int = first.length; () }),
        "foreign-owner" -> foreignOwner,
        "paramSymss-across" -> reversedAcross,
        "paramSymss-within" -> reversedWithin,
        "constructor" -> constructor,
        "extension" -> extension,
        "accessor" -> accessor,
        "given-method" -> givenMethod,
        "null" -> null.asInstanceOf[DefDef]
      )
      val candidateA = Q017CandidateAFactory.extractor(using q)
      val candidateB = Q017CandidateBFactory.extractor(using q)
      targets.map((label, target) =>
        (label, candidateA.unapply(target).isEmpty, candidateB.unapply(target).isEmpty)
      )

    rows.foreach(row => assert(row._2 && row._3, row))

  test("test selector diagnostics reject all non-admitted static rank-3 shapes"):
    inline def messages(inline source: String): List[String] =
      typeCheckErrors(source).map(_.message)

    val cases = List(
      messages(
        """import scala.quoted.*; import quasiquotes.q017.Q017CandidateAStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def collect(..$params): Int = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q017.Q017CandidateAStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"...$mods def collect(): Int = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q017.Q017CandidateAStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def collect(...$left)(...$right): Int = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q017.Q017CandidateAStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def collect(..$params)(...$paramss): Int = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q017.Q017CandidateAStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def collect(...): Int = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q017.Q017CandidateAStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def collect(...$paramss): Int = $left + $right" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q017.Q017CandidateAStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def collect(first: Int)(...$paramss): Int = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q017.Q017CandidateAStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def collect(...$paramss)(last: Int): Int = $body" => ()
             case _ => ()"""
      )
    )

    assert(cases.forall(_.nonEmpty), cases)
    assert(cases.flatten.forall(_.contains("Invalid Q017 standard dqq")), cases)

  test("dynamic rank-3 selection remains closed while the exact production shape is admitted"):
    val dynamicErrors = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.matching.RankedDefinitionPatternExtractor
        import quasiquotes.q017.Q017CandidateAStandardPattern
        def dynamic(using q: Quotes)(context: StringContext): RankedDefinitionPatternExtractor[
          q.reflect.DefDef,
          (Seq[Seq[q.reflect.ValDef]], q.reflect.Term)
        ] = Q017CandidateAStandardPattern.dqq(context)(using q)
      }"""
    )
    val productionErrors = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.matching.DefinitionPattern.dqq
        def compile(using q: Quotes)(target: q.reflect.DefDef): Unit =
          target match
            case dqq"def collect(...$paramss): Int = $body" => ()
            case _ => ()
      }"""
    )
    assert(dynamicErrors.nonEmpty, dynamicErrors)
    assertEquals(productionErrors, Nil)
