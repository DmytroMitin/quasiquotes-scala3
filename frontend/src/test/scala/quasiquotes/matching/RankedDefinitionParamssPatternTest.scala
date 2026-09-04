package quasiquotes.matching

import scala.compiletime.testing.typeCheckErrors
import scala.language.experimental.erasedDefinitions
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

final class RankedDefinitionParamssPatternTest extends munit.FunSuite:
  test("external direct and umbrella imports receive the exact rank-3 Definition type"):
    val _ = external.consumer.Q018ExternalDefinitionParamssConsumer

  test("rank-3 Definition matching preserves zero empty and N clause topology"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val rows = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*
      import DefinitionPattern.dqq

      def definition(expression: Expr[Any]): DefDef =
        expression.asTerm match
          case Inlined(_, _, Block(statements, _)) =>
            statements.collectFirst { case value: DefDef => value }.get
          case Block(statements, _) =>
            statements.collectFirst { case value: DefDef => value }.get
          case other => report.errorAndAbort(s"unexpected rank-3 Definition fixture: ${other.show}")

      val targets = List(
        definition('{ def collect: Int = 0; () }),
        definition('{ def collect(): Int = 0; () }),
        definition('{ def collect(first: String): Int = first.length; () }),
        definition('{ def collect(first: String)(second: Boolean, third: Int): Int = if second then first.length + third else third; () }),
        definition('{ def collect(first: String)()(third: Int, fourth: Long): Int = first.length + third + fourth.toInt; () }),
        definition('{ def collect(first: String)()(third: Int, fourth: Long)(fifth: Byte): Int = first.length + third + fourth.toInt + fifth.toInt; () })
      )
      val expectedCounts = List(Nil, List(0), List(1), List(1, 2), List(1, 0, 2), List(1, 0, 2, 1))

      targets.zip(expectedCounts).map { (target, expected) =>
        val originalClauses = target.paramss.collect { case clause: TermParamClause => clause }
        target match
          case dqq"def collect(...$paramss): Int = $body" =>
            val flattened = paramss.flatten
            (
              expected,
              paramss.map(_.size).toList,
              paramss.zip(originalClauses).forall((parameters, clause) =>
                parameters.zip(clause.params).forall((captured, original) => captured eq original)
              ),
              paramss.map(_.map(_.symbol)) == originalClauses.map(_.params.map(_.symbol)),
              flattened.map(_.symbol).distinct.size == flattened.size,
              flattened.forall(parameter =>
                parameter.symbol != Symbol.noSymbol && parameter.symbol.owner == target.symbol
              ),
              target.symbol.paramSymss == paramss.map(_.map(_.symbol).toList).toList,
              target.rhs.exists(_ eq body),
              target.returnTpt.tpe =:= TypeRepr.of[Int]
            )
          case _ => (expected, List(-1), false, false, false, false, false, false, false)
      }

    rows.foreach { row =>
      assertEquals(row._2, row._1, row)
      assert(row._3, row)
      assert(row._4, row)
      assert(row._5, row)
      assert(row._6, row)
      assert(row._7, row)
      assert(row._8, row)
      assert(row._9, row)
    }

  test("rank-3 Definition extractor rejects the complete bounded target mismatch matrix"):
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
      val duplicated = DefDef.copy(exact)(
        exact.name,
        List(TermParamClause(List(exactClauses.head.params.head, exactClauses.head.params.head))),
        exact.returnTpt,
        exact.rhs
      )
      val noSymbolParameterIsPubliclyUnconstructible =
        try
          ValDef(Symbol.noSymbol, None)
          false
        catch case _: AssertionError => true
      val noRhs = DefDef.copy(exact)(exact.name, exact.paramss, exact.returnTpt, None)
      val constructor = definition('{ class Sample(value: Int); () }, Some("<init>"))
      val extension = definition('{
        extension (value: String) def collect(flag: Boolean): Int = value.length
        ()
      }, Some("collect"))

      def flaggedAccessor(flags: Flags): DefDef =
        val symbol = Symbol.newMethod(
          Symbol.spliceOwner,
          "collect",
          MethodType(Nil)(_ => Nil, _ => TypeRepr.of[Int]),
          flags,
          Symbol.noSymbol
        )
        DefDef(symbol, _ => Some(Literal(IntConstant(0))))

      val givenMethod = definition('{
        given collect(using value: Int): Int = value
        ()
      })
      val extractor = DefinitionPattern.dqq(
        StringContext("def collect(...", "): Int = ", "")
      )(using q)
      val targets = List(
        "wrong-method" -> definition('{ def other(): Int = 0; () }),
        "wrong-result" -> definition('{ def collect(): String = ""; () }),
        "missing-rhs" -> noRhs,
        "type-parameter-clause" -> definition('{ def collect[A](value: A): Int = 0; () }),
        "contextual-using" -> definition('{ def collect(using first: String): Int = first.length; () }),
        "anonymous-given" -> definition('{ def collect(using String): Int = 0; () }),
        "implicit" -> definition('{ def collect(implicit first: String): Int = first.length; () }),
        "erased" -> definition('{ def collect(erased first: Int): Int = 0; () }),
        "default" -> definition('{ def collect(first: String = ""): Int = first.length; () }),
        "foreign-owner" -> foreignOwner,
        "paramSymss-across" -> reversedAcross,
        "paramSymss-within" -> reversedWithin,
        "duplicated-symbol" -> duplicated,
        "constructor" -> constructor,
        "extension" -> extension,
        "field-accessor" -> flaggedAccessor(Flags.FieldAccessor),
        "param-accessor" -> flaggedAccessor(Flags.ParamAccessor),
        "case-accessor" -> flaggedAccessor(Flags.CaseAccessor),
        "given-method" -> givenMethod,
        "null" -> null.asInstanceOf[DefDef]
      )
      (
        noSymbolParameterIsPubliclyUnconstructible,
        targets.map((label, target) => (label, extractor.unapply(target).isEmpty))
      )

    assert(rows._1, "public reflection must reject constructing a ValDef from noSymbol")
    rows._2.foreach(row => assert(row._2, row))

  test("static rank-3 Definition diagnostics reject unsupported ranks positions bodies and topology"):
    inline def messages(inline source: String): List[String] =
      typeCheckErrors(source).map(_.message)

    val rankMismatch = messages(
      """import scala.quoted.*
         import quasiquotes.matching.{DefinitionPattern, RankedDefinitionPatternExtractor}
         def f(using q: Quotes) =
           val _: RankedDefinitionPatternExtractor[
             q.reflect.DefDef,
             (Seq[Seq[q.reflect.ValDef]], q.reflect.Term)
           ] = DefinitionPattern.dqq(StringContext("def collect(..", "): Int = ", ""))(using q)"""
    )
    val cases = List(
      messages(
        """import scala.quoted.*; import quasiquotes.matching.DefinitionPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"...$mods def collect: Int = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.matching.DefinitionPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def collect(...$left)(...$right): Int = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.matching.DefinitionPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def collect(..$params)(...$paramss): Int = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.matching.DefinitionPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def collect(...): Int = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.matching.DefinitionPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def collect(...$paramss): Int = $left + $right" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.matching.DefinitionPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def collect(first: Int)(...$paramss): Int = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.matching.DefinitionPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def collect(...$paramss)(last: Int): Int = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.matching.DefinitionPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def other(...$paramss): Int = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.matching.DefinitionPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def collect(...$paramss): String = $body" => ()
             case _ => ()"""
      )
    )

    assert(rankMismatch.nonEmpty, rankMismatch)
    assert(cases.forall(_.nonEmpty), cases)
    assert(cases.flatten.forall(_.contains("Invalid dqq definition-pattern template")), cases)

  test("dynamic rank-3 Definition selection keeps the exact-one fallback"):
    val errors = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.matching.{DefinitionPattern, RankedDefinitionPatternExtractor}
        def dynamic(using q: Quotes)(context: StringContext): RankedDefinitionPatternExtractor[
          q.reflect.DefDef,
          (Seq[Seq[q.reflect.ValDef]], q.reflect.Term)
        ] = DefinitionPattern.dqq(context)(using q)
      }"""
    )
    assert(errors.nonEmpty, errors)
