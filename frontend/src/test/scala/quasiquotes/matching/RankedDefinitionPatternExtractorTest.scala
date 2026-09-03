package quasiquotes.matching

import scala.compiletime.testing.typeCheckErrors
import scala.language.experimental.erasedDefinitions
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

final class RankedDefinitionPatternExtractorTest extends munit.FunSuite:
  test("external package receives exact ranked Definition binder types"):
    val _ = external.consumer.RankedDefinitionPatternExternalConsumer

  test("ranked Definition matching preserves zero through five parameters and body identity"):
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
          case other => report.errorAndAbort(s"unexpected ranked Definition fixture: ${other.show}")

      val targets = List(
        definition('{ def collect(): Int = 0; () }),
        definition('{ def collect(first: String): Int = first.length; () }),
        definition('{ def collect(first: String, second: Boolean): Int = if second then first.length else 0; () }),
        definition('{ def collect(first: String, second: Boolean, third: Int): Int = if second then first.length + third else third; () }),
        definition('{ def collect(first: String, second: Boolean, third: Int, fourth: Long): Int = first.length + third + fourth.toInt; () }),
        definition('{ def collect(first: String, second: Boolean, third: Int, fourth: Long, fifth: Byte): Int = first.length + third + fourth.toInt + fifth.toInt; () })
      )

      targets.zipWithIndex.map { (target, expectedSize) =>
        target match
          case dqq"def collect(..$params): Int = $body" =>
            val original = target.paramss.head.asInstanceOf[TermParamClause].params
            (
              expectedSize,
              params.size,
              params.zip(original).forall((captured, expected) => captured eq expected),
              params.map(_.symbol) == original.map(_.symbol),
              params.map(_.symbol).distinct.size == params.size,
              params.forall(parameter =>
                parameter.symbol != Symbol.noSymbol && parameter.symbol.owner == target.symbol
              ),
              target.symbol.paramSymss == List(params.map(_.symbol).toList),
              target.rhs.exists(_ eq body)
            )
          case _ => (expectedSize, -1, false, false, false, false, false, false)
      }

    rows.foreach { row =>
      assertEquals(row._2, row._1, row)
      assert(row._3, row)
      assert(row._4, row)
      assert(row._5, row)
      assert(row._6, row)
      assert(row._7, row)
      assert(row._8, row)
    }

  test("ranked Definition extractor rejects the complete bounded target mismatch matrix"):
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

      val exact = definition('{ def collect(first: String, second: Boolean): Int = first.length; () })
      val foreign = definition('{ def collect(first: String, second: Boolean): Int = 0; () })
      val exactClause = exact.paramss.head.asInstanceOf[TermParamClause]
      val foreignOwner = DefDef.copy(exact)(exact.name, foreign.paramss, exact.returnTpt, exact.rhs)
      val reversed = DefDef.copy(exact)(
        exact.name,
        List(TermParamClause(exactClause.params.reverse)),
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
        "two-clauses" -> definition('{ def collect(first: String)(second: Boolean): Int = 0; () }),
        "implicit" -> definition('{ def collect(implicit first: String): Int = first.length; () }),
        "given" -> definition('{ def collect(using first: String): Int = first.length; () }),
        "erased" -> definition('{ def collect(erased first: Int): Int = 0; () }),
        "default" -> definition('{ def collect(first: String = ""): Int = first.length; () }),
        "foreign-owner" -> foreignOwner,
        "paramSymss-reversal" -> reversed,
        "constructor" -> constructor,
        "extension" -> extension,
        "accessor" -> accessor,
        "given-method" -> givenMethod,
        "null" -> null.asInstanceOf[DefDef]
      )
      val extractor = DefinitionPattern.dqq(
        StringContext("def collect(..", "): Int = ", "")
      )(using q)
      targets.map((label, target) => (label, extractor.unapply(target).isEmpty))

    rows.foreach(row => assert(row._2, row))

  test("static ranked Definition diagnostics reject unsupported ranks positions bodies and topology"):
    inline def messages(inline source: String): List[String] =
      typeCheckErrors(source).map(_.message)

    val cases = List(
      messages(
        """import scala.quoted.*; import quasiquotes.matching.DefinitionPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def collect(...$paramss): Int = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.matching.DefinitionPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"..$mods def collect(): Int = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.matching.DefinitionPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def collect(..$left, ..$right): Int = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.matching.DefinitionPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def collect(..): Int = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.matching.DefinitionPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def collect(..$params): Int = $left + $right" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.matching.DefinitionPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def collect(..$params)(other: Int): Int = $body" => ()
             case _ => ()"""
      )
    )

    assert(cases.forall(_.nonEmpty), cases)
    assert(cases.flatten.forall(_.contains("Invalid dqq definition-pattern template")), cases)

  test("dynamic ranked Definition selection does not bypass the exact-one fallback"):
    val errors = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.matching.{DefinitionPattern, RankedDefinitionPatternExtractor}
        def dynamic(using q: Quotes)(context: StringContext): RankedDefinitionPatternExtractor[
          q.reflect.DefDef,
          (Seq[q.reflect.ValDef], q.reflect.Term)
        ] = DefinitionPattern.dqq(context)(using q)
      }"""
    )
    assert(errors.nonEmpty, errors)
