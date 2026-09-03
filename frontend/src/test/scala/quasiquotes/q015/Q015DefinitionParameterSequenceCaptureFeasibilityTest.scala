package quasiquotes.q015

import scala.compiletime.testing.typeCheckErrors
import scala.language.experimental.erasedDefinitions
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

final class Q015DefinitionParameterSequenceCaptureFeasibilityTest extends munit.FunSuite:
  test("strategies B through D expose exact binder types to an external package"):
    val _ = external.consumer.Q015ExternalDefinitionPatternConsumer

  test("strategy A anonymous extractor is rejected at an external package pattern site"):
    val errors = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.q015.Q015StrategyAStandardDefinitionPattern.dqq
        def compile(using q: Quotes)(target: q.reflect.DefDef): Unit =
          target match
            case dqq"def collect(..$params): Int = $body" => ()
            case _ => ()
      }"""
    )
    assert(errors.exists(_.message.contains("cannot be used as an extractor")), errors)

  test("strategy B preserves zero one two and three original parameters plus the original body"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val rows = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*
      import Q015StrategyBStandardDefinitionPattern.dqq

      def definition(expression: Expr[Any]): DefDef =
        expression.asTerm match
          case Inlined(_, _, Block(statements, _)) =>
            statements.collectFirst { case value: DefDef => value }.get
          case Block(statements, _) =>
            statements.collectFirst { case value: DefDef => value }.get
          case other => report.errorAndAbort(s"unexpected Q015 fixture: ${other.show}")

      val targets = List(
        definition('{ def collect(): Int = 0; () }),
        definition('{ def collect(first: String): Int = first.length; () }),
        definition('{ def collect(first: String, second: Boolean): Int = if second then first.length else 0; () }),
        definition('{ def collect(first: String, second: Boolean, third: Int): Int = if second then first.length + third else third; () })
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

  test("strategy B rejects the bounded target mismatch matrix"):
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
      val extractor = Q015StrategyBFactory.extractor(using q)
      targets.map((label, target) => (label, extractor.unapply(target).isEmpty))

    rows.foreach(row => assert(row._2, row))

  test("static Q015 diagnostics reject ranks placements multiplicity bodies and topology"):
    inline def messages(inline source: String): List[String] =
      typeCheckErrors(source).map(_.message)

    val cases = List(
      messages(
        """import scala.quoted.*; import quasiquotes.q015.Q015StrategyBStandardDefinitionPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def collect(...$paramss): Int = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q015.Q015StrategyBStandardDefinitionPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"..$mods def collect(): Int = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q015.Q015StrategyBStandardDefinitionPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def collect(..$left, ..$right): Int = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q015.Q015StrategyBStandardDefinitionPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def collect(..): Int = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q015.Q015StrategyBStandardDefinitionPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def collect(..$params): Int = $left + $right" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q015.Q015StrategyBStandardDefinitionPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def collect(..$params)(other: Int): Int = $body" => ()
             case _ => ()"""
      )
    )

    assert(cases.forall(_.nonEmpty), cases)
    assert(cases.flatten.forall(_.contains("Invalid Q015 standard dqq")), cases)

  test("strategy C replacement breaks bare accepted typing while D loses useful bare capture precision"):
    val cBare = typeCheckErrors(
      """{
        import quasiquotes.q015.GeneralizedDefinitionPatternExtractor
        val pattern: GeneralizedDefinitionPatternExtractor = null
      }"""
    )
    val dBare = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.q015.RefinedDefinitionPatternExtractor
        def compile(using q: Quotes)(pattern: RefinedDefinitionPatternExtractor, target: q.reflect.DefDef) =
          pattern.unapply(target)
      }"""
    )
    assert(cBare.nonEmpty, cBare)
    assert(dBare.nonEmpty, dBare)
