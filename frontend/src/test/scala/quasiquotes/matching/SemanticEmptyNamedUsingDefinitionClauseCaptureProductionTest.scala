package quasiquotes.matching

import scala.annotation.StaticAnnotation
import scala.compiletime.testing.typeCheckErrors
import scala.language.experimental.erasedDefinitions
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

final class Q029Annotation extends StaticAnnotation

final class Q029InlineFixture:
  inline def inlineUsing(using ordering: Ordering[Int]): Int = 1
  transparent inline def transparentUsing(using ordering: Ordering[Int]): Int = 1

final class SemanticEmptyNamedUsingDefinitionClauseCaptureProductionTest extends munit.FunSuite:
  test("external packages receive exact direct and umbrella Q029 capture types"):
    val _ = external.consumer.Q029ExternalSemanticEmptyNamedUsingDefinitionClauseConsumer

  test("Q029 captures exact name binders result and body for 1 2 3 and N named using parameters"):
    import quasiquotes.Quasiquotes.dqq

    given Compiler = Compiler.make(getClass.getClassLoader)
    val rows = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*

      val definitions = fixtureDefinitions(using q)
      val omitted = dqq(StringContext("def ", "(using ..", "): ", " = ", ""))(using q)
      val captured = dqq(StringContext("", " def ", "(using ..", "): ", " = ", ""))(using q)

      List("one", "two", "three", "many").map { name =>
        val target = definitions(name)
        val clause = target.paramss.head.asInstanceOf[TermParamClause]
        val result = omitted.unapply(target).get
        (
          name,
          result._1 == target.name,
          result._2.size,
          result._2.zip(clause.params).forall((left, right) => left eq right),
          result._2.map(_.symbol) == clause.params.map(_.symbol),
          result._2.forall(parameter =>
            parameter.symbol != Symbol.noSymbol &&
              parameter.symbol.owner == target.symbol &&
              parameter.symbol.flags.is(Flags.Given) &&
              !parameter.symbol.flags.is(Flags.Implicit) &&
              !parameter.symbol.flags.is(Flags.Synthetic) &&
              !parameter.symbol.flags.is(Flags.Erased) &&
              !parameter.symbol.flags.is(Flags.HasDefault)
          ),
          result._2.map(_.symbol).distinct.size == result._2.size,
          target.symbol.paramSymss == List(clause.params.map(_.symbol)),
          result._3 =:= target.returnTpt.tpe,
          target.rhs.exists(_ eq result._4),
          captured.unapply(target).nonEmpty
        )
      }

    rows.foreach { row =>
      assert(row._2, row)
      assert(row._4, row)
      assert(row._5, row)
      assert(row._6, row)
      assert(row._7, row)
      assert(row._8, row)
      assert(row._9, row)
      assert(row._10, row)
      assert(row._11, row)
    }
    val cardinalities = rows.map(row => row._1 -> row._3).toMap
    assertEquals(cardinalities, Map("one" -> 1, "two" -> 2, "three" -> 3, "many" -> 4))

  test("Q028 and Q029 distinguish captured from omitted method modifiers on the same targets"):
    import quasiquotes.matching.DefinitionPattern.dqq

    given Compiler = Compiler.make(getClass.getClassLoader)
    val rows = withQuotes:
      val q = summon[Quotes]
      val definitions = fixtureDefinitions(using q)
      val omitted = dqq(StringContext("def ", "(using ..", "): ", " = ", ""))(using q)
      val captured = dqq(StringContext("", " def ", "(using ..", "): ", " = ", ""))(using q)
      val names = List(
        "one",
        "privateUsing",
        "protectedUsing",
        "finalUsing",
        "overrideUsing",
        "annotatedUsing",
        "qualifiedPrivateUsing",
        "qualifiedProtectedUsing",
        "implicitUsing",
        "infixUsing",
        "inlineUsing",
        "transparentUsing"
      )
      names.map(name => (name, captured.unapply(definitions(name)).nonEmpty, omitted.unapply(definitions(name)).nonEmpty))

    assertEquals(rows.head, ("one", true, true))
    rows.tail.foreach(row => assertEquals(row, (row._1, true, false)))

  test("Q029 rejects the complete nonordinary and structural target matrix"):
    import quasiquotes.matching.DefinitionPattern.dqq

    given Compiler = Compiler.make(getClass.getClassLoader)
    val rows = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*

      def definition(expression: Expr[Any], expectedName: String): DefDef =
        val found = scala.collection.mutable.ListBuffer.empty[DefDef]
        val traversal = new TreeTraverser:
          override def traverseTree(tree: Tree)(owner: Symbol): Unit =
            tree match
              case value: DefDef if value.name == expectedName => found += value
              case _ => ()
            super.traverseTree(tree)(owner)
        traversal.traverseTree(expression.asTerm)(Symbol.spliceOwner)
        found.head

      val exact = definition('{ def exact(using first: Ordering[Int], second: Numeric[Int]): Int = 1; () }, "exact")
      val foreign = definition('{ def foreign(using first: Ordering[Int], second: Numeric[Int]): Int = 1; () }, "foreign")
      val clause = exact.paramss.head.asInstanceOf[TermParamClause]
      val extractor = dqq(StringContext("def ", "(using ..", "): ", " = ", ""))(using q)
      val constructor = definition('{ class Sample(using value: Int); () }, "<init>")
      val extension = definition('{ extension (value: Int) def expanded(using other: Int): Int = value + other; () }, "expanded")
      val provided = definition('{ given provided(using value: Int): Int = value; () }, "provided")
      def flaggedAccessor(flags: Flags): DefDef =
        val symbol = Symbol.newMethod(
          Symbol.spliceOwner,
          "accessor",
          MethodType(Nil)(_ => Nil, _ => TypeRepr.of[String]),
          flags,
          Symbol.noSymbol
        )
        DefDef(symbol, _ => Some(Literal(StringConstant("value"))))

      val targets = List(
        "ordinary" -> definition('{ def ordinary(value: Int): Int = value; () }, "ordinary"),
        "zero-clauses" -> definition('{ def zero: Int = 0; () }, "zero"),
        "empty-ordinary" -> definition('{ def empty(): Int = 0; () }, "empty"),
        "anonymous-using" -> definition('{ def anonymous(using Ordering[Int]): Int = 1; () }, "anonymous"),
        "scala2-implicit" -> definition('{ def old(implicit value: Int): Int = value; () }, "old"),
        "erased" -> definition('{ def erasedClause(erased value: Int): Int = 0; () }, "erasedClause"),
        "mixed" -> definition('{ def mixed(value: Int)(using ordering: Ordering[Int]): Int = value; () }, "mixed"),
        "multiple-using" -> definition('{ def multiple(using ordering: Ordering[Int])(using numeric: Numeric[Int]): Int = 1; () }, "multiple"),
        "context-bound" -> definition('{ def bounded[A: Ordering]: Int = 1; () }, "bounded"),
        "context-bound-ordinary" -> definition('{ def boundedOrdinary[A: Ordering](value: A): A = value; () }, "boundedOrdinary"),
        "context-bound-using" -> definition('{ def boundedUsing[A: Ordering](using marker: Numeric[Int]): Int = 1; () }, "boundedUsing"),
        "generic" -> definition('{ def generic[A](using value: Ordering[A]): Int = 1; () }, "generic"),
        "default" -> definition('{ def defaulted(using value: Int = 1): Int = value; () }, "defaulted"),
        "foreign-owner" -> DefDef.copy(exact)(exact.name, foreign.paramss, exact.returnTpt, exact.rhs),
        "duplicate-symbol" -> DefDef.copy(exact)(exact.name, List(TermParamClause(List(clause.params.head, clause.params.head))), exact.returnTpt, exact.rhs),
        "reordered-symbols" -> DefDef.copy(exact)(exact.name, List(TermParamClause(clause.params.reverse)), exact.returnTpt, exact.rhs),
        "param-symss-mismatch" -> DefDef.copy(exact)(exact.name, List(TermParamClause(List(clause.params.head))), exact.returnTpt, exact.rhs),
        "missing-rhs" -> DefDef.copy(exact)(exact.name, exact.paramss, exact.returnTpt, None),
        "constructor" -> constructor,
        "extension" -> extension,
        "field-accessor" -> flaggedAccessor(Flags.FieldAccessor),
        "param-accessor" -> flaggedAccessor(Flags.ParamAccessor),
        "case-accessor" -> flaggedAccessor(Flags.CaseAccessor),
        "given" -> provided,
        "null" -> null.asInstanceOf[DefDef]
      )
      targets.map((label, target) => label -> extractor.unapply(target).isEmpty)

    rows.foreach(row => assert(row._2, row))

  test("standard selector keeps Q029 and Q028 as exact distinct static grammars"):
    inline def messages(inline source: String): List[String] = typeCheckErrors(source).map(_.message)
    inline def patternMessages(inline pattern: String): List[String] =
      messages(
        """import scala.quoted.*; import quasiquotes.matching.DefinitionPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             """ + pattern + """
             case _ => ()"""
      )

    val q029 = patternMessages("""case dqq"def $name(using ..$params): $result = $body" => ()""")
    val q028 = patternMessages("""case dqq"$mods def $name(using ..$params): $result = $body" => ()""")
    val rejected = List(
      patternMessages("""case dqq"private def $name(using ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"def fixed(using ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(usingx ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(using fixed: Int, ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(using ..$params, fixed: Int): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(using ...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name[..$tparams](using ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(value: Int)(using ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(using ..$first)(using ..$second): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(implicit ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(using erased ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(using ..$params): Int = $body" => ()"""),
      patternMessages("""case dqq"def $name(using ..$params): $result = $body + 1" => ()"""),
      patternMessages("""case dqq"def $name(using ..$params): $result = $left + $right" => ()"""),
      patternMessages("""case dqq"def $name(using ..$params): $result = $body extra" => ()"""),
      patternMessages("""case dqq"def $name(using .$params): $result = $body" => ()""")
    )

    assertEquals(q029, Nil)
    assertEquals(q028, Nil)
    assert(rejected.forall(_.nonEmpty), rejected)
    assert(rejected.flatten.forall(_.contains("Invalid dqq definition-pattern template")), rejected)

  test("dynamic Q029 selection retains the historical exact-one fallback"):
    val errors = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.matching.{DefinitionPattern, RankedDefinitionPatternExtractor}
         def f(using q: Quotes)(context: StringContext): RankedDefinitionPatternExtractor[
           q.reflect.DefDef,
           (String, Seq[q.reflect.ValDef], q.reflect.TypeRepr, q.reflect.Term)
         ] = DefinitionPattern.dqq(context)(using q)"""
    )
    assert(errors.nonEmpty, errors)

  private def fixtureDefinitions(using q: Quotes): Map[String, q.reflect.DefDef] =
    import q.reflect.*

    val definitions = scala.collection.mutable.Map.empty[String, DefDef]
    val traversal = new TreeTraverser:
      override def traverseTree(tree: Tree)(owner: Symbol): Unit =
        tree match
          case value: DefDef if !value.symbol.isClassConstructor => definitions.update(value.name, value)
          case _ => ()
        super.traverseTree(tree)(owner)

    traversal.traverseTree(
      '{
        trait Base:
          def overrideUsing(using ordering: Ordering[Int]): Int

        class Fixture extends Base:
          def one(using ordering: Ordering[Int]): Int = 1
          def two(using ordering: Ordering[Int], numeric: Numeric[Int]): Int = 2
          def three(using first: Ordering[Int], second: Numeric[Int], third: CanEqual[Int, Int]): Int = 3
          def many(using first: Ordering[Int], second: Numeric[Int], third: CanEqual[Int, Int], fourth: ValueOf[1]): Int = 4
          private def privateUsing(using ordering: Ordering[Int]): Int = 1
          protected def protectedUsing(using ordering: Ordering[Int]): Int = 1
          final def finalUsing(using ordering: Ordering[Int]): Int = 1
          override def overrideUsing(using ordering: Ordering[Int]): Int = 1
          @Q029Annotation def annotatedUsing(using ordering: Ordering[Int]): Int = 1
          private[matching] def qualifiedPrivateUsing(using ordering: Ordering[Int]): Int = 1
          protected[matching] def qualifiedProtectedUsing(using ordering: Ordering[Int]): Int = 1
          implicit def implicitUsing(using ordering: Ordering[Int]): Int = 1
          infix def infixUsing(using ordering: Ordering[Int]): Int = 1
        ()
      }.asTerm
    )(Symbol.spliceOwner)

    def inlineDefinition(name: String): DefDef =
      val target =
        TypeRepr.of[Q029InlineFixture].typeSymbol.declaredMethod(name).head.tree.asInstanceOf[DefDef]
      DefDef.copy(target)(target.name, target.paramss, target.returnTpt, Some(Literal(IntConstant(1))))

    definitions.update("inlineUsing", inlineDefinition("inlineUsing"))
    definitions.update(
      "transparentUsing",
      inlineDefinition("transparentUsing")
    )
    definitions.toMap
