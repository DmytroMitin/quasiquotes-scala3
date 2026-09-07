package quasiquotes.matching

import scala.annotation.StaticAnnotation
import scala.compiletime.testing.typeCheckErrors
import scala.language.experimental.erasedDefinitions
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

final class Q028Annotation extends StaticAnnotation

final class NamedUsingDefinitionClauseCaptureProductionTest extends munit.FunSuite:
  test("external packages receive exact direct and umbrella Q028 capture types"):
    val _ = external.consumer.Q028ExternalNamedUsingDefinitionClauseConsumer

  test("Q028 captures exact modifiers binders result and body for 1 2 and N named using parameters"):
    import quasiquotes.Quasiquotes.dqq

    given Compiler = Compiler.make(getClass.getClassLoader)
    val rows = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*

      val definitions = fixtureDefinitions(using q)
      val extractor = dqq(StringContext("", " def ", "(using ..", "): ", " = ", ""))(using q)
      val names = List(
        "one",
        "two",
        "three",
        "many",
        "privateUsing",
        "finalUsing",
        "annotatedUsing",
        "qualifiedUsing"
      )

      def sameScope(left: Option[TypeRepr], right: Option[TypeRepr]): Boolean =
        (left, right) match
          case (None, None) => true
          case (Some(a), Some(b)) => a =:= b
          case _ => false

      names.map { name =>
        val target = definitions(name)
        val clause = target.paramss.head.asInstanceOf[TermParamClause]
        val captured = extractor.unapply(target).get
        val modifiers = captured._1
        (
          name,
          modifiers.flags == target.symbol.flags,
          sameScope(modifiers.privateWithin, target.symbol.privateWithin),
          sameScope(modifiers.protectedWithin, target.symbol.protectedWithin),
          modifiers.annotations.size == target.symbol.annotations.size &&
            modifiers.annotations.zip(target.symbol.annotations).forall((left, right) => left eq right),
          captured._2 == target.name,
          captured._3.size,
          captured._3.zip(clause.params).forall((left, right) => left eq right),
          captured._3.map(_.symbol) == clause.params.map(_.symbol),
          captured._3.forall(parameter =>
            parameter.symbol != Symbol.noSymbol &&
              parameter.symbol.owner == target.symbol &&
              parameter.symbol.flags.is(Flags.Given) &&
              !parameter.symbol.flags.is(Flags.Synthetic) &&
              !parameter.symbol.flags.is(Flags.Erased) &&
              !parameter.symbol.flags.is(Flags.HasDefault)
          ),
          captured._3.map(_.symbol).distinct.size == captured._3.size,
          target.symbol.paramSymss == List(clause.params.map(_.symbol)),
          captured._4 =:= target.returnTpt.tpe,
          target.rhs.exists(_ eq captured._5)
        )
      }

    rows.foreach { row =>
      assert(row._2, row)
      assert(row._3, row)
      assert(row._4, row)
      assert(row._5, row)
      assert(row._6, row)
      assert(row._8, row)
      assert(row._9, row)
      assert(row._10, row)
      assert(row._11, row)
      assert(row._12, row)
      assert(row._13, row)
      assert(row._14, row)
    }
    val cardinalities = rows.map(row => row._1 -> row._7).toMap
    assertEquals(cardinalities("one"), 1)
    assertEquals(cardinalities("two"), 2)
    assertEquals(cardinalities("three"), 3)
    assertEquals(cardinalities("many"), 4)

  test("Q028 rejects every still-closed target family including context-bound aliases"):
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
      val extractor = dqq(StringContext("", " def ", "(using ..", "): ", " = ", ""))(using q)
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

  test("Q028 does not widen complete paramss siblings"):
    import quasiquotes.Quasiquotes.dqq

    given Compiler = Compiler.make(getClass.getClassLoader)
    val result = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*
      val target = fixtureDefinitions(using q)("one")
      val q020 = dqq(StringContext("def ", "(...", "): ", " = ", ""))(using q)
      val q026 = dqq(StringContext("", " def ", "(...", "): ", " = ", ""))(using q)
      val q022 = dqq(StringContext("def ", "[..", "](...", "): ", " = ", ""))(using q)
      val q025 = dqq(StringContext("", " def ", "[..", "](...", "): ", " = ", ""))(using q)
      val q028 = dqq(StringContext("", " def ", "(using ..", "): ", " = ", ""))(using q)
      (
        q028.unapply(target).nonEmpty,
        q020.unapply(target).isEmpty,
        q026.unapply(target).isEmpty,
        q022.unapply(target).isEmpty,
        q025.unapply(target).isEmpty
      )

    assertEquals(result, (true, true, true, true, true))

  test("standard selector admits only the exact Q028 static grammar"):
    inline def messages(inline source: String): List[String] = typeCheckErrors(source).map(_.message)
    inline def patternMessages(inline pattern: String): List[String] =
      messages(
        """import scala.quoted.*; import quasiquotes.matching.DefinitionPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             """ + pattern + """
             case _ => ()"""
      )

    val accepted = patternMessages("""case dqq"$mods def $name(using ..$params): $result = $body" => ()""")
    val q031 = patternMessages("""case dqq"$mods def $name(implicit ..$params): $result = $body" => ()""")
    val q044 = patternMessages("""case dqq"$mods def $name(..$params): $result = $body" => ()""")
    val rejected = List(
      patternMessages("""case dqq"private def $name(using ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods final def $name(using ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$left $right def $name(using ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$modsdef $name(using ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def fixed(using ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name[..$tparams](using ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(using ...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(using fixed: Int, ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(using ..$params, fixed: Int): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(using ..$first)(using ..$second): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(using erased ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(using ..$params): Int = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(using ..$params): $result = $body + 1" => ()"""),
      patternMessages("""case dqq"$mods def $name(using ..$params): $result = $left + $right" => ()"""),
      patternMessages("""case dqq"$mods def $name(using ..$params): $result = $body extra" => ()""")
    )

    assertEquals(accepted, Nil)
    assertEquals(q031, Nil)
    assertEquals(q044, Nil)
    assert(rejected.forall(_.nonEmpty), rejected)
    assert(rejected.flatten.forall(_.contains("Invalid dqq definition-pattern template")), rejected)

  test("dynamic Q028 selection retains the historical exact-one fallback"):
    val errors = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.matching.{DefinitionModifiers, DefinitionPattern, RankedDefinitionPatternExtractor}
         def f(using q: Quotes)(context: StringContext): RankedDefinitionPatternExtractor[
           q.reflect.DefDef,
           (DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term], String, Seq[q.reflect.ValDef], q.reflect.TypeRepr, q.reflect.Term)
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
          def finalUsing(using ordering: Ordering[Int]): Int

        class Fixture extends Base:
          def one(using ordering: Ordering[Int]): Int = 1
          def two(using ordering: Ordering[Int], numeric: Numeric[Int]): Int = 2
          def three(using first: Ordering[Int], second: Numeric[Int], third: CanEqual[Int, Int]): Int = 3
          def many(using first: Ordering[Int], second: Numeric[Int], third: CanEqual[Int, Int], fourth: ValueOf[1]): Int = 4
          private def privateUsing(using ordering: Ordering[Int]): Int = 1
          final override def finalUsing(using ordering: Ordering[Int]): Int = 1
          @Q028Annotation def annotatedUsing(using ordering: Ordering[Int]): Int = 1
          private[matching] def qualifiedUsing(using ordering: Ordering[Int]): Int = 1
        ()
      }.asTerm
    )(Symbol.spliceOwner)
    definitions.toMap
