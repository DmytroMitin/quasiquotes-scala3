package quasiquotes.matching

import scala.annotation.StaticAnnotation
import scala.compiletime.testing.typeCheckErrors
import scala.language.experimental.erasedDefinitions
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

final class Q031Annotation extends StaticAnnotation

final class Q032InlineFixture:
  inline def inlineImplicit(implicit ordering: Ordering[Int]): Int = 1
  transparent inline def transparentImplicit(implicit ordering: Ordering[Int]): Int = 1

final class Scala2ImplicitDefinitionClauseCaptureProductionTest extends munit.FunSuite:
  test("external packages receive exact direct and umbrella Q031 and Q032 capture types"):
    val _ = external.consumer.Q031ExternalScala2ImplicitDefinitionClauseConsumer
    val _ = external.consumer.Q032ExternalSemanticEmptyScala2ImplicitDefinitionClauseConsumer

  test("Q032 captures exact name binders result and body for 1 2 3 and N implicit parameters"):
    import quasiquotes.Quasiquotes.dqq

    given Compiler = Compiler.make(getClass.getClassLoader)
    val rows = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*

      val definitions = fixtureDefinitions(using q)
      val omitted = dqq(StringContext("def ", "(implicit ..", "): ", " = ", ""))(using q)
      val captured = dqq(StringContext("", " def ", "(implicit ..", "): ", " = ", ""))(using q)

      List("one", "two", "three", "many", "nestedResult").map { name =>
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
              parameter.symbol.flags.is(Flags.Implicit) &&
              !parameter.symbol.flags.is(Flags.Given) &&
              !parameter.symbol.flags.is(Flags.Synthetic) &&
              !parameter.symbol.flags.is(Flags.Erased) &&
              !parameter.symbol.flags.is(Flags.HasDefault)
          ),
          result._2.map(_.symbol).distinct.size == result._2.size,
          target.symbol.paramSymss == List(clause.params.map(_.symbol)),
          result._3 =:= target.returnTpt.tpe,
          target.rhs.exists(_ eq result._4),
          captured.unapply(target).nonEmpty,
          clause.isImplicit && !clause.isGiven && !clause.isErased
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
      assert(row._12, row)
    }
    val cardinalities = rows.map(row => row._1 -> row._3).toMap
    assertEquals(cardinalities("one"), 1)
    assertEquals(cardinalities("two"), 2)
    assertEquals(cardinalities("three"), 3)
    assertEquals(cardinalities("many"), 4)

  test("Q031 and Q032 distinguish captured from omitted method modifiers"):
    import quasiquotes.matching.DefinitionPattern.dqq

    given Compiler = Compiler.make(getClass.getClassLoader)
    val rows = withQuotes:
      val q = summon[Quotes]
      val definitions = fixtureDefinitions(using q)
      val omitted = dqq(StringContext("def ", "(implicit ..", "): ", " = ", ""))(using q)
      val captured = dqq(StringContext("", " def ", "(implicit ..", "): ", " = ", ""))(using q)
      val names = List(
        "one",
        "privateImplicit",
        "protectedImplicit",
        "finalImplicit",
        "overrideImplicit",
        "annotatedImplicit",
        "qualifiedPrivateImplicit",
        "qualifiedProtectedImplicit",
        "implicitMethod",
        "infixImplicit",
        "inlineImplicit",
        "transparentImplicit"
      )
      names.map(name =>
        (name, captured.unapply(definitions(name)).nonEmpty, omitted.unapply(definitions(name)).nonEmpty)
      )

    assertEquals(rows.head, ("one", true, true))
    rows.tail.foreach(row => assertEquals(row, (row._1, true, false)))

  test("Q031 captures exact modifiers binders result and body for 1 2 3 and N implicit parameters"):
    import quasiquotes.Quasiquotes.dqq

    given Compiler = Compiler.make(getClass.getClassLoader)
    val rows = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*

      val definitions = fixtureDefinitions(using q)
      val extractor = dqq(StringContext("", " def ", "(implicit ..", "): ", " = ", ""))(using q)
      val names = List(
        "one",
        "two",
        "three",
        "many",
        "nestedResult",
        "privateImplicit",
        "protectedImplicit",
        "finalImplicit",
        "overrideImplicit",
        "annotatedImplicit",
        "qualifiedPrivateImplicit",
        "qualifiedProtectedImplicit",
        "implicitMethod"
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
              parameter.symbol.flags.is(Flags.Implicit) &&
              !parameter.symbol.flags.is(Flags.Given) &&
              !parameter.symbol.flags.is(Flags.Synthetic) &&
              !parameter.symbol.flags.is(Flags.Erased) &&
              !parameter.symbol.flags.is(Flags.HasDefault)
          ),
          captured._3.map(_.symbol).distinct.size == captured._3.size,
          target.symbol.paramSymss == List(clause.params.map(_.symbol)),
          captured._4 =:= target.returnTpt.tpe,
          target.rhs.exists(_ eq captured._5),
          clause.isImplicit && !clause.isGiven && !clause.isErased
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
      assert(row._15, row)
    }
    val cardinalities = rows.map(row => row._1 -> row._7).toMap
    assertEquals(cardinalities("one"), 1)
    assertEquals(cardinalities("two"), 2)
    assertEquals(cardinalities("three"), 3)
    assertEquals(cardinalities("many"), 4)

  test("Q031 rejects every nonimplicit generic malformed and nondefinition target family"):
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

      val exact = definition('{ def exact(implicit first: Ordering[Int], second: Numeric[Int]): Int = 1; () }, "exact")
      val foreign = definition('{ def foreign(implicit first: Ordering[Int], second: Numeric[Int]): Int = 1; () }, "foreign")
      val namedUsing = definition('{ def namedUsing(using value: Int): Int = value; () }, "namedUsing")
      val clause = exact.paramss.head.asInstanceOf[TermParamClause]
      val usingClause = namedUsing.paramss.head.asInstanceOf[TermParamClause]
      val captured = dqq(StringContext("", " def ", "(implicit ..", "): ", " = ", ""))(using q)
      val omitted = dqq(StringContext("def ", "(implicit ..", "): ", " = ", ""))(using q)
      val constructor = definition('{ class Sample(implicit value: Int); () }, "<init>")
      val extension = definition('{ extension (value: Int) def expanded(implicit other: Int): Int = value + other; () }, "expanded")
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
        "named-using" -> definition('{ def named(using value: Int): Int = value; () }, "named"),
        "anonymous-using" -> definition('{ def anonymous(using Ordering[Int]): Int = 1; () }, "anonymous"),
        "generic-implicit" -> definition('{ def genericImplicit[A](implicit ordering: Ordering[A]): Int = 1; () }, "genericImplicit"),
        "context-bound" -> definition('{ def bounded[A: Ordering]: Int = 1; () }, "bounded"),
        "context-bound-ordinary" -> definition('{ def boundedOrdinary[A: Ordering](value: A): A = value; () }, "boundedOrdinary"),
        "context-bound-using" -> definition('{ def boundedUsing[A: Ordering](using marker: Numeric[Int]): Int = 1; () }, "boundedUsing"),
        "ordinary-then-implicit" -> definition('{ def mixed(value: Int)(implicit ordering: Ordering[Int]): Int = value; () }, "mixed"),
        "multiple-implicit-like" -> DefDef.copy(exact)(exact.name, List(clause, clause), exact.returnTpt, exact.rhs),
        "implicit-then-using" -> DefDef.copy(exact)(exact.name, List(clause, usingClause), exact.returnTpt, exact.rhs),
        "using-then-implicit" -> DefDef.copy(exact)(exact.name, List(usingClause, clause), exact.returnTpt, exact.rhs),
        "erased" -> definition('{ def erasedClause(erased value: Int): Int = 0; () }, "erasedClause"),
        "default" -> definition('{ def defaulted(implicit value: Int = 1): Int = value; () }, "defaulted"),
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
      targets.map((label, target) =>
        label -> (captured.unapply(target).isEmpty && omitted.unapply(target).isEmpty)
      )

    rows.foreach(row => assert(row._2, row))

  test("standard selector admits only the exact Q031 static grammar and preserves sibling selection"):
    inline def messages(inline source: String): List[String] = typeCheckErrors(source).map(_.message)
    inline def patternMessages(inline pattern: String): List[String] =
      messages(
        """import scala.quoted.*; import quasiquotes.matching.DefinitionPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             """ + pattern + """
             case _ => ()"""
      )

    val accepted = patternMessages("""case dqq"$mods def $name(implicit ..$params): $result = $body" => ()""")
    val siblingAccepted = List(
      patternMessages("""case dqq"def $name(implicit ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(using ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(using ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name[..$tparams](...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name[..$tparams](...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params): $result = $body" => ()""")
    )
    val rejected = List(
      patternMessages("""case dqq"private def $name(implicit ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods final def $name(implicit ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$left $right def $name(implicit ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$modsdef $name(implicit ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def fixed(implicit ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name[..$tparams](implicit ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(implicit ...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(implicit fixed: Int, ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(implicit ..$params, fixed: Int): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(value: Int)(implicit ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(implicit ..$first)(implicit ..$second): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(implicit ..$params): Int = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(implicit ..$params): $result = $body + 1" => ()"""),
      patternMessages("""case dqq"$mods def $name(implicit ..$params): $result = $left + $right" => ()"""),
      patternMessages("""case dqq"$mods def $name(implicit ..$params): $result = $body extra" => ()"""),
      patternMessages("""case dqq"$mods def $name(implicit .$params): $result = $body" => ()""")
    )

    assertEquals(accepted, Nil)
    assert(siblingAccepted.forall(_.isEmpty), siblingAccepted)
    assert(rejected.forall(_.nonEmpty), rejected)
    assert(rejected.flatten.forall(_.contains("Invalid dqq definition-pattern template")), rejected)

  test("standard selector admits only the exact Q032 static grammar"):
    inline def messages(inline source: String): List[String] = typeCheckErrors(source).map(_.message)
    inline def patternMessages(inline pattern: String): List[String] =
      messages(
        """import scala.quoted.*; import quasiquotes.matching.DefinitionPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             """ + pattern + """
             case _ => ()"""
      )

    val accepted = patternMessages("""case dqq"def $name(implicit ..$params): $result = $body" => ()""")
    val rejected = List(
      patternMessages("""case dqq"private def $name(implicit ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"def fixed(implicit ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(implicitx ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(implicit fixed: Int, ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(implicit ..$params, fixed: Int): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(implicit ...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name[..$tparams](implicit ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(value: Int)(implicit ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(implicit ..$first)(implicit ..$second): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(erased implicit ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(implicit ..$params): Int = $body" => ()"""),
      patternMessages("""case dqq"def $name(implicit ..$params): $result = $left + $right" => ()"""),
      patternMessages("""case dqq"def $name(implicit ..$params): $result = $body extra" => ()"""),
      patternMessages("""case dqq"def $name(implicit .$params): $result = $body" => ()""")
    )

    assertEquals(accepted, Nil)
    assert(rejected.forall(_.nonEmpty), rejected)
    assert(rejected.flatten.forall(_.contains("Invalid dqq definition-pattern template")), rejected)

  test("dynamic Q031 selection retains the historical exact-one fallback"):
    val errors = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.matching.{DefinitionModifiers, DefinitionPattern, RankedDefinitionPatternExtractor}
         def f(using q: Quotes)(context: StringContext): RankedDefinitionPatternExtractor[
           q.reflect.DefDef,
           (DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term], String, Seq[q.reflect.ValDef], q.reflect.TypeRepr, q.reflect.Term)
         ] = DefinitionPattern.dqq(context)(using q)"""
    )
    assert(errors.nonEmpty, errors)

  test("dynamic Q032 selection retains the historical exact-one fallback"):
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
          def overrideImplicit(implicit ordering: Ordering[Int]): Int

        class Fixture extends Base:
          def one(implicit ordering: Ordering[Int]): Int = 1
          def two(implicit ordering: Ordering[Int], numeric: Numeric[Int]): Int = 2
          def three(implicit first: Ordering[Int], second: Numeric[Int], third: CanEqual[Int, Int]): Int = 3
          def many(implicit first: Ordering[Int], second: Numeric[Int], third: CanEqual[Int, Int], fourth: ValueOf[1]): Int = 4
          def nestedResult(implicit ordering: Ordering[Int]): List[Option[Int]] = Nil
          private def privateImplicit(implicit ordering: Ordering[Int]): Int = 1
          protected def protectedImplicit(implicit ordering: Ordering[Int]): Int = 1
          final def finalImplicit(implicit ordering: Ordering[Int]): Int = 1
          override def overrideImplicit(implicit ordering: Ordering[Int]): Int = 1
          @Q031Annotation def annotatedImplicit(implicit ordering: Ordering[Int]): Int = 1
          private[matching] def qualifiedPrivateImplicit(implicit ordering: Ordering[Int]): Int = 1
          protected[matching] def qualifiedProtectedImplicit(implicit ordering: Ordering[Int]): Int = 1
          implicit def implicitMethod(implicit ordering: Ordering[Int]): Int = 1
          infix def infixImplicit(implicit ordering: Ordering[Int]): Int = 1
        ()
      }.asTerm
    )(Symbol.spliceOwner)

    def inlineDefinition(name: String): DefDef =
      val target =
        TypeRepr.of[Q032InlineFixture].typeSymbol.declaredMethod(name).head.tree.asInstanceOf[DefDef]
      DefDef.copy(target)(target.name, target.paramss, target.returnTpt, Some(Literal(IntConstant(1))))

    definitions.update("inlineImplicit", inlineDefinition("inlineImplicit"))
    definitions.update("transparentImplicit", inlineDefinition("transparentImplicit"))
    definitions.toMap
