package quasiquotes.matching

import scala.annotation.StaticAnnotation
import scala.compiletime.testing.typeCheckErrors
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

final class Q035Annotation extends StaticAnnotation

final class Q035InlineFixture:
  inline def inlineMixed(x: Int)(using ordering: Ordering[Int]): Int = x
  transparent inline def transparentMixed(x: Int)(using ordering: Ordering[Int]): Int = x

final class SemanticEmptyMixedOrdinaryNamedUsingDefinitionClauseCaptureProductionTest
    extends munit.FunSuite:
  test("external packages receive the exact Q035 production capture type"):
    val _ = external.consumer.Q035ExternalSemanticEmptyMixedOrdinaryNamedUsingConsumer

  test("Q035 preserves independent original clauses including present empty ordinary"):
    import quasiquotes.Quasiquotes.dqq

    given Compiler = Compiler.make(getClass.getClassLoader)
    val rows = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*

      val definitions = fixtureDefinitions(using q)
      val omitted = dqq(StringContext("def ", "(..", ")(using ..", "): ", " = ", ""))(using q)
      val captured =
        dqq(StringContext("", " def ", "(..", ")(using ..", "): ", " = ", ""))(using q)

      List(
        ("empty", 0, 1),
        ("oneOne", 1, 1),
        ("twoOne", 2, 1),
        ("oneTwo", 1, 2),
        ("twoTwo", 2, 2),
        ("many", 4, 1),
        ("nested", 1, 1),
        ("type", 1, 1)
      ).map { (name, ordinaryCount, usingCount) =>
        val target = definitions(name)
        val List(ordinary: TermParamClause, contextual: TermParamClause) = target.paramss: @unchecked
        val result = omitted.unapply(target).get
        val allCaptured = result._2 ++ result._3
        val allOriginal = ordinary.params ++ contextual.params
        (
          name,
          result._1 == target.name,
          result._2.size == ordinaryCount,
          result._3.size == usingCount,
          result._2.zip(ordinary.params).forall((left, right) => left eq right),
          result._3.zip(contextual.params).forall((left, right) => left eq right),
          allCaptured.map(_.symbol) == allOriginal.map(_.symbol),
          allCaptured.map(_.symbol).forall(_ != Symbol.noSymbol),
          allCaptured.map(_.symbol).distinct.size == allCaptured.size,
          allCaptured.forall(_.symbol.owner == target.symbol),
          result._2.forall(parameter =>
            !parameter.symbol.flags.is(Flags.Implicit) &&
              !parameter.symbol.flags.is(Flags.Given) &&
              !parameter.symbol.flags.is(Flags.Erased) &&
              !parameter.symbol.flags.is(Flags.HasDefault)
          ),
          result._3.forall(parameter =>
            parameter.symbol.flags.is(Flags.Given) &&
              !parameter.symbol.flags.is(Flags.Implicit) &&
              !parameter.symbol.flags.is(Flags.Synthetic) &&
              !parameter.symbol.flags.is(Flags.Erased) &&
              !parameter.symbol.flags.is(Flags.HasDefault)
          ),
          target.symbol.paramSymss == List(
            result._2.map(_.symbol).toList,
            result._3.map(_.symbol).toList
          ),
          result._4 =:= target.returnTpt.tpe,
          target.rhs.exists(_ eq result._5),
          captured.unapply(target).nonEmpty,
          !ordinary.isImplicit && !ordinary.isGiven && !ordinary.isErased,
          contextual.isGiven && !contextual.isImplicit && !contextual.isErased
        )
      }

    rows.foreach(row => row.productIterator.drop(1).foreach(value => assertEquals(value, true, row)))

  test("Q034 captures and Q035 rejects every semantically nonempty modifier family"):
    import quasiquotes.matching.DefinitionPattern.dqq

    given Compiler = Compiler.make(getClass.getClassLoader)
    val rows = withQuotes:
      val q = summon[Quotes]
      val definitions = fixtureDefinitions(using q)
      val omitted = dqq(StringContext("def ", "(..", ")(using ..", "): ", " = ", ""))(using q)
      val captured =
        dqq(StringContext("", " def ", "(..", ")(using ..", "): ", " = ", ""))(using q)
      val names = List(
        "oneOne",
        "privateMixed",
        "protectedMixed",
        "finalMixed",
        "overrideMixed",
        "annotatedMixed",
        "qualifiedPrivateMixed",
        "qualifiedProtectedMixed",
        "implicitMixed",
        "infixMixed",
        "inlineMixed",
        "transparentMixed"
      )
      names.map(name =>
        (name, captured.unapply(definitions(name)).nonEmpty, omitted.unapply(definitions(name)).nonEmpty)
      )

    assertEquals(rows.head, ("oneOne", true, true))
    rows.tail.foreach(row => assertEquals(row, (row._1, true, false)))

  test("standard Q035 selector admits only the exact modifier-omitted static grammar"):
    inline def messages(inline source: String): List[String] = typeCheckErrors(source).map(_.message)
    inline def patternMessages(inline pattern: String): List[String] =
      messages(
        """import scala.quoted.*; import quasiquotes.matching.DefinitionPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             """ + pattern + """
             case _ => ()"""
      )

    val accepted = patternMessages("""case dqq"def $name(..$params)(using ..$usingParams): $result = $body" => ()""")
    val q034 = patternMessages("""case dqq"$mods def $name(..$params)(using ..$usingParams): $result = $body" => ()""")
    val q028 = patternMessages("""case dqq"def $name(using ..$usingParams): $result = $body" => ()""")
    val rejected = List(
      patternMessages("""case dqq"private def $name(..$params)(using ..$usingParams): $result = $body" => ()"""),
      patternMessages("""case dqq"final def $name(..$params)(using ..$usingParams): $result = $body" => ()"""),
      patternMessages("""case dqq"def fixed(..$params)(using ..$usingParams): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(using ..$usingParams)(..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(..$first)(..$second): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(using ..$first)(using ..$second): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(...$paramss)(using ..$usingParams): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(..$params)(using ...$usingParamss): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(fixed: Int, ..$params)(using ..$usingParams): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(..$params)(using fixed: Int, ..$usingParams): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(..$params)(using ..$usingParams)(extra: Int): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name[..$tparams](..$params)(using ..$usingParams): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(..$params)(implicit ..$usingParams): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(erased ..$params)(using ..$usingParams): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(..$params)(using ..$usingParams): Int = $body" => ()"""),
      patternMessages("""case dqq"def $name(..$params)(using ..$usingParams): $result = $body + 1" => ()"""),
      patternMessages("""case dqq"def $name(..$params)(using ..$usingParams): $result = $left + $right" => ()"""),
      patternMessages("""case dqq"def $name(..$params)(using ..$usingParams): $result = $body extra" => ()"""),
      patternMessages("""case dqq"def $name(.$params)(using ..$usingParams): $result = $body" => ()""")
    )

    assertEquals(accepted, Nil)
    assertEquals(q034, Nil)
    assertEquals(q028, Nil)
    assert(rejected.forall(_.nonEmpty), rejected)
    assert(rejected.flatten.forall(_.contains("Invalid dqq definition-pattern template")), rejected)

    val capturedAsOmitted = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.matching.{DefinitionPattern, RankedDefinitionPatternExtractor}
         def f(using q: Quotes): RankedDefinitionPatternExtractor[
           q.reflect.DefDef,
           (String, Seq[q.reflect.ValDef], Seq[q.reflect.ValDef], q.reflect.TypeRepr, q.reflect.Term)
         ] = DefinitionPattern.dqq(StringContext("", " def ", "(..", ")(using ..", "): ", " = ", ""))(using q)"""
    )
    assert(capturedAsOmitted.nonEmpty, capturedAsOmitted)

    val dynamic = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.matching.{DefinitionPattern, RankedDefinitionPatternExtractor}
         def f(using q: Quotes)(context: StringContext): RankedDefinitionPatternExtractor[
           q.reflect.DefDef,
           (String, Seq[q.reflect.ValDef], Seq[q.reflect.ValDef], q.reflect.TypeRepr, q.reflect.Term)
         ] = DefinitionPattern.dqq(context)(using q)"""
    )
    assert(dynamic.nonEmpty, dynamic)

  private def fixtureDefinitions(using q: Quotes): Map[String, q.reflect.DefDef] =
    import q.reflect.*

    val definitions = scala.collection.mutable.Map.empty[String, DefDef]
    val traversal = new TreeTraverser:
      override def traverseTree(tree: Tree)(owner: Symbol): Unit =
        tree match
          case value: DefDef if !value.symbol.isClassConstructor => definitions.update(value.name, value)
          case _ => ()
        super.traverseTree(tree)(owner)

    traversal.traverseTree('{
      trait Base:
        def overrideMixed(x: Int)(using ordering: Ordering[Int]): Int

      class Fixture extends Base:
        def empty()(using ordering: Ordering[Int]): Int = 1
        def oneOne(x: Int)(using ordering: Ordering[Int]): Int = x
        def twoOne(x: Int, y: String)(using ordering: Ordering[Int]): Int = x
        def oneTwo(x: Int)(using ordering: Ordering[Int], numeric: Numeric[Int]): Int = x
        def twoTwo(x: Int, y: String)(using ordering: Ordering[Int], numeric: Numeric[Int]): Int = x
        def many(a: Int, b: Int, c: Int, d: Int)(using ordering: Ordering[Int]): Int = a
        def nested(x: Int)(using ordering: Ordering[Int]): List[Option[Int]] = List(Some(x))
        def `type`(x: Int)(using ordering: Ordering[Int]): Int = x
        private def privateMixed(x: Int)(using ordering: Ordering[Int]): Int = x
        protected def protectedMixed(x: Int)(using ordering: Ordering[Int]): Int = x
        final def finalMixed(x: Int)(using ordering: Ordering[Int]): Int = x
        override def overrideMixed(x: Int)(using ordering: Ordering[Int]): Int = x
        @Q035Annotation def annotatedMixed(x: Int)(using ordering: Ordering[Int]): Int = x
        private[matching] def qualifiedPrivateMixed(x: Int)(using ordering: Ordering[Int]): Int = x
        protected[matching] def qualifiedProtectedMixed(x: Int)(using ordering: Ordering[Int]): Int = x
        implicit def implicitMixed(x: Int)(using ordering: Ordering[Int]): Int = x
        infix def infixMixed(x: Int)(using ordering: Ordering[Int]): Int = x
      ()
    }.asTerm)(Symbol.spliceOwner)

    def inlineDefinition(name: String): DefDef =
      val target =
        TypeRepr.of[Q035InlineFixture].typeSymbol.declaredMethod(name).head.tree.asInstanceOf[DefDef]
      DefDef.copy(target)(target.name, target.paramss, target.returnTpt, Some(Literal(IntConstant(1))))

    definitions.update("inlineMixed", inlineDefinition("inlineMixed"))
    definitions.update("transparentMixed", inlineDefinition("transparentMixed"))
    definitions.toMap
