package quasiquotes.matching

import scala.annotation.StaticAnnotation
import scala.compiletime.testing.typeCheckErrors
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

final class Q045ScalametaOrdinaryAnnotation extends StaticAnnotation

abstract class Q045ScalametaOrdinaryInlineFixture:
  inline def inlineMixed(x: Int): Int = x
  transparent inline def transparentMixed(x: Int): Int = x
  def deferredMixed(x: Int): Int

final class Q045ScalametaOrdinarySemanticEmptyRank2ProductionTest
    extends munit.FunSuite:
  test("omitted unified rank-2 preserves Q044 modes identity order ownership result and body"):
    import quasiquotes.scalameta.ScalametaQuasiPattern.dqq
    given Compiler = Compiler.make(getClass.getClassLoader)
    val report = withQuotes:
      val q = summon[Quotes]
      val omitted = dqq(StringContext("def ", "(..", "): ", " = ", ""))(using q)
      val captured = dqq(StringContext("", " def ", "(..", "): ", " = ", ""))(using q)
      val rank3 = dqq(StringContext("", " def ", "(...", "): ", " = ", ""))(using q)
      quasiquotes.q044.Q044UnifiedOrdinaryRank2Harness.inspect(using q)(captured, rank3, Some(omitted))
    println(s"Q045_UNIFIED $report")
    assert(report.positive.nonEmpty && report.positive.values.forall(identity), report)
    assert(report.negative.nonEmpty && report.negative.values.forall(identity), report)
    assert(report.rank3.nonEmpty && report.rank3.values.forall(identity), report)
    assertEquals(report.modes("strict5").size, 5)
    assertEquals(report.modes("empty"), Nil)

  test("Q044 captures and Q045ScalametaOrdinary rejects every semantically nonempty modifier family"):
    import quasiquotes.scalameta.ScalametaQuasiPattern.dqq

    given Compiler = Compiler.make(getClass.getClassLoader)
    val rows = withQuotes:
      val q = summon[Quotes]
      val definitions = fixtureDefinitions(using q)
      val omitted = dqq(StringContext("def ", "(..", "): ", " = ", ""))(using q)
      val captured =
        dqq(StringContext("", " def ", "(..", "): ", " = ", ""))(using q)
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
        "transparentMixed",
        "deferredMixed"
      )
      names.map(name =>
        (name, captured.unapply(definitions(name)).nonEmpty, omitted.unapply(definitions(name)).nonEmpty)
      )

    assertEquals(rows.head, ("oneOne", true, true))
    rows.tail.foreach(row => assertEquals(row, (row._1, true, false)))

  test("real standard dqq accepts only the exact static Q045 grammar"):
    inline def messages(inline source: String): List[String] = typeCheckErrors(source).map(_.message)
    inline def patternMessages(inline pattern: String): List[String] =
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             """ + pattern + """
             case _ => ()"""
      )

    val accepted = patternMessages("""case dqq"def $name(..$params): $result = $body" => ()""")
    val rejected = List(
      patternMessages("""case dqq"final def $name(..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"def fixed(..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name[..$tparams](..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(fixed: Int, ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(..$params)(..$second): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(..$params): Int = $body" => ()"""),
      patternMessages("""case dqq"def $name(..$params): $result = $left + $right" => ()"""),
      patternMessages("""case dqq"def $name(.$params): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(..$params): $result = $body trailing" => ()"""),
      patternMessages("""case dqq"def $name(..$params): $result = $params" => ()""")
    )
    val dynamic = messages(
      """import scala.quoted.*
         import quasiquotes.matching.{DefinitionModifiers, DefinitionPattern, RankedDefinitionPatternExtractor}
         import quasiquotes.scalameta.ScalametaQuasiPattern.dqq
         def f(using q: Quotes)(context: StringContext): RankedDefinitionPatternExtractor[
           q.reflect.DefDef,
           (String,
             Seq[q.reflect.ValDef], q.reflect.TypeRepr, q.reflect.Term)
         ] = context.dqq"""
    )
    val neighboring = List(
      patternMessages("""case dqq"$mods def $name(..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(using ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(implicit ..$params): $result = $body" => ()""")
    )
    assertEquals(accepted, Nil)
    assert(rejected.forall(_.nonEmpty), rejected)
    assert(neighboring.forall(_.isEmpty), neighboring)
    assert(dynamic.nonEmpty, dynamic)

  test("Q024 semantic empty accepts harmless raw method structure"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val valid = withQuotes:
      val q = summon[Quotes]
      val target = fixtureDefinitions(using q)("oneOne")
      DefinitionModifierSemantics.isSemanticallyEmpty(using q)(target.symbol) &&
        target.symbol.flags != q.reflect.Flags.EmptyFlags
    assert(valid)

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
        def overrideMixed(x: Int): Int

      class Fixture extends Base:
        def empty(): Int = 1
        def oneOne(x: Int): Int = x
        def twoOne(x: Int, y: String): Int = x
        def oneTwo(x: Int): Int = x
        def twoTwo(x: Int, y: String): Int = x
        def many(a: Int, b: Int, c: Int, d: Int): Int = a
        def nested(x: Int): List[Option[Int]] = List(Some(x))
        def `type`(x: Int): Int = x
        private def privateMixed(x: Int): Int = x
        protected def protectedMixed(x: Int): Int = x
        final def finalMixed(x: Int): Int = x
        override def overrideMixed(x: Int): Int = x
        @Q045ScalametaOrdinaryAnnotation def annotatedMixed(x: Int): Int = x
        private[matching] def qualifiedPrivateMixed(x: Int): Int = x
        protected[matching] def qualifiedProtectedMixed(x: Int): Int = x
        implicit def implicitMixed(x: Int): Int = x
        infix def infixMixed(x: Int): Int = x
      ()
    }.asTerm)(Symbol.spliceOwner)

    def inlineDefinition(name: String): DefDef =
      val target =
        TypeRepr.of[Q045ScalametaOrdinaryInlineFixture].typeSymbol.declaredMethod(name).head.tree.asInstanceOf[DefDef]
      DefDef.copy(target)(target.name, target.paramss, target.returnTpt, Some(Literal(IntConstant(1))))

    definitions.update("inlineMixed", inlineDefinition("inlineMixed"))
    definitions.update("transparentMixed", inlineDefinition("transparentMixed"))
    // Retain source-owned binders and supply a RHS to isolate Deferred independently of missing RHS.
    definitions.update("deferredMixed", inlineDefinition("deferredMixed"))
    definitions.toMap
