package quasiquotes.hybrid

import scala.compiletime.testing.typeCheckErrors
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

final class Q037HybridAnnotation extends scala.annotation.StaticAnnotation

final class ScalametaMixedOrdinaryScala2ImplicitDefinitionClauseCaptureProductionTest
    extends munit.FunSuite:
  test("external packages receive the exact typed-Scalameta Q037 production capture type"):
    val _ = external.consumer.Q037ExternalScalametaMixedOrdinaryScala2ImplicitConsumer

  test("typed-Scalameta Q037 preserves the full matrix through shared standard target authority"):
    import quasiquotes.scalameta.ScalametaQuasiPattern.dqq

    given Compiler = Compiler.make(getClass.getClassLoader)
    val rows = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*

      val found = scala.collection.mutable.Map.empty[String, DefDef]
      val traversal = new TreeTraverser:
        override def traverseTree(tree: Tree)(owner: Symbol): Unit =
          tree match
            case value: DefDef if !value.symbol.isClassConstructor => found.update(value.name, value)
            case _ => ()
          super.traverseTree(tree)(owner)
      traversal.traverseTree('{
        class Fixture:
          def oneOne(x: Int)(implicit ord: Ordering[Int]): Int = x
          def twoOne(x: Int, y: String)(implicit ord: Ordering[Int]): Int = x
          def oneTwo(x: Int)(implicit ord: Ordering[Int], num: Numeric[Int]): Int = x
          def twoTwo(x: Int, y: String)(implicit ord: Ordering[Int], num: Numeric[Int]): Int = x
          def many(a: Int, b: Int, c: Int, d: Int)(implicit ord: Ordering[Int]): Int = a
          def nested(x: Int)(implicit ord: Ordering[Int]): List[Option[Int]] = List(Some(x))
          def empty()(implicit ord: Ordering[Int]): Int = 1
          final def modified(x: Int)(implicit ord: Ordering[Int]): Int = x
          @Q037HybridAnnotation private[hybrid] def annotated(x: Int)(implicit ord: Ordering[Int]): Int = x
        ()
      }.asTerm)(Symbol.spliceOwner)

      val hybrid =
        dqq(StringContext("", " def ", "(..", ")(implicit ..", "): ", " = ", ""))(using q)
      val standard =
        quasiquotes.matching.DefinitionPattern
          .dqq(StringContext("", " def ", "(..", ")(implicit ..", "): ", " = ", ""))(using q)

      def sameScope(left: Option[TypeRepr], right: Option[TypeRepr]): Boolean =
        (left, right) match
          case (None, None) => true
          case (Some(a), Some(b)) => a =:= b
          case _ => false

      List(
        ("oneOne", 1, 1),
        ("twoOne", 2, 1),
        ("oneTwo", 1, 2),
        ("twoTwo", 2, 2),
        ("many", 4, 1),
        ("nested", 1, 1),
        ("empty", 0, 1),
        ("modified", 1, 1),
        ("annotated", 1, 1)
      ).map { (name, ordinaryCount, implicitCount) =>
        val target = found(name)
        val List(ordinary: TermParamClause, contextual: TermParamClause) = target.paramss: @unchecked
        val hybridResult = hybrid.unapply(target).get
        val standardResult = standard.unapply(target).get
        val allCaptured = hybridResult._3 ++ hybridResult._4
        val allOriginal = ordinary.params ++ contextual.params
        (
          name,
          hybridResult._3.size == ordinaryCount,
          hybridResult._4.size == implicitCount,
          hybridResult._1.flags == standardResult._1.flags,
          sameScope(hybridResult._1.privateWithin, standardResult._1.privateWithin),
          sameScope(hybridResult._1.protectedWithin, standardResult._1.protectedWithin),
          hybridResult._1.annotations.size == standardResult._1.annotations.size &&
            hybridResult._1.annotations.zip(standardResult._1.annotations).forall((left, right) =>
              left eq right
            ),
          hybridResult._2 == standardResult._2,
          hybridResult._3.zip(ordinary.params).forall((left, right) => left eq right),
          hybridResult._4.zip(contextual.params).forall((left, right) => left eq right),
          hybridResult._3.zip(standardResult._3).forall((left, right) => left eq right),
          hybridResult._4.zip(standardResult._4).forall((left, right) => left eq right),
          allCaptured.map(_.symbol) == allOriginal.map(_.symbol),
          allCaptured.map(_.symbol).forall(_ != Symbol.noSymbol),
          allCaptured.map(_.symbol).distinct.size == allCaptured.size,
          allCaptured.forall(_.symbol.owner == target.symbol),
          hybridResult._3.forall(parameter =>
            !parameter.symbol.flags.is(Flags.Implicit) &&
              !parameter.symbol.flags.is(Flags.Given) &&
              !parameter.symbol.flags.is(Flags.Synthetic) &&
              !parameter.symbol.flags.is(Flags.Erased) &&
              !parameter.symbol.flags.is(Flags.HasDefault)
          ),
          hybridResult._4.forall(parameter =>
            parameter.symbol.flags.is(Flags.Implicit) &&
              !parameter.symbol.flags.is(Flags.Given) &&
              !parameter.symbol.flags.is(Flags.Synthetic) &&
              !parameter.symbol.flags.is(Flags.Erased) &&
              !parameter.symbol.flags.is(Flags.HasDefault)
          ),
          target.symbol.paramSymss == List(
            hybridResult._3.map(_.symbol).toList,
            hybridResult._4.map(_.symbol).toList
          ),
          hybridResult._5 =:= target.returnTpt.tpe,
          hybridResult._5 =:= standardResult._5,
          target.rhs.exists(_ eq hybridResult._6),
          hybridResult._6 eq standardResult._6
        )
      }

    rows.foreach(row => row.productIterator.drop(1).foreach(value => assertEquals(value, true, row)))

  test("typed-Scalameta source recognition is collision-safe and exact"):
    import quasiquotes.definitions.hybrid.ScalametaDefinitionFrontend

    val accepted = List(
      "", " def ", "(..", ")(implicit ..", "): ", " = ", ""
    )
    val collision = List(
      "__qq_scmeta_definition_method_0 __qq_scmeta_definition_ordinary_parameter_0 ",
      " def ", "(..", ")(implicit ..", "): ", " = ", ""
    )
    assert(ScalametaDefinitionFrontend
      .compileCapturedModifiersNameMixedOrdinaryScala2ImplicitParameterSequencesCapturedResultPattern(
        accepted
      ).isRight)
    assert(ScalametaDefinitionFrontend
      .compileCapturedModifiersNameMixedOrdinaryScala2ImplicitParameterSequencesCapturedResultPattern(
        collision
      ).isLeft)

  test("typed-Scalameta production selector admits only the exact Q037 grammar"):
    inline def messages(inline source: String): List[String] = typeCheckErrors(source).map(_.message)
    inline def patternMessages(inline pattern: String): List[String] =
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             """ + pattern + """
             case _ => ()"""
      )

    val accepted = patternMessages("""case dqq"$mods def $name(..$params)(implicit ..$implicitParams): $result = $body" => ()""")
    val siblings = List(
      patternMessages("""case dqq"$mods def $name(implicit ..$implicitParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(using ..$usingParams): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(..$params)(using ..$usingParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params): $result = $body" => ()""")
    )
    val rejected = List(
      patternMessages("""case dqq"def $name(..$params)(implicit ..$implicitParams): $result = $body" => ()"""),
      patternMessages("""case dqq"private $mods def $name(..$params)(implicit ..$implicitParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods final def $name(..$params)(implicit ..$implicitParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def fixed(..$params)(implicit ..$implicitParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(implicit ..$implicitParams)(..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(fixed: Int)(implicit ..$implicitParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(implicit fixed: Int): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$first)(..$second): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(implicit ..$first)(implicit ..$second): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(...$paramss)(implicit ..$implicitParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(implicit ...$implicitParamss): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(fixed: Int, ..$params)(implicit ..$implicitParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(implicit fixed: Int, ..$implicitParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name[..$tparams](..$params)(implicit ..$implicitParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(implicit ..$implicitParams)(extra: Int): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(erased ..$params)(implicit ..$implicitParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(implicit erased ..$implicitParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(implicit ..$implicitParams): Int = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(implicit ..$implicitParams): $result = $left + $right" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(implicit ..$implicitParams): $result = $body extra" => ()"""),
      patternMessages("""case dqq"$mods def $name(.$params)(implicit ..$implicitParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(implicit .$implicitParams): $result = $body" => ()""")
    )

    assertEquals(accepted, Nil)
    assert(siblings.forall(_.isEmpty), siblings)
    assert(rejected.forall(_.nonEmpty), rejected)
    assert(
      rejected.flatten.forall(_.contains("Invalid Scalameta dqq definition-pattern template")),
      rejected
    )

    val duplicate = patternMessages("""case dqq"$mods def $name(..$same)(implicit ..$same): $result = $body" => ()""")
    assert(duplicate.exists(_.contains("duplicate pattern variable")), duplicate)

    val dynamic = messages(
      """import scala.quoted.*; import quasiquotes.matching.{DefinitionModifiers, RankedDefinitionPatternExtractor}; import quasiquotes.scalameta.ScalametaQuasiPattern
         def f(using q: Quotes)(context: StringContext): RankedDefinitionPatternExtractor[
           q.reflect.DefDef,
           (DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term], String, Seq[q.reflect.ValDef], Seq[q.reflect.ValDef], q.reflect.TypeRepr, q.reflect.Term)
         ] = ScalametaQuasiPattern.dqq(context)(using q)"""
    )
    assert(dynamic.nonEmpty, dynamic)
