package quasiquotes.hybrid

import scala.compiletime.testing.typeCheckErrors
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

final class Q034HybridAnnotation extends scala.annotation.StaticAnnotation

final class ScalametaMixedOrdinaryNamedUsingDefinitionClauseCaptureProductionTest
    extends munit.FunSuite:
  test("external packages receive the exact typed-Scalameta Q034 production capture type"):
    val _ = external.consumer.Q034ExternalScalametaMixedOrdinaryNamedUsingConsumer

  test("typed-Scalameta Q034 delegates every selected target to standard production semantics"):
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
          def oneOne(x: Int)(using ord: Ordering[Int]): Int = x
          def twoOne(x: Int, y: String)(using ord: Ordering[Int]): Int = x
          def oneTwo(x: Int)(using ord: Ordering[Int], num: Numeric[Int]): Int = x
          def twoTwo(x: Int, y: String)(using ord: Ordering[Int], num: Numeric[Int]): Int = x
          def many(a: Int, b: Int, c: Int, d: Int)(using ord: Ordering[Int]): Int = a
          def nested(x: Int)(using ord: Ordering[Int]): List[Option[Int]] = List(Some(x))
          def empty()(using ord: Ordering[Int]): Int = 1
          final def modified(x: Int)(using ord: Ordering[Int]): Int = x
          @Q034HybridAnnotation private[hybrid] def annotated(x: Int)(using ord: Ordering[Int]): Int = x
        ()
      }.asTerm)(Symbol.spliceOwner)

      val hybrid =
        dqq(StringContext("", " def ", "(..", ")(using ..", "): ", " = ", ""))(using q)
      val standard =
        quasiquotes.matching.DefinitionPattern
          .dqq(StringContext("", " def ", "(..", ")(using ..", "): ", " = ", ""))(using q)

      def sameScope(left: Option[TypeRepr], right: Option[TypeRepr]): Boolean =
        (left, right) match
          case (None, None) => true
          case (Some(a), Some(b)) => a =:= b
          case _ => false

      List(
        "oneOne",
        "twoOne",
        "oneTwo",
        "twoTwo",
        "many",
        "nested",
        "empty",
        "modified",
        "annotated"
      ).map {
        name =>
          val target = found(name)
          val List(ordinary: TermParamClause, contextual: TermParamClause) = target.paramss: @unchecked
          val hybridResult = hybrid.unapply(target).get
          val standardResult = standard.unapply(target).get
          val allCaptured = hybridResult._3 ++ hybridResult._4
          val allOriginal = ordinary.params ++ contextual.params
          (
            name,
            hybridResult._1.flags == standardResult._1.flags,
            sameScope(hybridResult._1.privateWithin, standardResult._1.privateWithin),
            sameScope(hybridResult._1.protectedWithin, standardResult._1.protectedWithin),
            hybridResult._1.annotations.size == standardResult._1.annotations.size &&
              hybridResult._1.annotations.zip(standardResult._1.annotations).forall((left, right) =>
                left eq right
              ),
            hybridResult._2 == standardResult._2,
            hybridResult._3.zip(standardResult._3).forall((left, right) => left eq right),
            hybridResult._4.zip(standardResult._4).forall((left, right) => left eq right),
            hybridResult._3.size == standardResult._3.size,
            hybridResult._4.size == standardResult._4.size,
            allCaptured.map(_.symbol) == allOriginal.map(_.symbol),
            allCaptured.map(_.symbol).forall(_ != Symbol.noSymbol),
            allCaptured.map(_.symbol).distinct.size == allCaptured.size,
            allCaptured.forall(_.symbol.owner == target.symbol),
            hybridResult._3.forall(parameter =>
              !parameter.symbol.flags.is(Flags.Implicit) &&
                !parameter.symbol.flags.is(Flags.Given) &&
                !parameter.symbol.flags.is(Flags.Erased) &&
                !parameter.symbol.flags.is(Flags.HasDefault)
            ),
            hybridResult._4.forall(parameter =>
              parameter.symbol.flags.is(Flags.Given) &&
                !parameter.symbol.flags.is(Flags.Implicit) &&
                !parameter.symbol.flags.is(Flags.Synthetic) &&
                !parameter.symbol.flags.is(Flags.Erased) &&
                !parameter.symbol.flags.is(Flags.HasDefault)
            ),
            target.symbol.paramSymss == List(
              hybridResult._3.map(_.symbol).toList,
              hybridResult._4.map(_.symbol).toList
            ),
            hybridResult._5 =:= standardResult._5,
            hybridResult._6 eq standardResult._6
          )
      }

    rows.foreach(row => row.productIterator.drop(1).foreach(value => assertEquals(value, true, row)))

  test("typed-Scalameta Q034 rejects unselected source layouts and preserves sibling ownership"):
    inline def messages(inline source: String): List[String] = typeCheckErrors(source).map(_.message)
    inline def patternMessages(inline pattern: String): List[String] =
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             """ + pattern + """
             case _ => ()"""
      )

    val accepted = patternMessages("""case dqq"$mods def $name(..$params)(using ..$usingParams): $result = $body" => ()""")
    val q028 = patternMessages("""case dqq"$mods def $name(using ..$usingParams): $result = $body" => ()""")
    val q031 = patternMessages("""case dqq"$mods def $name(implicit ..$implicitParams): $result = $body" => ()""")
    val q035 = patternMessages("""case dqq"def $name(..$params)(using ..$usingParams): $result = $body" => ()""")
    val rejected = List(
      patternMessages("""case dqq"private $mods def $name(..$params)(using ..$usingParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods final def $name(..$params)(using ..$usingParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def fixed(..$params)(using ..$usingParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(using ..$usingParams)(..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$first)(..$second): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(using ..$first)(using ..$second): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(...$paramss)(using ..$usingParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(using ...$usingParamss): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(fixed: Int, ..$params)(using ..$usingParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(using fixed: Int, ..$usingParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name()(using ..$usingParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(using): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name[..$tparams](..$params)(using ..$usingParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(using ..$usingParams)(extra: Int): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(implicit ..$usingParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(erased ..$params)(using ..$usingParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(using ..$usingParams): Int = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(using ..$usingParams): $result = $body + 1" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(using ..$usingParams): $result = $left + $right" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(using ..$usingParams): $result = $body extra" => ()"""),
      patternMessages("""case dqq"$mods def $name(.$params)(using ..$usingParams): $result = $body" => ()""")
    )

    assertEquals(accepted, Nil)
    assertEquals(q028, Nil)
    assertEquals(q031, Nil)
    assertEquals(q035, Nil)
    assert(rejected.forall(_.nonEmpty), rejected)
    assert(
      rejected.flatten.forall(_.contains("Invalid Scalameta dqq definition-pattern template")),
      rejected
    )

    val dynamic = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.matching.{DefinitionModifiers, RankedDefinitionPatternExtractor}; import quasiquotes.scalameta.ScalametaQuasiPattern
         def f(using q: Quotes)(context: StringContext): RankedDefinitionPatternExtractor[
           q.reflect.DefDef,
           (DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term], String, Seq[q.reflect.ValDef], Seq[q.reflect.ValDef], q.reflect.TypeRepr, q.reflect.Term)
         ] = ScalametaQuasiPattern.dqq(context)(using q)"""
    )
    assert(dynamic.nonEmpty, dynamic)
