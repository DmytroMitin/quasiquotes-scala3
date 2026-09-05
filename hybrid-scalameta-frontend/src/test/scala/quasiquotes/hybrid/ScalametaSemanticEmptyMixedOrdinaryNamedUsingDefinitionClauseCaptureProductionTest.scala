package quasiquotes.hybrid

import scala.compiletime.testing.typeCheckErrors
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

final class ScalametaSemanticEmptyMixedOrdinaryNamedUsingDefinitionClauseCaptureProductionTest
    extends munit.FunSuite:
  test("external packages receive the exact typed-Scalameta Q035 production capture type"):
    val _ = external.consumer.Q035ExternalScalametaSemanticEmptyMixedOrdinaryNamedUsingConsumer

  test("typed-Scalameta Q035 delegates cardinality identity and modifier policy to standard semantics"):
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
          def empty()(using ord: Ordering[Int]): Int = 1
          def oneOne(x: Int)(using ord: Ordering[Int]): Int = x
          def twoOne(x: Int, y: String)(using ord: Ordering[Int]): Int = x
          def oneTwo(x: Int)(using ord: Ordering[Int], num: Numeric[Int]): Int = x
          def twoTwo(x: Int, y: String)(using ord: Ordering[Int], num: Numeric[Int]): Int = x
          def many(a: Int, b: Int, c: Int, d: Int)(using ord: Ordering[Int]): Int = a
          def nested(x: Int)(using ord: Ordering[Int]): List[Option[Int]] = List(Some(x))
          final def modified(x: Int)(using ord: Ordering[Int]): Int = x
        ()
      }.asTerm)(Symbol.spliceOwner)

      val hybridOmitted =
        dqq(StringContext("def ", "(..", ")(using ..", "): ", " = ", ""))(using q)
      val hybridCaptured =
        dqq(StringContext("", " def ", "(..", ")(using ..", "): ", " = ", ""))(using q)
      val standardOmitted = quasiquotes.matching.DefinitionPattern
        .dqq(StringContext("def ", "(..", ")(using ..", "): ", " = ", ""))(using q)

      List(
        ("empty", 0, 1),
        ("oneOne", 1, 1),
        ("twoOne", 2, 1),
        ("oneTwo", 1, 2),
        ("twoTwo", 2, 2),
        ("many", 4, 1),
        ("nested", 1, 1)
      ).map { (name, ordinaryCount, usingCount) =>
        val target = found(name)
        val hybrid = hybridOmitted.unapply(target).get
        val standard = standardOmitted.unapply(target).get
        val List(ordinary: TermParamClause, contextual: TermParamClause) = target.paramss: @unchecked
        val allCaptured = hybrid._2 ++ hybrid._3
        (
          name,
          hybrid._1 == target.name && hybrid._1 == standard._1,
          hybrid._2.size == ordinaryCount && hybrid._3.size == usingCount,
          hybrid._2.zip(ordinary.params).forall((left, right) => left eq right),
          hybrid._3.zip(contextual.params).forall((left, right) => left eq right),
          hybrid._2.zip(standard._2).forall((left, right) => left eq right),
          hybrid._3.zip(standard._3).forall((left, right) => left eq right),
          allCaptured.map(_.symbol).forall(_ != Symbol.noSymbol),
          allCaptured.map(_.symbol).distinct.size == allCaptured.size,
          allCaptured.forall(_.symbol.owner == target.symbol),
          target.symbol.paramSymss == List(
            hybrid._2.map(_.symbol).toList,
            hybrid._3.map(_.symbol).toList
          ),
          hybrid._4 =:= target.returnTpt.tpe,
          hybrid._4 =:= standard._4,
          target.rhs.exists(_ eq hybrid._5) && (hybrid._5 eq standard._5),
          hybridCaptured.unapply(target).nonEmpty
        )
      } -> (
        hybridCaptured.unapply(found("modified")).nonEmpty,
        hybridOmitted.unapply(found("modified")).isEmpty
      )

    rows._1.foreach(row => row.productIterator.drop(1).foreach(value => assertEquals(value, true, row)))
    assertEquals(rows._2, (true, true))

  test("typed-Scalameta Q035 accepts only the exact omitted-modifier source grammar"):
    inline def messages(inline source: String): List[String] = typeCheckErrors(source).map(_.message)
    inline def patternMessages(inline pattern: String): List[String] =
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             """ + pattern + """
             case _ => ()"""
      )

    val accepted = patternMessages("""case dqq"def $name(..$params)(using ..$usingParams): $result = $body" => ()""")
    val q034 = patternMessages("""case dqq"$mods def $name(..$params)(using ..$usingParams): $result = $body" => ()""")
    val q028 = patternMessages("""case dqq"def $name(using ..$usingParams): $result = $body" => ()""")
    val rejected = List(
      patternMessages("""case dqq"private def $name(..$params)(using ..$usingParams): $result = $body" => ()"""),
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
    assert(
      rejected.flatten.forall(_.contains("Invalid Scalameta dqq definition-pattern template")),
      rejected
    )

    val dynamic = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.matching.RankedDefinitionPatternExtractor; import quasiquotes.scalameta.ScalametaQuasiPattern
         def f(using q: Quotes)(context: StringContext): RankedDefinitionPatternExtractor[
           q.reflect.DefDef,
           (String, Seq[q.reflect.ValDef], Seq[q.reflect.ValDef], q.reflect.TypeRepr, q.reflect.Term)
         ] = ScalametaQuasiPattern.dqq(context)(using q)"""
    )
    assert(dynamic.nonEmpty, dynamic)
