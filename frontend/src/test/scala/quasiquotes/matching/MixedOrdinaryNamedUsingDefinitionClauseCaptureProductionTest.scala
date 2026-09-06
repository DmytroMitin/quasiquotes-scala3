package quasiquotes.matching

import scala.annotation.StaticAnnotation
import scala.compiletime.testing.typeCheckErrors
import scala.language.experimental.erasedDefinitions
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

final class Q034Annotation extends StaticAnnotation
trait Q034Marker

final class MixedOrdinaryNamedUsingDefinitionClauseCaptureProductionTest extends munit.FunSuite:
  test("external packages receive the exact Q034 production capture type"):
    val _ = external.consumer.Q034ExternalMixedOrdinaryNamedUsingConsumer

  test("Q034 preserves both clause sequences modifiers result body identity and cardinality"):
    import quasiquotes.Quasiquotes.dqq

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
          @Q034Annotation private[matching] def annotated(x: Int)(using ord: Ordering[Int]): Int = x
        ()
      }.asTerm)(Symbol.spliceOwner)

      def sameScope(left: Option[TypeRepr], right: Option[TypeRepr]): Boolean =
        (left, right) match
          case (None, None) => true
          case (Some(a), Some(b)) => a =:= b
          case _ => false

      val extractor =
        dqq(StringContext("", " def ", "(..", ")(using ..", "): ", " = ", ""))(using q)
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
      ).map { (name, ordinaryCount, usingCount) =>
        val target = found(name)
        val List(ordinary: TermParamClause, contextual: TermParamClause) = target.paramss: @unchecked
        val captured = extractor.unapply(target).get
        val allCaptured = captured._3 ++ captured._4
        val allOriginal = ordinary.params ++ contextual.params
        (
          name,
          captured._2 == name,
          captured._3.size == ordinaryCount,
          captured._4.size == usingCount,
          captured._3.zip(ordinary.params).forall((left, right) => left eq right),
          captured._4.zip(contextual.params).forall((left, right) => left eq right),
          allCaptured.map(_.symbol) == allOriginal.map(_.symbol),
          allCaptured.map(_.symbol).forall(_ != Symbol.noSymbol),
          allCaptured.map(_.symbol).distinct.size == allCaptured.size,
          allCaptured.forall(_.symbol.owner == target.symbol),
          captured._3.forall(parameter =>
            !parameter.symbol.flags.is(Flags.Implicit) &&
              !parameter.symbol.flags.is(Flags.Given) &&
              !parameter.symbol.flags.is(Flags.Erased) &&
              !parameter.symbol.flags.is(Flags.HasDefault)
          ),
          captured._4.forall(parameter =>
            parameter.symbol.flags.is(Flags.Given) &&
              !parameter.symbol.flags.is(Flags.Implicit) &&
              !parameter.symbol.flags.is(Flags.Synthetic) &&
              !parameter.symbol.flags.is(Flags.Erased) &&
              !parameter.symbol.flags.is(Flags.HasDefault)
          ),
          target.symbol.paramSymss == List(
            captured._3.map(_.symbol).toList,
            captured._4.map(_.symbol).toList
          ),
          captured._5 =:= target.returnTpt.tpe,
          target.rhs.exists(_ eq captured._6),
          captured._1.flags == target.symbol.flags,
          sameScope(captured._1.privateWithin, target.symbol.privateWithin),
          sameScope(captured._1.protectedWithin, target.symbol.protectedWithin),
          captured._1.annotations.size == target.symbol.annotations.size &&
            captured._1.annotations.zip(target.symbol.annotations).forall((left, right) => left eq right)
        )
      }

    rows.foreach(row => row.productIterator.drop(1).foreach(value => assertEquals(value, true, row)))

  test("Q034 rejects malformed topology symbols parameters and out-of-scope definitions"):
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

      val exact = definition('{ def exact(x: Int, y: Int)(using first: Ordering[Int], second: Numeric[Int]): Int = x; () }, "exact")
      val foreign = definition('{ def foreign(x: Int, y: Int)(using first: Ordering[Int], second: Numeric[Int]): Int = x; () }, "foreign")
      val List(ordinary: TermParamClause, contextual: TermParamClause) = exact.paramss: @unchecked
      val List(foreignOrdinary: TermParamClause, foreignContextual: TermParamClause) =
        foreign.paramss: @unchecked
      val constructor = definition('{ class Sample(x: Int)(using ord: Ordering[Int]); () }, "<init>")
      val extension = definition('{ extension (x: Int) def expanded(y: Int)(using ord: Ordering[Int]): Int = x + y; () }, "expanded")
      val provided = definition('{ given provided(using ord: Ordering[Int]): Int = 1; () }, "provided")
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
        "no-clauses" -> definition('{ def noClauses: Int = 0; () }, "noClauses"),
        "ordinary-only" -> definition('{ def ordinary(x: Int): Int = x; () }, "ordinary"),
        "using-only" -> definition('{ def onlyUsing(using ord: Ordering[Int]): Int = 1; () }, "onlyUsing"),
        "implicit-only" -> definition('{ def old(implicit ord: Ordering[Int]): Int = 1; () }, "old"),
        "ordinary-implicit" -> definition('{ def mixedOld(x: Int)(implicit ord: Ordering[Int]): Int = x; () }, "mixedOld"),
        "using-ordinary" -> definition('{ def wrongOrder(using ord: Ordering[Int])(x: Int): Int = x; () }, "wrongOrder"),
        "extra-clause" -> definition('{ def extra(x: Int)(using ord: Ordering[Int])(y: Int): Int = x + y; () }, "extra"),
        "two-using" -> definition('{ def twoUsing(using ord: Ordering[Int])(using num: Numeric[Int]): Int = 1; () }, "twoUsing"),
        "two-ordinary" -> definition('{ def twoOrdinary(x: Int)(y: Int): Int = x + y; () }, "twoOrdinary"),
        "anonymous-using" -> definition('{ def anonymous(x: Int)(using Ordering[Int]): Int = x; () }, "anonymous"),
        "context-bound-only" -> definition('{ def cbOnly[A: Ordering]: Int = 1; () }, "cbOnly"),
        "context-bound-ordinary" -> definition('{ def cbOrdinary[A: Ordering](value: A): A = value; () }, "cbOrdinary"),
        "context-bound-using" -> definition('{ def cbUsing[A: Ordering](using marker: Q034Marker): Int = 1; () }, "cbUsing"),
        "context-bound-ordinary-using" -> definition('{ def cb[A: Ordering](value: A)(using marker: Q034Marker): A = value; () }, "cb"),
        "generic" -> definition('{ def generic[A](value: A)(using ord: Ordering[A]): A = value; () }, "generic"),
        "default-ordinary" -> definition('{ def defaultOrdinary(x: Int = 1)(using ord: Ordering[Int]): Int = x; () }, "defaultOrdinary"),
        "default-using" -> definition('{ def defaultUsing(x: Int)(using ord: Ordering[Int] = null): Int = x; () }, "defaultUsing"),
        "erased-ordinary" -> definition('{ def erasedOrdinary(erased x: Int)(using ord: Ordering[Int]): Int = 1; () }, "erasedOrdinary"),
        "erased-contextual" -> definition('{ def erasedContextual(x: Int)(using erased ord: Ordering[Int]): Int = x; () }, "erasedContextual"),
        "foreign-ordinary-owner" -> DefDef.copy(exact)(exact.name, List(foreignOrdinary, contextual), exact.returnTpt, exact.rhs),
        "foreign-using-owner" -> DefDef.copy(exact)(exact.name, List(ordinary, foreignContextual), exact.returnTpt, exact.rhs),
        "duplicate-ordinary" -> DefDef.copy(exact)(exact.name, List(TermParamClause(List(ordinary.params.head, ordinary.params.head)), contextual), exact.returnTpt, exact.rhs),
        "reordered-ordinary" -> DefDef.copy(exact)(exact.name, List(TermParamClause(ordinary.params.reverse), contextual), exact.returnTpt, exact.rhs),
        "duplicate-using" -> DefDef.copy(exact)(exact.name, List(ordinary, TermParamClause(List(contextual.params.head, contextual.params.head))), exact.returnTpt, exact.rhs),
        "reordered-using" -> DefDef.copy(exact)(exact.name, List(ordinary, TermParamClause(contextual.params.reverse)), exact.returnTpt, exact.rhs),
        "cross-clause-duplicate" -> DefDef.copy(exact)(exact.name, List(ordinary, TermParamClause(List(ordinary.params.head))), exact.returnTpt, exact.rhs),
        "param-symss-mismatch" -> DefDef.copy(exact)(exact.name, List(TermParamClause(List(ordinary.params.head)), contextual), exact.returnTpt, exact.rhs),
        "missing-rhs" -> DefDef.copy(exact)(exact.name, exact.paramss, exact.returnTpt, None),
        "constructor" -> constructor,
        "extension" -> extension,
        "field-accessor" -> flaggedAccessor(Flags.FieldAccessor),
        "param-accessor" -> flaggedAccessor(Flags.ParamAccessor),
        "case-accessor" -> flaggedAccessor(Flags.CaseAccessor),
        "given-definition" -> provided,
        "null" -> null.asInstanceOf[DefDef]
      )
      val captured =
        dqq(StringContext("", " def ", "(..", ")(using ..", "): ", " = ", ""))(using q)
      val omitted =
        dqq(StringContext("def ", "(..", ")(using ..", "): ", " = ", ""))(using q)
      targets.map((label, target) =>
        label -> (captured.unapply(target).isEmpty && omitted.unapply(target).isEmpty)
      )

    rows.foreach(row => assert(row._2, row))

  test("standard production selector admits only the exact Q034 static grammar"):
    inline def messages(inline source: String): List[String] = typeCheckErrors(source).map(_.message)
    inline def patternMessages(inline pattern: String): List[String] =
      messages(
        """import scala.quoted.*; import quasiquotes.matching.DefinitionPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             """ + pattern + """
             case _ => ()"""
      )

    val accepted = patternMessages("""case dqq"$mods def $name(..$params)(using ..$usingParams): $result = $body" => ()""")
    val q028 = patternMessages("""case dqq"$mods def $name(using ..$usingParams): $result = $body" => ()""")
    val q035 = patternMessages("""case dqq"def $name(..$params)(using ..$usingParams): $result = $body" => ()""")
    val q037 = patternMessages("""case dqq"$mods def $name(..$params)(implicit ..$implicitParams): $result = $body" => ()""")
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
      patternMessages("""case dqq"$mods def $name(..$params)(using ..$usingParams)(extra: Int): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name[..$tparams](..$params)(using ..$usingParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(erased ..$params)(using ..$usingParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(using ..$usingParams): Int = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(using ..$usingParams): $result = $body + 1" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(using ..$usingParams): $result = $left + $right" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(using ..$usingParams): $result = $body extra" => ()"""),
      patternMessages("""case dqq"$mods def $name(.$params)(using ..$usingParams): $result = $body" => ()""")
    )

    assertEquals(accepted, Nil)
    assertEquals(q028, Nil)
    assertEquals(q035, Nil)
    assertEquals(q037, Nil)
    assert(rejected.forall(_.nonEmpty), rejected)
    assert(rejected.flatten.forall(_.contains("Invalid dqq definition-pattern template")), rejected)

    val dynamic = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.matching.{DefinitionModifiers, DefinitionPattern, RankedDefinitionPatternExtractor}
         def f(using q: Quotes)(context: StringContext): RankedDefinitionPatternExtractor[
           q.reflect.DefDef,
           (DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term], String, Seq[q.reflect.ValDef], Seq[q.reflect.ValDef], q.reflect.TypeRepr, q.reflect.Term)
         ] = DefinitionPattern.dqq(context)(using q)"""
    )
    assert(dynamic.nonEmpty, dynamic)
