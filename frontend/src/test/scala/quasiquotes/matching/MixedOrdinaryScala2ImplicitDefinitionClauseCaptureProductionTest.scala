package quasiquotes.matching

import scala.annotation.StaticAnnotation
import scala.compiletime.testing.typeCheckErrors
import scala.language.experimental.erasedDefinitions
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

final class Q037MethodAnnotation extends StaticAnnotation
final class Q037ParameterAnnotation extends StaticAnnotation
trait Q037Marker

final class MixedOrdinaryScala2ImplicitDefinitionClauseCaptureProductionTest
    extends munit.FunSuite:
  test("external packages receive the exact Q037 production capture type"):
    val _ = external.consumer.Q037ExternalMixedOrdinaryScala2ImplicitConsumer

  test("Q037 preserves two independent clauses modifiers result body identity and cardinality"):
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
          def oneOne(x: Int)(implicit ord: Ordering[Int]): Int = x
          def twoOne(x: Int, y: String)(implicit ord: Ordering[Int]): Int = x
          def oneTwo(x: Int)(implicit ord: Ordering[Int], num: Numeric[Int]): Int = x
          def twoTwo(x: Int, y: String)(implicit ord: Ordering[Int], num: Numeric[Int]): Int = x
          def many(a: Int, b: Int, c: Int, d: Int)(implicit ord: Ordering[Int]): Int = a
          def nested(x: Int)(implicit ord: Ordering[Int]): List[Option[Int]] = List(Some(x))
          def empty()(implicit ord: Ordering[Int]): Int = 1
          final def modified(x: Int)(implicit ord: Ordering[Int]): Int = x
          @Q037MethodAnnotation private[matching] def annotated(x: Int)(implicit ord: Ordering[Int]): Int = x
        ()
      }.asTerm)(Symbol.spliceOwner)

      def sameScope(left: Option[TypeRepr], right: Option[TypeRepr]): Boolean =
        (left, right) match
          case (None, None) => true
          case (Some(a), Some(b)) => a =:= b
          case _ => false

      val extractor =
        dqq(StringContext("", " def ", "(..", ")(implicit ..", "): ", " = ", ""))(using q)
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
        val captured = extractor.unapply(target).get
        val allCaptured = captured._3 ++ captured._4
        val allOriginal = ordinary.params ++ contextual.params
        (
          name,
          captured._2 == name,
          captured._3.size == ordinaryCount,
          captured._4.size == implicitCount,
          captured._3.zip(ordinary.params).forall((left, right) => left eq right),
          captured._4.zip(contextual.params).forall((left, right) => left eq right),
          allCaptured.map(_.symbol) == allOriginal.map(_.symbol),
          allCaptured.map(_.symbol).forall(_ != Symbol.noSymbol),
          allCaptured.map(_.symbol).distinct.size == allCaptured.size,
          allCaptured.forall(_.symbol.owner == target.symbol),
          captured._3.forall(parameter =>
            !parameter.symbol.flags.is(Flags.Implicit) &&
              !parameter.symbol.flags.is(Flags.Given) &&
              !parameter.symbol.flags.is(Flags.Synthetic) &&
              !parameter.symbol.flags.is(Flags.Erased) &&
              !parameter.symbol.flags.is(Flags.HasDefault)
          ),
          captured._4.forall(parameter =>
            parameter.symbol.flags.is(Flags.Implicit) &&
              !parameter.symbol.flags.is(Flags.Given) &&
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

  test("Q037 rejects the complete malformed and out-of-scope target matrix"):
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

      val exact = definition('{ def exact(x: Int, y: Int)(implicit first: Ordering[Int], second: Numeric[Int]): Int = x; () }, "exact")
      val foreign = definition('{ def foreign(x: Int, y: Int)(implicit first: Ordering[Int], second: Numeric[Int]): Int = x; () }, "foreign")
      val repeated = definition('{ def repeated(values: Int*): Int = values.size; () }, "repeated")
      val List(ordinary: TermParamClause, contextual: TermParamClause) = exact.paramss: @unchecked
      val List(foreignOrdinary: TermParamClause, foreignContextual: TermParamClause) = foreign.paramss: @unchecked
      val repeatedParameter = repeated.paramss.head.asInstanceOf[TermParamClause].params.head
      val repeatedInt = defn.RepeatedParamClass.typeRef.appliedTo(TypeRepr.of[Int])
      val repeatedImplicitSymbol = Symbol.newVal(
        exact.symbol,
        "repeatedImplicit",
        repeatedInt,
        Flags.Param | Flags.Implicit,
        Symbol.noSymbol
      )
      val repeatedImplicit = TermParamClause(List(ValDef(repeatedImplicitSymbol, None)))
      val erasedImplicitSymbol = Symbol.newVal(
        exact.symbol,
        "erasedImplicit",
        TypeRepr.of[Ordering[Int]],
        Flags.Param | Flags.Implicit | Flags.Erased,
        Symbol.noSymbol
      )
      val erasedImplicit = TermParamClause(List(ValDef(erasedImplicitSymbol, None)))
      val constructor = definition('{ class Sample(x: Int)(implicit ord: Ordering[Int]); () }, "<init>")
      val extension = definition('{ extension (x: Int) def expanded(y: Int)(implicit ord: Ordering[Int]): Int = x + y; () }, "expanded")
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
        "implicit-only" -> definition('{ def old(implicit ord: Ordering[Int]): Int = 1; () }, "old"),
        "using-only" -> definition('{ def onlyUsing(using ord: Ordering[Int]): Int = 1; () }, "onlyUsing"),
        "ordinary-using" -> definition('{ def mixedUsing(x: Int)(using ord: Ordering[Int]): Int = x; () }, "mixedUsing"),
        "using-ordinary" -> definition('{ def wrongOrder(using ord: Ordering[Int])(x: Int): Int = x; () }, "wrongOrder"),
        "implicit-ordinary-structural" -> DefDef.copy(exact)(exact.name, List(contextual, ordinary), exact.returnTpt, exact.rhs),
        "implicit-implicit-structural" -> DefDef.copy(exact)(exact.name, List(contextual, contextual), exact.returnTpt, exact.rhs),
        "ordinary-ordinary" -> definition('{ def twoOrdinary(x: Int)(y: Int): Int = x + y; () }, "twoOrdinary"),
        "third-clause" -> DefDef.copy(exact)(exact.name, List(ordinary, contextual, ordinary), exact.returnTpt, exact.rhs),
        "two-contextual" -> DefDef.copy(exact)(exact.name, List(contextual, contextual), exact.returnTpt, exact.rhs),
        "anonymous-using" -> definition('{ def anonymous(x: Int)(using Ordering[Int]): Int = x; () }, "anonymous"),
        "context-bound-only" -> definition('{ def cbOnly[A: Ordering]: Int = 1; () }, "cbOnly"),
        "context-bound-ordinary" -> definition('{ def cbOrdinary[A: Ordering](value: A): A = value; () }, "cbOrdinary"),
        "context-bound-implicit" -> definition('{ def cbImplicit[A: Ordering](implicit marker: Q037Marker): Int = 1; () }, "cbImplicit"),
        "context-bound-ordinary-implicit" -> definition('{ def cb[A: Ordering](value: A)(implicit marker: Q037Marker): A = value; () }, "cb"),
        "generic" -> definition('{ def generic[A](value: A)(implicit marker: Q037Marker): A = value; () }, "generic"),
        "default-ordinary" -> definition('{ def defaultOrdinary(x: Int = 1)(implicit ord: Ordering[Int]): Int = x; () }, "defaultOrdinary"),
        "default-implicit" -> definition('{ def defaultImplicit(x: Int)(implicit ord: Ordering[Int] = null): Int = x; () }, "defaultImplicit"),
        "repeated-ordinary-structural" -> DefDef.copy(exact)(exact.name, List(TermParamClause(List(repeatedParameter)), contextual), exact.returnTpt, exact.rhs),
        "repeated-implicit-structural" -> DefDef.copy(exact)(exact.name, List(ordinary, repeatedImplicit), exact.returnTpt, exact.rhs),
        "by-name-ordinary" -> definition('{ def byNameOrdinary(x: => Int)(implicit ord: Ordering[Int]): Int = x; () }, "byNameOrdinary"),
        "by-name-implicit" -> definition('{ def byNameImplicit(x: Int)(implicit delayed: => Int): Int = delayed; () }, "byNameImplicit"),
        "erased-ordinary" -> definition('{ def erasedOrdinary(erased x: Int)(implicit ord: Ordering[Int]): Int = 1; () }, "erasedOrdinary"),
        "erased-implicit-structural" -> DefDef.copy(exact)(exact.name, List(ordinary, erasedImplicit), exact.returnTpt, exact.rhs),
        "annotated-ordinary" -> definition('{ def annotatedOrdinary(@Q037ParameterAnnotation x: Int)(implicit ord: Ordering[Int]): Int = x; () }, "annotatedOrdinary"),
        "annotated-implicit" -> definition('{ def annotatedImplicit(x: Int)(implicit @Q037ParameterAnnotation ord: Ordering[Int]): Int = x; () }, "annotatedImplicit"),
        "foreign-ordinary-owner" -> DefDef.copy(exact)(exact.name, List(foreignOrdinary, contextual), exact.returnTpt, exact.rhs),
        "foreign-implicit-owner" -> DefDef.copy(exact)(exact.name, List(ordinary, foreignContextual), exact.returnTpt, exact.rhs),
        "duplicate-ordinary" -> DefDef.copy(exact)(exact.name, List(TermParamClause(List(ordinary.params.head, ordinary.params.head)), contextual), exact.returnTpt, exact.rhs),
        "reordered-ordinary" -> DefDef.copy(exact)(exact.name, List(TermParamClause(ordinary.params.reverse), contextual), exact.returnTpt, exact.rhs),
        "duplicate-implicit" -> DefDef.copy(exact)(exact.name, List(ordinary, TermParamClause(List(contextual.params.head, contextual.params.head))), exact.returnTpt, exact.rhs),
        "reordered-implicit" -> DefDef.copy(exact)(exact.name, List(ordinary, TermParamClause(contextual.params.reverse)), exact.returnTpt, exact.rhs),
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
      val extractor =
        dqq(StringContext("", " def ", "(..", ")(implicit ..", "): ", " = ", ""))(using q)
      targets.map((label, target) => label -> extractor.unapply(target).isEmpty)

    rows.foreach(row => assert(row._2, row))

  test("standard production selector admits only the exact Q037 static grammar"):
    inline def messages(inline source: String): List[String] = typeCheckErrors(source).map(_.message)
    inline def patternMessages(inline pattern: String): List[String] =
      messages(
        """import scala.quoted.*; import quasiquotes.matching.DefinitionPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             """ + pattern + """
             case _ => ()"""
      )

    val accepted = patternMessages("""case dqq"$mods def $name(..$params)(implicit ..$implicitParams): $result = $body" => ()""")
    val siblings = List(
      patternMessages("""case dqq"$mods def $name(implicit ..$implicitParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(using ..$usingParams): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(..$params)(using ..$usingParams): $result = $body" => ()""")
    )
    val rejected = List(
      patternMessages("""case dqq"def $name(..$params)(implicit ..$implicitParams): $result = $body" => ()"""),
      patternMessages("""case dqq"private $mods def $name(..$params)(implicit ..$implicitParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods final def $name(..$params)(implicit ..$implicitParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def fixed(..$params)(implicit ..$implicitParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(implicit ..$implicitParams)(..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params): $result = $body" => ()"""),
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
      patternMessages("""case dqq"$mods def $name(..$params)(implicit ..$implicitParams): $result = $body + 1" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(implicit ..$implicitParams): $result = $left + $right" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(implicit ..$implicitParams): $result = $body extra" => ()"""),
      patternMessages("""case dqq"$mods def $name(.$params)(implicit ..$implicitParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(implicit .$implicitParams): $result = $body" => ()""")
    )

    assertEquals(accepted, Nil)
    assert(siblings.forall(_.isEmpty), siblings)
    assert(rejected.forall(_.nonEmpty), rejected)
    assert(rejected.flatten.forall(_.contains("Invalid dqq definition-pattern template")), rejected)

    val duplicate = patternMessages("""case dqq"$mods def $name(..$same)(implicit ..$same): $result = $body" => ()""")
    assert(duplicate.exists(_.contains("duplicate pattern variable")), duplicate)

    val dynamic = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.matching.{DefinitionModifiers, DefinitionPattern, RankedDefinitionPatternExtractor}
         def f(using q: Quotes)(context: StringContext): RankedDefinitionPatternExtractor[
           q.reflect.DefDef,
           (DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term], String, Seq[q.reflect.ValDef], Seq[q.reflect.ValDef], q.reflect.TypeRepr, q.reflect.Term)
         ] = DefinitionPattern.dqq(context)(using q)"""
    )
    assert(dynamic.nonEmpty, dynamic)
