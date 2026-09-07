package quasiquotes.hybrid

import scala.annotation.StaticAnnotation
import scala.compiletime.testing.typeCheckErrors
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

final class Q031ScalametaAnnotation extends StaticAnnotation

final class Q032ScalametaInlineFixture:
  inline def inlineImplicit(implicit ordering: Ordering[Int]): Int = 1
  transparent inline def transparentImplicit(implicit ordering: Ordering[Int]): Int = 1

final class ScalametaScala2ImplicitDefinitionClauseCaptureProductionTest
    extends munit.FunSuite:
  test("external packages receive exact direct and umbrella typed-Scalameta Q031 and Q032 capture types"):
    val _ = external.consumer.Q031ExternalScalametaScala2ImplicitDefinitionClauseConsumer
    val _ = external.consumer.Q032ExternalScalametaSemanticEmptyScala2ImplicitDefinitionClauseConsumer

  test("typed-Scalameta Q031 recognizes 1 2 3 and N implicit binders and delegates exact target semantics"):
    import quasiquotes.scalameta.ScalametaQuasiPattern.dqq

    given Compiler = Compiler.make(getClass.getClassLoader)
    val rows = withQuotes:
      val q = summon[Quotes]
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
            private def modified(implicit ordering: Ordering[Int]): List[Option[Int]] = Nil
            protected def protectedImplicit(implicit ordering: Ordering[Int]): Int = 1
            final def finalImplicit(implicit ordering: Ordering[Int]): Int = 1
            override def overrideImplicit(implicit ordering: Ordering[Int]): Int = 1
            private[hybrid] def qualifiedPrivateImplicit(implicit ordering: Ordering[Int]): Int = 1
            protected[hybrid] def qualifiedProtectedImplicit(implicit ordering: Ordering[Int]): Int = 1
            implicit def implicitMethod(implicit ordering: Ordering[Int]): Int = 1
            @Q031ScalametaAnnotation def annotated(implicit ordering: Ordering[Int]): Int = 1
            infix def infixImplicit(implicit ordering: Ordering[Int]): Int = 1
          ()
        }.asTerm
      )(Symbol.spliceOwner)

      def inlineDefinition(name: String): DefDef =
        val target =
          TypeRepr.of[Q032ScalametaInlineFixture].typeSymbol.declaredMethod(name).head.tree.asInstanceOf[DefDef]
        DefDef.copy(target)(target.name, target.paramss, target.returnTpt, Some(Literal(IntConstant(1))))

      definitions.update("inlineImplicit", inlineDefinition("inlineImplicit"))
      definitions.update("transparentImplicit", inlineDefinition("transparentImplicit"))

      val hybrid = dqq(StringContext("", " def ", "(implicit ..", "): ", " = ", ""))(using q)
      val standard = quasiquotes.matching.DefinitionPattern.dqq(
        StringContext("", " def ", "(implicit ..", "): ", " = ", "")
      )(using q)
      val omitted = dqq(StringContext("def ", "(implicit ..", "): ", " = ", ""))(using q)

      def sameScope(left: Option[TypeRepr], right: Option[TypeRepr]): Boolean =
        (left, right) match
          case (None, None) => true
          case (Some(a), Some(b)) => a =:= b
          case _ => false

      val capturedRows = List(
        "one",
        "two",
        "three",
        "many",
        "nestedResult",
        "modified",
        "protectedImplicit",
        "finalImplicit",
        "qualifiedPrivateImplicit",
        "qualifiedProtectedImplicit",
        "implicitMethod",
        "annotated"
      ).map { name =>
        val target = definitions(name)
        val clause = target.paramss.head.asInstanceOf[TermParamClause]
        val left = hybrid.unapply(target).get
        val right = standard.unapply(target).get
        (
          name,
          left._1.flags == target.symbol.flags && left._1.flags == right._1.flags,
          sameScope(left._1.privateWithin, target.symbol.privateWithin) &&
            sameScope(left._1.privateWithin, right._1.privateWithin),
          sameScope(left._1.protectedWithin, target.symbol.protectedWithin) &&
            sameScope(left._1.protectedWithin, right._1.protectedWithin),
          left._1.annotations.size == target.symbol.annotations.size &&
            left._1.annotations.size == right._1.annotations.size &&
            left._1.annotations.zip(right._1.annotations).forall((a, b) => a eq b) &&
            left._1.annotations.zip(target.symbol.annotations).forall((a, b) => a eq b),
          left._2 == target.name && left._2 == right._2,
          left._3.size,
          left._3.zip(clause.params).forall((a, b) => a eq b) &&
            left._3.zip(right._3).forall((a, b) => a eq b),
          left._3.map(_.symbol) == clause.params.map(_.symbol),
          left._3.forall(parameter =>
            parameter.symbol != Symbol.noSymbol &&
              parameter.symbol.owner == target.symbol &&
              parameter.symbol.flags.is(Flags.Implicit) &&
              !parameter.symbol.flags.is(Flags.Given) &&
              !parameter.symbol.flags.is(Flags.Synthetic) &&
              !parameter.symbol.flags.is(Flags.Erased) &&
              !parameter.symbol.flags.is(Flags.HasDefault)
          ),
          left._3.map(_.symbol).distinct.size == left._3.size,
          target.symbol.paramSymss == List(clause.params.map(_.symbol)),
          left._4 =:= target.returnTpt.tpe,
          target.rhs.exists(_ eq left._5),
          right._4 =:= left._4 && (right._5 eq left._5)
        )
      }
      val plainRows = List("one", "two", "three", "many", "nestedResult").map { name =>
        val target = definitions(name)
        val clause = target.paramss.head.asInstanceOf[TermParamClause]
        val value = omitted.unapply(target).get
        (
          name,
          value._1 == target.name,
          value._2.size,
          value._2.zip(clause.params).forall((left, right) => left eq right),
          value._2.map(_.symbol) == clause.params.map(_.symbol),
          value._2.forall(parameter =>
            parameter.symbol != Symbol.noSymbol &&
              parameter.symbol.owner == target.symbol &&
              parameter.symbol.flags.is(Flags.Implicit) &&
              !parameter.symbol.flags.is(Flags.Given) &&
              !parameter.symbol.flags.is(Flags.Synthetic) &&
              !parameter.symbol.flags.is(Flags.Erased) &&
              !parameter.symbol.flags.is(Flags.HasDefault)
          ),
          value._2.map(_.symbol).distinct.size == value._2.size,
          target.symbol.paramSymss == List(clause.params.map(_.symbol)),
          value._3 =:= target.returnTpt.tpe,
          target.rhs.exists(_ eq value._4),
          hybrid.unapply(target).nonEmpty
        )
      }
      val modifierRows = List(
        "one",
        "modified",
        "protectedImplicit",
        "finalImplicit",
        "overrideImplicit",
        "qualifiedPrivateImplicit",
        "qualifiedProtectedImplicit",
        "implicitMethod",
        "annotated",
        "infixImplicit",
        "inlineImplicit",
        "transparentImplicit"
      ).map(name =>
        (name, hybrid.unapply(definitions(name)).nonEmpty, omitted.unapply(definitions(name)).nonEmpty)
      )
      (capturedRows, plainRows, modifierRows)

    val (capturedRows, plainRows, modifierRows) = rows
    capturedRows.foreach { row =>
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
    val cardinalities = capturedRows.map(row => row._1 -> row._7).toMap
    assertEquals(cardinalities("one"), 1)
    assertEquals(cardinalities("two"), 2)
    assertEquals(cardinalities("three"), 3)
    assertEquals(cardinalities("many"), 4)
    plainRows.foreach { row =>
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
    assertEquals(
      plainRows.map(row => row._1 -> row._3).toMap,
      Map("one" -> 1, "two" -> 2, "three" -> 3, "many" -> 4, "nestedResult" -> 1)
    )
    assertEquals(modifierRows.head, ("one", true, true))
    modifierRows.tail.foreach(row => assertEquals(row, (row._1, true, false)))

  test("typed-Scalameta Q031 preserves using generic context-bound and complete-paramss closure"):
    import quasiquotes.scalameta.ScalametaQuasiPattern.dqq

    given Compiler = Compiler.make(getClass.getClassLoader)
    val result = withQuotes:
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

      val implicitTarget = definition('{ def old(implicit ordering: Ordering[Int]): Int = 1; () }, "old")
      val namedUsing = definition('{ def named(using ordering: Ordering[Int]): Int = 1; () }, "named")
      val anonymousUsing = definition('{ def anonymous(using Ordering[Int]): Int = 1; () }, "anonymous")
      val genericImplicit = definition('{ def genericImplicit[A](implicit ordering: Ordering[A]): Int = 1; () }, "genericImplicit")
      val bounded = definition('{ def bounded[A: Ordering]: Int = 1; () }, "bounded")
      val boundedOrdinary = definition('{ def boundedOrdinary[A: Ordering](value: A): A = value; () }, "boundedOrdinary")
      val boundedUsing = definition('{ def boundedUsing[A: Ordering](using marker: Numeric[Int]): Int = 1; () }, "boundedUsing")
      val q031 = dqq(StringContext("", " def ", "(implicit ..", "): ", " = ", ""))(using q)
      val q032 = dqq(StringContext("def ", "(implicit ..", "): ", " = ", ""))(using q)
      val q028 = dqq(StringContext("", " def ", "(using ..", "): ", " = ", ""))(using q)
      val q026 = dqq(StringContext("", " def ", "(...", "): ", " = ", ""))(using q)
      (
        q031.unapply(implicitTarget).nonEmpty,
        q031.unapply(namedUsing).isEmpty,
        q031.unapply(anonymousUsing).isEmpty,
        q031.unapply(genericImplicit).isEmpty,
        q031.unapply(bounded).isEmpty,
        q031.unapply(boundedOrdinary).isEmpty,
        q031.unapply(boundedUsing).isEmpty,
        q032.unapply(implicitTarget).nonEmpty,
        q032.unapply(namedUsing).isEmpty,
        q032.unapply(anonymousUsing).isEmpty,
        q032.unapply(genericImplicit).isEmpty,
        q032.unapply(bounded).isEmpty,
        q032.unapply(boundedOrdinary).isEmpty,
        q032.unapply(boundedUsing).isEmpty,
        q028.unapply(implicitTarget).isEmpty,
        q026.unapply(implicitTarget).isEmpty
      )

    assertEquals(result, (true, true, true, true, true, true, true, true, true, true, true, true, true, true, true, true))

  test("typed-Scalameta selector admits only the exact Q031 static grammar"):
    inline def messages(inline source: String): List[String] = typeCheckErrors(source).map(_.message)
    inline def patternMessages(inline pattern: String): List[String] =
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             """ + pattern + """
             case _ => ()"""
      )

    val accepted = patternMessages("""case dqq"$mods def $name(implicit ..$params): $result = $body" => ()""")
    val siblingAccepted = List(
      patternMessages("""case dqq"def $name(implicit ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(using ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name(using ..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(...$paramss): $result = $body" => ()"""),
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
      patternMessages("""case dqq"$mods def $name(implicit ..$params): $result = $left + $right" => ()"""),
      patternMessages("""case dqq"$mods def $name(implicit ..$params): $result = $body extra" => ()"""),
      patternMessages("""case dqq"$mods def $name(implicit .$params): $result = $body" => ()""")
    )

    assertEquals(accepted, Nil)
    assert(siblingAccepted.forall(_.isEmpty), siblingAccepted)
    assert(rejected.forall(_.nonEmpty), rejected)
    assert(rejected.flatten.forall(_.contains("Invalid Scalameta dqq definition-pattern template")), rejected)

  test("typed-Scalameta selector admits only the exact Q032 static grammar"):
    inline def messages(inline source: String): List[String] = typeCheckErrors(source).map(_.message)
    inline def patternMessages(inline pattern: String): List[String] =
      messages(
        """import scala.quoted.*; import quasiquotes.scalameta.ScalametaQuasiPattern.dqq
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
    assert(rejected.flatten.forall(_.contains("Invalid Scalameta dqq definition-pattern template")), rejected)

  test("typed-Scalameta dynamic Q031 selection retains exact-one fallback"):
    val errors = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.matching.{DefinitionModifiers, RankedDefinitionPatternExtractor}; import quasiquotes.scalameta.ScalametaQuasiPattern
         def f(using q: Quotes)(context: StringContext): RankedDefinitionPatternExtractor[
           q.reflect.DefDef,
           (DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term], String, Seq[q.reflect.ValDef], q.reflect.TypeRepr, q.reflect.Term)
         ] = ScalametaQuasiPattern.dqq(context)(using q)"""
    )
    assert(errors.nonEmpty, errors)

  test("typed-Scalameta dynamic Q032 selection retains exact-one fallback"):
    val errors = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.matching.RankedDefinitionPatternExtractor; import quasiquotes.scalameta.ScalametaQuasiPattern
         def f(using q: Quotes)(context: StringContext): RankedDefinitionPatternExtractor[
           q.reflect.DefDef,
           (String, Seq[q.reflect.ValDef], q.reflect.TypeRepr, q.reflect.Term)
         ] = ScalametaQuasiPattern.dqq(context)(using q)"""
    )
    assert(errors.nonEmpty, errors)
