package quasiquotes.q033

import scala.annotation.StaticAnnotation
import scala.compiletime.testing.typeCheckErrors
import scala.language.experimental.erasedDefinitions
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

final class Q033Annotation extends StaticAnnotation
trait Q033Marker

final class Q033MixedOrdinaryNamedUsingFeasibilityTest extends munit.FunSuite:
  test("test-only Q033 grammar exposes exact external capture types"):
    val _ = external.consumer.Q033ExternalMixedOrdinaryNamedUsingConsumer

  test("public reflection keeps absent empty ordinary and mixed clause topology distinct"):
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

      val targets = List(
        "no-clauses" -> definition('{ def noClauses: Int = 0; () }, "noClauses"),
        "empty-ordinary" -> definition('{ def emptyOrdinary(): Int = 0; () }, "emptyOrdinary"),
        "ordinary" -> definition('{ def ordinary(x: Int): Int = x; () }, "ordinary"),
        "named-using" -> definition('{ def namedUsing(using ord: Ordering[Int]): Int = 1; () }, "namedUsing"),
        "mixed" -> definition('{ def mixed(x: Int)(using ord: Ordering[Int]): Int = x; () }, "mixed"),
        "mixed-many" -> definition('{ def mixedMany(x: Int, y: String)(using ord: Ordering[Int], num: Numeric[Int]): Int = x; () }, "mixedMany"),
        "empty-then-using" -> definition('{ def emptyThenUsing()(using ord: Ordering[Int]): Int = 1; () }, "emptyThenUsing")
      )
      val candidate = Q033MixedClauseCandidateFactory.capturedModifiers(using q)

      targets.map { (label, target) =>
        val termClauses = target.paramss.collect { case clause: TermParamClause => clause }
        (
          label,
          target.paramss.map {
            case _: TypeParamClause => "type"
            case clause: TermParamClause =>
              if clause.isGiven then "using"
              else if clause.isImplicit then "implicit"
              else if clause.isErased then "erased"
              else "ordinary"
          },
          termClauses.map(_.params.size),
          target.symbol.paramSymss.map(_.size),
          target.symbol.paramSymss == target.paramss.map(_.params.map(_.symbol)),
          candidate.unapply(target).nonEmpty
        )
      }

    rows.foreach(row => println(s"Q033_TOPOLOGY ${dotty.tools.dotc.config.Properties.versionNumberString} $row"))
    assertEquals(rows.map(row => row._1 -> row._2).toMap, Map(
      "no-clauses" -> Nil,
      "empty-ordinary" -> List("ordinary"),
      "ordinary" -> List("ordinary"),
      "named-using" -> List("using"),
      "mixed" -> List("ordinary", "using"),
      "mixed-many" -> List("ordinary", "using"),
      "empty-then-using" -> List("ordinary", "using")
    ))
    assertEquals(rows.map(row => row._1 -> row._3).toMap.apply("empty-ordinary"), List(0))
    assertEquals(rows.map(row => row._1 -> row._3).toMap.apply("empty-then-using"), List(0, 1))
    assertEquals(rows.map(row => row._1 -> row._4).toMap.apply("empty-then-using"), List(0, 1))
    assert(rows.forall(_._5), rows)
    assertEquals(rows.filter(_._6).map(_._1).toSet, Set("mixed", "mixed-many", "empty-then-using"))

    val emptyUsingErrors = typeCheckErrors("def invalid(using): Int = 1")
    assert(emptyUsingErrors.nonEmpty, emptyUsingErrors)

  test("candidate A preserves cardinality identity ownership modes modifiers result and body"):
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
          @Q033Annotation private[q033] def annotated(x: Int)(using ord: Ordering[Int]): Int = x
        ()
      }.asTerm)(Symbol.spliceOwner)

      def sameScope(left: Option[TypeRepr], right: Option[TypeRepr]): Boolean =
        (left, right) match
          case (None, None) => true
          case (Some(a), Some(b)) => a =:= b
          case _ => false

      val candidate = Q033MixedClauseCandidateFactory.capturedModifiers(using q)
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
        val captured = candidate.unapply(target).get
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

    rows.foreach { row =>
      row.productIterator.drop(1).foreach(value => assertEquals(value, true, row))
    }

  test("candidate A rejects the complete malformed and out-of-scope target matrix"):
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
        "context-bound" -> definition('{ def cb[A: Ordering](value: A)(using marker: Q033Marker): A = value; () }, "cb"),
        "context-bound-using" -> definition('{ def cbUsing[A: Ordering](using marker: Q033Marker): Int = 1; () }, "cbUsing"),
        "generic" -> definition('{ def generic[A](value: A)(using ord: Ordering[A]): A = value; () }, "generic"),
        "default-ordinary" -> definition('{ def defaultOrdinary(x: Int = 1)(using ord: Ordering[Int]): Int = x; () }, "defaultOrdinary"),
        "default-using" -> definition('{ def defaultUsing(x: Int)(using ord: Ordering[Int] = null): Int = x; () }, "defaultUsing"),
        "erased-ordinary" -> definition('{ def erasedOrdinary(erased x: Int)(using ord: Ordering[Int]): Int = 1; () }, "erasedOrdinary"),
        "foreign-owner" -> DefDef.copy(exact)(exact.name, foreign.paramss, exact.returnTpt, exact.rhs),
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
      val candidate = Q033MixedClauseCandidateFactory.capturedModifiers(using q)
      targets.map((label, target) => label -> candidate.unapply(target).isEmpty)

    rows.foreach(row => assert(row._2, row))

  test("test-only grammar admits only ordinary-then-named-using with two independent rank-2 captures"):
    inline def messages(inline source: String): List[String] = typeCheckErrors(source).map(_.message)
    inline def patternMessages(inline pattern: String): List[String] =
      messages(
        """import scala.quoted.*; import quasiquotes.q033.Q033StandardDefinitionPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             """ + pattern + """
             case _ => ()"""
      )

    val accepted = patternMessages("""case dqq"$mods def $name(..$params)(using ..$usingParams): $result = $body" => ()""")
    val rejected = List(
      patternMessages("""case dqq"$mods def $name(using ..$usingParams)(..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$first)(..$second): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(using ..$first)(using ..$second): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(...$paramss)(using ..$usingParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(using ...$usingParamss): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(fixed: Int, ..$params)(using ..$usingParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(using fixed: Int, ..$usingParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name()(using ..$usingParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(using): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$same)(using ..$same): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(using ..$usingParams)(extra: Int): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name[..$tparams](..$params)(using ..$usingParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(using ..$usingParams): Int = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(using ..$usingParams): $result = $body + 1" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(using ..$usingParams): $result = $left + $right" => ()"""),
      patternMessages("""case dqq"def $name(..$params)(using ..$usingParams): $result = $body" => ()""")
    )

    assertEquals(accepted, Nil)
    assert(rejected.forall(_.nonEmpty), rejected)
    assert(
      rejected.flatten.forall(message =>
        message.contains("Invalid Q033 standard dqq") ||
          message.contains("duplicate pattern variable: same")
      ),
      rejected
    )

    val dynamic = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.q033.Q033StandardDefinitionPattern
         def f(using q: Quotes)(context: StringContext) = Q033StandardDefinitionPattern.dqq(context)(using q)"""
    )
    assert(dynamic.exists(_.message.contains("must be statically known")), dynamic)

  test("real production dqq remains closed for the conceptual Q033 grammar"):
    val errors = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.matching.DefinitionPattern.dqq
         def f(using q: Quotes)(d: q.reflect.DefDef) = d match
           case dqq"$mods def $name(..$params)(using ..$usingParams): $result = $body" => ()
           case _ => ()"""
    ).map(_.message)
    assert(errors.nonEmpty, errors)
    assert(errors.exists(_.contains("Invalid dqq definition-pattern template")), errors)
