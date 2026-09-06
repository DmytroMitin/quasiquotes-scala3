package quasiquotes.q036

import scala.annotation.StaticAnnotation
import scala.compiletime.testing.typeCheckErrors
import scala.language.experimental.erasedDefinitions
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

final class Q036Annotation extends StaticAnnotation
trait Q036Marker

final class Q036MixedOrdinaryScala2ImplicitFeasibilityTest extends munit.FunSuite:
  test("test-only Q036 grammar exposes exact external capture types"):
    val _ = external.consumer.Q036ExternalMixedOrdinaryScala2ImplicitConsumer

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
        "one-implicit" -> definition('{ def oneImplicit(implicit ord: Ordering[Int]): Int = 1; () }, "oneImplicit"),
        "named-using" -> definition('{ def namedUsing(using ord: Ordering[Int]): Int = 1; () }, "namedUsing"),
        "mixed" -> definition('{ def mixed(x: Int)(implicit ord: Ordering[Int]): Int = x; () }, "mixed"),
        "mixed-many" -> definition('{ def mixedMany(x: Int, y: String)(implicit ord: Ordering[Int], num: Numeric[Int]): Int = x; () }, "mixedMany"),
        "empty-then-implicit" -> definition('{ def emptyThenImplicit()(implicit ord: Ordering[Int]): Int = 1; () }, "emptyThenImplicit")
      )
      val candidate = Q036MixedClauseCandidateFactory.capturedModifiers(using q)

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

    rows.foreach(row => println(s"Q036_TOPOLOGY ${dotty.tools.dotc.config.Properties.versionNumberString} $row"))
    assertEquals(rows.map(row => row._1 -> row._2).toMap, Map(
      "no-clauses" -> Nil,
      "empty-ordinary" -> List("ordinary"),
      "ordinary" -> List("ordinary"),
      "one-implicit" -> List("implicit"),
      "named-using" -> List("using"),
      "mixed" -> List("ordinary", "implicit"),
      "mixed-many" -> List("ordinary", "implicit"),
      "empty-then-implicit" -> List("ordinary", "implicit")
    ))
    assertEquals(rows.map(row => row._1 -> row._3).toMap.apply("empty-ordinary"), List(0))
    assertEquals(rows.map(row => row._1 -> row._3).toMap.apply("empty-then-implicit"), List(0, 1))
    assertEquals(rows.map(row => row._1 -> row._4).toMap.apply("empty-then-implicit"), List(0, 1))
    assert(rows.forall(_._5), rows)
    assertEquals(rows.filter(_._6).map(_._1).toSet, Set("mixed", "mixed-many", "empty-then-implicit"))

    val emptyImplicitErrors = typeCheckErrors("def invalid(implicit): Int = 1")
    assert(emptyImplicitErrors.nonEmpty, emptyImplicitErrors)

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
          def oneOne(x: Int)(implicit ord: Ordering[Int]): Int = x
          def twoOne(x: Int, y: String)(implicit ord: Ordering[Int]): Int = x
          def oneTwo(x: Int)(implicit ord: Ordering[Int], num: Numeric[Int]): Int = x
          def twoTwo(x: Int, y: String)(implicit ord: Ordering[Int], num: Numeric[Int]): Int = x
          def many(a: Int, b: Int, c: Int, d: Int)(implicit ord: Ordering[Int]): Int = a
          def nested(x: Int)(implicit ord: Ordering[Int]): List[Option[Int]] = List(Some(x))
          def empty()(implicit ord: Ordering[Int]): Int = 1
          final def modified(x: Int)(implicit ord: Ordering[Int]): Int = x
          @Q036Annotation private[q036] def annotated(x: Int)(implicit ord: Ordering[Int]): Int = x
        ()
      }.asTerm)(Symbol.spliceOwner)

      def sameScope(left: Option[TypeRepr], right: Option[TypeRepr]): Boolean =
        (left, right) match
          case (None, None) => true
          case (Some(a), Some(b)) => a =:= b
          case _ => false

      val candidate = Q036MixedClauseCandidateFactory.capturedModifiers(using q)
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
        val captured = candidate.unapply(target).get
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

      val exact = definition('{ def exact(x: Int, y: Int)(implicit first: Ordering[Int], second: Numeric[Int]): Int = x; () }, "exact")
      val foreign = definition('{ def foreign(x: Int, y: Int)(implicit first: Ordering[Int], second: Numeric[Int]): Int = x; () }, "foreign")
      val List(ordinary: TermParamClause, contextual: TermParamClause) = exact.paramss: @unchecked
      val erasedImplicitSymbol = Symbol.newVal(
        exact.symbol,
        "erasedImplicit",
        TypeRepr.of[Ordering[Int]],
        Flags.Implicit | Flags.Erased,
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
        "using-only" -> definition('{ def onlyUsing(using ord: Ordering[Int]): Int = 1; () }, "onlyUsing"),
        "implicit-only" -> definition('{ def old(implicit ord: Ordering[Int]): Int = 1; () }, "old"),
        "ordinary-using" -> definition('{ def mixedUsing(x: Int)(using ord: Ordering[Int]): Int = x; () }, "mixedUsing"),
        "using-ordinary" -> definition('{ def wrongOrder(using ord: Ordering[Int])(x: Int): Int = x; () }, "wrongOrder"),
        "extra-clause-structural" -> DefDef.copy(exact)(exact.name, List(ordinary, contextual, ordinary), exact.returnTpt, exact.rhs),
        "two-implicit-structural" -> DefDef.copy(exact)(exact.name, List(contextual, contextual), exact.returnTpt, exact.rhs),
        "two-ordinary" -> definition('{ def twoOrdinary(x: Int)(y: Int): Int = x + y; () }, "twoOrdinary"),
        "anonymous-using" -> definition('{ def anonymous(x: Int)(using Ordering[Int]): Int = x; () }, "anonymous"),
        "context-bound" -> definition('{ def cb[A: Ordering](value: A)(implicit marker: Q036Marker): A = value; () }, "cb"),
        "context-bound-implicit-only" -> definition('{ def cbImplicit[A: Ordering](implicit marker: Q036Marker): Int = 1; () }, "cbImplicit"),
        "context-bound-using" -> definition('{ def cbUsing[A: Ordering](using marker: Q036Marker): Int = 1; () }, "cbUsing"),
        "generic" -> definition('{ def genericImplicit[A](value: A)(implicit marker: Q036Marker): A = value; () }, "genericImplicit"),
        "default-ordinary" -> definition('{ def defaultOrdinary(x: Int = 1)(implicit ord: Ordering[Int]): Int = x; () }, "defaultOrdinary"),
        "default-implicit" -> definition('{ def defaultImplicit(x: Int)(implicit ord: Ordering[Int] = null): Int = x; () }, "defaultImplicit"),
        "erased-ordinary" -> definition('{ def erasedOrdinary(erased x: Int)(implicit ord: Ordering[Int]): Int = 1; () }, "erasedOrdinary"),
        "erased-implicit-structural" -> DefDef.copy(exact)(exact.name, List(ordinary, erasedImplicit), exact.returnTpt, exact.rhs),
        "foreign-owner" -> DefDef.copy(exact)(exact.name, foreign.paramss, exact.returnTpt, exact.rhs),
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
      val candidate = Q036MixedClauseCandidateFactory.capturedModifiers(using q)
      targets.map((label, target) => label -> candidate.unapply(target).isEmpty)

    rows.foreach(row => assert(row._2, row))

  test("test-only grammar admits only ordinary-then-Scala-2-implicit with two independent rank-2 captures"):
    inline def messages(inline source: String): List[String] = typeCheckErrors(source).map(_.message)
    inline def patternMessages(inline pattern: String): List[String] =
      messages(
        """import scala.quoted.*; import quasiquotes.q036.Q036StandardDefinitionPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             """ + pattern + """
             case _ => ()"""
      )

    val accepted = patternMessages("""case dqq"$mods def $name(..$params)(implicit ..$usingParams): $result = $body" => ()""")
    val rejected = List(
      patternMessages("""case dqq"$mods def $name(using ..$usingParams)(..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$first)(..$second): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(using ..$first)(implicit ..$second): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$first)(implicit ..$second)(implicit ..$third): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(...$paramss)(implicit ..$usingParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(implicit ...$usingParamss): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(fixed: Int, ..$params)(implicit ..$usingParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(implicit fixed: Int, ..$usingParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name()(implicit ..$usingParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(implicit): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$same)(implicit ..$same): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(implicit ..$usingParams)(extra: Int): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name[..$tparams](..$params)(implicit ..$usingParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def fixed(..$params)(implicit ..$usingParams): $result = $body" => ()"""),
      patternMessages("""case dqq"private def $name(..$params)(implicit ..$usingParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(implicit ..$usingParams): Int = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(implicit ..$usingParams): $result = $body + 1" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(implicit ..$usingParams): $result = $left + $right" => ()"""),
      patternMessages("""case dqq"$mods def $name(.$params)(implicit ..$usingParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(implicit .$usingParams): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(..$params)(implicit ..$usingParams): $result = $body trailing" => ()"""),
      patternMessages("""case dqq"def $name(..$params)(implicit ..$usingParams): $result = $body" => ()""")
    )

    assertEquals(accepted, Nil)
    assert(rejected.forall(_.nonEmpty), rejected)
    assert(
      rejected.flatten.forall(message =>
        message.contains("Invalid Q036 standard dqq") ||
          message.contains("duplicate pattern variable: same")
      ),
      rejected
    )

    val dynamic = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.q036.Q036StandardDefinitionPattern
         def f(using q: Quotes)(context: StringContext) = Q036StandardDefinitionPattern.dqq(context)(using q)"""
    )
    assert(dynamic.exists(_.message.contains("must be statically known")), dynamic)

  test("real production dqq now selects the Q036 conceptual grammar through Q037"):
    val errors = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.matching.DefinitionPattern.dqq
         def f(using q: Quotes)(d: q.reflect.DefDef) = d match
           case dqq"$mods def $name(..$params)(implicit ..$usingParams): $result = $body" => ()
           case _ => ()"""
    ).map(_.message)
    assertEquals(errors, Nil)
