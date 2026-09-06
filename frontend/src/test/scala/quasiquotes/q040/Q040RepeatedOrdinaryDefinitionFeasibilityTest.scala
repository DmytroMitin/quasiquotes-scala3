package quasiquotes.q040

import scala.annotation.StaticAnnotation
import scala.compiletime.testing.typeCheckErrors
import scala.language.experimental.erasedDefinitions
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

final class Q040Annotation extends StaticAnnotation

final class Q040RepeatedOrdinaryDefinitionFeasibilityTest extends munit.FunSuite:
  test("test-only Q040 grammar exposes the exact external repeated-parameter capture type"):
    val _ = external.consumer.Q040ExternalRepeatedOrdinaryDefinitionConsumer

  test("public Quotes exposes a stable repeated TypeRepr attached to the original ValDef"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val rows = withQuotes:
      Q040QuotesRepeatedEvidence.inspect

    rows.foreach(row => println(s"Q040_QUOTES $row"))
    val methods = rows.map(row => row.name -> row).toMap
    assertEquals(methods.keySet, Set(
      "plainSeq",
      "repeatedOnly",
      "prefixRepeated",
      "nestedRepeated",
      "generic",
      "multiple",
      "byName",
      "modified"
    ))
    assert(rows.forall(_.exactParamSymss), rows)
    assert(rows.flatMap(_.parameters).forall(parameter =>
      parameter.symbolPresent &&
        parameter.ownerIsMethod &&
        parameter.isParam &&
        !parameter.isImplicit &&
        !parameter.isGiven &&
        !parameter.isSynthetic &&
        !parameter.isErased &&
        !parameter.hasDefault &&
        parameter.positionStart >= 0 &&
        parameter.positionEnd > parameter.positionStart
    ), rows)

    val plain = methods("plainSeq").parameters.head
    assertEquals(plain.typeFamily, "applied-1")
    assert(!plain.repeatedByValDefStructure, plain)
    assert(!plain.repeatedByAnnotationSymbol, plain)
    assert(!plain.repeatedByMethodStructure, plain)
    assert(plain.sameAsSeqInt, plain)

    val repeated = methods("repeatedOnly").parameters.head
    assertEquals(repeated.typeFamily, "repeated-annotated")
    assert(repeated.repeatedByValDefStructure, repeated)
    assert(repeated.repeatedByAnnotationSymbol, repeated)
    assert(repeated.repeatedByMethodStructure, repeated)
    assertEquals(repeated.valDefElementFamily, Some("type-reference"))
    assertEquals(repeated.methodElementFamily, Some("type-reference"))
    assert(repeated.sameAsSeqInt, repeated)

    val prefix = methods("prefixRepeated").parameters
    assertEquals(prefix.map(_.repeatedByValDefStructure), List(false, true))
    assertEquals(prefix.map(_.repeatedByMethodStructure), List(false, true))
    assertEquals(prefix.last.valDefElementFamily, Some("type-reference"))
    assertEquals(methods("nestedRepeated").parameters.head.valDefElementFamily, Some("applied-1"))
    assertEquals(methods("generic").clauseKinds, List("type", "ordinary"))
    assert(methods("generic").parameters.head.repeatedByValDefStructure, methods("generic"))
    assert(!methods("generic").parameters.head.repeatedByMethodStructure, methods("generic"))
    assertEquals(methods("multiple").clauseKinds, List("ordinary", "ordinary"))
    assertEquals(methods("multiple").clauseSizes, List(1, 1))
    assert(methods("multiple").parameters.last.repeatedByValDefStructure, methods("multiple"))
    assertEquals(methods("byName").parameters.head.typeFamily, "by-name")
    assert(rows.filterNot(_.name == "generic").forall(_.methodTypeFamily == "method"), rows)
    assertEquals(methods("generic").methodTypeFamily, "poly-method")
    assert(methods.values.flatMap(_.methodParameterFamilies.flatten).exists(_ == "repeated-applied-1"), rows)

  test("test-only candidate preserves binders topology modifiers result and RHS identity"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val rows = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*

      val definitions = fixtureDefinitions(using q)
      val candidate = Q040RepeatedOrdinaryCandidateFactory.capturedModifiers(using q)

      def sameScope(left: Option[TypeRepr], right: Option[TypeRepr]): Boolean =
        (left, right) match
          case (None, None) => true
          case (Some(a), Some(b)) => a =:= b
          case _ => false

      List("repeatedOnly", "prefixRepeated", "nestedRepeated", "modified").map { name =>
        val target = definitions(name)
        val original = target.paramss.head.asInstanceOf[TermParamClause].params
        val captured = candidate.unapply(target).get
        (
          name,
          captured._1.flags == target.symbol.flags,
          sameScope(captured._1.privateWithin, target.symbol.privateWithin),
          sameScope(captured._1.protectedWithin, target.symbol.protectedWithin),
          captured._1.annotations.size == target.symbol.annotations.size &&
            captured._1.annotations.zip(target.symbol.annotations).forall((left, right) => left eq right),
          captured._2 == target.name,
          captured._3.zip(original).forall((left, right) => left eq right),
          captured._3.map(_.symbol).forall(_ != Symbol.noSymbol),
          captured._3.map(_.symbol).distinct.size == captured._3.size,
          captured._3.forall(_.symbol.owner == target.symbol),
          target.symbol.paramSymss == List(captured._3.map(_.symbol).toList),
          Q040RepeatedOrdinaryCandidateFactory.repeatedElementType(using q)(captured._3.last).nonEmpty,
          captured._4 =:= target.returnTpt.tpe,
          target.rhs.exists(_ eq captured._5)
        )
      }

    rows.foreach(row => row.productIterator.drop(1).foreach(value => assertEquals(value, true, row)))

  test("test-only candidate rejects nonrepeated out-of-scope and structurally corrupted targets"):
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

      val exact = definition('{ def exact(head: Int, tail: String*): Int = head + tail.size; () }, "exact")
      val foreign = definition('{ def foreign(head: Int, tail: String*): Int = head + tail.size; () }, "foreign")
      val clause = exact.paramss.head.asInstanceOf[TermParamClause]
      val constructor = definition('{ class Sample(values: Int*); () }, "<init>")
      val extension = definition('{ extension (value: Int) def expanded(values: Int*): Int = values.size; () }, "expanded")
      val provided = definition('{ given provided(using value: Int): Int = value; () }, "provided")
      def flaggedAccessor(flags: Flags): DefDef =
        val symbol = Symbol.newMethod(
          Symbol.spliceOwner,
          "accessor",
          MethodType(Nil)(_ => Nil, _ => TypeRepr.of[String]),
          flags,
          Symbol.noSymbol
        )
        DefDef(symbol, _ => Some(Literal(StringConstant("value"))))
      val repeatedInt = defn.RepeatedParamClass.typeRef.appliedTo(TypeRepr.of[Int])
      val syntheticSymbol = Symbol.newVal(
        exact.symbol,
        "syntheticRepeated",
        repeatedInt,
        Flags.Synthetic,
        Symbol.noSymbol
      )
      val syntheticClause = TermParamClause(List(ValDef(syntheticSymbol, None)))
      val candidate = Q040RepeatedOrdinaryCandidateFactory.capturedModifiers(using q)
      val targets = List(
        "plain" -> definition('{ def plain(value: Int): Int = value; () }, "plain"),
        "plain-seq" -> definition('{ def plainSeq(values: Seq[Int]): Int = values.size; () }, "plainSeq"),
        "generic" -> definition('{ def generic[A](values: A*): Int = values.size; () }, "generic"),
        "multiple-ordinary" -> definition('{ def multiple(head: Int)(tail: String*): Int = head + tail.size; () }, "multiple"),
        "by-name" -> definition('{ def byName(value: => Int): Int = value; () }, "byName"),
        "defaulted" -> definition('{ def defaulted(value: Int = 1): Int = value; () }, "defaulted"),
        "using" -> definition('{ def usingClause(using value: Int): Int = value; () }, "usingClause"),
        "implicit" -> definition('{ def implicitClause(implicit value: Int): Int = value; () }, "implicitClause"),
        "erased" -> definition('{ def erasedClause(erased value: Int): Int = 0; () }, "erasedClause"),
        "parameter-annotation" -> definition('{ def annotated(@Q040Annotation values: Int*): Int = values.size; () }, "annotated"),
        "foreign-owner" -> DefDef.copy(exact)(exact.name, foreign.paramss, exact.returnTpt, exact.rhs),
        "duplicate" -> DefDef.copy(exact)(exact.name, List(TermParamClause(List(clause.params.last, clause.params.last))), exact.returnTpt, exact.rhs),
        "reordered" -> DefDef.copy(exact)(exact.name, List(TermParamClause(clause.params.reverse)), exact.returnTpt, exact.rhs),
        "param-symss-mismatch" -> DefDef.copy(exact)(exact.name, List(TermParamClause(List(clause.params.last))), exact.returnTpt, exact.rhs),
        "synthetic-structural" -> DefDef.copy(exact)(exact.name, List(syntheticClause), exact.returnTpt, exact.rhs),
        "missing-rhs" -> DefDef.copy(exact)(exact.name, exact.paramss, exact.returnTpt, None),
        "constructor" -> constructor,
        "extension" -> extension,
        "field-accessor" -> flaggedAccessor(Flags.FieldAccessor),
        "param-accessor" -> flaggedAccessor(Flags.ParamAccessor),
        "case-accessor" -> flaggedAccessor(Flags.CaseAccessor),
        "given" -> provided,
        "null" -> null.asInstanceOf[DefDef]
      )
      targets.map((label, target) => label -> candidate.unapply(target).isEmpty)

    rows.foreach(row => assert(row._2, row))

  test("source validity matrix distinguishes valid repeated placement from rejected forms"):
    inline def errors(inline source: String): List[String] = typeCheckErrors(source).map(_.message)

    val valid = List(
      "plain-seq" -> errors("def plainSeq(xs: Seq[Int]): Int = xs.size"),
      "repeated-only" -> errors("def repeatedOnly(xs: Int*): Int = xs.size"),
      "prefix-repeated" -> errors("def prefixRepeated(head: Int, tail: String*): Int = head + tail.size"),
      "nested-repeated" -> errors("def nestedRepeated(xs: List[Option[Int]]*): Int = xs.size"),
      "generic-contrast" -> errors("def generic[A](xs: A*): Int = xs.size"),
      "multiple-clauses" -> errors("def multiple(prefix: Int)(tail: Long*): Int = prefix + tail.size"),
      "by-name-contrast" -> errors("def byName(value: => Seq[Int]): Int = value.size")
    )
    val invalid = List(
      "following-parameter" -> errors("def following(xs: Int*, y: Int): Int = y"),
      "two-repeated" -> errors("def twice(xs: Int*, ys: String*): Int = 0"),
      "repeated-default" -> errors("def repeatedDefault(xs: Int* = Seq(1)): Int = xs.size")
    )
    val contextualContrasts = List(
      "using-repeated" -> errors("def usingRepeated(using xs: Int*): Int = xs.size"),
      "implicit-repeated" -> errors("def implicitRepeated(implicit xs: Int*): Int = xs.size")
    )
    println(s"Q040_SOURCE_VALIDITY valid=$valid invalid=$invalid contextual=$contextualContrasts")
    assert(valid.forall(_._2.isEmpty), valid)
    assert(invalid.forall(_._2.nonEmpty), invalid)
    assert(
      contextualContrasts.forall(_._2.isEmpty) || contextualContrasts.forall(_._2.nonEmpty),
      contextualContrasts
    )

  test("current production admits repeated parameters only through its existing rank-3 ordinary selector"):
    import quasiquotes.matching.DefinitionPattern.dqq

    given Compiler = Compiler.make(getClass.getClassLoader)
    val rows = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*
      val definitions = fixtureDefinitions(using q)
      val existing = dqq(StringContext("", " def ", "(...", "): ", " = ", ""))(using q)
      val repeated = existing.unapply(definitions("repeatedOnly"))
      val plain = existing.unapply(definitions("plainSeq"))
      (
        repeated.nonEmpty,
        repeated.exists(_._3.flatten.last eq definitions("repeatedOnly").paramss.head.asInstanceOf[TermParamClause].params.last),
        plain.nonEmpty
      )

    assertEquals(rows, (true, true, true))

    val rankTwo = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.matching.DefinitionPattern.dqq
         def f(using q: Quotes)(d: q.reflect.DefDef) = d match
           case dqq\"$mods def $name(..$params): $result = $body\" => ()
           case _ => ()"""
    )
    assert(rankTwo.nonEmpty, rankTwo)

  test("test-only static grammar is exact and rejects dynamic or malformed layouts"):
    inline def messages(inline source: String): List[String] = typeCheckErrors(source).map(_.message)
    inline def candidateMessages(inline pattern: String): List[String] =
      messages(
        """import scala.quoted.*; import quasiquotes.q040.Q040RepeatedOrdinaryDefinitionPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             """ + pattern + """
             case _ => ()"""
      )

    val accepted = candidateMessages("""case dqq"$mods def $name(..$params): $result = $body" => ()""")
    val rejected = List(
      candidateMessages("""case dqq"def $name(..$params): $result = $body" => ()"""),
      candidateMessages("""case dqq"$mods def fixed(..$params): $result = $body" => ()"""),
      candidateMessages("""case dqq"$mods def $name(...$paramss): $result = $body" => ()"""),
      candidateMessages("""case dqq"$mods def $name[..$tparams](..$params): $result = $body" => ()"""),
      candidateMessages("""case dqq"$mods def $name(fixed: Int, ..$params): $result = $body" => ()"""),
      candidateMessages("""case dqq"$mods def $name(..$params)(..$second): $result = $body" => ()"""),
      candidateMessages("""case dqq"$mods def $name(using ..$params): $result = $body" => ()"""),
      candidateMessages("""case dqq"$mods def $name(..$params): Int = $body" => ()"""),
      candidateMessages("""case dqq"$mods def $name(..$params): $result = $left + $right" => ()"""),
      candidateMessages("""case dqq"$mods def $name(.$params): $result = $body" => ()"""),
      candidateMessages("""case dqq"$mods def $name(..$params): $result = $body trailing" => ()"""),
      candidateMessages("""case dqq"$mods def $name(..$params): $result = $params" => ()""")
    )
    val dynamic = messages(
      """import scala.quoted.*; import quasiquotes.q040.Q040RepeatedOrdinaryDefinitionPattern.dqq
         def f(using q: Quotes)(context: StringContext) = context.dqq"""
    )
    assertEquals(accepted, Nil)
    assert(rejected.forall(_.nonEmpty), rejected)
    assert(rejected.dropRight(1).flatten.forall(_.contains("Invalid Q040 standard dqq")), rejected)
    assert(rejected.last.exists(_.contains("duplicate pattern variable")), rejected.last)
    assert(dynamic.nonEmpty, dynamic)

  private def fixtureDefinitions(using q: Quotes): Map[String, q.reflect.DefDef] =
    import q.reflect.*

    val found = scala.collection.mutable.Map.empty[String, DefDef]
    val traversal = new TreeTraverser:
      override def traverseTree(tree: Tree)(owner: Symbol): Unit =
        tree match
          case definition: DefDef => found.update(definition.name, definition)
          case _ => ()
        super.traverseTree(tree)(owner)
    traversal.traverseTree('{
      class Fixture:
        def plainSeq(xs: Seq[Int]): Int = xs.size
        def repeatedOnly(xs: Int*): Int = xs.size
        def prefixRepeated(head: Int, tail: String*): Int = head + tail.size
        def nestedRepeated(xs: List[Option[Int]]*): List[Option[Int]] = xs.head
        final def modified(xs: Int*): Int = xs.size
      ()
    }.asTerm)(Symbol.spliceOwner)
    found.toMap
