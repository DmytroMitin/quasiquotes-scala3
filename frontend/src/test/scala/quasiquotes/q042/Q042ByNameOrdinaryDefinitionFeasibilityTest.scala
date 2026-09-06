package quasiquotes.q042

import scala.annotation.StaticAnnotation
import scala.compiletime.testing.typeCheckErrors
import scala.language.experimental.erasedDefinitions
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

final class Q042Annotation extends StaticAnnotation

final class Q042ByNameOrdinaryDefinitionFeasibilityTest extends munit.FunSuite:
  test("test-only Q042 grammar exposes the exact external by-name parameter capture type"):
    val _ = external.consumer.Q042ExternalByNameOrdinaryDefinitionConsumer

  test("public Quotes exposes by-name structure on ValDef symbol and MethodType"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val rows = withQuotes:
      Q042QuotesByNameEvidence.inspect

    rows.foreach(row => println(s"Q042_QUOTES $row"))
    val methods = rows.map(row => row.name -> row).toMap
    assertEquals(methods.keySet, Set(
      "strict", "byName", "thunk", "nested", "prefix", "middle", "two",
      "cardinality", "generic", "multiple", "defaulted"
    ))
    assert(rows.forall(_.exactParamSymss), rows)
    assert(rows.forall(_.rhsPresent), rows)
    assert(rows.flatMap(_.parameters).forall(parameter =>
      parameter.symbolPresent && parameter.ownerIsMethod && parameter.isParam &&
        !parameter.isImplicit && !parameter.isGiven && !parameter.isSynthetic &&
        !parameter.isErased && parameter.positionStart >= 0 &&
        parameter.positionEnd > parameter.positionStart
    ), rows)

    val strict = methods("strict").parameters.head
    assertEquals((strict.valDefTypeFamily, strict.symbolTypeFamily, strict.methodTypeFamily),
      ("type-reference", "type-reference", "type-reference"))
    assertEquals((strict.valDefByName, strict.symbolByName, strict.methodByName), (false, false, false))
    assert(strict.elementsAgree, strict)
    assert(strict.sameAsInt, strict)
    assert(!strict.sameAsFunction0Int, strict)

    val byName = methods("byName").parameters.head
    assertEquals((byName.valDefTypeFamily, byName.symbolTypeFamily, byName.methodTypeFamily),
      ("by-name", "type-reference", "by-name"))
    assertEquals((byName.valDefByName, byName.symbolByName, byName.methodByName), (true, false, true))
    assertEquals((byName.valDefElementFamily, byName.symbolElementFamily, byName.methodElementFamily),
      (Some("type-reference"), None, Some("type-reference")))
    assert(byName.elementsAgree, byName)
    assert(byName.symbolMatchesElement, byName)
    assert(!byName.sameAsFunction0Int, byName)

    val thunk = methods("thunk").parameters.head
    assertEquals((thunk.valDefByName, thunk.symbolByName, thunk.methodByName), (false, false, false))
    assert(thunk.sameAsFunction0Int, thunk)
    assert(!thunk.sameAsInt, thunk)

    assertEquals(methods("nested").parameters.head.valDefElementFamily, Some("applied-1"))
    assertEquals(methods("prefix").parameters.map(_.valDefByName), List(false, true))
    assertEquals(methods("middle").parameters.map(_.valDefByName), List(false, true, false))
    assertEquals(methods("two").parameters.map(_.valDefByName), List(true, true))
    assertEquals(methods("cardinality").parameters.map(_.valDefByName), List(true, false, true, false, true))
    assertEquals(methods("cardinality").clauseSizes, List(5))
    assertEquals(methods("generic").clauseKinds, List("type", "ordinary"))
    assertEquals(methods("multiple").clauseKinds, List("ordinary", "ordinary"))
    assert(methods("defaulted").parameters.head.hasDefault, methods("defaulted"))

  test("test-only candidate preserves original binders order ownership modifiers result and RHS"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val rows = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*
      val definitions = fixtureDefinitions(using q)
      val candidate = Q042ByNameOrdinaryCandidateFactory.capturedModifiers(using q)

      def sameScope(left: Option[TypeRepr], right: Option[TypeRepr]): Boolean =
        (left, right) match
          case (None, None) => true
          case (Some(a), Some(b)) => a =:= b
          case _ => false

      List("byName", "nested", "prefix", "middle", "two", "cardinality", "modified").map { name =>
        val target = definitions(name)
        val original = ordinaryParameters(using q)(target)
        val captured = candidate.unapply(target).get
        (
          name,
          captured._1.flags == target.symbol.flags,
          sameScope(captured._1.privateWithin, target.symbol.privateWithin),
          sameScope(captured._1.protectedWithin, target.symbol.protectedWithin),
          captured._1.annotations.size == target.symbol.annotations.size &&
            captured._1.annotations.zip(target.symbol.annotations).forall((left, right) => left eq right),
          captured._2 == target.name,
          captured._3.size == original.size,
          captured._3.zip(original).forall((left, right) => left eq right),
          captured._3.map(_.symbol).forall(_ != Symbol.noSymbol),
          captured._3.map(_.symbol).distinct.size == captured._3.size,
          captured._3.forall(_.symbol.owner == target.symbol),
          target.symbol.paramSymss == List(captured._3.map(_.symbol).toList),
          captured._3.zipWithIndex.filter((parameter, _) =>
            Q042ByNameOrdinaryCandidateFactory.valDefByNameElementType(using q)(parameter).nonEmpty
          ).forall((parameter, index) =>
            val tree = Q042ByNameOrdinaryCandidateFactory.valDefByNameElementType(using q)(parameter)
            val method = Q042ByNameOrdinaryCandidateFactory.methodByNameElementType(using q)(target, index)
            (tree, method) match
              case (Some(a), Some(c)) => a =:= c && parameter.symbol.termRef.widen =:= a
              case _ => false
          ),
          captured._4 =:= target.returnTpt.tpe,
          target.rhs.exists(_ eq captured._5)
        )
      }

    rows.foreach(row => row.productIterator.drop(1).foreach(value => assertEquals(value, true, row)))

  test("test-only candidate rejects out-of-scope and malformed by-name targets"):
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

      def substituted(target: DefDef, name: String, tpe: TypeRepr, flags: Flags): DefDef =
        val original = ordinaryParameters(using q)(target)
        val symbol = Symbol.newVal(target.symbol, name, tpe, flags, Symbol.noSymbol)
        val replacement = ValDef(symbol, None)
        DefDef.copy(target)(target.name, List(TermParamClause(original.updated(1, replacement))), target.returnTpt, target.rhs)

      def flaggedAccessor(flags: Flags): DefDef =
        val symbol = Symbol.newMethod(
          Symbol.spliceOwner,
          "accessor",
          MethodType(Nil)(_ => Nil, _ => TypeRepr.of[String]),
          flags,
          Symbol.noSymbol
        )
        DefDef(symbol, _ => Some(Literal(StringConstant("value"))))

      val exact = definition('{ def exact(first: Int, delayed: => String, last: Long): String = delayed; () }, "exact")
      val foreign = definition('{ def foreign(first: Int, delayed: => String, last: Long): String = delayed; () }, "foreign")
      val original = ordinaryParameters(using q)(exact)
      val byNameInt = ByNameType(TypeRepr.of[Int])
      val candidate = Q042ByNameOrdinaryCandidateFactory.capturedModifiers(using q)
      final class NullTargetHolder:
        var value: DefDef = scala.compiletime.uninitialized
      val nullTarget = new NullTargetHolder().value
      val constructor = definition('{ class Sample(value: Int); () }, "<init>")
      val extension = definition('{ extension (base: Int) def expanded(value: => Int): Int = value; () }, "expanded")
      val provided = definition('{ given provided(using value: Int): Int = value; () }, "provided")
      val targets = List(
        "strict" -> definition('{ def strict(value: Int): Int = value; () }, "strict"),
        "function0" -> definition('{ def thunk(value: () => Int): Int = value(); () }, "thunk"),
        "repeated" -> definition('{ def repeated(values: Int*): Int = values.size; () }, "repeated"),
        "generic" -> definition('{ def generic[A](value: => A): A = value; () }, "generic"),
        "multiple-ordinary" -> definition('{ def multiple(first: Int)(delayed: => String): String = delayed; () }, "multiple"),
        "defaulted" -> definition('{ def defaulted(value: => Int = 1): Int = value; () }, "defaulted"),
        "parameter-annotation" -> definition('{ def annotated(@Q042Annotation value: => Int): Int = value; () }, "annotated"),
        "using" -> definition('{ def usingClause(using value: Int): Int = value; () }, "usingClause"),
        "implicit" -> definition('{ def implicitClause(implicit value: Int): Int = value; () }, "implicitClause"),
        "erased" -> definition('{ def erasedClause(erased value: Int): Int = 0; () }, "erasedClause"),
        "foreign-owner" -> DefDef.copy(exact)(exact.name, foreign.paramss, exact.returnTpt, exact.rhs),
        "duplicate" -> DefDef.copy(exact)(exact.name, List(TermParamClause(List(original(1), original(1), original(2)))), exact.returnTpt, exact.rhs),
        "reordered" -> DefDef.copy(exact)(exact.name, List(TermParamClause(original.reverse)), exact.returnTpt, exact.rhs),
        "param-symss-mismatch" -> DefDef.copy(exact)(exact.name, List(TermParamClause(original.dropRight(1))), exact.returnTpt, exact.rhs),
        "strict-substitution" -> substituted(exact, "strictReplacement", TypeRepr.of[Int], Flags.Param),
        "function-substitution" -> substituted(exact, "functionReplacement", TypeRepr.of[() => Int], Flags.Param),
        "synthetic-substitution" -> substituted(exact, "syntheticReplacement", byNameInt, Flags.Param | Flags.Synthetic),
        "erased-substitution" -> substituted(exact, "erasedReplacement", byNameInt, Flags.Param | Flags.Erased),
        "defaulted-substitution" -> substituted(exact, "defaultedReplacement", byNameInt, Flags.Param | Flags.HasDefault),
        "missing-rhs" -> DefDef.copy(exact)(exact.name, exact.paramss, exact.returnTpt, None),
        "constructor" -> constructor,
        "extension" -> extension,
        "field-accessor" -> flaggedAccessor(Flags.FieldAccessor),
        "param-accessor" -> flaggedAccessor(Flags.ParamAccessor),
        "case-accessor" -> flaggedAccessor(Flags.CaseAccessor),
        "given" -> provided
      )
      targets.map((label, target) => label -> candidate.unapply(target).isEmpty) :+
        ("null" -> candidate.unapply(nullTarget).isEmpty)

    rows.foreach(row => assert(row._2, row))

  test("source validity matrix records by-name contrasts without manufacturing invalid targets"):
    inline def errors(inline source: String): List[String] = typeCheckErrors(source).map(_.message)

    val requiredValid = List(
      "strict" -> errors("def strict(x: Int): Int = x"),
      "by-name" -> errors("def byName(x: => Int): Int = x"),
      "function0" -> errors("def thunk(x: () => Int): Int = x()"),
      "nested" -> errors("def nested(x: => List[Option[Int]]): Int = x.size"),
      "two-by-name" -> errors("def two(a: => Int, b: => String): String = b"),
      "many" -> errors("def many(a: => Int, b: String, c: => Long, d: Boolean, e: => Double): Double = e"),
      "generic" -> errors("def generic[A](x: => A): A = x"),
      "multiple-clauses" -> errors("def multiple(a: Int)(b: => String): String = b"),
      "defaulted" -> errors("def defaulted(x: => Int = 1): Int = x"),
      "repeated" -> errors("def repeated(xs: Int*): Int = xs.size")
    )
    val contrasts = List(
      "using-by-name" -> errors("def contextual(using x: => Int): Int = x"),
      "implicit-by-name" -> errors("def implicitByName(implicit x: => Int): Int = x"),
      "erased-by-name" -> errors("import scala.language.experimental.erasedDefinitions; def erasedByName(erased x: => Int): Int = 0"),
      "annotated-by-name" -> errors("import scala.annotation.StaticAnnotation; class Ann extends StaticAnnotation; def annotated(@Ann x: => Int): Int = x")
    )
    println(s"Q042_SOURCE_VALIDITY required=$requiredValid contrasts=$contrasts")
    assert(requiredValid.forall(_._2.isEmpty), requiredValid)
    assert(contrasts.forall(_._2.isEmpty), contrasts)

  test("current production rank-3 selector behavior is characterized while rank-2 stays closed"):
    import quasiquotes.matching.DefinitionPattern.dqq
    given Compiler = Compiler.make(getClass.getClassLoader)
    val rows = withQuotes:
      val q = summon[Quotes]
      val definitions = fixtureDefinitions(using q)
      val existing = dqq(StringContext("", " def ", "(...", "): ", " = ", ""))(using q)
      val byName = existing.unapply(definitions("byName"))
      val strict = existing.unapply(definitions("strict"))
      val thunk = existing.unapply(definitions("thunk"))
      (
        byName.nonEmpty,
        byName.exists(_._3.flatten.head eq ordinaryParameters(using q)(definitions("byName")).head),
        strict.nonEmpty,
        thunk.nonEmpty
      )
    assertEquals(rows, (true, true, true, true))

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
        """import scala.quoted.*; import quasiquotes.q042.Q042ByNameOrdinaryDefinitionPattern.dqq
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
      """import scala.quoted.*; import quasiquotes.q042.Q042ByNameOrdinaryDefinitionPattern.dqq
         def f(using q: Quotes)(context: StringContext) = context.dqq"""
    )
    assertEquals(accepted, Nil)
    assert(rejected.forall(_.nonEmpty), rejected)
    assert(rejected.dropRight(1).flatten.forall(_.contains("Invalid Q042 standard dqq")), rejected)
    assert(rejected.last.exists(_.contains("duplicate pattern variable")), rejected.last)
    assert(dynamic.nonEmpty, dynamic)

  private def ordinaryParameters(using q: Quotes)(target: q.reflect.DefDef): List[q.reflect.ValDef] =
    import q.reflect.*
    target.paramss match
      case List(clause: TermParamClause) => clause.params
      case _ => Nil

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
        def strict(value: Int): Int = value
        def byName(value: => Int): Int = value
        def thunk(value: () => Int): Int = value()
        def nested(value: => List[Option[Int]]): Int = value.size
        def prefix(first: Int, delayed: => String): String = delayed
        def middle(first: Int, delayed: => String, last: Long): String = delayed
        def two(first: => Int, second: => String): String = second
        def cardinality(a: => Int, b: String, c: => Long, d: Boolean, e: => Double): Double = e
        final def modified(value: => Int): Int = value
      ()
    }.asTerm)(Symbol.spliceOwner)
    found.toMap
