package quasiquotes.matching

import scala.annotation.StaticAnnotation
import scala.compiletime.testing.typeCheckErrors
import scala.language.experimental.erasedDefinitions
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

final class Q026FirstAnnotation extends StaticAnnotation
final class Q026SecondAnnotation extends StaticAnnotation

final class NongenericDefinitionModifierCaptureProductionTest extends munit.FunSuite:
  test("external packages receive exact direct and umbrella five-capture types"):
    val _ = external.consumer.Q026ExternalNongenericDefinitionModifierCaptureConsumer

  test("Q026 captures exact reflection values while Q020 preserves semantic-empty omission"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val rows = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*

      val definitions = fixtureDefinitions(using q)
      val captured = RankedDefinitionPatternExtractorFactory
        .capturedModifiersNameParamssResult(using q)
      val omitted = RankedDefinitionPatternExtractorFactory.capturedNameParamssResult(using q)
      val names = List(
        "plain",
        "privateMethod",
        "protectedMethod",
        "finalMethod",
        "overrideMethod",
        "oneAnnotation",
        "multipleAnnotations",
        "qualifiedPrivate",
        "qualifiedProtected",
        "implicitMethod",
        "infixMethod"
      )

      def sameScope(left: Option[TypeRepr], right: Option[TypeRepr]): Boolean =
        (left, right) match
          case (None, None) => true
          case (Some(a), Some(b)) => a =:= b
          case _ => false

      names.map { name =>
        val target = definitions(name)
        val result = captured.unapply(target).get
        val modifiers = result._1
        val termClauses = target.paramss.map(_.asInstanceOf[TermParamClause])
        (
          name,
          modifiers.flags == target.symbol.flags,
          sameScope(modifiers.privateWithin, target.symbol.privateWithin),
          sameScope(modifiers.protectedWithin, target.symbol.protectedWithin),
          modifiers.annotations.size == target.symbol.annotations.size &&
            modifiers.annotations.zip(target.symbol.annotations).forall((left, right) => left eq right),
          result._2 == target.name,
          result._3.zip(termClauses).forall((capturedClause, originalClause) =>
            capturedClause.zip(originalClause.params).forall((left, right) => left eq right)
          ),
          result._3.flatten.forall(_.symbol.owner == target.symbol),
          target.symbol.paramSymss == termClauses.map(_.params.map(_.symbol)),
          result._4 =:= target.returnTpt.tpe,
          target.rhs.exists(_ eq result._5),
          omitted.unapply(target).nonEmpty,
          modifiers.privateWithin.nonEmpty,
          modifiers.protectedWithin.nonEmpty,
          modifiers.annotations.map(_.tpe.typeSymbol.name)
        )
      }

    rows.foreach { row =>
      assert(row._2, row)
      assert(row._3, row)
      assert(row._4, row)
      assert(row._5, row)
      assert(row._6, row)
      assert(row._7, row)
      assert(row._8, row)
      assert(row._9, row)
      assert(row._10, row)
      assert(row._11, row)
    }
    assert(rows.head._12, rows)
    assert(rows.tail.forall(row => !row._12), rows)
    assert(!rows.head._13 && !rows.head._14 && rows.head._15.isEmpty, rows.head)
    assert(rows.find(_._1 == "qualifiedPrivate").get._13, rows)
    assert(rows.find(_._1 == "qualifiedProtected").get._14, rows)
    assertEquals(rows.find(_._1 == "oneAnnotation").get._15, List("Q026FirstAnnotation"))
    assertEquals(
      rows.find(_._1 == "multipleAnnotations").get._15,
      List("Q026SecondAnnotation", "Q026FirstAnnotation")
    )

  test("zero one and N modifiers and zero one-empty and N term clauses remain distinct"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val row = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*
      val definitions = fixtureDefinitions(using q)
      val extractor = RankedDefinitionPatternExtractorFactory
        .capturedModifiersNameParamssResult(using q)
      val semanticFlags = List(
        Flags.Private,
        Flags.PrivateLocal,
        Flags.Protected,
        Flags.Final,
        Flags.Override,
        Flags.Inline,
        Flags.Transparent,
        Flags.Implicit,
        Flags.Infix
      )
      def factCount(name: String): Int =
        val modifiers = extractor.unapply(definitions(name)).get._1
        semanticFlags.count(modifiers.flags.is) + modifiers.privateWithin.size +
          modifiers.protectedWithin.size + modifiers.annotations.size
      (
        factCount("plain"),
        factCount("finalMethod"),
        factCount("multipleAnnotations"),
        extractor.unapply(definitions("zeroClauses")).get._3.map(_.size),
        extractor.unapply(definitions("oneEmptyClause")).get._3.map(_.size),
        extractor.unapply(definitions("multipleClauses")).get._3.map(_.size)
      )

    assertEquals(row, (0, 1, 2, Seq.empty, Seq(0), Seq(1, 0, 2)))

  test("same-universe inline and transparent-inline methods capture flags while Q020 rejects"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val row = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*

      def nongeneric(name: String, flags: Flags): DefDef =
        val methodType = MethodType(List("value"))(_ => List(TypeRepr.of[Int]), _ => TypeRepr.of[Int])
        val symbol = Symbol.newMethod(Symbol.spliceOwner, name, methodType, flags, Symbol.noSymbol)
        DefDef(symbol, clauses => Some(clauses.head.head.asInstanceOf[Term]))

      val inline = nongeneric("inlineMethod", Flags.Inline)
      val transparent = nongeneric("transparentInlineMethod", Flags.Inline | Flags.Transparent)
      val captured = RankedDefinitionPatternExtractorFactory
        .capturedModifiersNameParamssResult(using q)
      val omitted = RankedDefinitionPatternExtractorFactory.capturedNameParamssResult(using q)
      (
        captured.unapply(inline).exists(_._1.flags.is(Flags.Inline)),
        captured.unapply(transparent).exists(result =>
          result._1.flags.is(Flags.Inline) && result._1.flags.is(Flags.Transparent)
        ),
        omitted.unapply(inline).isEmpty,
        omitted.unapply(transparent).isEmpty
      )

    assertEquals(row, (true, true, true, true))

  test("Q020 Q026 Q022 and Q025 preserve modifier and type-clause topology"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val row = withQuotes:
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
      val plainNongeneric = definition('{ def plainNongeneric(value: Int): Int = value; () }, "plainNongeneric")
      val finalNongeneric = definition('{
        class Modified:
          final def finalNongeneric(value: Int): Int = value
        ()
      }, "finalNongeneric")
      val plainGeneric = definition('{ def plainGeneric[A](value: A): A = value; () }, "plainGeneric")
      val finalGeneric = definition('{
        class Modified:
          final def finalGeneric[A](value: A): A = value
        ()
      }, "finalGeneric")
      val q020 = RankedDefinitionPatternExtractorFactory.capturedNameParamssResult(using q)
      val q026 = RankedDefinitionPatternExtractorFactory.capturedModifiersNameParamssResult(using q)
      val q022 = RankedDefinitionPatternExtractorFactory.capturedNameTypeParamsParamssResult(using q)
      val q025 = RankedDefinitionPatternExtractorFactory.capturedModifiersNameTypeParamsParamssResult(using q)
      (
        q020.unapply(plainNongeneric).nonEmpty,
        q026.unapply(plainNongeneric).nonEmpty,
        q020.unapply(finalNongeneric).isEmpty,
        q026.unapply(finalNongeneric).nonEmpty,
        q020.unapply(plainGeneric).isEmpty,
        q026.unapply(plainGeneric).isEmpty,
        q022.unapply(plainGeneric).nonEmpty,
        q025.unapply(plainGeneric).nonEmpty,
        q022.unapply(finalGeneric).isEmpty,
        q025.unapply(finalGeneric).nonEmpty,
        q022.unapply(plainNongeneric).isEmpty,
        q025.unapply(plainNongeneric).isEmpty
      )

    assertEquals(row, (true, true, true, true, true, true, true, true, true, true, true, true))

  test("five-capture matching preserves the complete Q020 negative target boundary"):
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

      val exact = definition('{ def exact(first: Int)(second: String): Int = first + second.length; () }, "exact")
      val foreign = definition('{ def foreign(first: Int)(second: String): Int = first + second.length; () }, "foreign")
      val firstClause = exact.paramss.head.asInstanceOf[TermParamClause]
      val secondClause = exact.paramss(1).asInstanceOf[TermParamClause]
      val generic = definition('{ def generic[A](value: A): A = value; () }, "generic")
      val constructor = definition('{ class Sample(value: Int); () }, "<init>")
      val extension = definition('{ extension (value: Int) def expanded(other: Int): Int = value + other; () }, "expanded")
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
      val extractor = RankedDefinitionPatternExtractorFactory
        .capturedModifiersNameParamssResult(using q)
      val targets = List(
        "generic" -> generic,
        "missing-rhs" -> DefDef.copy(exact)(exact.name, exact.paramss, exact.returnTpt, None),
        "foreign-owner" -> DefDef.copy(exact)(exact.name, foreign.paramss, exact.returnTpt, exact.rhs),
        "reordered-terms" -> DefDef.copy(exact)(exact.name, List(secondClause, firstClause), exact.returnTpt, exact.rhs),
        "duplicate-term-symbol" -> DefDef.copy(exact)(exact.name, List(TermParamClause(List(firstClause.params.head, firstClause.params.head))), exact.returnTpt, exact.rhs),
        "param-symss-mismatch" -> DefDef.copy(exact)(exact.name, List(firstClause), exact.returnTpt, exact.rhs),
        "non-term-clause" -> DefDef.copy(exact)(exact.name, generic.paramss, exact.returnTpt, exact.rhs),
        "using" -> definition('{ def contextual(using value: Int): Int = value; () }, "contextual"),
        "anonymous-given" -> definition('{ def anonymous(using Int): Int = summon[Int]; () }, "anonymous"),
        "implicit-clause" -> definition('{ def implicitClause(implicit value: Int): Int = value; () }, "implicitClause"),
        "erased-clause" -> definition('{ def erasedClause(erased value: Int): Int = 0; () }, "erasedClause"),
        "default" -> definition('{ def defaulted(value: Int = 1): Int = value; () }, "defaulted"),
        "constructor" -> constructor,
        "extension" -> extension,
        "field-accessor" -> flaggedAccessor(Flags.FieldAccessor),
        "param-accessor" -> flaggedAccessor(Flags.ParamAccessor),
        "case-accessor" -> flaggedAccessor(Flags.CaseAccessor),
        "given" -> provided,
        "null" -> null.asInstanceOf[DefDef]
      )
      targets.map((label, target) => label -> extractor.unapply(target).isEmpty)

    rows.foreach(row => assert(row._2, row))

  test("standard selector admits only the exact six-part five-capture layout"):
    inline def messages(inline source: String): List[String] = typeCheckErrors(source).map(_.message)
    inline def patternMessages(inline pattern: String): List[String] =
      messages(
        """import scala.quoted.*; import quasiquotes.matching.DefinitionPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             """ + pattern + """
             case _ => ()"""
      )

    val accepted = messages(
      """import scala.quoted.*; import quasiquotes.matching.DefinitionPattern.dqq
         def f(using q: Quotes)(d: q.reflect.DefDef) = d match
           case dqq"$mods def $name(...$paramss): $result = $body" => ()
           case _ => ()"""
    )
    val existingQ020 = messages(
      """import scala.quoted.*; import quasiquotes.matching.DefinitionPattern.dqq
         def f(using q: Quotes)(d: q.reflect.DefDef) = d match
           case dqq"def $name(...$paramss): $result = $body" => ()
           case _ => ()"""
    )
    val existingQ025 = messages(
      """import scala.quoted.*; import quasiquotes.matching.DefinitionPattern.dqq
         def f(using q: Quotes)(d: q.reflect.DefDef) = d match
           case dqq"$mods def $name[..$tparams](...$paramss): $result = $body" => ()
           case _ => ()"""
    )
    val q044 = patternMessages("""case dqq"$mods def $name(..$params): $result = $body" => ()""")
    val rejected = List(
      patternMessages("""case dqq"private def $name(...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods final def $name(...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"$left $right def $name(...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"$modsdef $name(...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def fixed(...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name[A](...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(first: Int)(...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(...$paramss)(last: Int): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(...$paramss): Int = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(...$paramss) = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name(...$paramss): $result = $body + 1" => ()"""),
      patternMessages("""case dqq"$mods def $name(...$paramss): $result = $left + $right" => ()"""),
      patternMessages("""case dqq"$mods def $name(...$paramss): $result = $body extra" => ()""")
    )

    assertEquals(accepted, Nil)
    assertEquals(existingQ020, Nil)
    assertEquals(existingQ025, Nil)
    assertEquals(q044, Nil)
    assert(rejected.forall(_.nonEmpty), rejected)
    assert(rejected.flatten.forall(_.contains("Invalid dqq definition-pattern template")), rejected)

  test("dynamic five-capture selection retains the historical exact-one fallback"):
    val errors = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.matching.{DefinitionModifiers, DefinitionPattern, RankedDefinitionPatternExtractor}
         def f(using q: Quotes)(context: StringContext): RankedDefinitionPatternExtractor[
           q.reflect.DefDef,
           (DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term], String, Seq[Seq[q.reflect.ValDef]], q.reflect.TypeRepr, q.reflect.Term)
         ] = DefinitionPattern.dqq(context)(using q)"""
    )
    assert(errors.nonEmpty, errors)

  private def fixtureDefinitions(using q: Quotes): Map[String, q.reflect.DefDef] =
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
          def overrideMethod(value: Int): Int

        class Fixture extends Base:
          def plain(value: Int): Int = value
          private def privateMethod(value: Int): Int = value
          protected def protectedMethod(value: Int): Int = value
          final def finalMethod(value: Int): Int = value
          override def overrideMethod(value: Int): Int = value
          @Q026FirstAnnotation def oneAnnotation(value: Int): Int = value
          @Q026FirstAnnotation @Q026SecondAnnotation
          def multipleAnnotations(value: Int): Int = value
          private[matching] def qualifiedPrivate(value: Int): Int = value
          protected[matching] def qualifiedProtected(value: Int): Int = value
          implicit def implicitMethod(value: Int): Int = value
          infix def infixMethod(value: Int): Int = value
          def zeroClauses: Int = 0
          def oneEmptyClause(): Int = 0
          def multipleClauses(first: Int)()(second: String, third: Int): Int =
            first + second.length + third
        ()
      }.asTerm
    )(Symbol.spliceOwner)
    definitions.toMap
