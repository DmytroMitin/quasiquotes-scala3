package quasiquotes.q023

import scala.annotation.StaticAnnotation
import scala.compiletime.testing.typeCheckErrors
import scala.quoted.*
import scala.quoted.staging.*

import quasiquotes.matching.RankedDefinitionPatternExtractorFactory

final class Q023FirstAnnotation extends StaticAnnotation
final class Q023SecondAnnotation extends StaticAnnotation

final class Q023DefinitionModifierCaptureFeasibilityTest extends munit.FunSuite:
  test("Q023 candidates expose exact external-package binder types"):
    val _ = external.consumer.Q023ExternalDefinitionModifierCaptureConsumer

  test("public reflection exposes flags scopes and ordered annotation terms"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val rows = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*

      val definitions: Map[String, q.reflect.DefDef] = fixtureDefinitions(using q)
      val extractor = Q023CandidateFactory.structured(using q)

      def flagNames(flags: Flags): List[String] =
        List(
          "Method" -> Flags.Method,
          "Private" -> Flags.Private,
          "Protected" -> Flags.Protected,
          "Final" -> Flags.Final,
          "Override" -> Flags.Override,
          "Inline" -> Flags.Inline,
          "Transparent" -> Flags.Transparent,
          "Local" -> Flags.Local,
          "Synthetic" -> Flags.Synthetic,
          "Artifact" -> Flags.Artifact
        ).collect { case (name, flag) if flags.is(flag) => name }

      val names = List(
        "plain",
        "privateMethod",
        "protectedMethod",
        "finalMethod",
        "overrideMethod",
        "oneAnnotation",
        "multipleAnnotations",
        "qualifiedPrivate",
        "qualifiedProtected"
      )

      names.map { name =>
        val target = definitions(name)
        val captured = extractor.unapply(target).get
        val modifiers = captured._1
        val directAnnotations = target.symbol.annotations
        (
          name,
          modifiers.flags == target.symbol.flags,
          flagNames(modifiers.flags),
          modifiers.privateWithin.map(_.show),
          modifiers.protectedWithin.map(_.show),
          modifiers.annotations.map(_.tpe.typeSymbol.name),
          modifiers.annotations.zip(directAnnotations).forall((left, right) => left eq right),
          captured._2 == target.name,
          captured._3.zip(target.paramss.head.asInstanceOf[TypeParamClause].params)
            .forall((left, right) => left eq right),
          captured._4.flatten.zip(target.paramss.tail.flatMap(_.params))
            .forall((left, right) => left eq right),
          captured._5 =:= target.returnTpt.tpe,
          target.rhs.exists(_ eq captured._6)
        )
      }

    rows.foreach(row => println(s"Q023_PUBLIC_MODIFIERS $row"))
    assert(rows.forall(_._2), rows)
    assertEquals(rows.find(_._1 == "plain").get._3, List("Method"))
    assertEquals(rows.find(_._1 == "privateMethod").get._3, List("Method", "Private", "Local"))
    assertEquals(rows.find(_._1 == "protectedMethod").get._3, List("Method", "Protected"))
    assertEquals(rows.find(_._1 == "finalMethod").get._3, List("Method", "Final"))
    assertEquals(rows.find(_._1 == "overrideMethod").get._3, List("Method", "Override"))
    assertEquals(
      rows.find(_._1 == "oneAnnotation").get._6,
      List("Q023FirstAnnotation")
    )
    assertEquals(
      rows.find(_._1 == "multipleAnnotations").get._6,
      List("Q023SecondAnnotation", "Q023FirstAnnotation")
    )
    assert(rows.find(_._1 == "qualifiedPrivate").get._4.nonEmpty, rows)
    assert(rows.find(_._1 == "qualifiedProtected").get._5.nonEmpty, rows)
    assert(rows.forall(_._7), rows)
    assert(rows.forall(_._8), rows)
    assert(rows.forall(_._9), rows)
    assert(rows.forall(_._10), rows)
    assert(rows.forall(_._11), rows)
    assert(rows.forall(_._12), rows)

  test("flags-only and whole-Symbol controls are exact but semantically incomplete or overbroad"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val result = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*
      val definitions: Map[String, q.reflect.DefDef] = fixtureDefinitions(using q)
      val target = definitions("multipleAnnotations")
      val flags = Q023CandidateFactory.flags(using q).unapply(target).get._1
      val symbol = Q023CandidateFactory.symbol(using q).unapply(target).get._1
      (
        flags == target.symbol.flags,
        symbol == target.symbol,
        target.symbol.annotations.size,
        target.symbol.privateWithin,
        target.symbol.owner != Symbol.noSymbol,
        target.symbol.paramSymss.nonEmpty
      )

    assert(result._1, result)
    assert(result._2, result)
    assertEquals(result._3, 2)
    assertEquals(result._4, None)
    assert(result._5, result)
    assert(result._6, result)

  test("Q020 and Q022 omitted-modifier templates require semantic-empty modifiers after Q024"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val rows = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*
      val definitions: Map[String, q.reflect.DefDef] = fixtureDefinitions(using q)
      val q020 = RankedDefinitionPatternExtractorFactory.capturedNameParamssResult(using q)
      val q022 = RankedDefinitionPatternExtractorFactory.capturedNameTypeParamsParamssResult(using q)
      val families = List(
        "plain",
        "privateMethod",
        "protectedMethod",
        "finalMethod",
        "overrideMethod",
        "oneAnnotation",
        "multipleAnnotations",
        "qualifiedPrivate",
        "qualifiedProtected"
      )
      families.map { genericName =>
        val nongenericName = s"nongeneric${genericName.head.toUpper}${genericName.tail}"
        (genericName, q020.unapply(definitions(nongenericName)).nonEmpty, q022.unapply(definitions(genericName)).nonEmpty)
      }

    rows.foreach(row => println(s"Q023_OMITTED_MODS $row"))
    assert(rows.head._2, rows)
    assert(rows.head._3, rows)
    assert(rows.tail.forall(!_._2), rows)
    assert(rows.tail.forall(!_._3), rows)

  test("inline and transparent inline flags and omitted-template behavior are public and stable"):
    val row = Q023InlineProbe.evidence
    Q023InlineProbe.details.foreach(detail => println(s"Q023_INLINE_DETAIL $detail"))
    println(s"Q023_INLINE_MODIFIERS $row")
    assertEquals(row, (true, true))

  test("same-universe inline targets are rejected by omitted patterns and captured by Q023"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val row = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*

      def nongeneric(name: String, flags: Flags): DefDef =
        val methodType = MethodType(List("value"))(_ => List(TypeRepr.of[Int]), _ => TypeRepr.of[Int])
        val symbol = Symbol.newMethod(Symbol.spliceOwner, name, methodType, flags, Symbol.noSymbol)
        DefDef(symbol, clauses => Some(clauses.head.head.asInstanceOf[Term]))

      def generic(name: String, flags: Flags): DefDef =
        val methodType = PolyType(List("A"))(
          _ => List(TypeBounds.empty),
          poly => MethodType(List("value"))(_ => List(poly.param(0)), _ => poly.param(0))
        )
        val symbol = Symbol.newMethod(Symbol.spliceOwner, name, methodType, flags, Symbol.noSymbol)
        DefDef(symbol, clauses => Some(clauses.last.head.asInstanceOf[Term]))

      val nongenericInline = nongeneric("nongenericInline", Flags.Inline)
      val nongenericTransparent = nongeneric("nongenericTransparent", Flags.Inline | Flags.Transparent)
      val genericInline = generic("genericInline", Flags.Inline)
      val genericTransparent = generic("genericTransparent", Flags.Inline | Flags.Transparent)
      val q020 = RankedDefinitionPatternExtractorFactory.capturedNameParamssResult(using q)
      val q022 = RankedDefinitionPatternExtractorFactory.capturedNameTypeParamsParamssResult(using q)
      val q023 = Q023CandidateFactory.structured(using q)
      (
        q020.unapply(nongenericInline).nonEmpty,
        q020.unapply(nongenericTransparent).nonEmpty,
        q022.unapply(genericInline).nonEmpty,
        q022.unapply(genericTransparent).nonEmpty,
        q023.unapply(genericInline).exists(_._1.flags.is(Flags.Inline)),
        q023.unapply(genericTransparent).exists(capture =>
          capture._1.flags.is(Flags.Inline) && capture._1.flags.is(Flags.Transparent)
        )
      )

    println(s"Q023_INLINE_SAME_UNIVERSE $row")
    assertEquals(row, (false, false, false, false, true, true))

  test("structured view represents zero one and N modifier facts without changing capture rank"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val row = withQuotes:
      val q = summon[Quotes]
      val definitions: Map[String, q.reflect.DefDef] = fixtureDefinitions(using q)
      val extractor = Q023CandidateFactory.structured(using q)
      val plain = extractor.unapply(definitions("plain")).get._1
      val one = extractor.unapply(definitions("finalMethod")).get._1
      val many = extractor.unapply(definitions("multipleAnnotations")).get._1
      (
        plain.annotations.size,
        one.annotations.size,
        many.annotations.size,
        plain.privateWithin.size,
        one.privateWithin.size,
        many.privateWithin.size
      )

    assertEquals(row, (0, 0, 2, 0, 0, 0))

  test("structured candidate preserves the Q022 fail-closed target boundary"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val rows = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*

      def definition(expression: Expr[Any], expectedName: String): DefDef =
        val definitions = scala.collection.mutable.ListBuffer.empty[DefDef]
        val traversal = new TreeTraverser:
          override def traverseTree(tree: Tree)(owner: Symbol): Unit =
            tree match
              case value: DefDef if value.name == expectedName => definitions += value
              case _ => ()
            super.traverseTree(tree)(owner)
        traversal.traverseTree(expression.asTerm)(Symbol.spliceOwner)
        definitions.head

      val exact = definition('{ def exact[A, B](first: A, second: B): B = second; () }, "exact")
      val foreign = definition('{ def foreign[A, B](first: A, second: B): B = second; () }, "foreign")
      val typeClause = exact.paramss.head.asInstanceOf[TypeParamClause]
      val targets = List(
        "nongeneric" -> definition('{ def nongeneric(value: Int): Int = value; () }, "nongeneric"),
        "missing-rhs" -> DefDef.copy(exact)(exact.name, exact.paramss, exact.returnTpt, None),
        "foreign-owner" -> DefDef.copy(exact)(exact.name, foreign.paramss, exact.returnTpt, exact.rhs),
        "default" -> definition(
          '{ def defaulted[A](value: A = null.asInstanceOf[A]): A = value; () },
          "defaulted"
        ),
        "context-bound" -> definition(
          '{ def contextual[A: Ordering](value: A): A = value; () },
          "contextual"
        ),
        "reordered-type-symbols" -> DefDef.copy(exact)(
          exact.name,
          TypeParamClause(typeClause.params.reverse) :: exact.paramss.tail,
          exact.returnTpt,
          exact.rhs
        )
      )
      val extractor = Q023CandidateFactory.structured(using q)
      targets.map((label, target) => label -> extractor.unapply(target).isEmpty) :+
        ("null" -> extractor.unapply(null.asInstanceOf[q.reflect.DefDef]).isEmpty)

    rows.foreach(row => assert(row._2, row))

  test("standard selector accepts only the exact six-capture modifier layout"):
    inline def messages(inline source: String): List[String] = typeCheckErrors(source).map(_.message)

    val accepted = messages(
      """import scala.quoted.*; import quasiquotes.q023.Q023StructuredStandardPattern.dqq
         def f(using q: Quotes)(d: q.reflect.DefDef) = d match
           case dqq"$mods def $name[..$tparams](...$paramss): $result = $body" => ()
           case _ => ()"""
    )
    val rejected = List(
      messages(
        """import scala.quoted.*; import quasiquotes.q023.Q023StructuredStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name[..$tparams](...$paramss): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q023.Q023StructuredStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"private def $name[..$tparams](...$paramss): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q023.Q023StructuredStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"$mods final def $name[..$tparams](...$paramss): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q023.Q023StructuredStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"$left $right def $name[..$tparams](...$paramss): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q023.Q023StructuredStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"$mods def fixed[..$tparams](...$paramss): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q023.Q023StructuredStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"$mods def $name[...$tparams](...$paramss): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q023.Q023StructuredStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"$mods def $name[..$tparams](..$params): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q023.Q023StructuredStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"$mods def $name[..$tparams](...$paramss): Int = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q023.Q023StructuredStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"$mods def $name[..$tparams](...$paramss): $result = $left + $right" => ()
             case _ => ()"""
      )
    )

    assertEquals(accepted, Nil)
    assert(rejected.forall(_.nonEmpty), rejected)

  test("standard candidate dynamic selection remains closed"):
    val errors = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.q023.Q023StructuredStandardPattern
        def dynamic(using q: Quotes)(context: StringContext) =
          Q023StructuredStandardPattern.dqq(context)(using q)
      }"""
    )
    assert(errors.nonEmpty, errors)

  private def fixtureDefinitions(using q: Quotes): Map[String, q.reflect.DefDef] =
    import q.reflect.*

    val definitions = scala.collection.mutable.Map.empty[String, DefDef]
    val traversal = new TreeTraverser:
      override def traverseTree(tree: Tree)(owner: Symbol): Unit =
        tree match
          case value: DefDef if !value.symbol.isClassConstructor =>
            definitions.update(value.name, value)
          case _ => ()
        super.traverseTree(tree)(owner)

    traversal.traverseTree(
      '{
        trait Base:
          def overrideMethod[A](value: A): A
          def nongenericOverrideMethod(value: Int): Int

        class Fixture extends Base:
          def plain[A](value: A): A = value
          private def privateMethod[A](value: A): A = value
          protected def protectedMethod[A](value: A): A = value
          final def finalMethod[A](value: A): A = value
          override def overrideMethod[A](value: A): A = value
          @Q023FirstAnnotation def oneAnnotation[A](value: A): A = value
          @Q023FirstAnnotation @Q023SecondAnnotation
          def multipleAnnotations[A](value: A): A = value
          private[q023] def qualifiedPrivate[A](value: A): A = value
          protected[q023] def qualifiedProtected[A](value: A): A = value

          def nongenericPlain(value: Int): Int = value
          private def nongenericPrivateMethod(value: Int): Int = value
          protected def nongenericProtectedMethod(value: Int): Int = value
          final def nongenericFinalMethod(value: Int): Int = value
          override def nongenericOverrideMethod(value: Int): Int = value
          @Q023FirstAnnotation def nongenericOneAnnotation(value: Int): Int = value
          @Q023FirstAnnotation @Q023SecondAnnotation
          def nongenericMultipleAnnotations(value: Int): Int = value
          private[q023] def nongenericQualifiedPrivate(value: Int): Int = value
          protected[q023] def nongenericQualifiedProtected(value: Int): Int = value
        ()
      }.asTerm
    )(Symbol.spliceOwner)
    definitions.toMap
