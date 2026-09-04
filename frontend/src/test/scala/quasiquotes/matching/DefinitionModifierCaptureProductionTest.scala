package quasiquotes.matching

import scala.annotation.StaticAnnotation
import scala.compiletime.testing.typeCheckErrors
import scala.language.experimental.erasedDefinitions
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

final class Q025FirstAnnotation extends StaticAnnotation
final class Q025SecondAnnotation extends StaticAnnotation

final class DefinitionModifierCaptureProductionTest extends munit.FunSuite:
  test("external packages receive exact direct and umbrella six-capture types"):
    val _ = external.consumer.Q025ExternalDefinitionModifierCaptureConsumer

  test("production modifier capture preserves exact public reflection values and Q024 contrast"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val rows = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*

      val definitions = fixtureDefinitions(using q)
      val captured = RankedDefinitionPatternExtractorFactory
        .capturedModifiersNameTypeParamsParamssResult(using q)
      val omitted = RankedDefinitionPatternExtractorFactory
        .capturedNameTypeParamsParamssResult(using q)
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
        val typeClause = target.paramss.head.asInstanceOf[TypeParamClause]
        val termClauses = target.paramss.tail.map(_.asInstanceOf[TermParamClause])
        (
          name,
          modifiers.flags == target.symbol.flags,
          sameScope(modifiers.privateWithin, target.symbol.privateWithin),
          sameScope(modifiers.protectedWithin, target.symbol.protectedWithin),
          modifiers.annotations.size == target.symbol.annotations.size &&
            modifiers.annotations.zip(target.symbol.annotations).forall((left, right) => left eq right),
          result._2 == target.name,
          result._3.zip(typeClause.params).forall((left, right) => left eq right),
          result._3.forall(parameter => parameter.symbol.owner == target.symbol),
          result._4.zip(termClauses).forall((capturedClause, originalClause) =>
            capturedClause.zip(originalClause.params).forall((left, right) => left eq right)
          ),
          target.symbol.paramSymss ==
            typeClause.params.map(_.symbol) :: termClauses.map(_.params.map(_.symbol)),
          result._5 =:= target.returnTpt.tpe,
          target.rhs.exists(_ eq result._6),
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
      assert(row._12, row)
    }
    assert(rows.head._13, rows)
    assert(rows.tail.forall(row => !row._13), rows)
    assert(!rows.head._14 && !rows.head._15 && rows.head._16.isEmpty, rows.head)
    assert(rows.find(_._1 == "qualifiedPrivate").get._14, rows)
    assert(rows.find(_._1 == "qualifiedProtected").get._15, rows)
    assertEquals(rows.find(_._1 == "oneAnnotation").get._16, List("Q025FirstAnnotation"))
    assertEquals(
      rows.find(_._1 == "multipleAnnotations").get._16,
      List("Q025SecondAnnotation", "Q025FirstAnnotation")
    )

  test("zero one and N modifier facts remain inside one carrier scalar"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val row = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*
      val definitions = fixtureDefinitions(using q)
      val extractor = RankedDefinitionPatternExtractorFactory
        .capturedModifiersNameTypeParamsParamssResult(using q)
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
      (factCount("plain"), factCount("finalMethod"), factCount("multipleAnnotations"))

    assertEquals(row, (0, 1, 2))

  test("same-universe inline and transparent-inline methods capture flags while Q024 rejects"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val row = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*

      def generic(name: String, flags: Flags): DefDef =
        val methodType = PolyType(List("A"))(
          _ => List(TypeBounds.empty),
          poly => MethodType(List("value"))(_ => List(poly.param(0)), _ => poly.param(0))
        )
        val symbol = Symbol.newMethod(Symbol.spliceOwner, name, methodType, flags, Symbol.noSymbol)
        DefDef(symbol, clauses => Some(clauses.last.head.asInstanceOf[Term]))

      val inline = generic("inlineMethod", Flags.Inline)
      val transparent = generic("transparentInlineMethod", Flags.Inline | Flags.Transparent)
      val captured = RankedDefinitionPatternExtractorFactory
        .capturedModifiersNameTypeParamsParamssResult(using q)
      val omitted = RankedDefinitionPatternExtractorFactory
        .capturedNameTypeParamsParamssResult(using q)
      (
        captured.unapply(inline).exists(_._1.flags.is(Flags.Inline)),
        captured.unapply(transparent).exists(result =>
          result._1.flags.is(Flags.Inline) && result._1.flags.is(Flags.Transparent)
        ),
        omitted.unapply(inline).isEmpty,
        omitted.unapply(transparent).isEmpty
      )

    assertEquals(row, (true, true, true, true))

  test("six-capture matching preserves the complete Q022 negative target boundary"):
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
      val termClause = exact.paramss(1).asInstanceOf[TermParamClause]
      val constructor = definition('{ class Sample[A](value: A); () }, "<init>")
      val extension = definition('{ extension [A](value: A) def expanded[B](other: B): A = value; () }, "expanded")
      val provided = definition('{ given provided[A](using value: A): A = value; () }, "provided")
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
        .capturedModifiersNameTypeParamsParamssResult(using q)
      val targets = List(
        "nongeneric" -> definition('{ def nongeneric(value: Int): Int = value; () }, "nongeneric"),
        "missing-rhs" -> DefDef.copy(exact)(exact.name, exact.paramss, exact.returnTpt, None),
        "foreign-owner" -> DefDef.copy(exact)(exact.name, foreign.paramss, exact.returnTpt, exact.rhs),
        "duplicate-type-symbol" -> DefDef.copy(exact)(exact.name, TypeParamClause(List(typeClause.params.head, typeClause.params.head)) :: exact.paramss.tail, exact.returnTpt, exact.rhs),
        "reordered-types" -> DefDef.copy(exact)(exact.name, TypeParamClause(typeClause.params.reverse) :: exact.paramss.tail, exact.returnTpt, exact.rhs),
        "reordered-terms" -> DefDef.copy(exact)(exact.name, List(typeClause, TermParamClause(termClause.params.reverse)), exact.returnTpt, exact.rhs),
        "multiple-type-clauses" -> DefDef.copy(exact)(exact.name, List(typeClause, typeClause, termClause), exact.returnTpt, exact.rhs),
        "type-clause-not-first" -> DefDef.copy(exact)(exact.name, List(termClause, typeClause), exact.returnTpt, exact.rhs),
        "default" -> definition('{ def defaulted[A](value: A = null.asInstanceOf[A]): A = value; () }, "defaulted"),
        "context-bound" -> definition('{ def contextual[A: Ordering](value: A): A = value; () }, "contextual"),
        "using" -> definition('{ def contextual[A](using value: A): A = value; () }, "contextual"),
        "anonymous-given" -> definition('{ def anonymous[A](using A): A = summon[A]; () }, "anonymous"),
        "implicit-clause" -> definition('{ def implicitClause[A](implicit value: A): A = value; () }, "implicitClause"),
        "erased-clause" -> definition('{ def erasedClause[A](erased value: A): A = null.asInstanceOf[A]; () }, "erasedClause"),
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

  test("standard production selector admits only the exact seven-part static layout"):
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
           case dqq"$mods def $name[..$tparams](...$paramss): $result = $body" => ()
           case _ => ()"""
    )
    val existingFive = messages(
      """import scala.quoted.*; import quasiquotes.matching.DefinitionPattern.dqq
         def f(using q: Quotes)(d: q.reflect.DefDef) = d match
           case dqq"def $name[..$tparams](...$paramss): $result = $body" => ()
           case _ => ()"""
    )
    val rejected = List(
      patternMessages("""case dqq"private def $name[..$tparams](...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods final def $name[..$tparams](...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"$left $right def $name[..$tparams](...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"$modsdef $name[..$tparams](...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def fixed[..$tparams](...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name[A](...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name[...$tparams](...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name[..$tparams](..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name[..$tparams](first: Int)(...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name[..$tparams](...$paramss)(last: Int): $result = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name[..$tparams](...$paramss): Int = $body" => ()"""),
      patternMessages("""case dqq"$mods def $name[..$tparams](...$paramss): $result = $body + 1" => ()"""),
      patternMessages("""case dqq"$mods def $name[..$tparams](...$paramss): $result = $left + $right" => ()""")
    )
    val fiveAsSix = messages(
      """import scala.quoted.*; import quasiquotes.matching.{DefinitionModifiers, DefinitionPattern, RankedDefinitionPatternExtractor}
         def f(using q: Quotes): RankedDefinitionPatternExtractor[q.reflect.DefDef, (DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term], String, Seq[q.reflect.TypeDef], Seq[Seq[q.reflect.ValDef]], q.reflect.TypeRepr, q.reflect.Term)] =
           DefinitionPattern.dqq(StringContext("def ", "[..", "](...", "): ", " = ", ""))(using q)"""
    )

    assertEquals(accepted, Nil)
    assertEquals(existingFive, Nil)
    assert(rejected.forall(_.nonEmpty), rejected)
    assert(rejected.flatten.forall(_.contains("Invalid dqq definition-pattern template")), rejected)
    assert(fiveAsSix.nonEmpty, fiveAsSix)

  test("dynamic six-capture selection retains the historical exact-one fallback"):
    val errors = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.matching.{DefinitionModifiers, DefinitionPattern, RankedDefinitionPatternExtractor}
         def f(using q: Quotes)(context: StringContext): RankedDefinitionPatternExtractor[
           q.reflect.DefDef,
           (DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term], String, Seq[q.reflect.TypeDef], Seq[Seq[q.reflect.ValDef]], q.reflect.TypeRepr, q.reflect.Term)
         ] = DefinitionPattern.dqq(context)(using q)"""
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

        class Fixture extends Base:
          def plain[A](value: A): A = value
          private def privateMethod[A](value: A): A = value
          protected def protectedMethod[A](value: A): A = value
          final def finalMethod[A](value: A): A = value
          override def overrideMethod[A](value: A): A = value
          @Q025FirstAnnotation def oneAnnotation[A](value: A): A = value
          @Q025FirstAnnotation @Q025SecondAnnotation
          def multipleAnnotations[A](value: A): A = value
          private[matching] def qualifiedPrivate[A](value: A): A = value
          protected[matching] def qualifiedProtected[A](value: A): A = value
          implicit def implicitMethod[A](value: A): A = value
          infix def infixMethod[A](value: A): A = value
        ()
      }.asTerm
    )(Symbol.spliceOwner)
    definitions.toMap
