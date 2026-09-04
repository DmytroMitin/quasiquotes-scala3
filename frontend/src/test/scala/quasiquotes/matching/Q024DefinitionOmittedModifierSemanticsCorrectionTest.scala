package quasiquotes.matching

import scala.annotation.StaticAnnotation
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

final class Q024FirstAnnotation extends StaticAnnotation
final class Q024SecondAnnotation extends StaticAnnotation

final class Q024DefinitionOmittedModifierSemanticsCorrectionTest extends munit.FunSuite:
  test("characterize semantically plain Definition symbols across owners types clauses and result syntax"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val rows = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*

      val expected = Set(
        "classExplicit",
        "classInferred",
        "classGeneric",
        "classZeroClauses",
        "classEmptyClause",
        "classMultipleClauses",
        "objectExplicit",
        "objectGeneric",
        "localExplicit",
        "localInferred",
        "localGeneric",
        "localZeroClauses",
        "localEmptyClause",
        "localMultipleClauses"
      )
      val definitions = scala.collection.mutable.ListBuffer.empty[DefDef]
      val traversal = new TreeTraverser:
        override def traverseTree(tree: Tree)(owner: Symbol): Unit =
          tree match
            case value: DefDef if expected(value.name) => definitions += value
            case _ => ()
          super.traverseTree(tree)(owner)

      traversal.traverseTree(
        '{
          class PlainClass:
            def classExplicit(value: Int): Int = value
            def classInferred(value: Int) = value
            def classGeneric[A](value: A): A = value
            def classZeroClauses: Int = 0
            def classEmptyClause(): Int = 0
            def classMultipleClauses(first: Int)(second: String): Int = first + second.length

          object PlainObject:
            def objectExplicit(value: Int): Int = value
            def objectGeneric[A](value: A): A = value

          def localExplicit(value: Int): Int = value
          def localInferred(value: Int) = value
          def localGeneric[A](value: A): A = value
          def localZeroClauses: Int = 0
          def localEmptyClause(): Int = 0
          def localMultipleClauses(first: Int)(second: String): Int = first + second.length
          ()
        }.asTerm
      )(Symbol.spliceOwner)

      def visibleFlags(symbol: Symbol): List[String] =
        List(
          "Private" -> Flags.Private,
          "PrivateLocal" -> Flags.PrivateLocal,
          "Protected" -> Flags.Protected,
          "Final" -> Flags.Final,
          "Override" -> Flags.Override,
          "Deferred" -> Flags.Deferred,
          "Inline" -> Flags.Inline,
          "Transparent" -> Flags.Transparent,
          "Implicit" -> Flags.Implicit,
          "Given" -> Flags.Given,
          "Erased" -> Flags.Erased,
          "Infix" -> Flags.Infix,
          "Exported" -> Flags.Exported,
          "JavaStatic" -> Flags.JavaStatic,
          "Macro" -> Flags.Macro
        ).collect { case (name, flag) if symbol.flags.is(flag) => name }

      definitions.toList.sortBy(_.name).map { definition =>
        val symbol = definition.symbol
        (
          definition.name,
          symbol.owner.name,
          symbol.flags.is(Flags.Method),
          symbol.flags.is(Flags.Local),
          symbol.flags.is(Flags.Synthetic),
          symbol.flags.is(Flags.Artifact),
          visibleFlags(symbol),
          symbol.annotations.map(_.tpe.typeSymbol.name),
          symbol.privateWithin.map(_.show),
          symbol.protectedWithin.map(_.show),
          definition.paramss.map(_.getClass.getSimpleName)
        )
      }

    rows.foreach(row => println(s"Q024_PLAIN_CHARACTERIZATION $row"))
    assertEquals(rows.map(_._1).toSet, Set(
      "classExplicit",
      "classInferred",
      "classGeneric",
      "classZeroClauses",
      "classEmptyClause",
      "classMultipleClauses",
      "objectExplicit",
      "objectGeneric",
      "localExplicit",
      "localInferred",
      "localGeneric",
      "localZeroClauses",
      "localEmptyClause",
      "localMultipleClauses"
    ))
    assert(rows.forall(_._3), rows)
    assert(rows.forall(_._7.isEmpty), rows)
    assert(rows.forall(_._8.isEmpty), rows)
    assert(rows.forall(_._9.isEmpty), rows)
    assert(rows.forall(_._10.isEmpty), rows)

  test("modifier-omitting standard patterns reject semantic modifiers across every Definition family"):
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

      val exactOne = DefinitionPattern
        .singleParameter("def exactOne(value: Int): Int = $body")
        .toOption
        .get
      val exactTwo = DefinitionPatternExtractor
        .compileExactTwo("def exactTwo(left: Int, right: String): Int = $body")
        .toOption
        .get
      val rank2 = RankedDefinitionPatternExtractorFactory.exactCollect(using q)
      val rank3 = RankedDefinitionPatternExtractorFactory.exactCollectParamss(using q)
      val q020 = RankedDefinitionPatternExtractorFactory.capturedNameParamssResult(using q)
      val q022 = RankedDefinitionPatternExtractorFactory.capturedNameTypeParamsParamssResult(using q)

      val plainExactOne = definition('{
        class Plain:
          def exactOne(value: Int): Int = value
        ()
      }, "exactOne")
      val finalExactOne = definition('{
        class Modified:
          final def exactOne(value: Int): Int = value
        ()
      }, "exactOne")
      val plainExactTwo = definition('{
        class Plain:
          def exactTwo(left: Int, right: String): Int = left + right.length
        ()
      }, "exactTwo")
      val finalExactTwo = definition('{
        class Modified:
          final def exactTwo(left: Int, right: String): Int = left + right.length
        ()
      }, "exactTwo")
      val plainCollect = definition('{
        class Plain:
          def collect(value: Int): Int = value
        ()
      }, "collect")
      val finalCollect = definition('{
        class Modified:
          final def collect(value: Int): Int = value
        ()
      }, "collect")
      val plainGeneric = definition('{
        class Plain:
          def generic[A](value: A): A = value
        ()
      }, "generic")
      val finalGeneric = definition('{
        class Modified:
          final def generic[A](value: A): A = value
        ()
      }, "generic")

      List(
        "exact-one-plain" -> exactOne.unapply(using q)(plainExactOne).nonEmpty,
        "exact-one-final" -> exactOne.unapply(using q)(finalExactOne).isEmpty,
        "exact-two-plain" -> exactTwo.unapply(using q)(plainExactTwo).nonEmpty,
        "exact-two-final" -> exactTwo.unapply(using q)(finalExactTwo).isEmpty,
        "rank-2-plain" -> rank2.unapply(plainCollect).nonEmpty,
        "rank-2-final" -> rank2.unapply(finalCollect).isEmpty,
        "rank-3-plain" -> rank3.unapply(plainCollect).nonEmpty,
        "rank-3-final" -> rank3.unapply(finalCollect).isEmpty,
        "Q020-plain" -> q020.unapply(plainCollect).nonEmpty,
        "Q020-final" -> q020.unapply(finalCollect).isEmpty,
        "Q022-plain" -> q022.unapply(plainGeneric).nonEmpty,
        "Q022-final" -> q022.unapply(finalGeneric).isEmpty
      )

    rows.foreach(row => assert(row._2, row))

  test("Q020 and Q022 omitted modifiers reject the complete source modifier matrix"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val rows = withQuotes:
      val q = summon[Quotes]
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
            def q020Override(value: Int): Int
            def q022Override[A](value: A): A

          class Fixture extends Base:
            private def q020Private(value: Int): Int = value
            protected def q020Protected(value: Int): Int = value
            final def q020Final(value: Int): Int = value
            override def q020Override(value: Int): Int = value
            @Q024FirstAnnotation def q020OneAnnotation(value: Int): Int = value
            @Q024FirstAnnotation @Q024SecondAnnotation
            def q020MultipleAnnotations(value: Int): Int = value
            private[matching] def q020QualifiedPrivate(value: Int): Int = value
            protected[matching] def q020QualifiedProtected(value: Int): Int = value
            implicit def q020Implicit(value: Int): Int = value
            infix def q020Infix(value: Int): Int = value

            private def q022Private[A](value: A): A = value
            protected def q022Protected[A](value: A): A = value
            final def q022Final[A](value: A): A = value
            override def q022Override[A](value: A): A = value
            @Q024FirstAnnotation def q022OneAnnotation[A](value: A): A = value
            @Q024FirstAnnotation @Q024SecondAnnotation
            def q022MultipleAnnotations[A](value: A): A = value
            private[matching] def q022QualifiedPrivate[A](value: A): A = value
            protected[matching] def q022QualifiedProtected[A](value: A): A = value
            implicit def q022Implicit[A](value: A): A = value
            infix def q022Infix[A](value: A): A = value
          ()
        }.asTerm
      )(Symbol.spliceOwner)

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

      val q020 = RankedDefinitionPatternExtractorFactory.capturedNameParamssResult(using q)
      val q022 = RankedDefinitionPatternExtractorFactory.capturedNameTypeParamsParamssResult(using q)
      val sourceCategories = List(
        "Private",
        "Protected",
        "Final",
        "Override",
        "OneAnnotation",
        "MultipleAnnotations",
        "QualifiedPrivate",
        "QualifiedProtected",
        "Implicit",
        "Infix"
      )
      val sourceRows = sourceCategories.flatMap { category =>
        List(
          s"Q020-$category" -> q020.unapply(definitions(s"q020$category")).isEmpty,
          s"Q022-$category" -> q022.unapply(definitions(s"q022$category")).isEmpty
        )
      }
      val sameUniverseRows = List(
        "Q020-inline" -> q020.unapply(nongeneric("q020Inline", Flags.Inline)).isEmpty,
        "Q020-transparent-inline" ->
          q020.unapply(nongeneric("q020TransparentInline", Flags.Inline | Flags.Transparent)).isEmpty,
        "Q022-inline" -> q022.unapply(generic("q022Inline", Flags.Inline)).isEmpty,
        "Q022-transparent-inline" ->
          q022.unapply(generic("q022TransparentInline", Flags.Inline | Flags.Transparent)).isEmpty,
        "Q020-given" -> q020.unapply(nongeneric("q020Given", Flags.Given)).isEmpty,
        "Q022-given" -> q022.unapply(generic("q022Given", Flags.Given)).isEmpty
      )
      val additionalSemanticFlags = List(
        "deferred" -> Flags.Deferred,
        "erased" -> Flags.Erased,
        "exported" -> Flags.Exported,
        "java-static" -> Flags.JavaStatic,
        "macro" -> Flags.Macro
      )
      val additionalFlagRows = additionalSemanticFlags.flatMap { (label, flags) =>
        List(
          s"Q020-$label" -> q020.unapply(nongeneric(s"q020-$label", flags)).isEmpty,
          s"Q022-$label" -> q022.unapply(generic(s"q022-$label", flags)).isEmpty
        )
      }
      sourceRows ++ sameUniverseRows ++ additionalFlagRows

    rows.foreach(row => assert(row._2, row))
