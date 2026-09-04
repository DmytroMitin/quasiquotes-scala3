package quasiquotes.hybrid

import scala.annotation.StaticAnnotation
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

final class Q024ScalametaFirstAnnotation extends StaticAnnotation
final class Q024ScalametaSecondAnnotation extends StaticAnnotation

final class Q024ScalametaDefinitionOmittedModifierSemanticsCorrectionTest extends munit.FunSuite:
  test("modifier-omitting typed-Scalameta patterns share the corrected semantics across every Definition family"):
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

      val exactOne = quasiquotes.scalameta.ScalametaQuasiPattern.dqq(
        StringContext("def exactOne(value: Int): Int = ", "")
      )(using q)
      val exactTwo = quasiquotes.scalameta.ScalametaQuasiPattern.dqq(
        StringContext("def exactTwo(left: Int, right: String): Int = ", "")
      )(using q)
      val rank2 = quasiquotes.scalameta.ScalametaQuasiPattern.dqq(
        StringContext("def collect(..", "): Int = ", "")
      )(using q)
      val rank3 = quasiquotes.scalameta.ScalametaQuasiPattern.dqq(
        StringContext("def collect(...", "): Int = ", "")
      )(using q)
      val q020 = quasiquotes.scalameta.ScalametaQuasiPattern.dqq(
        StringContext("def ", "(...", "): ", " = ", "")
      )(using q)
      val q022 = quasiquotes.scalameta.ScalametaQuasiPattern.dqq(
        StringContext("def ", "[..", "](...", "): ", " = ", "")
      )(using q)

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

  test("typed-Scalameta Q020 and Q022 reject annotations scopes flags and same-universe inline modifiers"):
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
          class Fixture:
            private def nongenericPrivate(value: Int): Int = value
            protected def nongenericProtected(value: Int): Int = value
            @Q024ScalametaFirstAnnotation def nongenericOneAnnotation(value: Int): Int = value
            @Q024ScalametaFirstAnnotation @Q024ScalametaSecondAnnotation
            def nongenericMultipleAnnotations(value: Int): Int = value
            private[hybrid] def nongenericQualifiedPrivate(value: Int): Int = value
            protected[hybrid] def nongenericQualifiedProtected(value: Int): Int = value
            implicit def nongenericImplicit(value: Int): Int = value
            infix def nongenericInfix(value: Int): Int = value

            private def genericPrivate[A](value: A): A = value
            protected def genericProtected[A](value: A): A = value
            @Q024ScalametaFirstAnnotation def genericOneAnnotation[A](value: A): A = value
            @Q024ScalametaFirstAnnotation @Q024ScalametaSecondAnnotation
            def genericMultipleAnnotations[A](value: A): A = value
            private[hybrid] def genericQualifiedPrivate[A](value: A): A = value
            protected[hybrid] def genericQualifiedProtected[A](value: A): A = value
            implicit def genericImplicit[A](value: A): A = value
            infix def genericInfix[A](value: A): A = value
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

      val q020 = quasiquotes.scalameta.ScalametaQuasiPattern.dqq(
        StringContext("def ", "(...", "): ", " = ", "")
      )(using q)
      val q022 = quasiquotes.scalameta.ScalametaQuasiPattern.dqq(
        StringContext("def ", "[..", "](...", "): ", " = ", "")
      )(using q)
      val categories = List(
        "Private",
        "Protected",
        "OneAnnotation",
        "MultipleAnnotations",
        "QualifiedPrivate",
        "QualifiedProtected",
        "Implicit",
        "Infix"
      )
      val sourceRows = categories.flatMap { category =>
        List(
          s"Q020-$category" -> q020.unapply(definitions(s"nongeneric$category")).isEmpty,
          s"Q022-$category" -> q022.unapply(definitions(s"generic$category")).isEmpty
        )
      }
      sourceRows ++ List(
        "Q020-inline" -> q020.unapply(nongeneric("nongenericInline", Flags.Inline)).isEmpty,
        "Q020-transparent-inline" ->
          q020.unapply(nongeneric("nongenericTransparent", Flags.Inline | Flags.Transparent)).isEmpty,
        "Q022-inline" -> q022.unapply(generic("genericInline", Flags.Inline)).isEmpty,
        "Q022-transparent-inline" ->
          q022.unapply(generic("genericTransparent", Flags.Inline | Flags.Transparent)).isEmpty
      )

    rows.foreach(row => assert(row._2, row))
