package quasiquotes.hybrid

import scala.compiletime.testing.typeCheckErrors
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

final class Q021ScalametaDefinitionTypeParameterCaptureFeasibilityTest extends munit.FunSuite:
  test("Q021 typed-Scalameta candidates expose exact external-package binder types"):
    val _ = external.consumer.Q021ExternalScalametaDefinitionTypeParameterCaptureConsumer

  test("typed-Scalameta selectors preserve the three exact carrier products and dependent binders"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val row = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*
      val definitions = scala.collection.mutable.ListBuffer.empty[DefDef]
      val traversal = new TreeTraverser:
        override def traverseTree(tree: Tree)(owner: Symbol): Unit =
          tree match
            case value: DefDef if value.name == "dependent" => definitions += value
            case _ => ()
          super.traverseTree(tree)(owner)
      traversal.traverseTree(
        '{ def dependent[A, B <: List[A]](first: A)(second: B): B = second; () }.asTerm
      )(Symbol.spliceOwner)
      val target = definitions.head
      val originalTypeParams = target.paramss.head.asInstanceOf[TypeParamClause].params
      val originalTermClauses = target.paramss.tail.map(_.asInstanceOf[TermParamClause])

      val typeDefs =
        import quasiquotes.hybrid.q021.Q021TypeDefScalametaPattern.dqq
        target match
          case dqq"def $name[..$tparams](...$paramss): $result = $body" =>
            (
              name,
              tparams.map(_.name).toList,
              tparams.zip(originalTypeParams).forall((captured, original) => captured eq original),
              paramss.map(_.size).toList,
              paramss.zip(originalTermClauses).forall((captured, original) =>
                captured.zip(original.params).forall((left, right) => left eq right)
              ),
              result.show == target.returnTpt.tpe.show,
              target.rhs.exists(_ eq body)
            )
          case _ => ("<no-match>", Nil, false, Nil, false, false, false)
      val symbols =
        import quasiquotes.hybrid.q021.Q021SymbolScalametaPattern.dqq
        target match
          case dqq"def $name[..$tparams](...$paramss): $result = $body" =>
            tparams.zip(originalTypeParams).forall((captured, original) => captured == original.symbol)
          case _ => false
      val products =
        import quasiquotes.hybrid.q021.Q021NameBoundsScalametaPattern.dqq
        target match
          case dqq"def $name[..$tparams](...$paramss): $result = $body" =>
            val secondUpper = tparams(1)._2.hi
            tparams.map(_._1).toList -> (secondUpper match
              case AppliedType(_, argument :: Nil) => argument.typeSymbol == originalTypeParams.head.symbol
              case _ => false)
          case _ => Nil -> false
      (typeDefs, symbols, products)

    println(s"Q021_SCALAMETA_TYPE_PARAMETER $row")
    assertEquals(row._1._1, "dependent", row)
    assertEquals(row._1._2, List("A", "B"), row)
    assert(row._1._3, row)
    assertEquals(row._1._4, List(1, 1), row)
    assert(row._1._5, row)
    assert(row._1._6, row)
    assert(row._1._7, row)
    assert(row._2, row)
    assertEquals(row._3._1, List("A", "B"), row)
    assert(row._3._2, row)

  test("typed-Scalameta structural classifier accepts only the exact five-capture layout"):
    inline def messages(inline source: String): List[String] = typeCheckErrors(source).map(_.message)

    val accepted = messages(
      """import scala.quoted.*; import quasiquotes.hybrid.q021.Q021TypeDefScalametaPattern.dqq
         def f(using q: Quotes)(d: q.reflect.DefDef) = d match
           case dqq"def $name[..$tparams](...$paramss): $result = $body" => ()
           case _ => ()"""
    )
    val rejected = List(
      messages(
        """import scala.quoted.*; import quasiquotes.hybrid.q021.Q021TypeDefScalametaPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name(...$paramss): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.hybrid.q021.Q021TypeDefScalametaPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def fixed[..$tparams](...$paramss): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.hybrid.q021.Q021TypeDefScalametaPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name[A](...$paramss): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.hybrid.q021.Q021TypeDefScalametaPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name[...$tparams](...$paramss): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.hybrid.q021.Q021TypeDefScalametaPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name..$tparams(...$paramss): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.hybrid.q021.Q021TypeDefScalametaPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name[..$left, ..$right](...$paramss): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.hybrid.q021.Q021TypeDefScalametaPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name[..$tparams](..$params)(...$paramss): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.hybrid.q021.Q021TypeDefScalametaPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name[..$tparams](first: Int)(...$paramss): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.hybrid.q021.Q021TypeDefScalametaPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name[..$tparams](...$paramss)(last: Int): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.hybrid.q021.Q021TypeDefScalametaPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name[..$tparams](..$params): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.hybrid.q021.Q021TypeDefScalametaPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name[..$tparams](...$paramss) = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.hybrid.q021.Q021TypeDefScalametaPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name[..$tparams](...$paramss): Int = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.hybrid.q021.Q021TypeDefScalametaPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name[..$tparams](...$paramss): $result = $body + 1" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.hybrid.q021.Q021TypeDefScalametaPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name[..$tparams](...$paramss): $result = $left + $right" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.hybrid.q021.Q021TypeDefScalametaPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"private def $name[..$tparams](...$paramss): $result = $body" => ()
             case _ => ()"""
      )
    )

    assertEquals(accepted, Nil)
    assert(rejected.forall(_.nonEmpty), rejected)
    assert(rejected.flatten.forall(_.contains("Invalid Q021 typed-Scalameta dqq")), rejected)

  test("Q021 typed-Scalameta candidate dynamic selection remains closed"):
    val errors = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.matching.RankedDefinitionPatternExtractor
        import quasiquotes.hybrid.q021.Q021TypeDefScalametaPattern
        def dynamic(using q: Quotes)(context: StringContext): RankedDefinitionPatternExtractor[
          q.reflect.DefDef,
          (String, Seq[q.reflect.TypeDef], Seq[Seq[q.reflect.ValDef]], q.reflect.TypeRepr, q.reflect.Term)
        ] = Q021TypeDefScalametaPattern.dqq(context)(using q)
      }"""
    )
    assert(errors.nonEmpty, errors)
