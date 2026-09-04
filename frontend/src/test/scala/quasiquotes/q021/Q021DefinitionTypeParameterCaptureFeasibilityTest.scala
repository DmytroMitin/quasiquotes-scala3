package quasiquotes.q021

import scala.compiletime.testing.typeCheckErrors
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

final class Q021DefinitionTypeParameterCaptureFeasibilityTest extends munit.FunSuite:
  test("Q021 standard candidates expose exact external-package binder types"):
    val _ = external.consumer.Q021ExternalDefinitionTypeParameterCaptureConsumer

  test("all viable candidates expose the three exact ranked carrier types"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    withQuotes:
      val q = summon[Quotes]
      import quasiquotes.matching.RankedDefinitionPatternExtractor

      val _: RankedDefinitionPatternExtractor[
        q.reflect.DefDef,
        (String, Seq[q.reflect.TypeDef], Seq[Seq[q.reflect.ValDef]], q.reflect.TypeRepr, q.reflect.Term)
      ] = Q021CandidateFactory.typeDefs(using q)
      val _: RankedDefinitionPatternExtractor[
        q.reflect.DefDef,
        (String, Seq[q.reflect.Symbol], Seq[Seq[q.reflect.ValDef]], q.reflect.TypeRepr, q.reflect.Term)
      ] = Q021CandidateFactory.symbols(using q)
      val _: RankedDefinitionPatternExtractor[
        q.reflect.DefDef,
        (String, Seq[(String, q.reflect.TypeBounds)], Seq[Seq[q.reflect.ValDef]], q.reflect.TypeRepr, q.reflect.Term)
      ] = Q021CandidateFactory.nameBounds(using q)

  test("type parameters preserve order tree identity owner names bounds and dependent binder identity"):
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

      val targets = List(
        ("id", List("A"), List(1), definition('{ def id[A](value: A): A = value; () }, "id")),
        ("pair", List("A", "B"), List(2), definition('{ def pair[A, B](left: A, right: B): (A, B) = (left, right); () }, "pair")),
        ("upper", List("A"), List(1), definition('{ def upper[A <: AnyVal](value: A): A = value; () }, "upper")),
        ("bounded", List("A"), List(1), definition('{ def bounded[A >: String <: Any](value: A): A = value; () }, "bounded")),
        ("dependent", List("A", "B"), List(1, 1), definition('{ def dependent[A, B <: List[A]](first: A)(second: B): B = second; () }, "dependent"))
      )

      targets.map { (expectedName, expectedTypeNames, expectedTermCounts, target) =>
        val typeClause = target.paramss.head.asInstanceOf[TypeParamClause]
        val termClauses = target.paramss.tail.map(_.asInstanceOf[TermParamClause])
        val typeDefCapture = Q021CandidateFactory.typeDefs(using q).unapply(target).get
        val symbolCapture = Q021CandidateFactory.symbols(using q).unapply(target).get
        val productCapture = Q021CandidateFactory.nameBounds(using q).unapply(target).get
        val originalBounds = typeClause.params.map(_.rhs.asInstanceOf[TypeBoundsTree].tpe)
        val dependentIdentity =
          if expectedName != "dependent" then true
          else
            originalBounds(1).hi match
              case AppliedType(_, argument :: Nil) => argument.typeSymbol == typeClause.params.head.symbol
              case _ => false
        (
          expectedName,
          expectedTypeNames,
          expectedTermCounts,
          target.name,
          typeDefCapture._1,
          typeDefCapture._2.map(_.name).toList,
          typeDefCapture._2.zip(typeClause.params).forall((captured, original) => captured eq original),
          typeDefCapture._2.forall(_.symbol.owner == target.symbol),
          symbolCapture._2.zip(typeClause.params).forall((captured, original) => captured == original.symbol),
          productCapture._2.map(_._1).toList,
          productCapture._2.map(_._2).zip(originalBounds).forall((captured, original) =>
            captured.low =:= original.low && captured.hi =:= original.hi
          ),
          typeDefCapture._3.map(_.size).toList,
          typeDefCapture._3.zip(termClauses).forall((captured, original) =>
            captured.zip(original.params).forall((left, right) => left eq right)
          ),
          target.symbol.paramSymss ==
            typeClause.params.map(_.symbol) :: termClauses.map(_.params.map(_.symbol)),
          typeDefCapture._4 =:= target.returnTpt.tpe,
          target.rhs.exists(_ eq typeDefCapture._5),
          dependentIdentity,
          originalBounds.map(bounds => (bounds.low.show, bounds.hi.show))
        )
      }

    rows.foreach { row =>
      println(s"Q021_STANDARD_TYPE_PARAMETER $row")
      assertEquals(row._4, row._1, row)
      assertEquals(row._5, row._1, row)
      assertEquals(row._6, row._2, row)
      assert(row._7, row)
      assert(row._8, row)
      assert(row._9, row)
      assertEquals(row._10, row._2, row)
      assert(row._11, row)
      assertEquals(row._12, row._3, row)
      assert(row._13, row)
      assert(row._14, row)
      assert(row._15, row)
      assert(row._16, row)
      assert(row._17, row)
    }

  test("nongeneric targets fail because the source tree has no type-parameter clause"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val result = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*
      val definitions = scala.collection.mutable.ListBuffer.empty[DefDef]
      val traversal = new TreeTraverser:
        override def traverseTree(tree: Tree)(owner: Symbol): Unit =
          tree match
            case value: DefDef if value.name == "plain" => definitions += value
            case _ => ()
          super.traverseTree(tree)(owner)
      traversal.traverseTree('{ def plain(value: Int): Int = value; () }.asTerm)(Symbol.spliceOwner)
      val target = definitions.head
      (
        target.paramss.map {
          case _: TypeParamClause => true
          case _ => false
        },
        Q021CandidateFactory.typeDefs(using q).unapply(target).isEmpty,
        Q021CandidateFactory.symbols(using q).unapply(target).isEmpty,
        Q021CandidateFactory.nameBounds(using q).unapply(target).isEmpty,
        try
          TypeParamClause(Nil)
          false
        catch case _: IllegalArgumentException => true
      )

    println(s"Q021_ZERO_TPARAMS_TOPOLOGY $result")
    assertEquals(result._1, List(false))
    assert(result._2)
    assert(result._3)
    assert(result._4)
    assert(result._5, "the public TypeParamClause constructor must reject a synthetic empty clause")
    val emptySourceErrors = typeCheckErrors("def invalid[](): Int = 1")
    assert(emptySourceErrors.nonEmpty, emptySourceErrors)

  test("context bounds are characterized separately and remain outside the candidate boundary"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val row = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*
      val definitions = scala.collection.mutable.ListBuffer.empty[DefDef]
      val traversal = new TreeTraverser:
        override def traverseTree(tree: Tree)(owner: Symbol): Unit =
          tree match
            case value: DefDef if value.name == "contextual" => definitions += value
            case _ => ()
          super.traverseTree(tree)(owner)
      traversal.traverseTree('{ def contextual[A: Ordering](value: A): A = value; () }.asTerm)(Symbol.spliceOwner)
      val target = definitions.head
      (
        target.paramss.head match
          case _: TypeParamClause => true
          case _ => false,
        target.paramss.tail.map {
          case clause: TermParamClause => (clause.params.map(_.name), clause.isGiven, clause.isImplicit)
          case _ => (Nil, false, false)
        },
        Q021CandidateFactory.typeDefs(using q).unapply(target).isEmpty
      )

    println(s"Q021_CONTEXT_BOUND_TOPOLOGY $row")
    assert(row._1, row)
    assert(row._2.exists(clause => clause._2 || clause._3), row)
    assert(row._3, row)

  test("all candidates reject malformed ownership ordering and nonordinary target topology"):
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
      val targets = List(
        "missing-rhs" -> DefDef.copy(exact)(exact.name, exact.paramss, exact.returnTpt, None),
        "foreign-owner" -> DefDef.copy(exact)(exact.name, foreign.paramss, exact.returnTpt, exact.rhs),
        "reversed-type-symbols" -> DefDef.copy(exact)(
          exact.name,
          TypeParamClause(typeClause.params.reverse) :: exact.paramss.tail,
          exact.returnTpt,
          exact.rhs
        ),
        "reversed-term-symbols" -> DefDef.copy(exact)(
          exact.name,
          List(typeClause, TermParamClause(termClause.params.reverse)),
          exact.returnTpt,
          exact.rhs
        ),
        "default-term" -> definition('{ def defaulted[A](value: A = null.asInstanceOf[A]): A = value; () }, "defaulted"),
        "context-bound" -> definition('{ def contextual[A: Ordering](value: A): A = value; () }, "contextual")
      )
      val rejectors: List[DefDef => Boolean] = List(
        target => Q021CandidateFactory.typeDefs(using q).unapply(target).isEmpty,
        target => Q021CandidateFactory.symbols(using q).unapply(target).isEmpty,
        target => Q021CandidateFactory.nameBounds(using q).unapply(target).isEmpty
      )
      targets.map((label, target) => label -> rejectors.forall(_(target)))

    rows.foreach(row => assert(row._2, row))

  test("standard selector accepts only the exact static five-capture layout"):
    inline def messages(inline source: String): List[String] = typeCheckErrors(source).map(_.message)

    val accepted = messages(
      """import scala.quoted.*; import quasiquotes.q021.Q021TypeDefStandardPattern.dqq
         def f(using q: Quotes)(d: q.reflect.DefDef) = d match
           case dqq"def $name[..$tparams](...$paramss): $result = $body" => ()
           case _ => ()"""
    )
    val rejected = List(
      messages(
        """import scala.quoted.*; import quasiquotes.q021.Q021TypeDefStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name(...$paramss): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q021.Q021TypeDefStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def fixed[..$tparams](...$paramss): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q021.Q021TypeDefStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name[A](...$paramss): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q021.Q021TypeDefStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name[...$tparams](...$paramss): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q021.Q021TypeDefStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name..$tparams(...$paramss): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q021.Q021TypeDefStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name[..$left, ..$right](...$paramss): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q021.Q021TypeDefStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name[..$tparams](..$params)(...$paramss): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q021.Q021TypeDefStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name[..$tparams](first: Int)(...$paramss): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q021.Q021TypeDefStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name[..$tparams](...$paramss)(last: Int): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q021.Q021TypeDefStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name[..$tparams](..$params): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q021.Q021TypeDefStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name[..$tparams](...$paramss) = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q021.Q021TypeDefStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name[..$tparams](...$paramss): Int = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q021.Q021TypeDefStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name[..$tparams](...$paramss): $result = $body + 1" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q021.Q021TypeDefStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name[..$tparams](...$paramss): $result = $left + $right" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q021.Q021TypeDefStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"private def $name[..$tparams](...$paramss): $result = $body" => ()
             case _ => ()"""
      )
    )
    assertEquals(accepted, Nil)
    assert(rejected.forall(_.nonEmpty), rejected)

  test("production Q020 rejects the unadmitted five-capture type-parameter syntax"):
    val errors = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.Quasiquotes.dqq
         def f(using q: Quotes)(d: q.reflect.DefDef) = d match
           case dqq"def $name[..$tparams](...$paramss): $result = $body" => ()
           case _ => ()"""
    )
    assert(errors.nonEmpty, errors)

  test("Q021 standard candidate dynamic selection remains closed"):
    val errors = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.matching.RankedDefinitionPatternExtractor
        import quasiquotes.q021.Q021TypeDefStandardPattern
        def dynamic(using q: Quotes)(context: StringContext): RankedDefinitionPatternExtractor[
          q.reflect.DefDef,
          (String, Seq[q.reflect.TypeDef], Seq[Seq[q.reflect.ValDef]], q.reflect.TypeRepr, q.reflect.Term)
        ] = Q021TypeDefStandardPattern.dqq(context)(using q)
      }"""
    )
    assert(errors.nonEmpty, errors)
