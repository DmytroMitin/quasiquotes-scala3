package quasiquotes.q019

import scala.compiletime.testing.typeCheckErrors
import scala.language.experimental.erasedDefinitions
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

final class Q019DefinitionCaptureFeasibilityTest extends munit.FunSuite:
  test("Q019 standard candidates expose exact external-package binder types"):
    val _ = external.consumer.Q019ExternalDefinitionCaptureConsumer

  test("all four name-result pairings expose exact ranked carrier types"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    withQuotes:
      val q = summon[Quotes]
      import quasiquotes.matching.RankedDefinitionPatternExtractor

      val _: RankedDefinitionPatternExtractor[
        q.reflect.DefDef,
        (String, Seq[Seq[q.reflect.ValDef]], q.reflect.TypeRepr, q.reflect.Term)
      ] = Q019CandidateFactory.semantic(using q)
      val _: RankedDefinitionPatternExtractor[
        q.reflect.DefDef,
        (q.reflect.Symbol, Seq[Seq[q.reflect.ValDef]], q.reflect.TypeTree, q.reflect.Term)
      ] = Q019CandidateFactory.tree(using q)
      val _: RankedDefinitionPatternExtractor[
        q.reflect.DefDef,
        (String, Seq[Seq[q.reflect.ValDef]], q.reflect.TypeTree, q.reflect.Term)
      ] = Q019CandidateFactory.stringTree(using q)
      val _: RankedDefinitionPatternExtractor[
        q.reflect.DefDef,
        (q.reflect.Symbol, Seq[Seq[q.reflect.ValDef]], q.reflect.TypeRepr, q.reflect.Term)
      ] = Q019CandidateFactory.symbolTypeRepr(using q)

  test("semantic and reflection-tree candidates preserve the complete success matrix"):
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
        ("plain", "Int", Nil, definition('{ def plain: Int = 1; () }, "plain")),
        ("mixed_Name42", "String", List(0), definition('{ def mixed_Name42(): String = "ok"; () }, "mixed_Name42")),
        ("type", "List[Int]", List(1), definition('{ def `type`(value: Int): List[Int] = List(value); () }, "type")),
        ("++", "Either[Int, String]", List(1, 1), definition('{ def ++(left: Int)(right: String): Either[Int, String] = Left(left); () }, "++")),
        ("tupleResult", "scala.Tuple2[scala.Int, scala.Predef.String]", List(1, 0, 2), definition('{ def tupleResult(first: Int)()(second: String, third: Long): (Int, String) = (first, second); () }, "tupleResult")),
        ("functionResult", "scala.Function1[scala.Int, scala.Predef.String]", List(1), definition('{ def functionResult(prefix: String): Int => String = value => prefix + value; () }, "functionResult"))
      )
      val semantic = Q019CandidateFactory.semantic(using q)
      val tree = Q019CandidateFactory.tree(using q)
      val stringTree = Q019CandidateFactory.stringTree(using q)
      val symbolTypeRepr = Q019CandidateFactory.symbolTypeRepr(using q)

      targets.map { (expectedName, expectedTypeHint, expectedCounts, target) =>
        val originalClauses = target.paramss.collect { case clause: TermParamClause => clause }
        val semanticCapture = semantic.unapply(target).get
        val treeCapture = tree.unapply(target).get
        val stringTreeCapture = stringTree.unapply(target).get
        val symbolTypeReprCapture = symbolTypeRepr.unapply(target).get
        val parameters = semanticCapture._2.flatten
        (
          expectedName,
          expectedTypeHint,
          expectedCounts,
          target.name,
          target.symbol.name,
          semanticCapture._1,
          treeCapture._1 == target.symbol &&
            treeCapture._1.owner == target.symbol.owner &&
            treeCapture._1.flags == target.symbol.flags &&
            treeCapture._1.isDefDef,
          semanticCapture._2.map(_.size).toList,
          semanticCapture._2.zip(originalClauses).forall((captured, original) =>
            captured.zip(original.params).forall((left, right) => left eq right)
          ),
          parameters.forall(parameter =>
            parameter.symbol != Symbol.noSymbol && parameter.symbol.owner == target.symbol
          ),
          target.symbol.paramSymss == semanticCapture._2.map(_.map(_.symbol).toList).toList,
          semanticCapture._3 =:= target.returnTpt.tpe,
          treeCapture._3 eq target.returnTpt,
          stringTreeCapture._1 == expectedName && (stringTreeCapture._3 eq target.returnTpt),
          symbolTypeReprCapture._1 == target.symbol && (symbolTypeReprCapture._3 =:= target.returnTpt.tpe),
          target.rhs.exists(_ eq semanticCapture._4) && target.rhs.exists(_ eq treeCapture._4),
          semanticCapture._3.show,
          semanticCapture._3.asInstanceOf[AnyRef] eq target.returnTpt.tpe.asInstanceOf[AnyRef]
        )
      }

    rows.foreach { row =>
      println(s"Q019_STANDARD_NAME_RESULT $row")
      assertEquals(row._4, row._1, row)
      assertEquals(row._5, row._1, row)
      assertEquals(row._6, row._1, row)
      assert(row._7, row)
      assertEquals(row._8, row._3, row)
      assert(row._9, row)
      assert(row._10, row)
      assert(row._11, row)
      assert(row._12, row)
      assert(row._13, row)
      assert(row._14, row)
      assert(row._15, row)
      assert(row._16, row)
      assert(row._17.nonEmpty, row)
    }

  test("all candidates reject the bounded target mismatch matrix"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val rows = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*

      def definition(expression: Expr[Any], name: Option[String] = None): DefDef =
        val definitions = scala.collection.mutable.ListBuffer.empty[DefDef]
        val traversal = new TreeTraverser:
          override def traverseTree(tree: Tree)(owner: Symbol): Unit =
            tree match
              case value: DefDef => definitions += value
              case _ => ()
            super.traverseTree(tree)(owner)
        traversal.traverseTree(expression.asTerm)(Symbol.spliceOwner)
        name.fold(definitions.head)(selected => definitions.find(_.name == selected).get)

      val exact = definition('{ def exact(first: String)(second: Boolean, third: Int): Either[Int, String] = Left(third); () })
      val foreign = definition('{ def foreign(first: String)(second: Boolean, third: Int): Either[Int, String] = Left(third); () })
      val exactClauses = exact.paramss.collect { case clause: TermParamClause => clause }
      val foreignOwner = DefDef.copy(exact)(exact.name, foreign.paramss, exact.returnTpt, exact.rhs)
      val reversedAcross = DefDef.copy(exact)(exact.name, exactClauses.reverse.toList, exact.returnTpt, exact.rhs)
      val reversedWithin = DefDef.copy(exact)(
        exact.name,
        List(exactClauses.head, TermParamClause(exactClauses(1).params.reverse)),
        exact.returnTpt,
        exact.rhs
      )
      val noRhs = DefDef.copy(exact)(exact.name, exact.paramss, exact.returnTpt, None)
      val constructor = definition('{ class Sample(value: Int); () }, Some("<init>"))
      val extension = definition('{
        extension (value: String) def expanded(flag: Boolean): String = value
        ()
      }, Some("expanded"))

      def flaggedAccessor(flags: Flags): DefDef =
        val symbol = Symbol.newMethod(
          Symbol.spliceOwner,
          "accessor",
          MethodType(Nil)(_ => Nil, _ => TypeRepr.of[String]),
          flags,
          Symbol.noSymbol
        )
        DefDef(symbol, _ => Some(Literal(StringConstant("value"))))

      val provided = definition('{
        given provided(using value: Int): String = value.toString
        ()
      })
      val targets = List(
        "missing-rhs" -> noRhs,
        "type-parameter-clause" -> definition('{ def generic[A](value: A): A = value; () }),
        "contextual-using" -> definition('{ def contextual(using value: String): String = value; () }),
        "anonymous-given" -> definition('{ def anonymous(using String): String = ""; () }),
        "implicit" -> definition('{ def implicitClause(implicit value: String): String = value; () }),
        "erased" -> definition('{ def erasedClause(erased value: Int): Int = 0; () }),
        "default" -> definition('{ def defaulted(value: String = ""): String = value; () }),
        "foreign-owner" -> foreignOwner,
        "paramSymss-across" -> reversedAcross,
        "paramSymss-within" -> reversedWithin,
        "constructor" -> constructor,
        "extension" -> extension,
        "field-accessor" -> flaggedAccessor(Flags.FieldAccessor),
        "param-accessor" -> flaggedAccessor(Flags.ParamAccessor),
        "case-accessor" -> flaggedAccessor(Flags.CaseAccessor),
        "provided-given" -> provided,
        "null" -> null.asInstanceOf[DefDef]
      )
      val extractors: List[DefDef => Boolean] = List(
        (target: DefDef) => Q019CandidateFactory.semantic(using q).unapply(target).isEmpty,
        (target: DefDef) => Q019CandidateFactory.tree(using q).unapply(target).isEmpty,
        (target: DefDef) => Q019CandidateFactory.stringTree(using q).unapply(target).isEmpty,
        (target: DefDef) => Q019CandidateFactory.symbolTypeRepr(using q).unapply(target).isEmpty
      )
      targets.map((label, target) => (label, extractors.forall(_(target))))

    rows.foreach(row => assert(row._2, row))

  test("standard selector accepts only the exact static four-capture layout"):
    inline def messages(inline source: String): List[String] =
      typeCheckErrors(source).map(_.message)

    val accepted = messages(
      """import scala.quoted.*; import quasiquotes.q019.Q019SemanticStandardPattern.dqq
         def f(using q: Quotes)(d: q.reflect.DefDef) = d match
           case dqq"def $name(...$paramss): $result = $body" => ()
           case _ => ()"""
    )
    val cases = List(
      messages(
        """import scala.quoted.*; import quasiquotes.q019.Q019SemanticStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def fixed(...$paramss): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q019.Q019SemanticStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name(...$paramss) = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q019.Q019SemanticStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name(...$paramss): Int = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q019.Q019SemanticStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name(..$params): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q019.Q019SemanticStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name(first: Int)(...$paramss): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q019.Q019SemanticStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name(...$paramss)(last: Int): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q019.Q019SemanticStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name(...$left)(...$right): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q019.Q019SemanticStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name(..$params)(...$paramss): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q019.Q019SemanticStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name(...): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q019.Q019SemanticStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name(...$paramss): $result = $body + 1" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q019.Q019SemanticStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name(...$paramss): $result = $left + $right" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q019.Q019SemanticStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"private def $name(...$paramss): $result = $body" => ()
             case _ => ()"""
      ),
      messages(
        """import scala.quoted.*; import quasiquotes.q019.Q019SemanticStandardPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             case dqq"def $name[A](...$paramss): $result = $body" => ()
             case _ => ()"""
      )
    )

    assertEquals(accepted, Nil)
    assert(cases.forall(_.nonEmpty), cases)
    assert(cases.flatten.forall(_.contains("Invalid Q019 standard dqq")), cases)

  test("dynamic selection and production name-result capture remain closed"):
    val dynamicErrors = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.matching.RankedDefinitionPatternExtractor
        import quasiquotes.q019.Q019SemanticStandardPattern
        def dynamic(using q: Quotes)(context: StringContext): RankedDefinitionPatternExtractor[
          q.reflect.DefDef,
          (String, Seq[Seq[q.reflect.ValDef]], q.reflect.TypeRepr, q.reflect.Term)
        ] = Q019SemanticStandardPattern.dqq(context)(using q)
      }"""
    )
    val productionErrors = typeCheckErrors(
      """{
        import scala.quoted.*
        import quasiquotes.matching.DefinitionPattern.dqq
        def compile(using q: Quotes)(target: q.reflect.DefDef): Unit =
          target match
            case dqq"def $name(...$paramss): $result = $body" => ()
            case _ => ()
      }"""
    )
    assert(dynamicErrors.nonEmpty, dynamicErrors)
    assert(productionErrors.nonEmpty, productionErrors)
