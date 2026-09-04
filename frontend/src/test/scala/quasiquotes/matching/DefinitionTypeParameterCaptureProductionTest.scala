package quasiquotes.matching

import scala.compiletime.testing.typeCheckErrors
import scala.language.experimental.erasedDefinitions
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

final class DefinitionTypeParameterCaptureProductionTest extends munit.FunSuite:
  test("external packages receive exact direct and umbrella five-capture types"):
    val _ = external.consumer.Q022ExternalDefinitionTypeParameterCaptureConsumer

  test("production five-capture matching preserves TypeDef bounds identity and all clause topology"):
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
        ("zero", List("A"), Nil, definition('{ def zero[A]: Int = 1; () }, "zero")),
        ("id", List("A"), List(1), definition('{ def id[A](value: A): A = value; () }, "id")),
        ("pair", List("A", "B"), List(2), definition('{ def pair[A, B](a: A, b: B): (A, B) = (a, b); () }, "pair")),
        ("triple", List("A", "B", "C"), List(3), definition('{ def triple[A, B, C](a: A, b: B, c: C): C = c; () }, "triple")),
        ("upper", List("A"), List(1), definition('{ def upper[A <: AnyVal](value: A): A = value; () }, "upper")),
        ("bounded", List("A"), List(1), definition('{ def bounded[A >: String <: Any](value: A): A = value; () }, "bounded")),
        ("dependent", List("A", "B"), List(1, 1), definition('{ def dependent[A, B <: List[A]](a: A)(b: B): B = b; () }, "dependent")),
        ("manyClauses", List("A", "B", "C", "D"), List(1, 0, 2), definition('{ def manyClauses[A, B, C, D](a: A)()(b: B, c: C): D = null.asInstanceOf[D]; () }, "manyClauses"))
      )
      val extractor = DefinitionPattern.dqq(
        StringContext("def ", "[..", "](...", "): ", " = ", "")
      )(using q)

      targets.map { (expectedName, expectedTypeNames, expectedTermCounts, target) =>
        val typeClause = target.paramss.head.asInstanceOf[TypeParamClause]
        val termClauses = target.paramss.tail.map(_.asInstanceOf[TermParamClause])
        val capture = extractor.unapply(target).get
        val bounds = capture._2.map(_.rhs.asInstanceOf[TypeBoundsTree].tpe)
        val boundSemantics = target.name match
          case "upper" => bounds.head.hi =:= TypeRepr.of[AnyVal]
          case "bounded" => bounds.head.low =:= TypeRepr.of[String] && bounds.head.hi =:= TypeRepr.of[Any]
          case _ => bounds.forall(_.low =:= TypeRepr.of[Nothing])
        val dependentIdentity =
          if target.name != "dependent" then true
          else
            val arguments = bounds(1).hi.typeArgs
            arguments.size == 1 &&
              arguments.head.typeSymbol == typeClause.params.head.symbol
        (
          expectedName,
          expectedTypeNames,
          expectedTermCounts,
          capture._1,
          capture._2.map(_.name).toList,
          capture._2.zip(typeClause.params).forall((captured, original) => captured eq original),
          capture._2.forall(parameter => parameter.symbol != Symbol.noSymbol && parameter.symbol.owner == target.symbol),
          capture._2.map(_.symbol).distinct.size == capture._2.size,
          capture._3.map(_.size).toList,
          capture._3.zip(termClauses).forall((captured, original) => captured.zip(original.params).forall((left, right) => left eq right)),
          capture._3.flatten.forall(parameter => parameter.symbol != Symbol.noSymbol && parameter.symbol.owner == target.symbol),
          target.symbol.paramSymss == typeClause.params.map(_.symbol) :: termClauses.map(_.params.map(_.symbol)),
          boundSemantics,
          dependentIdentity,
          capture._4 =:= target.returnTpt.tpe,
          target.rhs.exists(_ eq capture._5)
        )
      }

    rows.foreach { row =>
      assertEquals(row._4, row._1, row)
      assertEquals(row._5, row._2, row)
      assert(row._6, row)
      assert(row._7, row)
      assert(row._8, row)
      assertEquals(row._9, row._3, row)
      assert(row._10, row)
      assert(row._11, row)
      assert(row._12, row)
      assert(row._13, row)
      assert(row._14, row)
      assert(row._15, row)
      assert(row._16, row)
    }

  test("production five-capture matching rejects absent malformed contextual and non-method targets"):
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
      val extractor = DefinitionPattern.dqq(StringContext("def ", "[..", "](...", "): ", " = ", ""))(using q)
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
        "implicit" -> definition('{ def implicitClause[A](implicit value: A): A = value; () }, "implicitClause"),
        "erased" -> definition('{ def erasedClause[A](erased value: A): A = null.asInstanceOf[A]; () }, "erasedClause"),
        "constructor" -> constructor,
        "extension" -> extension,
        "field-accessor" -> flaggedAccessor(Flags.FieldAccessor),
        "param-accessor" -> flaggedAccessor(Flags.ParamAccessor),
        "case-accessor" -> flaggedAccessor(Flags.CaseAccessor),
        "given" -> provided,
        "null" -> null.asInstanceOf[DefDef]
      )
      (
        targets.map((label, target) => label -> extractor.unapply(target).isEmpty),
        try
          TypeParamClause(Nil)
          false
        catch case _: IllegalArgumentException => true
      )

    rows._1.foreach(row => assert(row._2, row))
    assert(rows._2, "public reflection must reject a synthetic empty TypeParamClause")
    assert(typeCheckErrors("def invalid[](): Int = 1").nonEmpty)

  test("standard production selector admits only the exact static six-part layout"):
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
           case dqq"def $name[..$tparams](...$paramss): $result = $body" => ()
           case _ => ()"""
    )
    val rejected = List(
      patternMessages("""case dqq"def fixed[..$tparams](...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name[A](...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name[...$tparams](...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name..$tparams(...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name[..$left, ..$right](...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name[..$tparams](..$params): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name[..$tparams](first: Int)(...$paramss): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name[..$tparams](...$paramss)(last: Int): $result = $body" => ()"""),
      patternMessages("""case dqq"def $name[..$tparams](...$paramss): Int = $body" => ()"""),
      patternMessages("""case dqq"def $name[..$tparams](...$paramss): $result = $body + 1" => ()"""),
      patternMessages("""case dqq"def $name[..$tparams](...$paramss): $result = $left + $right" => ()"""),
      patternMessages("""case dqq"private def $name[..$tparams](...$paramss): $result = $body" => ()""")
    )
    assertEquals(accepted, Nil)
    assertEquals(
      patternMessages("""case dqq"def $name(...$paramss): $result = $body" => ()"""),
      Nil
    )
    assert(rejected.forall(_.nonEmpty), rejected)
    assert(rejected.flatten.forall(_.contains("Invalid dqq definition-pattern template")), rejected)

  test("standard five-capture dynamic selection retains exact-one fallback"):
    val errors = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.matching.{DefinitionPattern, RankedDefinitionPatternExtractor}
         def f(using q: Quotes)(context: StringContext): RankedDefinitionPatternExtractor[
           q.reflect.DefDef,
           (String, Seq[q.reflect.TypeDef], Seq[Seq[q.reflect.ValDef]], q.reflect.TypeRepr, q.reflect.Term)
         ] = DefinitionPattern.dqq(context)(using q)"""
    )
    assert(errors.nonEmpty, errors)
