package quasiquotes.q038

import scala.annotation.StaticAnnotation
import scala.compiletime.testing.typeCheckErrors
import scala.language.experimental.erasedDefinitions
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

final class Q038Annotation extends StaticAnnotation
trait Q038Marker

final class Q038DefaultedOrdinaryDefinitionFeasibilityTest extends munit.FunSuite:
  test("test-only Q038 grammar exposes the exact external presence-only capture type"):
    val _ = external.consumer.Q038ExternalDefaultedOrdinaryDefinitionConsumer

  test("public Quotes exposes the bounded default-parameter and getter evidence"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val rows = withQuotes:
      Q038QuotesDefaultEvidence.inspect

    rows.methods.foreach(row => println(s"Q038_METHOD $row"))
    rows.getters.foreach(row => println(s"Q038_GETTER $row"))
    val methods = rows.methods.map(row => row.name -> row).toMap
    assertEquals(methods("emptyOrdinary").clauseSizes, List(0))
    assertEquals(methods("emptyOrdinary").symbolClauseSizes, List(0))
    assert(rows.methods.forall(_.exactParamSymss), rows)
    assert(rows.methods.flatMap(_.parameters).forall(parameter =>
      parameter.rhsFamily == "absent" &&
        parameter.symbolPresent &&
        parameter.ownerIsMethod &&
        !parameter.isImplicit &&
        !parameter.isGiven &&
        !parameter.isSynthetic &&
        !parameter.isErased &&
        parameter.hasPosition
    ), rows)
    assertEquals(methods("noDefault").parameters.map(_.hasDefault), List(false))
    assertEquals(methods("oneDefault").parameters.map(_.hasDefault), List(true))
    assertEquals(methods("trailingDefault").parameters.map(_.hasDefault), List(false, true))
    assertEquals(methods("twoDefaults").parameters.map(_.hasDefault), List(true, true))
    assertEquals(methods("manyDefaults").parameters.map(_.hasDefault), List(true, false, true, true))

    val getters = rows.getters.map(row => row.name -> row).toMap
    assertEquals(getters.keySet, Set(
      "oneDefault$default$1",
      "trailingDefault$default$2",
      "twoDefaults$default$1",
      "twoDefaults$default$2",
      "nonliteralDefault$default$1",
      "stableSelectionDefault$default$1",
      "callExpressionDefault$default$1",
      "manyDefaults$default$1",
      "manyDefaults$default$3",
      "manyDefaults$default$4"
    ))
    assert(rows.getters.forall(getter =>
      getter.ownerName.endsWith("$Fixture") &&
        !getter.isSynthetic &&
        getter.isMethod &&
        getter.clauseSizes.isEmpty &&
        getter.rhsFamily != "absent" &&
        getter.hasPosition
    ), rows)
    assertEquals(getters("oneDefault$default$1").rhsFamily, "literal")
    assertEquals(getters("stableSelectionDefault$default$1").rhsFamily, "selection")
    assertEquals(getters("callExpressionDefault$default$1").rhsFamily, "call")

  test("public Quotes exposes multi-clause generic and inherited getter contrasts"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val rows = withQuotes:
      Q038QuotesDefaultContrastEvidence.inspect

    rows.methods.foreach(row => println(s"Q038_CONTRAST_METHOD $row"))
    rows.getters.foreach(row => println(s"Q038_CONTRAST_GETTER $row"))
    val methods = rows.methods.groupBy(row => (row.ownerSuffix, row.name))
    assertEquals(methods(("$Contrast", "multiple")).head.termDefaultFlags, List(List(true), List(true)))
    assertEquals(methods(("$Contrast", "depends")).head.termDefaultFlags, List(List(false), List(true)))
    assertEquals(methods(("$Contrast", "generic")).head.clauseKinds, List("type", "ordinary"))
    assertEquals(methods(("$Parent", "inherited")).head.termDefaultFlags, List(List(true)))
    assertEquals(methods(("$Child", "inherited")).head.termDefaultFlags, List(List(false)))

    val getters = rows.getters.map(row => row.name -> row).toMap
    assertEquals(getters("multiple$default$1").clauseKinds, Nil)
    assertEquals(getters("multiple$default$2").clauseKinds, List("ordinary"))
    assertEquals(getters("multiple$default$2").clauseSizes, List(1))
    assertEquals(getters("depends$default$2").clauseKinds, List("ordinary"))
    assert(getters("depends$default$2").referencedTerms.contains("x"), getters)
    assertEquals(getters("generic$default$1").clauseKinds, List("type"))
    assert(rows.getters.exists(row => row.ownerSuffix == "$Parent" && row.name == "inherited$default$1"), rows)
    assert(!rows.getters.exists(row => row.ownerSuffix == "$Child" && row.name == "inherited$default$1"), rows)

    val overloadErrors = typeCheckErrors(
      "class Invalid { def overloaded(x: Int = 1): Int = x; def overloaded(x: String = \"x\"): String = x }"
    )
    assert(overloadErrors.nonEmpty, overloadErrors)

  test("presence-only candidate preserves original binders topology modifiers result and body"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val rows = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*

      val found = scala.collection.mutable.Map.empty[String, DefDef]
      val traversal = new TreeTraverser:
        override def traverseTree(tree: Tree)(owner: Symbol): Unit =
          tree match
            case definition: DefDef if !definition.name.contains("$default$") =>
              found.update(definition.name, definition)
            case _ => ()
          super.traverseTree(tree)(owner)
      traversal.traverseTree('{
        class Fixture:
          def oneDefault(x: Int = 1): Int = x
          def trailingDefault(x: Int, y: String = "x"): String = y
          def twoDefaults(x: Int = 1, y: String = "x"): String = y
          def manyDefaults(a: Int = 1, b: Int, c: String = "c", d: Long = 4L): Long = d
          final def modified(x: Int = 1): Int = x
          @Q038Annotation private[q038] def annotated(x: Int = 1): Int = x
        ()
      }.asTerm)(Symbol.spliceOwner)

      def sameScope(left: Option[TypeRepr], right: Option[TypeRepr]): Boolean =
        (left, right) match
          case (None, None) => true
          case (Some(a), Some(b)) => a =:= b
          case _ => false

      val candidate = Q038DefaultedOrdinaryCandidateFactory.capturedModifiers(using q)
      List(
        "oneDefault" -> List(true),
        "trailingDefault" -> List(false, true),
        "twoDefaults" -> List(true, true),
        "manyDefaults" -> List(true, false, true, true),
        "modified" -> List(true),
        "annotated" -> List(true)
      ).map { (name, defaultFlags) =>
        val target = found(name)
        val clause = target.paramss.head.asInstanceOf[TermParamClause]
        val captured = candidate.unapply(target).get
        (
          name,
          captured._2 == name,
          captured._3.map(_.symbol.flags.is(Flags.HasDefault)).toList == defaultFlags,
          captured._3.zip(clause.params).forall((left, right) => left eq right),
          captured._3.forall(_.rhs.isEmpty),
          captured._3.map(_.symbol).forall(_ != Symbol.noSymbol),
          captured._3.map(_.symbol).distinct.size == captured._3.size,
          captured._3.forall(_.symbol.owner == target.symbol),
          target.symbol.paramSymss == List(captured._3.map(_.symbol).toList),
          captured._4 =:= target.returnTpt.tpe,
          target.rhs.exists(_ eq captured._5),
          captured._1.flags == target.symbol.flags,
          sameScope(captured._1.privateWithin, target.symbol.privateWithin),
          sameScope(captured._1.protectedWithin, target.symbol.protectedWithin),
          captured._1.annotations.size == target.symbol.annotations.size &&
            captured._1.annotations.zip(target.symbol.annotations).forall((left, right) => left eq right)
        )
      }

    rows.foreach(row => row.productIterator.drop(1).foreach(value => assertEquals(value, true, row)))

  test("presence-only candidate rejects out-of-scope and malformed targets"):
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

      val exact = definition('{ def exact(x: Int = 1, y: Int = 2): Int = x; () }, "exact")
      val foreign = definition('{ def foreign(x: Int = 1, y: Int = 2): Int = x; () }, "foreign")
      val clause = exact.paramss.head.asInstanceOf[TermParamClause]
      val constructor = definition('{ class Sample(x: Int = 1); () }, "<init>")
      val extension = definition('{ extension (x: Int) def expanded(y: Int = 1): Int = x + y; () }, "expanded")
      val provided = definition('{ given provided(using value: Int = 1): Int = value; () }, "provided")
      def flaggedAccessor(flags: Flags): DefDef =
        val symbol = Symbol.newMethod(
          Symbol.spliceOwner,
          "accessor",
          MethodType(Nil)(_ => Nil, _ => TypeRepr.of[String]),
          flags,
          Symbol.noSymbol
        )
        DefDef(symbol, _ => Some(Literal(StringConstant("value"))))
      val syntheticSymbol = Symbol.newVal(
        exact.symbol,
        "syntheticDefault",
        TypeRepr.of[Int],
        Flags.Synthetic | Flags.HasDefault,
        Symbol.noSymbol
      )
      val syntheticClause = TermParamClause(List(ValDef(syntheticSymbol, None)))
      val candidate = Q038DefaultedOrdinaryCandidateFactory.capturedModifiers(using q)
      val targets = List(
        "no-default" -> definition('{ def noDefault(x: Int): Int = x; () }, "noDefault"),
        "no-parameters" -> definition('{ def empty(): Int = 0; () }, "empty"),
        "generic" -> definition('{ def generic[A](x: A = null.asInstanceOf[A]): A = x; () }, "generic"),
        "context-bound" -> definition('{ def contextual[A: Ordering](x: A = null.asInstanceOf[A]): A = x; () }, "contextual"),
        "using" -> definition('{ def usingDefault(using x: Int = 1): Int = x; () }, "usingDefault"),
        "implicit" -> definition('{ def implicitDefault(implicit x: Int = 1): Int = x; () }, "implicitDefault"),
        "erased-default" -> definition('{ def erasedDefault(erased x: Int = 1): Int = 0; () }, "erasedDefault"),
        "parameter-annotation" -> definition('{ def parameterAnnotated(@Q038Annotation x: Int = 1): Int = x; () }, "parameterAnnotated"),
        "multiple-ordinary" -> definition('{ def multiple(x: Int = 1)(y: Int = 2): Int = x + y; () }, "multiple"),
        "repeated" -> definition('{ def repeated(xs: Int*): Int = xs.size; () }, "repeated"),
        "foreign-owner" -> DefDef.copy(exact)(exact.name, foreign.paramss, exact.returnTpt, exact.rhs),
        "duplicate" -> DefDef.copy(exact)(exact.name, List(TermParamClause(List(clause.params.head, clause.params.head))), exact.returnTpt, exact.rhs),
        "reordered" -> DefDef.copy(exact)(exact.name, List(TermParamClause(clause.params.reverse)), exact.returnTpt, exact.rhs),
        "param-symss-mismatch" -> DefDef.copy(exact)(exact.name, List(TermParamClause(List(clause.params.head))), exact.returnTpt, exact.rhs),
        "synthetic-structural" -> DefDef.copy(exact)(exact.name, List(syntheticClause), exact.returnTpt, exact.rhs),
        "missing-rhs" -> DefDef.copy(exact)(exact.name, exact.paramss, exact.returnTpt, None),
        "constructor" -> constructor,
        "extension" -> extension,
        "field-accessor" -> flaggedAccessor(Flags.FieldAccessor),
        "param-accessor" -> flaggedAccessor(Flags.ParamAccessor),
        "case-accessor" -> flaggedAccessor(Flags.CaseAccessor),
        "given" -> provided,
        "null" -> null.asInstanceOf[DefDef]
      )
      targets.map((label, target) => label -> candidate.unapply(target).isEmpty)

    rows.foreach(row => assert(row._2, row))

  test("test-only grammar is exact and real production dqq now selects Q044"):
    inline def messages(inline source: String): List[String] = typeCheckErrors(source).map(_.message)
    inline def candidateMessages(inline pattern: String): List[String] =
      messages(
        """import scala.quoted.*; import quasiquotes.q038.Q038DefaultedOrdinaryDefinitionPattern.dqq
           def f(using q: Quotes)(d: q.reflect.DefDef) = d match
             """ + pattern + """
             case _ => ()"""
      )

    val accepted = candidateMessages("""case dqq"$mods def $name(..$params): $result = $body" => ()""")
    val rejected = List(
      candidateMessages("""case dqq"def $name(..$params): $result = $body" => ()"""),
      candidateMessages("""case dqq"$mods def fixed(..$params): $result = $body" => ()"""),
      candidateMessages("""case dqq"$mods def $name(...$paramss): $result = $body" => ()"""),
      candidateMessages("""case dqq"$mods def $name(..$params)(extra: Int): $result = $body" => ()"""),
      candidateMessages("""case dqq"$mods def $name[..$tparams](..$params): $result = $body" => ()"""),
      candidateMessages("""case dqq"$mods def $name(..$params): Int = $body" => ()"""),
      candidateMessages("""case dqq"$mods def $name(..$params): $result = $left + $right" => ()"""),
      candidateMessages("""case dqq"$mods def $name(.$params): $result = $body" => ()"""),
      candidateMessages("""case dqq"$mods def $name(..$params): $result = $body trailing" => ()""")
    )
    assertEquals(accepted, Nil)
    assert(rejected.forall(_.nonEmpty), rejected)

    val production = messages(
      """import scala.quoted.*; import quasiquotes.matching.DefinitionPattern.dqq
         def f(using q: Quotes)(d: q.reflect.DefDef) = d match
           case dqq"$mods def $name(..$params): $result = $body" => ()
           case _ => ()"""
    )
    assertEquals(production, Nil)

    assertEquals(
      typeCheckErrors("import scala.language.experimental.erasedDefinitions; def valid(erased x: Int = 1): Int = 0"),
      Nil
    )
