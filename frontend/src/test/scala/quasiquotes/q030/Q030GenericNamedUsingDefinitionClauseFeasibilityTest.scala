package quasiquotes.q030

import scala.compiletime.testing.typeCheckErrors
import scala.language.experimental.erasedDefinitions
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

import quasiquotes.matching.Q030GenericNamedUsingCandidateFactory

trait Q030Marker

final case class Q030ParameterFacts(
    name: String,
    ownerIsTarget: Boolean,
    isImplicit: Boolean,
    isGiven: Boolean,
    isSynthetic: Boolean,
    isErased: Boolean,
    hasDefault: Boolean,
    isArtifact: Boolean,
    sourceCode: Option[String]
)

final case class Q030ClauseFacts(
    mode: (Boolean, Boolean, Boolean),
    parameters: List[Q030ParameterFacts]
)

final case class Q030TargetFacts(
    label: String,
    topology: List[String],
    typeNames: List[String],
    typeOwnersExact: Boolean,
    typeSymbolsDistinct: Boolean,
    publicBounds: List[String],
    typeAnnotationsEmpty: Boolean,
    clauses: List[Q030ClauseFacts],
    paramSymssAligned: Boolean,
    candidateMatches: Boolean
)

final class Q030GenericNamedUsingDefinitionClauseFeasibilityTest extends munit.FunSuite:
  test("Q030 candidate exposes exact external captured and semantic-empty types"):
    val _ = external.consumer.Q030ExternalGenericNamedUsingDefinitionClauseConsumer

  test("public Quotes characterizes generic named using and context-bound evidence on every line"):
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

      val targets = List(
        "generic-one" -> definition('{ def genericOne[A](using ordering: Ordering[A]): Int = 1; () }, "genericOne"),
        "generic-two" -> definition('{ def genericTwo[A, B](using ev: Conversion[A, B]): Int = 1; () }, "genericTwo"),
        "upper-bound" -> definition('{ def upperBound[A <: AnyRef](using ordering: Ordering[A]): Int = 1; () }, "upperBound"),
        "dependent-bound" -> definition('{ def dependentBound[A, B <: List[A]](using marker: Numeric[Int]): Int = 1; () }, "dependentBound"),
        "authored-evidence-name" -> definition('{ def authoredEvidenceName[A](using evidence$1: Ordering[A]): Int = 1; () }, "authoredEvidenceName"),
        "context-bound" -> definition('{ def contextBound[A: Ordering]: Int = 1; () }, "contextBound"),
        "context-bound-using" -> definition('{ def contextBoundUsing[A: Ordering](using marker: Q030Marker): Int = 1; () }, "contextBoundUsing"),
        "context-bound-ordinary" -> definition('{ def contextBoundOrdinary[A: Ordering](value: A): A = value; () }, "contextBoundOrdinary"),
        "context-bound-ordinary-using" -> definition('{ def contextBoundOrdinaryUsing[A: Ordering](value: A)(using marker: Q030Marker): A = value; () }, "contextBoundOrdinaryUsing"),
        "multiple-context-bounds-using" -> definition('{ def multipleContextBounds[A: Ordering : Numeric](using marker: Q030Marker): Int = 1; () }, "multipleContextBounds"),
        "anonymous-using" -> definition('{ def anonymousUsing[A](using Ordering[A]): Int = 1; () }, "anonymousUsing"),
        "scala2-implicit" -> definition('{ def scala2Implicit[A](implicit ordering: Ordering[A]): Int = 1; () }, "scala2Implicit"),
        "erased" -> definition('{ def erasedParameter[A](erased erasedToken: Ordering[A]): Int = 1; () }, "erasedParameter")
      )
      val candidate = Q030GenericNamedUsingCandidateFactory.capturedModifiers(using q)

      targets.map { (label, target) =>
        val typeClauses = target.paramss.collect { case clause: TypeParamClause => clause }
        val termClauses = target.paramss.collect { case clause: TermParamClause => clause }
        val expectedSymbols =
          typeClauses.map(_.params.map(_.symbol)) ++ termClauses.map(_.params.map(_.symbol))
        Q030TargetFacts(
          label,
          target.paramss.map {
            case _: TypeParamClause => "TypeParamClause"
            case _: TermParamClause => "TermParamClause"
          },
          typeClauses.flatMap(_.params).map(_.name),
          typeClauses.flatMap(_.params).forall(_.symbol.owner == target.symbol),
          typeClauses.flatMap(_.params).map(_.symbol).distinct.size == typeClauses.flatMap(_.params).size,
          typeClauses.flatMap(_.params).map(_.rhs.asInstanceOf[TypeBoundsTree].tpe.show),
          typeClauses.flatMap(_.params).forall(_.symbol.annotations.isEmpty),
          termClauses.map(clause =>
            Q030ClauseFacts(
              (clause.isImplicit, clause.isGiven, clause.isErased),
              clause.params.map(parameter =>
                Q030ParameterFacts(
                  parameter.name,
                  parameter.symbol.owner == target.symbol,
                  parameter.symbol.flags.is(Flags.Implicit),
                  parameter.symbol.flags.is(Flags.Given),
                  parameter.symbol.flags.is(Flags.Synthetic),
                  parameter.symbol.flags.is(Flags.Erased),
                  parameter.symbol.flags.is(Flags.HasDefault),
                  parameter.symbol.flags.is(Flags.Artifact),
                  parameter.pos.sourceCode
                )
              )
            )
          ),
          target.symbol.paramSymss == expectedSymbols,
          candidate.unapply(target).nonEmpty
        )
      }

    val compilerLine = dotty.tools.dotc.config.Properties.versionNumberString
    rows.foreach(row => println(s"Q030_PUBLIC_QUOTES $compilerLine $row"))
    rows.foreach { row =>
      assert(row.paramSymssAligned, row)
      assert(row.typeAnnotationsEmpty, row)
      assert(row.typeOwnersExact, row)
      assert(row.typeSymbolsDistinct, row)
      assert(row.clauses.flattenParameters.forall(_.ownerIsTarget), row)
    }
    val safeMatches = Set(
      "generic-one",
      "generic-two",
      "upper-bound",
      "dependent-bound",
      "authored-evidence-name"
    )
    val unsafeContextBoundAliases =
      if compilerLine.startsWith("3.3") then Set.empty[String]
      else Set("context-bound", "context-bound-using", "multiple-context-bounds-using")
    val expectedMatches = safeMatches ++ unsafeContextBoundAliases
    assertEquals(rows.filter(_.candidateMatches).map(_.label).toSet, expectedMatches)

    val named = rows.find(_.label == "generic-one").get
    assertEquals(named.topology, List("TypeParamClause", "TermParamClause"))
    assertEquals(named.clauses.map(_.mode), List((false, true, false)))
    assert(named.clauses.flattenParameters.forall(p => p.isGiven && !p.isSynthetic), named)

    val anonymous = rows.find(_.label == "anonymous-using").get
    assert(anonymous.clauses.flattenParameters.forall(_.isSynthetic), anonymous)

    val implicitRow = rows.find(_.label == "scala2-implicit").get
    assertEquals(implicitRow.clauses.map(_.mode), List((true, false, false)))

    val erased = rows.find(_.label == "erased").get
    assertEquals(erased.clauses.map(_.mode), List((false, false, false)))
    assert(erased.clauses.flattenParameters.forall(_.isErased), erased)

    val contextRows = rows.filter(row =>
      row.label.startsWith("context-bound") || row.label == "multiple-context-bounds-using"
    )
    contextRows.foreach { row =>
      val generated = row.clauses.flattenParameters.filter(_.name.startsWith("evidence$"))
      assert(generated.nonEmpty, row)
      assert(generated.forall(parameter => parameter.isImplicit || parameter.isGiven), row)
      assert(generated.forall(parameter => !parameter.isSynthetic && !parameter.isArtifact), row)
      assert(row.publicBounds.forall(bound => !bound.contains("Ordering") && !bound.contains("Numeric")), row)
    }
    val authoredEvidenceName = rows.find(_.label == "authored-evidence-name").get
    assert(authoredEvidenceName.clauses.flattenParameters.exists(_.name == "evidence$1"), authoredEvidenceName)
    assert(authoredEvidenceName.clauses.flattenParameters.forall(parameter => !parameter.isSynthetic && !parameter.isArtifact), authoredEvidenceName)
    val merged = rows.find(_.label == "context-bound-using").get
    assert(merged.clauses.flattenParameters.exists(parameter => parameter.name == "marker" && !parameter.isSynthetic), merged)
    assert(merged.clauses.flattenParameters.exists(_.name.startsWith("evidence$")), merged)
    assert(merged.clauses.flattenParameters.forall(parameter => !parameter.isSynthetic), merged)
    val ordinaryMerged = rows.find(_.label == "context-bound-ordinary-using").get
    assertEquals(
      ordinaryMerged.topology,
      List("TypeParamClause", "TermParamClause", "TermParamClause")
    )
    assertEquals(ordinaryMerged.clauses.head.parameters.map(_.name), List("value"))
    assertEquals(
      ordinaryMerged.clauses(1).parameters.map(_.name),
      List("evidence$1", "marker")
    )
    val multipleMerged = rows.find(_.label == "multiple-context-bounds-using").get
    assertEquals(
      multipleMerged.clauses.flattenParameters.map(_.name),
      List("evidence$1", "evidence$2", "marker")
    )
    if compilerLine.startsWith("3.3") then
      assertEquals(rows.find(_.label == "context-bound").get.clauses.map(_.mode), List((true, false, false)))
      assertEquals(merged.clauses.map(_.mode), List((true, false, false)))
    else
      assertEquals(rows.find(_.label == "context-bound").get.clauses.map(_.mode), List((false, true, false)))
      assertEquals(merged.clauses.map(_.mode), List((false, true, false)))

  test("candidate A preserves 1 2 3 N TypeDef and named using ValDef identities"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val rows = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*

      def definitions(expression: Expr[Any]): Map[String, DefDef] =
        val found = scala.collection.mutable.Map.empty[String, DefDef]
        val traversal = new TreeTraverser:
          override def traverseTree(tree: Tree)(owner: Symbol): Unit =
            tree match
              case value: DefDef if !value.symbol.isClassConstructor => found.update(value.name, value)
              case _ => ()
            super.traverseTree(tree)(owner)
        traversal.traverseTree(expression.asTerm)(Symbol.spliceOwner)
        found.toMap

      val targets = definitions('{
        def one[A](using first: Ordering[A]): Int = 1
        def two[A, B](using first: Ordering[A], second: Conversion[A, B]): Int = 2
        def three[A, B, C](using first: Ordering[A], second: Ordering[B], third: Ordering[C]): Int = 3
        def many[A, B, C, D](using first: Ordering[A], second: Ordering[B], third: Ordering[C], fourth: Ordering[D]): Int = 4
        def upper[A <: AnyRef](using first: Ordering[A]): Int = 5
        def dependent[A, B <: List[A]](using first: Ordering[A], second: Ordering[B]): B = null.asInstanceOf[B]
        ()
      })
      val captured = Q030GenericNamedUsingCandidateFactory.capturedModifiers(using q)
      val semanticEmpty = Q030GenericNamedUsingCandidateFactory.semanticEmpty(using q)
      List(
        ("one", 1, 1),
        ("two", 2, 2),
        ("three", 3, 3),
        ("many", 4, 4),
        ("upper", 1, 1),
        ("dependent", 2, 2)
      ).map { (name, typeCount, parameterCount) =>
        val target = targets(name)
        val typeClause = target.paramss.head.asInstanceOf[TypeParamClause]
        val termClause = target.paramss(1).asInstanceOf[TermParamClause]
        val result = captured.unapply(target).get
        val bounds = result._3.map(_.rhs.asInstanceOf[TypeBoundsTree].tpe)
        val boundsCorrect =
          if name == "upper" then bounds.head.hi =:= TypeRepr.of[AnyRef]
          else if name == "dependent" then
            bounds(1).hi.typeArgs match
              case argument :: Nil => argument.typeSymbol == typeClause.params.head.symbol
              case _ => false
          else true
        (
          name,
          result._2 == name,
          result._3.size == typeCount,
          result._3.zip(typeClause.params).forall((left, right) => left eq right),
          result._3.forall(parameter => parameter.symbol != Symbol.noSymbol && parameter.symbol.owner == target.symbol),
          result._4.size == parameterCount,
          result._4.zip(termClause.params).forall((left, right) => left eq right),
          result._4.forall(parameter =>
            parameter.symbol != Symbol.noSymbol &&
              parameter.symbol.owner == target.symbol &&
              parameter.symbol.flags.is(Flags.Given) &&
              !parameter.symbol.flags.is(Flags.Synthetic) &&
              !parameter.symbol.flags.is(Flags.Erased) &&
              !parameter.symbol.flags.is(Flags.HasDefault)
          ),
          target.symbol.paramSymss == List(typeClause.params.map(_.symbol), termClause.params.map(_.symbol)),
          result._5 =:= target.returnTpt.tpe,
          target.rhs.exists(_ eq result._6),
          result._1.flags == target.symbol.flags,
          semanticEmpty.unapply(target).nonEmpty,
          boundsCorrect
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
      assert(row._13, row)
      assert(row._14, row)
    }

  test("candidate exposes the exact context-bound aliases while rejecting other malformed families"):
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

      val exact = definition('{ def exact[A, B](using first: Ordering[A], second: Ordering[B]): Int = 1; () }, "exact")
      val foreign = definition('{ def foreign[A, B](using first: Ordering[A], second: Ordering[B]): Int = 1; () }, "foreign")
      val typeClause = exact.paramss.head.asInstanceOf[TypeParamClause]
      val termClause = exact.paramss(1).asInstanceOf[TermParamClause]
      val constructor = definition('{ class Sample[A](using value: A); () }, "<init>")
      val extension = definition('{ extension [A](value: A) def expanded[B](using other: B): A = value; () }, "expanded")
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

      val targets = List(
        "nongeneric" -> definition('{ def nongeneric(using value: Ordering[Int]): Int = 1; () }, "nongeneric"),
        "context-bound" -> definition('{ def bounded[A: Ordering]: Int = 1; () }, "bounded"),
        "context-bound-using" -> definition('{ def boundedUsing[A: Ordering](using marker: Q030Marker): Int = 1; () }, "boundedUsing"),
        "context-bound-ordinary" -> definition('{ def boundedOrdinary[A: Ordering](value: A): A = value; () }, "boundedOrdinary"),
        "context-bound-ordinary-using" -> definition('{ def boundedMixed[A: Ordering](value: A)(using marker: Q030Marker): A = value; () }, "boundedMixed"),
        "multiple-context-bounds" -> definition('{ def multipleBounds[A: Ordering : Numeric](using marker: Q030Marker): Int = 1; () }, "multipleBounds"),
        "anonymous" -> definition('{ def anonymous[A](using Ordering[A]): Int = 1; () }, "anonymous"),
        "implicit" -> definition('{ def old[A](implicit value: Ordering[A]): Int = 1; () }, "old"),
        "ordinary" -> definition('{ def ordinary[A](value: A): A = value; () }, "ordinary"),
        "ordinary-then-using" -> definition('{ def mixed[A](value: A)(using ordering: Ordering[A]): A = value; () }, "mixed"),
        "multiple-using" -> definition('{ def multiple[A](using ordering: Ordering[A])(using numeric: Numeric[Int]): Int = 1; () }, "multiple"),
        "zero-term" -> definition('{ def zero[A]: Int = 1; () }, "zero"),
        "empty-ordinary" -> definition('{ def empty[A](): Int = 1; () }, "empty"),
        "default" -> definition('{ def defaulted[A](using value: Ordering[A] = null): Int = 1; () }, "defaulted"),
        "erased" -> definition('{ def erasedClause[A](erased value: Ordering[A]): Int = 1; () }, "erasedClause"),
        "foreign-owner" -> DefDef.copy(exact)(exact.name, foreign.paramss, exact.returnTpt, exact.rhs),
        "duplicate-types" -> DefDef.copy(exact)(exact.name, List(TypeParamClause(List(typeClause.params.head, typeClause.params.head)), termClause), exact.returnTpt, exact.rhs),
        "reordered-types" -> DefDef.copy(exact)(exact.name, List(TypeParamClause(typeClause.params.reverse), termClause), exact.returnTpt, exact.rhs),
        "duplicate-terms" -> DefDef.copy(exact)(exact.name, List(typeClause, TermParamClause(List(termClause.params.head, termClause.params.head))), exact.returnTpt, exact.rhs),
        "reordered-terms" -> DefDef.copy(exact)(exact.name, List(typeClause, TermParamClause(termClause.params.reverse)), exact.returnTpt, exact.rhs),
        "param-symss-mismatch" -> DefDef.copy(exact)(exact.name, List(typeClause, TermParamClause(List(termClause.params.head))), exact.returnTpt, exact.rhs),
        "multiple-type-clauses" -> DefDef.copy(exact)(exact.name, List(typeClause, typeClause, termClause), exact.returnTpt, exact.rhs),
        "type-clause-not-first" -> DefDef.copy(exact)(exact.name, List(termClause, typeClause), exact.returnTpt, exact.rhs),
        "missing-rhs" -> DefDef.copy(exact)(exact.name, exact.paramss, exact.returnTpt, None),
        "constructor" -> constructor,
        "extension" -> extension,
        "field-accessor" -> flaggedAccessor(Flags.FieldAccessor),
        "param-accessor" -> flaggedAccessor(Flags.ParamAccessor),
        "case-accessor" -> flaggedAccessor(Flags.CaseAccessor),
        "given" -> provided,
        "null" -> null.asInstanceOf[DefDef]
      )
      val candidate = Q030GenericNamedUsingCandidateFactory.capturedModifiers(using q)
      targets.map((label, target) => label -> candidate.unapply(target).isEmpty)

    val compilerLine = dotty.tools.dotc.config.Properties.versionNumberString
    val admitted = rows.collect { case (label, false) => label }.toSet
    val expectedAliases =
      if compilerLine.startsWith("3.3") then Set.empty[String]
      else Set("context-bound", "context-bound-using", "multiple-context-bounds")
    assertEquals(admitted, expectedAliases)

  test("captured and semantic-empty candidate siblings differ only by Q024 modifiers"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val result = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*
      val found = scala.collection.mutable.Map.empty[String, DefDef]
      val traversal = new TreeTraverser:
        override def traverseTree(tree: Tree)(owner: Symbol): Unit =
          tree match
            case value: DefDef if !value.symbol.isClassConstructor => found.update(value.name, value)
            case _ => ()
          super.traverseTree(tree)(owner)
      traversal.traverseTree('{
        class Fixture:
          def plain[A](using ordering: Ordering[A]): Int = 1
          final def modified[A](using ordering: Ordering[A]): Int = 1
        ()
      }.asTerm)(Symbol.spliceOwner)
      val captured = Q030GenericNamedUsingCandidateFactory.capturedModifiers(using q)
      val semanticEmpty = Q030GenericNamedUsingCandidateFactory.semanticEmpty(using q)
      (
        captured.unapply(found("plain")).nonEmpty,
        semanticEmpty.unapply(found("plain")).nonEmpty,
        captured.unapply(found("modified")).nonEmpty,
        semanticEmpty.unapply(found("modified")).isEmpty
      )

    assertEquals(result, (true, true, true, true))

  test("Q022 Q025 Q028 and Q029 production selectors remain closed for generic named using"):
    import quasiquotes.Quasiquotes.dqq

    given Compiler = Compiler.make(getClass.getClassLoader)
    val result = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*
      val found = scala.collection.mutable.ListBuffer.empty[DefDef]
      val traversal = new TreeTraverser:
        override def traverseTree(tree: Tree)(owner: Symbol): Unit =
          tree match
            case value: DefDef if value.name == "genericUsing" => found += value
            case _ => ()
          super.traverseTree(tree)(owner)
      traversal.traverseTree('{ def genericUsing[A](using ordering: Ordering[A]): Int = 1; () }.asTerm)(Symbol.spliceOwner)
      val target = found.head
      val q022 = dqq(StringContext("def ", "[..", "](...", "): ", " = ", ""))(using q)
      val q025 = dqq(StringContext("", " def ", "[..", "](...", "): ", " = ", ""))(using q)
      val q028 = dqq(StringContext("", " def ", "(using ..", "): ", " = ", ""))(using q)
      val q029 = dqq(StringContext("def ", "(using ..", "): ", " = ", ""))(using q)
      (
        q022.unapply(target).isEmpty,
        q025.unapply(target).isEmpty,
        q028.unapply(target).isEmpty,
        q029.unapply(target).isEmpty
      )

    assertEquals(result, (true, true, true, true))
    val errors = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.Quasiquotes.dqq
         def f(using q: Quotes)(d: q.reflect.DefDef) = d match
           case dqq"$mods def $name[..$tparams](using ..$params): $result = $body" => ()
           case _ => ()"""
    )
    assert(errors.nonEmpty, errors)
    assert(errors.exists(_.message.contains("Invalid dqq definition-pattern template")), errors)

extension (clauses: List[Q030ClauseFacts])
  private def flattenParameters: List[Q030ParameterFacts] = clauses.flatMap(_.parameters)
