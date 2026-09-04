package quasiquotes.q027

import scala.compiletime.testing.typeCheckErrors
import scala.language.experimental.erasedDefinitions
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

trait Q027Marker

final class Q027DefinitionClauseFeasibilityTest extends munit.FunSuite:
  test("Q027 candidates expose exact external path-dependent types"):
    val _ = external.consumer.Q027ExternalDefinitionClauseConsumer

  test("public reflection characterizes ordinary contextual implicit erased mixed and context-bound topology"):
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
        "ordinary" -> definition('{ def ordinary(value: Int): Int = value; () }, "ordinary"),
        "named-using" -> definition('{ def namedUsing(using ordering: Ordering[Int]): Int = 1; () }, "namedUsing"),
        "anonymous-using" -> definition('{ def anonymousUsing(using Ordering[Int]): Int = 1; () }, "anonymousUsing"),
        "scala2-implicit" -> definition('{ def scala2Implicit(implicit ordering: Ordering[Int]): Int = 1; () }, "scala2Implicit"),
        "ordinary-then-using" -> definition('{ def mixed(value: Int)(using ordering: Ordering[Int]): Int = value; () }, "mixed"),
        "multiple-using" -> definition('{ def multipleUsing(using first: Ordering[Int])(using second: Numeric[Int]): Int = 1; () }, "multipleUsing"),
        "erased" -> definition('{ def erasedClause(erased token: Int): Int = 1; () }, "erasedClause"),
        "context-bound" -> definition('{ def contextBound[A: Ordering]: Int = 1; () }, "contextBound"),
        "context-bound-ordinary" -> definition('{ def contextBoundOrdinary[A: Ordering](value: A): A = value; () }, "contextBoundOrdinary"),
        "context-bound-explicit-using" -> definition('{ def contextBoundUsing[A: Ordering](value: A)(using marker: Q027Marker): A = value; () }, "contextBoundUsing")
      )

      targets.map { (label, target) =>
        val typeClauses = target.paramss.collect { case clause: TypeParamClause => clause }
        val termClauses = target.paramss.collect { case clause: TermParamClause => clause }
        val nested = Q027CandidateFactory.nested(using q).unapply(target).get._1
        val native = Q027CandidateFactory.native(using q).unapply(target).get._1
        val structured = Q027CandidateFactory.structured(using q).unapply(target).get._1
        val flags = termClauses.map(_.params.map(parameter =>
          (
            parameter.symbol.flags.is(Flags.Implicit),
            parameter.symbol.flags.is(Flags.Given),
            parameter.symbol.flags.is(Flags.Erased),
            parameter.symbol.flags.is(Flags.Synthetic),
            parameter.symbol.flags.is(Flags.Artifact),
            parameter.pos.sourceCode
          )
        ))
        val clauseModes = termClauses.map(clause =>
          (clause.isImplicit, clause.isGiven, clause.isErased)
        )
        val nestedModes = nested.map(Q027CandidateFactory.modeFromParameters(using q))
        val nativeModes = native.map(Q027CandidateFactory.nativeMode(using q))
        val structuredModes = structured.map(_.mode)
        val expectedSymbols =
          typeClauses.map(_.params.map(_.symbol)) ++ termClauses.map(_.params.map(_.symbol))
        val typeFacts = typeClauses.flatMap(_.params).map(parameter =>
          (
            parameter.name,
            parameter.symbol.annotations.map(_.tpe.typeSymbol.fullName),
            parameter.rhs.show
          )
        )
        (
          label,
          target.paramss.map {
            case _: TypeParamClause => "TypeParamClause"
            case _: TermParamClause => "TermParamClause"
          },
          termClauses.map(_.params.map(_.name)),
          clauseModes,
          flags,
          target.symbol.paramSymss == expectedSymbols,
          native.zip(termClauses).forall((captured, original) => captured eq original),
          nested.zip(termClauses).forall((captured, original) =>
            captured.zip(original.params).forall((left, right) => left eq right)
          ),
          nestedModes,
          nativeModes,
          structuredModes,
          typeFacts
        )
      }

    val compilerLine = dotty.tools.dotc.config.Properties.versionNumberString
    rows.foreach(row => println(s"Q027_PUBLIC_REFLECTION $compilerLine $row"))
    rows.foreach { row =>
      assert(row._6, row)
      assert(row._7, row)
      assert(row._8, row)
      assertEquals(row._10, row._11, row)
    }
    val nestedVsNativeDisagreements = rows.collect {
      case row if row._9 != row._10 => row._1
    }
    if compilerLine.startsWith("3.3") then
      assertEquals(nestedVsNativeDisagreements, List("context-bound-explicit-using"))
    else assertEquals(nestedVsNativeDisagreements, Nil)
    assertEquals(rows.find(_._1 == "ordinary").get._11, List(Q027ClauseMode.Ordinary))
    assertEquals(rows.find(_._1 == "named-using").get._11, List(Q027ClauseMode.Contextual))
    assertEquals(rows.find(_._1 == "anonymous-using").get._11, List(Q027ClauseMode.Contextual))
    assertEquals(rows.find(_._1 == "scala2-implicit").get._11, List(Q027ClauseMode.Scala2Implicit))
    assertEquals(rows.find(_._1 == "ordinary-then-using").get._11, List(Q027ClauseMode.Ordinary, Q027ClauseMode.Contextual))
    assertEquals(rows.find(_._1 == "multiple-using").get._11, List(Q027ClauseMode.Contextual, Q027ClauseMode.Contextual))
    assertEquals(rows.find(_._1 == "erased").get._11, List(Q027ClauseMode.Erased))

  test("mode-fixed test grammars retain existing ValDef captures without widening complete paramss"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val result = withQuotes:
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

      val named = definition('{ def named(using ordering: Ordering[Int]): Int = 1; () }, "named")
      val mixed = definition('{ def mixed(value: Int)(using ordering: Ordering[Int]): Int = value; () }, "mixed")
      val anonymous = definition('{ def anonymous(using Ordering[Int]): Int = 1; () }, "anonymous")
      val namedClause = named.paramss.head.asInstanceOf[TermParamClause]
      val mixedClauses = mixed.paramss.map(_.asInstanceOf[TermParamClause])
      val namedCapture = Q027CandidateFactory.namedUsing(using q).unapply(named)
      val mixedCapture = Q027CandidateFactory.ordinaryThenNamedUsing(using q).unapply(mixed)
      (
        namedCapture.exists(_._1.zip(namedClause.params).forall((left, right) => left eq right)),
        mixedCapture.exists { (ordinary, contextual) =>
          ordinary.zip(mixedClauses.head.params).forall((left, right) => left eq right) &&
            contextual.zip(mixedClauses.last.params).forall((left, right) => left eq right)
        },
        Q027CandidateFactory.namedUsing(using q).unapply(anonymous).isEmpty,
        Q027CandidateFactory.ordinaryThenNamedUsing(using q).unapply(named).isEmpty
      )

    assert(result._1, result)
    assert(result._2, result)
    assert(result._3, result)
    assert(result._4, result)

  test("multiple explicit contextual clause legality is recorded per compiler line"):
    inline def errors(inline source: String): List[String] = typeCheckErrors(source).map(_.message)
    val diagnostics = errors(
      "def multiple(using first: Ordering[Int])(using second: Numeric[Int]): Int = 1"
    )
    println(s"Q027_MULTIPLE_CONTEXTUAL ${dotty.tools.dotc.config.Properties.versionNumberString} $diagnostics")
    assertEquals(diagnostics, Nil)

  test("all four full-structure production selectors remain closed for nonordinary context-bound and default clauses"):
    import quasiquotes.Quasiquotes.dqq

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

      def q020(target: DefDef): Boolean = target match
        case dqq"def $name(...$paramss): $result = $body" => true
        case _ => false
      def q026(target: DefDef): Boolean = target match
        case dqq"$mods def $name(...$paramss): $result = $body" => true
        case _ => false
      def q022(target: DefDef): Boolean = target match
        case dqq"def $name[..$tparams](...$paramss): $result = $body" => true
        case _ => false
      def q025(target: DefDef): Boolean = target match
        case dqq"$mods def $name[..$tparams](...$paramss): $result = $body" => true
        case _ => false

      val nongeneric = List(
        definition('{ def named(using ordering: Ordering[Int]): Int = 1; () }, "named"),
        definition('{ def implicitClause(implicit ordering: Ordering[Int]): Int = 1; () }, "implicitClause"),
        definition('{ def erasedClause(erased token: Int): Int = 1; () }, "erasedClause"),
        definition('{ def defaulted(value: Int = 1): Int = value; () }, "defaulted")
      )
      val generic = List(
        definition('{ def genericUsing[A](value: A)(using Ordering[A]): A = value; () }, "genericUsing"),
        definition('{ def bounded[A: Ordering](value: A): A = value; () }, "bounded"),
        definition('{ def genericDefault[A](value: A = null.asInstanceOf[A]): A = value; () }, "genericDefault")
      )
      (
        nongeneric.map(target => (!q020(target), !q026(target))),
        generic.map(target => (!q022(target), !q025(target)))
      )

    assert(rows._1.forall(row => row._1 && row._2), rows)
    assert(rows._2.forall(row => row._1 && row._2), rows)
