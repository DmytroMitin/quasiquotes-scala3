package quasiquotes.construct.q067

import scala.quoted.*

import _root_.quasiquotes.construct.{PlaceholderSource, QuasiTypeSplice, QuasiquoteBuilder, QuasiquoteHole, SelectedMemberName}
import _root_.quasiquotes.construct.hybrid.{HybridTermFrontend, ScalametaTermFrontend}
import _root_.quasiquotes.matching.TermMatcher
import _root_.quasiquotes.scalameta.TermFrontend

object Q067ScalametaP3ConstructionMacros:
  final case class OwnerEvidence(
      value: Int,
      engine: String,
      methodOwnedBySplice: Boolean,
      distinctSymbols: Boolean,
      parameterOwnedByMethod: Boolean,
      bodyUsesParameter: Boolean,
      resultUsesMethod: Boolean,
      parameterAndResultTypesEqual: Boolean,
      exactArgumentIdentity: Boolean,
      standardExactArgumentIdentity: Boolean,
      sameStructuralRolesAsStandard: Boolean
  )

  final case class RoundTripEvidence(
      hybridMatched: Boolean,
      standardMatched: Boolean,
      hybridOutcome: String,
      standardOutcome: String,
      exactHybridArgumentIdentity: Boolean,
      exactStandardArgumentIdentity: Boolean
  )

  inline def intIdentity(inline value: Int): Int = ${ intIdentityImpl('value) }
  inline def stringIdentity(inline value: String): String = ${ stringIdentityImpl('value) }
  inline def booleanIdentity(inline value: Boolean): Boolean = ${ booleanIdentityImpl('value) }
  inline def alphaRenamedIdentity(inline value: Int): Int = ${ alphaRenamedIdentityImpl('value) }
  inline def ownerEvidence(inline value: Int): OwnerEvidence = ${ ownerEvidenceImpl('value) }
  inline def literalOwnerEvidence: OwnerEvidence = ${ ownerEvidenceImpl('{ 47 }) }
  inline def roundTripEvidence(inline value: Int): RoundTripEvidence = ${ roundTripEvidenceImpl('value) }
  inline def negativeEvidence: List[String] = ${ negativeEvidenceImpl }
  inline def incorrectLexicalSourceCategory: String = ${ incorrectLexicalSourceCategoryImpl }

  private def intIdentityImpl(value: Expr[Int])(using q: Quotes): Expr[Int] =
    import q.reflect.*
    import _root_.quasiquotes.scalameta.ScalametaQuasiquotes.qr
    qr"""{ def id(value: Int): Int = value; id(${value.asTerm}) }""".asExprOf[Int]

  private def stringIdentityImpl(value: Expr[String])(using q: Quotes): Expr[String] =
    import q.reflect.*
    import _root_.quasiquotes.scalameta.ScalametaQuasiquotes.qr
    qr"""{ def identityString(text: String): String = text; identityString(${value.asTerm}) }"""
      .asExprOf[String]

  private def booleanIdentityImpl(value: Expr[Boolean])(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import _root_.quasiquotes.scalameta.ScalametaQuasiquotes.qr
    qr"""{ def identityBoolean(flag: Boolean): Boolean = flag; identityBoolean(${value.asTerm}) }"""
      .asExprOf[Boolean]

  private def alphaRenamedIdentityImpl(value: Expr[Int])(using q: Quotes): Expr[Int] =
    import q.reflect.*
    import _root_.quasiquotes.scalameta.ScalametaQuasiquotes.qr
    qr"""{ def _renamed42(_argument7: Int): Int = _argument7; _renamed42(${value.asTerm}) }"""
      .asExprOf[Int]

  private def ownerEvidenceImpl(value: Expr[Int])(using q: Quotes): Expr[OwnerEvidence] =
    import q.reflect.*
    import _root_.quasiquotes.scalameta.ScalametaQuasiquotes.qr

    val argument = value.asTerm
    val parts = Seq("{ def id(value: Int): Int = value; id(", ") }")
    val hybrid = StringContext(parts*).qr(argument)
    val standard = QuasiquoteBuilder.build(parts, Seq(argument)).fold(
      error => report.errorAndAbort(error.message),
      identity
    )
    val engine = TermFrontend.build(parts, Seq(argument)).fold(
      error => report.errorAndAbort(error.message),
      _.engine.toString
    )

    def inspect(term: Term): (DefDef, ValDef, Term, Term) =
      term match
        case Block(List(definition: DefDef), Apply(function, List(loweredArgument))) =>
          (definition, definition.termParamss.flatMap(_.params).head, function, loweredArgument)
        case other => report.errorAndAbort(s"unexpected P3 tree: ${Printer.TreeStructure.show(other)}")

    val (definition, parameter, function, loweredArgument) = inspect(hybrid)
    val (standardDefinition, standardParameter, standardFunction, standardArgument) = inspect(standard)
    val sameRoles =
      definition.rhs.exists(_.symbol == parameter.symbol) &&
        function.symbol == definition.symbol &&
        standardDefinition.rhs.exists(_.symbol == standardParameter.symbol) &&
        standardFunction.symbol == standardDefinition.symbol &&
        definition.returnTpt.tpe =:= standardDefinition.returnTpt.tpe &&
        parameter.tpt.tpe =:= standardParameter.tpt.tpe

    '{
      OwnerEvidence(
        ${ loweredArgument.asExprOf[Int] },
        ${ Expr(engine) },
        ${ Expr(definition.symbol.owner == Symbol.spliceOwner) },
        ${ Expr(definition.symbol != parameter.symbol) },
        ${ Expr(parameter.symbol.owner == definition.symbol) },
        ${ Expr(definition.rhs.exists(_.symbol == parameter.symbol)) },
        ${ Expr(function.symbol == definition.symbol) },
        ${ Expr(parameter.tpt.tpe =:= definition.returnTpt.tpe) },
        ${ Expr(loweredArgument.asInstanceOf[AnyRef].eq(argument.asInstanceOf[AnyRef])) },
        ${ Expr(standardArgument.asInstanceOf[AnyRef].eq(argument.asInstanceOf[AnyRef])) },
        ${ Expr(sameRoles) }
      )
    }

  private def roundTripEvidenceImpl(value: Expr[Int])(using q: Quotes): Expr[RoundTripEvidence] =
    import q.reflect.*
    import _root_.quasiquotes.scalameta.ScalametaQuasiquotes.qr

    val argument = value.asTerm
    val parts = Seq("{ def id(value: Int): Int = value; id(", ") }")
    val hybrid = StringContext(parts*).qr(argument)
    val standard = QuasiquoteBuilder.build(parts, Seq(argument)).fold(
      error => report.errorAndAbort(error.message),
      identity
    )
    val compiled = TermFrontend
      .compile("{ def id(value: Int): Int = value; id($argument) }")
      .fold(error => report.errorAndAbort(error.message), identity)

    def outcome(target: Term): Either[String, Unit] =
      TermMatcher.matchTerm(compiled.pattern, target).left.map(_.toString).map(_ => ())
    def argumentOf(target: Term): Term = target match
      case Block(List(_: DefDef), Apply(_, List(rawArgument))) => rawArgument
      case other => report.errorAndAbort(s"unexpected P3 tree: ${Printer.TreeStructure.show(other)}")

    val hybridOutcome = outcome(hybrid)
    val standardOutcome = outcome(standard)
    '{
      RoundTripEvidence(
        ${ Expr(hybridOutcome.isRight) },
        ${ Expr(standardOutcome.isRight) },
        ${ Expr(hybridOutcome.fold(identity, _ => "MATCHED")) },
        ${ Expr(standardOutcome.fold(identity, _ => "MATCHED")) },
        ${ Expr(argumentOf(hybrid).asInstanceOf[AnyRef].eq(argument.asInstanceOf[AnyRef])) },
        ${ Expr(argumentOf(standard).asInstanceOf[AnyRef].eq(argument.asInstanceOf[AnyRef])) }
      )
    }

  private def negativeEvidenceImpl(using q: Quotes): Expr[List[String]] =
    import q.reflect.*

    val intArgument = Expr(1).asTerm
    val stringArgument = Expr("x").asTerm
    val ownedArgument = '{
      def nested(value: Int): Int = value
      nested(1)
    }.asTerm

    def split(source: String): Seq[String] = source.split("\\$argument", -1).toSeq
    def outcome(
        label: String,
        source: String,
        arguments: Seq[Term | TypeRepr | QuasiTypeSplice | SelectedMemberName] = Seq(intArgument)
    ): String =
      val first = HybridTermFrontend.build(split(source), arguments)
      val second = HybridTermFrontend.build(split(source), arguments)
      val rendered = first.fold(
        failure => s"${failure.category}:${failure.start}:${failure.end}:${failure.detail}",
        result => s"ADMIT:${result.engine}"
      )
      val deterministic = first.map(_.engine) == second.map(_.engine) &&
        first.left.map(_.message) == second.left.map(_.message)
      s"$label|$rendered|deterministic=$deterministic"

    val rows = List(
      outcome("wrong-body", "{ def id(value: Int): Int = 1; id($argument) }"),
      outcome("recursive-body", "{ def id(value: Int): Int = id(value); id($argument) }"),
      outcome("wrong-callee", "{ def id(value: Int): Int = value; other($argument) }"),
      outcome("missing-call", "{ def id(value: Int): Int = value; id }"),
      outcome("extra-call", "{ def id(value: Int): Int = value; id($argument); id(1) }"),
      outcome("inferred-result", "{ def id(value: Int) = value; id($argument) }"),
      outcome("mismatched-types", "{ def id(value: Int): String = value.toString; id($argument) }"),
      outcome("unsupported-type", "{ def id(value: Double): Double = value; id($argument) }"),
      outcome("zero-params", "{ def id(): Int = 1; id($argument) }"),
      outcome("two-params", "{ def id(left: Int, right: Int): Int = left; id($argument, 1) }"),
      outcome("two-clauses", "{ def id(value: Int)(other: Int): Int = value; id($argument)(1) }"),
      outcome("type-param", "{ def id[A](value: Int): Int = value; id($argument) }"),
      outcome("using", "{ def id(using value: Int): Int = value; id(using $argument) }"),
      outcome("implicit", "{ implicit def id(value: Int): Int = value; id($argument) }"),
      outcome("by-name", "{ def id(value: => Int): Int = value; id($argument) }"),
      outcome("repeated", "{ def id(value: Int*): Int = value.head; id($argument) }"),
      outcome("default", "{ def id(value: Int = 1): Int = value; id($argument) }"),
      outcome("inline", "{ inline def id(value: Int): Int = value; id($argument) }"),
      outcome("annotation", "{ @deprecated(\"q067\", \"\") def id(value: Int): Int = value; id($argument) }"),
      outcome("parameter-annotation", "{ def id(@deprecated(\"q067\", \"\") value: Int): Int = value; id($argument) }"),
      outcome("parameter-modifier", "{ def id(private val value: Int): Int = value; id($argument) }"),
      outcome("multiple-defs", "{ def first(value: Int): Int = value; def second(value: Int): Int = value; first($argument) }"),
      outcome("mixed-val-def", "{ val seed: Int = 1; def id(value: Int): Int = value; id($argument) }"),
      outcome("nested-def", "{ def id(value: Int): Int = { def nested(x: Int): Int = x; nested(value) }; id($argument) }"),
      outcome("unicode-method", "{ def λ(value: Int): Int = value; λ($argument) }"),
      outcome("unicode-parameter", "{ def id(λ: Int): Int = λ; id($argument) }"),
      outcome("quoted-method", "{ def `type`(value: Int): Int = value; `type`($argument) }"),
      outcome("symbolic-method", "{ def +(value: Int): Int = value; +($argument) }"),
      outcome("misplaced-body", "{ def id(value: Int): Int = $argument; id(1) }"),
      outcome("misplaced-callee", "{ def id(value: Int): Int = value; $argument(1) }"),
      outcome("misplaced-type", "{ def id(value: $argument): Int = value; id(1) }"),
      outcome(
        "reflected-type-hole",
        "{ def id(value: $argument): Int = value; id(1) }",
        Seq(TypeRepr.of[Int])
      ),
      outcome(
        "reflected-type-as-term",
        "{ def id(value: Int): Int = value; id($argument) }",
        Seq(TypeRepr.of[Int])
      ),
      outcome("wrong-argument-type", "{ def id(value: Int): Int = value; id($argument) }", Seq(stringArgument)),
      outcome("owned-definition", "{ def id(value: Int): Int = value; id($argument) }", Seq(ownedArgument)),
      outcome("null-argument", "{ def id(value: Int): Int = value; id($argument) }", Seq(null.asInstanceOf[Term])),
      outcome("zero-hole", "{ def id(value: Int): Int = value; id(__qq_term_hole_0) }", Nil),
      outcome(
        "two-holes",
        "{ def id(value: Int): Int = value; id($argument) /* $argument */ }",
        Seq(intArgument, intArgument)
      )
    )
    Expr.ofList(rows.map(Expr(_)))

  private def incorrectLexicalSourceCategoryImpl(using q: Quotes): Expr[String] =
    import q.reflect.*

    val argument = Expr(1).asTerm
    val holes: Seq[QuasiquoteHole[Term, TypeRepr]] = Seq(QuasiquoteHole.Term(argument))
    val source = PlaceholderSource
      .synthesizeCategorized(
        Seq("{ def id(value: Int): Int = value; id(", ") }"),
        holes
      )
      .fold(error => report.errorAndAbort(error.message), identity)
    val tree = ScalametaTermFrontend
      .parse(source.source)
      .fold(error => report.errorAndAbort(error.message), identity)
    val binding = source.bindings.head
    val start = source.source.indexOf(binding.name)
    val corrupted = source.copy(source = source.source.updated(start, 'x'))
    val category = ScalametaTermFrontend
      .lowerTree(
        tree,
        Map(binding.name -> argument),
        Map.empty,
        Map.empty,
        Map.empty,
        source.literalCategorizedNames,
        Some(corrupted)
      )
      .fold(_.category, _ => "ADMIT")
    Expr(category)
