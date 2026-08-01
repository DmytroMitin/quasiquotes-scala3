package quasiquotes.construct

import scala.quoted.*

import quasiquotes.definitions.*
import quasiquotes.parser.TermShape
import quasiquotes.source.*
import quasiquotes.terms.ConstructedTerm
import quasiquotes.types.TypeNormalForm

/** Package-internal macro proof surface used only by the Phase 53 tests. */
private[quasiquotes] object DefinitionQuasiquoteMacroExamples:
  inline def successfulCaller(inline anchor: Int): String =
    ${ successfulCallerImpl('anchor) }

  inline def exactPositionSummary(
      inline definitionTypeAnchor: Int,
      inline bodyTermAnchor: Int,
      inline bodyTypeAnchor: Int
  ): String =
    ${
      exactPositionSummaryImpl(
        'definitionTypeAnchor,
        'bodyTermAnchor,
        'bodyTypeAnchor
      )
    }

  inline def fallbackPositionSummary(inline anchor: Int): String =
    ${ fallbackPositionSummaryImpl('anchor) }

  inline def invalidDefinitionType(inline anchor: String): String =
    ${ invalidDefinitionTypeImpl('anchor) }

  inline def invalidLiteralSyntax: String =
    ${ invalidLiteralSyntaxImpl }

  inline def invalidCallerArity(inline anchor: String): String =
    ${ invalidCallerArityImpl('anchor) }

  private def successfulCallerImpl(anchor: Expr[Int])(using Quotes): Expr[String] =
    import quotes.reflect.*

    val result = DefinitionQuasiquoteMacroCaller.constructOrAbort(
      parts = Seq("def answer: ", " = 1"),
      arguments = Seq(
        DefinitionArguments.definitionType(TypeNormalForm.STypeIdent("Int"))
      ),
      argumentTerms = Seq(anchor.asTerm)
    )
    val occurrence = result.sourceEvidence.interpolationOccurrences.head
    Expr(
      s"${result.constructed.render}|argument=${occurrence.argumentIndex}|category=${occurrence.category}"
    )

  private def exactPositionSummaryImpl(
      definitionTypeAnchor: Expr[Int],
      bodyTermAnchor: Expr[Int],
      bodyTypeAnchor: Expr[Int]
  )(using Quotes): Expr[String] =
    import quotes.reflect.*
    import DefinitionQuasiquotes.*

    val definitionTypeFailure =
      StringContext("def answer: ", " = 1")
        .dqr(
          DefinitionArguments.definitionType(
            TypeNormalForm.STypeIdent("AnyVal")
          )
        )
        .left
        .toOption
        .get
    val bodyTermFailure =
      StringContext("def answer: ", " = 1")
        .dqr(DefinitionArguments.bodyTerm(simpleTerm))
        .left
        .toOption
        .get
    val bodyTypeFailure =
      StringContext("def answer: Int = ", "")
        .dqr(
          DefinitionArguments.bodyType(
            TypeNormalForm.STypeIdent("Int")
          )
        )
        .left
        .toOption
        .get

    val cases = Vector(
      definitionTypeFailure -> definitionTypeAnchor.asTerm,
      bodyTermFailure -> bodyTermAnchor.asTerm,
      bodyTypeFailure -> bodyTypeAnchor.asTerm
    )
    val exact = cases.map { case (failure, term) =>
      val selected =
        DefinitionQuasiquoteMacroDiagnosticReporter.positionFor(
          failure,
          Seq(term)
        )
      samePosition(selected, term.pos)
    }
    val messagesClean = cases.forall { case (failure, _) =>
      !failure.diagnostic.message.contains("definitionArgument") &&
      !failure.diagnostic.message.contains("__qq_dt_")
    }
    val categories = cases.map { case (failure, _) =>
      failure.location.toVector.flatMap(_.origins).collect {
        case SourceOrigin.InterpolationArgument(_, _, category) => category
      }.distinct.head
    }

    Expr(
      s"exact=${exact.mkString(",")}|categories=${categories.mkString(",")}|messages-clean=$messagesClean"
    )

  private def fallbackPositionSummaryImpl(anchor: Expr[Int])(using Quotes): Expr[String] =
    import quotes.reflect.*
    import DefinitionQuasiquoteError.*

    val term = anchor.asTerm
    val fallback = Position.ofMacroExpansion
    val whole = DiagnosticLocation(
      SourceId.DefinitionConstructionTemplate,
      SourceSpan(0, 2),
      Vector(
        SourceOrigin.LiteralPart(
          SourceId.DefinitionConstructionTemplate,
          0,
          SourceSpan(0, 1)
        ),
        SourceOrigin.InterpolationArgument(
          SourceId.DefinitionConstructionTemplate,
          0,
          InterpolationCategory.DefinitionBodyTermSplice
        )
      ),
      DiagnosticPrecision.WholeSource
    )
    val failure = LocatedDiagnostic(
      CompletionFailure(
        DefinitionConstructionError.BodyConstructionFailure(
          "failure involving literal identifier definitionArgument0"
        ),
        "failure involving literal identifier definitionArgument0",
        None,
        None
      ),
      Some(whole)
    )
    val wholePosition =
      DefinitionQuasiquoteMacroDiagnosticReporter.positionFor(
        failure,
        Seq(term)
      )
    val missingPosition =
      DefinitionQuasiquoteMacroDiagnosticReporter.positionFor(
        failure.copy(location = None),
        Seq(term)
      )
    val exactLocation = whole.copy(
      origins = Vector(
        SourceOrigin.InterpolationArgument(
          SourceId.DefinitionConstructionTemplate,
          4,
          InterpolationCategory.DefinitionBodyTermSplice
        )
      ),
      precision = DiagnosticPrecision.ExactOccurrence
    )
    val outOfRangePosition =
      DefinitionQuasiquoteMacroDiagnosticReporter.positionFor(
        failure.copy(location = Some(exactLocation)),
        Seq(term)
      )
    val generatedTerm = Literal(IntConstant(0))
    val generatedPosition = MacroArgumentPositionResolver.resolve(
      0,
      Seq(generatedTerm)
    )

    Expr(
      s"whole-fallback=${samePosition(wholePosition, fallback)} " +
        s"missing-fallback=${samePosition(missingPosition, fallback)} " +
        s"out-of-range-fallback=${samePosition(outOfRangePosition, fallback)} " +
        s"generated-fallback=${samePosition(generatedPosition, fallback)} " +
        s"identity-unattributed=${failure.diagnostic.asInstanceOf[CompletionFailure].argumentIndex.isEmpty}"
    )

  private def invalidDefinitionTypeImpl(anchor: Expr[String])(using Quotes): Expr[String] =
    import quotes.reflect.*

    DefinitionQuasiquoteMacroCaller.constructOrAbort(
      parts = Seq("def answer: ", " = 1"),
      arguments = Seq(
        DefinitionArguments.definitionType(
          TypeNormalForm.STypeIdent("AnyVal")
        )
      ),
      argumentTerms = Seq(anchor.asTerm)
    )
    Expr("unreachable")

  private def invalidLiteralSyntaxImpl(using Quotes): Expr[String] =
    DefinitionQuasiquoteMacroCaller.constructOrAbort(
      parts = Seq("def answer: Int = ("),
      arguments = Seq.empty,
      argumentTerms = Seq.empty
    )
    Expr("unreachable")

  private def invalidCallerArityImpl(anchor: Expr[String])(using Quotes): Expr[String] =
    DefinitionQuasiquoteMacroCaller.constructOrAbort(
      parts = Seq("def answer: ", " = 1"),
      arguments = Seq(
        DefinitionArguments.definitionType(TypeNormalForm.STypeIdent("Int"))
      ),
      argumentTerms = Seq.empty
    )
    Expr("unreachable")

  private def simpleTerm: ConstructedTerm =
    ConstructedTerm
      .fromShape(TermShape.Literal("1"))
      .fold(error => throw new IllegalStateException(error.message), identity)

  private def samePosition(using q: Quotes)(
      left: q.reflect.Position,
      right: q.reflect.Position
  ): Boolean =
    left.start == right.start && left.end == right.end
