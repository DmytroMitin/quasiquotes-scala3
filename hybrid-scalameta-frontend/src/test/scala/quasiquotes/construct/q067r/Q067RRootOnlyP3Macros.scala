package quasiquotes.construct.q067r

import scala.quoted.*

import _root_.quasiquotes.construct.{PlaceholderSource, QuasiquoteBuilder, QuasiquoteHole}
import _root_.quasiquotes.construct.hybrid.{HybridTermFrontend, ScalametaTermFrontend}
import _root_.quasiquotes.source.{InterpolationCategory, SourceOrigin, SourceSpan}

object Q067RRootOnlyP3Macros:
  final case class AdmissionRow(
      label: String,
      parsed: Boolean,
      exactCompilerAccepted: Boolean,
      bindingCount: Int,
      exactTermOrigin: Boolean,
      exactArgumentIdentity: Boolean,
      standardOutcome: String,
      hybridOutcome: String
  )

  inline def admissionMatrix(inline argument: Int): List[AdmissionRow] =
    ${ admissionMatrixImpl('argument) }

  inline def rootRuntime(inline argument: Int): Int = ${ rootRuntimeImpl('argument) }

  private def rootRuntimeImpl(argument: Expr[Int])(using q: Quotes): Expr[Int] =
    import q.reflect.*
    import _root_.quasiquotes.scalameta.ScalametaQuasiquotes.qr
    qr"""{ def id(value: Int): Int = value; id(${argument.asTerm}) }""".asExprOf[Int]

  private def admissionMatrixImpl(argument: Expr[Int])(using q: Quotes): Expr[List[AdmissionRow]] =
    import q.reflect.*

    val sources = List(
      "root" -> "{ def id(value: Int): Int = value; id($argument) }",
      "if-branch" -> "if true then { def id(value: Int): Int = value; id($argument) } else 0",
      "apply-argument" -> "((outer: Int) => outer)({ def id(value: Int): Int = value; id($argument) })",
      "tuple-element" -> "({ def id(value: Int): Int = value; id($argument) }, 0)",
      "p2-initializer" -> "{ val outer: Int = { def id(value: Int): Int = value; id($argument) }; outer }",
      "lambda-body" -> "(outer: Int) => { def id(value: Int): Int = value; id($argument) }"
    )

    def row(label: String, sourceText: String): AdmissionRow =
      val parts = sourceText.split("\\$argument", -1).toSeq
      val arguments = Seq(argument.asTerm)
      val holes: Seq[QuasiquoteHole[Term, TypeRepr]] = Seq(QuasiquoteHole.Term(argument.asTerm))
      val synthesized = PlaceholderSource
        .synthesizeCategorized(parts, holes)
        .fold(error => report.errorAndAbort(error.message), identity)
      val parsed = ScalametaTermFrontend.parse(synthesized.source).isRight
      val exactCompilerAccepted = ScalametaTermFrontend.validateExactCompiler(synthesized.source).isRight
      val binding = synthesized.bindings.head
      val start = synthesized.source.indexOf(binding.name)
      val span = SourceSpan(start, start + binding.name.length)
      val exactTermOrigin = synthesized.originMap.originsFor(span).exists {
        case _root_.quasiquotes.source.MappedOrigin(
              generatedSpan,
              SourceOrigin.InterpolationArgument(_, 0, InterpolationCategory.TermSplice)
            ) => generatedSpan == span
        case _ => false
      }
      val exactArgumentIdentity = binding.hole match
        case QuasiquoteHole.Term(term) => term.asInstanceOf[AnyRef].eq(argument.asTerm.asInstanceOf[AnyRef])
        case _ => false
      val standardOutcome = QuasiquoteBuilder
        .build(parts, arguments)
        .fold(error => s"REJECT:${error.message}", _ => "ADMIT")
      val hybridOutcome = HybridTermFrontend
        .build(parts, arguments)
        .fold(
          failure => s"${failure.category}:${failure.start}:${failure.end}:${failure.detail}",
          result => s"ADMIT:${result.engine}"
        )
      AdmissionRow(
        label,
        parsed,
        exactCompilerAccepted,
        synthesized.bindings.size,
        exactTermOrigin,
        exactArgumentIdentity,
        standardOutcome,
        hybridOutcome
      )

    Expr.ofList(sources.map { case (label, source) =>
      val value = row(label, source)
      '{
        AdmissionRow(
          ${ Expr(value.label) },
          ${ Expr(value.parsed) },
          ${ Expr(value.exactCompilerAccepted) },
          ${ Expr(value.bindingCount) },
          ${ Expr(value.exactTermOrigin) },
          ${ Expr(value.exactArgumentIdentity) },
          ${ Expr(value.standardOutcome) },
          ${ Expr(value.hybridOutcome) }
        )
      }
    })
