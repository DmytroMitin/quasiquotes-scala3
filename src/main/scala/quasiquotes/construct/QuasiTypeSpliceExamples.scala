package quasiquotes.construct

import scala.quoted.*

import quasiquotes.types.*

object QuasiTypeSpliceExamples:
  inline def spliceSummary(
      termKind: String,
      templateSource: String,
      bindingName: String,
      bindingSource: String
  ): String =
    ${ spliceSummaryImpl('termKind, 'templateSource, 'bindingName, 'bindingSource) }

  inline def spliceSummary(
      termKind: String,
      templateSource: String,
      firstBindingName: String,
      firstBindingSource: String,
      secondBindingName: String,
      secondBindingSource: String
  ): String =
    ${ spliceSummaryImpl('termKind, 'templateSource, 'firstBindingName, 'firstBindingSource, 'secondBindingName, 'secondBindingSource) }

  inline def nestedAppliedSplice(value: List[Int]): List[Int] =
    ${ nestedAppliedSpliceImpl('value) }

  inline def equivalenceSummary(value: List[Int]): String =
    ${ equivalenceSummaryImpl('value) }

  inline def placeholderSourceSummary: String =
    ${ placeholderSourceSummaryImpl }

  inline def markerInTermPositionMessage: String =
    ${ markerInTermPositionMessageImpl }

  inline def termInTypePositionMessage: String =
    ${ termInTypePositionMessageImpl }

  inline def unsupportedTypePositionMessage: String =
    ${ unsupportedTypePositionMessageImpl }

  inline def unsupportedNormalFormMessage: String =
    ${ unsupportedNormalFormMessageImpl }

  inline def unknownPlaceholderMessage: String =
    ${ unknownPlaceholderMessageImpl }

  private def spliceSummaryImpl(
      termKind: Expr[String],
      templateSource: Expr[String],
      bindingName: Expr[String],
      bindingSource: Expr[String]
  )(using Quotes): Expr[String] =
    val summary =
      for
        binding <- TypeNormalForm.fromSource(bindingSource.valueOrAbort)
        constructed <- QuasiTypequotes.tqr(templateSource.valueOrAbort, bindingName.valueOrAbort -> binding)
        term <- termFor(termKind.valueOrAbort)
        result <- buildTypeSplice(term, constructed)
        inspected <- inspectTypedResult(result)
      yield summarize(result, constructed, inspected)
    Expr(summary.fold(_.message, identity))

  private def spliceSummaryImpl(
      termKind: Expr[String],
      templateSource: Expr[String],
      firstBindingName: Expr[String],
      firstBindingSource: Expr[String],
      secondBindingName: Expr[String],
      secondBindingSource: Expr[String]
  )(using Quotes): Expr[String] =
    val summary =
      for
        firstBinding <- TypeNormalForm.fromSource(firstBindingSource.valueOrAbort)
        secondBinding <- TypeNormalForm.fromSource(secondBindingSource.valueOrAbort)
        constructed <- QuasiTypequotes.tqr(
          templateSource.valueOrAbort,
          firstBindingName.valueOrAbort -> firstBinding,
          secondBindingName.valueOrAbort -> secondBinding
        )
        term <- termFor(termKind.valueOrAbort)
        result <- buildTypeSplice(term, constructed)
        inspected <- inspectTypedResult(result)
      yield summarize(result, constructed, inspected)
    Expr(summary.fold(_.message, identity))

  private def nestedAppliedSpliceImpl(value: Expr[List[Int]])(using Quotes): Expr[List[Int]] =
    import quotes.reflect.*
    import Quasiquotes.*
    import QuasiTypeSplices.typeSplice

    val constructed = QuasiTypequotes.tqr("List[$t]", "t" -> TypeNormalForm.STypeIdent("Int")).fold(throw _, identity)
    val functionTerm = Select.unique('{ (items: List[Int]) => items }.asTerm, "apply")
    val valueTerm = value.asTerm
    qr"$functionTerm((${valueTerm}: ${typeSplice(constructed)}))".asExprOf[List[Int]]

  private def equivalenceSummaryImpl(value: Expr[List[Int]])(using Quotes): Expr[String] =
    import quotes.reflect.*

    val result =
      for
        constructed <- QuasiTypequotes.tqr("List[$t]", "t" -> TypeNormalForm.STypeIdent("Int"))
        explicit <- TypedTermConstruct.ascribe(value.asTerm, constructed)
        interpolated <- buildTypeSplice(value.asTerm, constructed)
        explicitType <- inspectTypedResult(explicit)
        interpolatedType <- inspectTypedResult(interpolated)
      yield
        val explicitStructure = explicit.show(using Printer.TreeStructure)
        val interpolatedStructure = interpolated.show(using Printer.TreeStructure)
        s"sameStructure=${explicitStructure == interpolatedStructure} sameNormalForm=${explicitType == interpolatedType} typed=${isTyped(interpolated)}"
    Expr(result.fold(_.message, identity))

  private def placeholderSourceSummaryImpl(using Quotes): Expr[String] =
    import quotes.reflect.*

    val constructed = QuasiTypequotes.tqr("List[$t]", "t" -> TypeNormalForm.STypeIdent("Int")).fold(throw _, identity)
    val holes: Seq[QuasiquoteHole[Term]] = Seq(
      QuasiquoteHole.Term('{ List(1) }.asTerm),
      QuasiquoteHole.ConstructedTypeSplice(constructed)
    )
    val source = PlaceholderSource.synthesizeCategorized(Seq("(", ": ", ")"), holes).fold(
      error => throw new IllegalArgumentException(error.message),
      identity
    )
    Expr(source.source)

  private def markerInTermPositionMessageImpl(using Quotes): Expr[String] =
    val constructed = ConstructedType(TypeNormalForm.STypeIdent("Int"))
    errorMessage(QuasiquoteBuilder.build(Seq("", ""), Seq(QuasiTypeSplices.typeSplice(constructed))))

  private def termInTypePositionMessageImpl(using Quotes): Expr[String] =
    import quotes.reflect.*
    errorMessage(QuasiquoteBuilder.build(Seq("(", ": ", ")"), Seq('{ 1 }.asTerm, '{ 2 }.asTerm)))

  private def unsupportedTypePositionMessageImpl(using Quotes): Expr[String] =
    import quotes.reflect.*
    val constructed = ConstructedType(TypeNormalForm.STypeIdent("Int"))
    errorMessage(
      QuasiquoteBuilder.build(
        Seq("identity[", "](", ")"),
        Seq(QuasiTypeSplices.typeSplice(constructed), '{ 1 }.asTerm)
      )
    )

  private def unsupportedNormalFormMessageImpl(using Quotes): Expr[String] =
    import quotes.reflect.*
    val constructed = ConstructedType(TypeNormalForm.STypeIdent("AnyVal"))
    errorMessage(
      QuasiquoteBuilder.build(
        Seq("(", ": ", ")"),
        Seq('{ 1 }.asTerm, QuasiTypeSplices.typeSplice(constructed))
      )
    )

  private def unknownPlaceholderMessageImpl(using Quotes): Expr[String] =
    errorMessage(QuasiquoteBuilder.build(Seq("__qq_type_hole_99"), Nil))

  private def buildTypeSplice(using q: Quotes)(
      term: q.reflect.Term,
      constructed: ConstructedType
  ): Either[TypeQuasiquoteError, q.reflect.Term] =
    import Quasiquotes.*
    import QuasiTypeSplices.typeSplice

    Right(qr"(${term}: ${typeSplice(constructed)})")

  private def inspectTypedResult(using q: Quotes)(term: q.reflect.Term): Either[TypeQuasiquoteError, TypeNormalForm] =
    import q.reflect.*
    term match
      case Typed(_, typeTree) => TargetTypeReprInspector.inspect(typeTree.tpe)
      case other => Left(TypeQuasiquoteError(s"Expected reflect Typed result but found: ${other.show(using Printer.TreeStructure)}"))

  private def summarize(using q: Quotes)(
      result: q.reflect.Term,
      constructed: ConstructedType,
      inspected: TypeNormalForm
  ): String =
    s"typed=${isTyped(result)} constructed=${constructed.normalForm.render} inspected=${inspected.render} matched=${constructed.normalForm == inspected}"

  private def isTyped(using q: Quotes)(term: q.reflect.Term): Boolean =
    import q.reflect.*
    term match
      case Typed(_, _) => true
      case _ => false

  private def errorMessage(using q: Quotes)(
      result: Either[QuasiquoteError, q.reflect.Term]
  ): Expr[String] =
    import q.reflect.*
    Expr(result.fold(_.message, _.show(using Printer.TreeStructure)))

  private def termFor(using Quotes)(kind: String): Either[TypeQuasiquoteError, quotes.reflect.Term] =
    import quotes.reflect.*
    kind match
      case "int" => Right('{ 1 }.asTerm)
      case "listInt" => Right('{ List(1) }.asTerm)
      case "optionString" => Right('{ Option("value") }.asTerm)
      case "tupleIntString" => Right('{ (1, "value") }.asTerm)
      case "functionIntString" => Right('{ (value: Int) => value.toString }.asTerm)
      case "tupleIntInt" => Right('{ (1, 1) }.asTerm)
      case other => Left(TypeQuasiquoteError(s"Unknown controlled type-splice example kind: $other"))
