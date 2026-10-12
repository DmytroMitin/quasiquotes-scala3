package quasiquotes.construct.q066

import scala.meta.*
import scala.quoted.*
import scala.util.control.NonFatal

import _root_.quasiquotes.construct.*
import _root_.quasiquotes.construct.hybrid.{HybridTermFrontend, ScalametaTermFrontend}
import _root_.quasiquotes.hybrid.{P2LocalValScalametaAdmission, TermQ3DialectPolicy}
import _root_.quasiquotes.matching.TermMatcher
import _root_.quasiquotes.scalameta.TermFrontend
import _root_.quasiquotes.source.{InterpolationCategory, SourceOrigin, SourceSpan}
import _root_.quasiquotes.types.{TypeNormalForm, TypeNormalFormSource, TypeReprLowerer}

/** Q066-only construction stand-in. It deliberately lives in Test and exposes no
  * production or public API surface.
  */
object Q066ScalametaP3ConstructionProbe:
  final case class Failure(stage: String, detail: String):
    def message: String = s"$stage: $detail"

  final case class SyntaxEvidence(
      synthesizedSource: String,
      parserSource: String,
      rootKind: String,
      statementKinds: List[String],
      methodName: String,
      parameterName: String,
      parameterType: TypeNormalForm,
      resultType: TypeNormalForm,
      argumentSpan: SourceSpan,
      argumentOriginIndex: Int,
      argumentOriginCategory: InterpolationCategory,
      exactCompilerAccepted: Boolean,
      p2AdmissionAccepted: Boolean
  )

  final case class Built[T](term: T, originalArgument: T, syntax: SyntaxEvidence)

  def build(using q: Quotes)(
      parts: Seq[String],
      arguments: Seq[q.reflect.Term]
  ): Either[Failure, Built[q.reflect.Term]] =
    import q.reflect.*

    Either
      .cond(
        !arguments.exists(_.asInstanceOf[AnyRef] eq null),
        (),
        Failure("Q066_P3_SOURCE_ADAPTER_REJECTION", "null caller-owned Term arguments are unsupported")
      )
      .flatMap { _ =>
        val holes: Seq[QuasiquoteHole[q.reflect.Term, Nothing]] =
          arguments.map(QuasiquoteHole.Term(_))

        PlaceholderSource
          .synthesizeCategorized(parts, holes)
          .left
          .map(error => Failure("SOURCE_SYNTHESIS", error.message))
          .flatMap { synthesized =>
            val parserSource = synthesized.bindings.foldLeft(synthesized.source) { (source, binding) =>
              source.replace("$" + binding.name, "${" + binding.name + "}")
            }
            for
              tree <- ScalametaTermFrontend
                .parse(parserSource, TermQ3DialectPolicy.selected)
                .left.map(error => Failure(error.category, error.detail))
              _ <- ScalametaTermFrontend
                .validateExactCompiler(synthesized.source)
                .left.map(error => Failure(error.category, error.detail))
              built <- lowerFamily(tree, synthesized, parserSource)
            yield built
          }
      }

  private def lowerFamily(using q: Quotes)(
      tree: scala.meta.Term,
      synthesized: CategorizedPlaceholderSource[q.reflect.Term, Nothing],
      parserSource: String
  ): Either[Failure, Built[q.reflect.Term]] =
    import q.reflect.*

    def reject(detail: String): Either[Failure, Nothing] =
      Left(Failure("Q066_P3_SEMANTIC_REJECTION", detail))

    def requireCondition(condition: Boolean, detail: String): Either[Failure, Unit] =
      Either.cond(condition, (), Failure("Q066_P3_SEMANTIC_REJECTION", detail))

    def fixedType(tpe: scala.meta.Type): Either[Failure, (TypeNormalForm, TypeRepr)] =
      for
        normal <- TypeNormalFormSource
          .fromSource(tpe.syntax)
          .left.map(error => Failure("Q066_P3_TYPE_REJECTION", error.message))
        _ <- requireCondition(
          normal match
            case TypeNormalForm.STypeIdent("Int" | "String" | "Boolean") => true
            case _ => false,
          s"P3 supports only fixed Int/String/Boolean Types, obtained ${normal.render}"
        )
        repr <- TypeReprLowerer
          .lowerNormalForm(normal)
          .left.map(error => Failure("Q066_P3_TYPE_REJECTION", error.message))
      yield normal -> repr

    def isSimpleBinderName(name: String): Boolean =
      name != "_" && name.matches("[A-Za-z_$][A-Za-z0-9_$]*")

    def containsOwnedDefinition(term: Term): Boolean =
      var found = false
      val traverser = new TreeTraverser:
        override def traverseTree(tree: Tree)(owner: Symbol): Unit =
          tree match
            case _: ValDef | _: DefDef | _: ClassDef => found = true
            case _ if !found => super.traverseTree(tree)(owner)
            case _ => ()
      traverser.traverseTree(term)(Symbol.spliceOwner)
      found

    for
      pair <- tree match
        case block: scala.meta.Term.Block =>
          block.stats match
            case (definition: scala.meta.Defn.Def) :: (result: scala.meta.Term) :: Nil =>
              Right(definition -> result)
            case _ => reject("P3 requires exactly one local Defn.Def and one following Term")
        case _ => reject("P3 requires a Scalameta Term.Block root")
      (definition, result) = pair
      _ <- requireCondition(definition.mods.isEmpty, "P3 local method modifiers or annotations are unsupported")
      _ <- requireCondition(
        definition.name.syntax == definition.name.value && isSimpleBinderName(definition.name.value),
        "P3 requires a simple local method binder"
      )
      group <- definition.paramClauseGroups match
        case value :: Nil => Right(value)
        case _ => reject("P3 requires one parameter-clause group")
      _ <- requireCondition(group.tparamClause.values.isEmpty, "P3 type parameters are unsupported")
      clause <- group.paramClauses match
        case value :: Nil if value.mod.isEmpty => Right(value)
        case _ => reject("P3 requires one ordinary parameter clause")
      parameter <- clause.values match
        case value :: Nil => Right(value)
        case _ => reject("P3 requires exactly one ordinary parameter")
      _ <- requireCondition(
        parameter.mods.isEmpty && parameter.default.isEmpty,
        "P3 requires one unmodified strict parameter without a default"
      )
      _ <- requireCondition(
        parameter.name.syntax == parameter.name.value && isSimpleBinderName(parameter.name.value),
        "P3 requires a simple parameter binder"
      )
      parameterTypeTree <- parameter.decltpe.toRight(
        Failure("Q066_P3_TYPE_REJECTION", "P3 requires an explicit parameter Type")
      )
      resultTypeTree <- definition.decltpe.toRight(
        Failure("Q066_P3_TYPE_REJECTION", "P3 requires an explicit result Type")
      )
      parameterType <- fixedType(parameterTypeTree)
      resultType <- fixedType(resultTypeTree)
      _ <- requireCondition(
        parameterType._1 == resultType._1 && parameterType._2 =:= resultType._2,
        "P3 parameter and result Types must be identical"
      )
      _ <- definition.body match
        case body: scala.meta.Term.Name if body.value == parameter.name.value => Right(())
        case _ => reject("P3 method body must be exactly its own parameter reference")
      argumentTree <- result match
        case application: scala.meta.Term.Apply =>
          application.fun match
            case callee: scala.meta.Term.Name
                if callee.value == definition.name.value && application.args.size == 1 =>
              Right(application.args.head)
            case _ => reject("P3 result must call its own local method with one ordinary argument")
        case _ => reject("P3 result must be one ordinary Apply")
      binding <- argumentTree match
        case name: scala.meta.Term.Name =>
          synthesized.bindings.collectFirst {
            case candidate @ PlaceholderBinding(bindingName, QuasiquoteHole.Term(_))
                if bindingName == name.value => candidate
          }.toRight(Failure("Q066_P3_SOURCE_ADAPTER_REJECTION", "P3 call argument must be the generated Term-hole transport"))
        case _ => reject("P3 call argument must be exactly one scalar Term splice")
      _ <- requireCondition(
        synthesized.bindings.size == 1,
        "P3 requires exactly one scalar Term splice and no other interpolation arguments"
      )
      argument = binding.hole.asInstanceOf[QuasiquoteHole.Term[q.reflect.Term]].term
      _ <- requireCondition(
        !containsOwnedDefinition(argument),
        "P3 caller-owned Term splice must not contain owned definitions"
      )
      _ <- requireCondition(
        argument.tpe.widen <:< parameterType._2,
        s"P3 call argument Type ${Printer.TypeReprCode.show(argument.tpe.widen)} is not compatible with ${Printer.TypeReprCode.show(parameterType._2)}"
      )
      origin <- synthesized.originMap
        .originsFor(SourceSpan(argumentTree.pos.start, argumentTree.pos.end))
        .collectFirst {
          case mapped @ _root_.quasiquotes.source.MappedOrigin(
                _,
                SourceOrigin.InterpolationArgument(_, _, InterpolationCategory.TermSplice)
              ) => mapped
        }
        .toRight(Failure("Q066_P3_SOURCE_ADAPTER_REJECTION", "Term-hole source origin is absent"))
      lowered <-
        try
          val methodType = MethodType(List(parameter.name.value))(
            _ => List(parameterType._2),
            _ => resultType._2
          )
          val methodSymbol = Symbol.newMethod(Symbol.spliceOwner, definition.name.value, methodType)
          var callbackFailure: Option[Failure] = None
          val loweredDefinition = DefDef(methodSymbol, parameterClauses =>
            parameterClauses match
              case List(List(actualParameter)) => Some(Ref(actualParameter.symbol))
              case _ =>
                callbackFailure = Some(
                  Failure("Q066_P3_QUOTES_REJECTION", "Quotes DefDef callback did not expose one parameter")
                )
                Some(Literal(UnitConstant()))
          )
          callbackFailure match
            case Some(failure) => Left(failure)
            case None =>
              val call = Ref(methodSymbol).appliedToArgs(List(argument))
              Right(Block(List(loweredDefinition), call))
        catch
          case NonFatal(error) =>
            Left(
              Failure(
                "Q066_P3_QUOTES_REJECTION",
                Option(error.getMessage).getOrElse(error.getClass.getSimpleName)
              )
            )
      syntax =
        val SourceOrigin.InterpolationArgument(_, originIndex, category) = origin.origin: @unchecked
        SyntaxEvidence(
          synthesized.source,
          parserSource,
          tree.productPrefix,
          tree.asInstanceOf[scala.meta.Term.Block].stats.map(_.productPrefix),
          definition.name.value,
          parameter.name.value,
          parameterType._1,
          resultType._1,
          SourceSpan(argumentTree.pos.start, argumentTree.pos.end),
          originIndex,
          category,
          exactCompilerAccepted = true,
          p2AdmissionAccepted = P2LocalValScalametaAdmission.validate(tree).isRight
        )
    yield Built(lowered, argument, syntax)

object Q066ScalametaP3ConstructionMacros:
  final case class OwnerEvidence(
      value: Int,
      methodOwnedBySplice: Boolean,
      distinctSymbols: Boolean,
      parameterOwnedByMethod: Boolean,
      bodyUsesParameter: Boolean,
      resultUsesMethod: Boolean,
      exactArgumentIdentity: Boolean,
      standardExactArgumentIdentity: Boolean,
      sameStructuralRolesAsStandard: Boolean
  )

  final case class RoundTripEvidence(
      hybridMatched: Boolean,
      standardMatched: Boolean,
      hybridOutcome: String,
      standardOutcome: String,
      adapterExactArgumentIdentity: Boolean,
      standardExactArgumentIdentity: Boolean,
      sameImmediateRoundTripOutcome: Boolean
  )

  inline def intValue(inline value: Int): Int = ${ intValueImpl('value) }
  inline def stringValue(inline value: String): String = ${ stringValueImpl('value) }
  inline def booleanValue(inline value: Boolean): Boolean = ${ booleanValueImpl('value) }
  inline def alphaRenamedValue(inline value: Int): Int = ${ alphaRenamedValueImpl('value) }
  inline def ownerEvidence(inline value: Int): OwnerEvidence = ${ ownerEvidenceImpl('value) }
  inline def literalEvidence: OwnerEvidence = ${ literalEvidenceImpl }
  inline def hybridRoundTripEvidence(inline value: Int): RoundTripEvidence = ${ hybridRoundTripEvidenceImpl('value) }
  inline def literalRoundTripEvidence: RoundTripEvidence = ${ literalRoundTripEvidenceImpl }
  inline def stageEvidence: List[String] = ${ stageEvidenceImpl }
  inline def negativeEvidence: List[String] = ${ negativeEvidenceImpl }

  private def orAbort[T](result: Either[Q066ScalametaP3ConstructionProbe.Failure, T])(using Quotes): T =
    result.fold(error => quotes.reflect.report.errorAndAbort(error.message), identity)

  private def build(using q: Quotes)(
      methodName: String,
      parameterName: String,
      typeName: String,
      argument: q.reflect.Term
  ): q.reflect.Term =
    orAbort(
      Q066ScalametaP3ConstructionProbe.build(
        Seq(s"{ def $methodName($parameterName: $typeName): $typeName = $parameterName; $methodName(", ") }"),
        Seq(argument)
      )
    ).term

  private def intValueImpl(value: Expr[Int])(using q: Quotes): Expr[Int] =
    import q.reflect.*
    build("id", "value", "Int", value.asTerm).asExprOf[Int]

  private def stringValueImpl(value: Expr[String])(using q: Quotes): Expr[String] =
    import q.reflect.*
    build("identityString", "text", "String", value.asTerm).asExprOf[String]

  private def booleanValueImpl(value: Expr[Boolean])(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    build("identityBoolean", "flag", "Boolean", value.asTerm).asExprOf[Boolean]

  private def alphaRenamedValueImpl(value: Expr[Int])(using q: Quotes): Expr[Int] =
    import q.reflect.*
    build("_renamed42", "_argument7", "Int", value.asTerm).asExprOf[Int]

  private def literalEvidenceImpl(using q: Quotes): Expr[OwnerEvidence] =
    import q.reflect.*
    ownerEvidenceFor(Expr(41).asTerm)

  private def ownerEvidenceImpl(value: Expr[Int])(using q: Quotes): Expr[OwnerEvidence] =
    import q.reflect.*
    ownerEvidenceFor(value.asTerm)

  private def hybridRoundTripEvidenceImpl(value: Expr[Int])(using q: Quotes): Expr[RoundTripEvidence] =
    import q.reflect.*
    roundTripEvidenceFor(value.asTerm)

  private def literalRoundTripEvidenceImpl(using q: Quotes): Expr[RoundTripEvidence] =
    import q.reflect.*
    roundTripEvidenceFor(Expr(47).asTerm)

  private def roundTripEvidenceFor(using q: Quotes)(argument: q.reflect.Term): Expr[RoundTripEvidence] =
    import q.reflect.*
    val candidate = build("id", "value", "Int", argument)
    val parts = Seq("{ def id(value: Int): Int = value; id(", ") }")
    val standard = QuasiquoteBuilder.build(parts, Seq(argument)).fold(
      error => report.errorAndAbort(error.message),
      identity
    )
    val candidateArgument = candidate match
      case Block(List(_: DefDef), Apply(_, List(value))) => value
      case other => report.errorAndAbort(s"unexpected constructed P3 tree: ${Printer.TreeStructure.show(other)}")
    val standardArgument = standard match
      case Block(List(_: DefDef), Apply(_, List(value))) => value
      case other => report.errorAndAbort(s"unexpected standard P3 tree: ${Printer.TreeStructure.show(other)}")
    val compiled = TermFrontend
      .compile("{ def id(value: Int): Int = value; id($argument) }")
      .fold(error => report.errorAndAbort(error.toString), identity)
    def matchOutcome(target: Term): Either[String, Unit] =
      TermMatcher.matchTerm(compiled.pattern, target).left.map(_.toString).map(_ => ())
    val hybridMatch = matchOutcome(candidate)
    val standardMatch = matchOutcome(standard)
    val hybridOutcome = hybridMatch.fold(identity, _ => "MATCHED")
    val standardOutcome = standardMatch.fold(identity, _ => "MATCHED")
    '{
      RoundTripEvidence(
        ${ Expr(hybridMatch.isRight) },
        ${ Expr(standardMatch.isRight) },
        ${ Expr(hybridOutcome) },
        ${ Expr(standardOutcome) },
        ${ Expr(candidateArgument.asInstanceOf[AnyRef].eq(argument.asInstanceOf[AnyRef])) },
        ${ Expr(standardArgument.asInstanceOf[AnyRef].eq(argument.asInstanceOf[AnyRef])) },
        ${ Expr(hybridOutcome == standardOutcome) }
      )
    }

  private def ownerEvidenceFor(using q: Quotes)(argument: q.reflect.Term): Expr[OwnerEvidence] =
    import q.reflect.*
    val parts = Seq("{ def id(value: Int): Int = value; id(", ") }")
    val candidate = orAbort(Q066ScalametaP3ConstructionProbe.build(parts, Seq(argument))).term
    val standard = QuasiquoteBuilder.build(parts, Seq(argument)).fold(
      error => report.errorAndAbort(error.message),
      identity
    )

    def inspect(term: Term): (DefDef, ValDef, Term, Term) =
      term match
        case Block(List(definition: DefDef), Apply(function, List(loweredArgument))) =>
          val parameter = definition.termParamss.flatMap(_.params).head
          (definition, parameter, function, loweredArgument)
        case other => report.errorAndAbort(s"unexpected P3 tree: ${Printer.TreeStructure.show(other)}")

    val (definition, parameter, function, loweredArgument) = inspect(candidate)
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
        ${ Expr(definition.symbol.owner == Symbol.spliceOwner) },
        ${ Expr(definition.symbol != parameter.symbol) },
        ${ Expr(parameter.symbol.owner == definition.symbol) },
        ${ Expr(definition.rhs.exists(_.symbol == parameter.symbol)) },
        ${ Expr(function.symbol == definition.symbol) },
        ${ Expr(loweredArgument.asInstanceOf[AnyRef].eq(argument.asInstanceOf[AnyRef])) },
        ${ Expr(standardArgument.asInstanceOf[AnyRef].eq(argument.asInstanceOf[AnyRef])) },
        ${ Expr(sameRoles) }
      )
    }

  private def stageEvidenceImpl(using q: Quotes): Expr[List[String]] =
    import q.reflect.*
    val argument = Expr(7).asTerm
    val parts = Seq("{ def id(value: Int): Int = value; id(", ") }")
    val built = orAbort(Q066ScalametaP3ConstructionProbe.build(parts, Seq(argument)))
    val production = ScalametaTermFrontend.lower(parts, Seq(argument))
    val hybrid = HybridTermFrontend.build(parts, Seq(argument))
    val values = List(
      s"source=${built.syntax.synthesizedSource}",
      s"parser=${built.syntax.parserSource}",
      s"root=${built.syntax.rootKind}",
      s"stats=${built.syntax.statementKinds.mkString(",")}",
      s"names=${built.syntax.methodName},${built.syntax.parameterName}",
      s"types=${built.syntax.parameterType.render},${built.syntax.resultType.render}",
      s"span=${built.syntax.argumentSpan.start}..${built.syntax.argumentSpan.end}",
      s"origin=${built.syntax.argumentOriginIndex}:${built.syntax.argumentOriginCategory}",
      s"exact=${built.syntax.exactCompilerAccepted}",
      s"p2=${built.syntax.p2AdmissionAccepted}",
      s"production=${production.fold(_.category, _ => "ADMIT")}",
      s"hybrid=${hybrid.fold(_.category, result => s"ADMIT:${result.engine}")}",
      s"identity=${built.originalArgument.asInstanceOf[AnyRef].eq(argument.asInstanceOf[AnyRef])}"
    )
    Expr.ofList(values.map(Expr(_)))

  private def negativeEvidenceImpl(using q: Quotes): Expr[List[String]] =
    import q.reflect.*
    val intArgument = Expr(1).asTerm
    val stringArgument = Expr("x").asTerm
    val ownedArgument = '{
      def nested(value: Int): Int = value
      nested(1)
    }.asTerm

    def split(source: String): Seq[String] = source.split("\\$argument", -1).toSeq
    def outcome(label: String, source: String, arguments: Seq[Term] = Seq(intArgument)): String =
      Q066ScalametaP3ConstructionProbe
        .build(split(source), arguments)
        .fold(failure => s"$label:${failure.stage}", _ => s"$label:ADMIT")

    val cases = List(
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
      outcome("annotation", "{ @deprecated(\"q066\", \"\") def id(value: Int): Int = value; id($argument) }"),
      outcome("parameter-modifier", "{ def id(private val value: Int): Int = value; id($argument) }"),
      outcome("parameter-annotation", "{ def id(@deprecated(\"q066\", \"\") value: Int): Int = value; id($argument) }"),
      outcome("multiple-defs", "{ def first(value: Int): Int = value; def second(value: Int): Int = value; first($argument) }"),
      outcome("mixed-val-def", "{ val seed: Int = 1; def id(value: Int): Int = value; id($argument) }"),
      outcome("nested-def", "{ def id(value: Int): Int = { def nested(x: Int): Int = x; nested(value) }; id($argument) }"),
      outcome("unicode-method", "{ def λ(value: Int): Int = value; λ($argument) }"),
      outcome("unicode-parameter", "{ def id(λ: Int): Int = λ; id($argument) }"),
      outcome("quoted-method", "{ def `type`(value: Int): Int = value; `type`($argument) }"),
      outcome("symbolic-method", "{ def +(value: Int): Int = value; +($argument) }"),
      outcome("anonymous-method", "{ def _(value: Int): Int = value; _($argument) }"),
      outcome("misplaced-body-splice", "{ def id(value: Int): Int = $argument; id(1) }"),
      outcome("misplaced-callee-splice", "{ def id(value: Int): Int = value; $argument(1) }"),
      outcome("misplaced-type-splice", "{ def id(value: $argument): Int = value; id(1) }"),
      outcome("wrong-argument-type", "{ def id(value: Int): Int = value; id($argument) }", Seq(stringArgument)),
      outcome("owned-definition-splice", "{ def id(value: Int): Int = value; id($argument) }", Seq(ownedArgument)),
      outcome(
        "null-argument",
        "{ def id(value: Int): Int = value; id($argument) }",
        Seq(null.asInstanceOf[Term])
      ),
      outcome("no-hole", "{ def id(value: Int): Int = value; id(1) }", Nil),
      outcome(
        "two-holes",
        "{ def id(value: Int): Int = value; id(($argument, $argument)._1) }",
        Seq(intArgument, intArgument)
      )
    )
    Expr.ofList(cases.map(Expr(_)))
