package quasiquotes.construct.hybrid

import scala.quoted.{Expr, Quotes, Varargs}
import scala.util.control.NonFatal

import scala.meta.*
import scala.meta.parsers.Parsed
import dotty.tools.dotc.core.Types

import _root_.quasiquotes.construct.*
import _root_.quasiquotes.parser.{BinderId, ConstructorNamePolicy, Lambda1DiagnosticMessages, TinyTermParser}
import _root_.quasiquotes.parser.P1BlockDiagnosticMessages
import _root_.quasiquotes.parser.P2LocalValDiagnosticMessages
import _root_.quasiquotes.hybrid.TermQ3DialectPolicy
import _root_.quasiquotes.hybrid.P2LocalValScalametaAdmission
import _root_.quasiquotes.types.toTypeRepr
import _root_.quasiquotes.types.{TypeNormalForm, TypeNormalFormSource, TypeReprLowerer}
import _root_.quasiquotes.source.{InterpolationCategory, SourceOrigin, SourceSpan}

/** Public-Scalameta syntax parsing followed by project-owned Quotes lowering.
  * This lives only in the unpublished side-by-side module.
  */
private[quasiquotes] object ScalametaTermFrontend:
  private val UnaryMethodByOperator = Map(
    "+" -> "unary_+",
    "-" -> "unary_-",
    "!" -> "unary_!",
    "~" -> "unary_~"
  )

  final case class Failure(
      category: String,
      start: Int,
      end: Int,
      detail: String
  ) derives CanEqual:
    def message: String = s"$category[$start..$end]: $detail"

  object Failure:
    def parse(start: Int, end: Int, detail: String): Failure =
      Failure("SCALAMETA_PARSE_FAILURE", start, end, detail)

    def exactCompiler(detail: String): Failure =
      Failure("EXACT_COMPILER_SYNTAX_REJECTED", 0, 0, detail)

    def unsupported(tree: scala.meta.Tree, detail: String): Failure =
      Failure("SCALAMETA_LOWERING_UNSUPPORTED", tree.pos.start, tree.pos.end, s"${tree.productPrefix}: $detail")

    def lowering(detail: String): Failure =
      Failure("SCALAMETA_TYPED_LOWERING_FAILURE", 0, 0, detail)

    def template(detail: String): Failure =
      Failure("TERM_TEMPLATE_FAILURE", 0, 0, detail)

    def p3Source(tree: scala.meta.Tree, detail: String): Failure =
      Failure("SCALAMETA_P3_SOURCE_REJECTION", tree.pos.start, tree.pos.end, detail)

    def p3Semantic(tree: scala.meta.Tree, detail: String): Failure =
      Failure("SCALAMETA_P3_SEMANTIC_REJECTION", tree.pos.start, tree.pos.end, detail)

    def p3Type(tree: scala.meta.Tree, detail: String): Failure =
      Failure("SCALAMETA_P3_TYPE_REJECTION", tree.pos.start, tree.pos.end, detail)

    def p3Quotes(tree: scala.meta.Tree, detail: String): Failure =
      Failure("SCALAMETA_P3_QUOTES_REJECTION", tree.pos.start, tree.pos.end, detail)

    def selectedMember(tree: scala.meta.Tree, category: String, detail: String): Failure =
      Failure(category, tree.pos.start, tree.pos.end, detail)

  def parse(
      source: String,
      dialect: Dialect = TermQ3DialectPolicy.selected
  ): Either[Failure, scala.meta.Term] =
    dialect(source).parse[scala.meta.Term] match
      case Parsed.Success(tree) => Right(tree)
      case error: Parsed.Error =>
        Left(Failure.parse(error.pos.start, error.pos.end, error.message))

  def validateExactCompiler(source: String): Either[Failure, Unit] =
    TinyTermParser.parse(source) match
      case Right(_) => Right(())
      case Left(error) => Left(Failure.exactCompiler(s"${error.kind}: ${error.summary}"))

  def lower(using q: Quotes)(
      parts: Seq[String],
      arguments: Seq[q.reflect.Term | q.reflect.TypeRepr | QuasiTypeSplice | SelectedMemberName],
      dialect: Dialect = TermQ3DialectPolicy.selected
  ): Either[Failure, q.reflect.Term] =
    val holes: Seq[QuasiquoteHole[q.reflect.Term, q.reflect.TypeRepr]] = arguments.map {
      case splice: QuasiTypeSplice => QuasiquoteHole.ConstructedTypeSplice(splice.constructedType)
      case name: SelectedMemberName => QuasiquoteHole.SelectedMemberNameSplice(name)
      case reflectedType: Types.Type =>
        QuasiquoteHole.ReflectedTypeSplice(
          reflectedType.asInstanceOf[q.reflect.TypeRepr]
        )
      case term => QuasiquoteHole.Term(term.asInstanceOf[q.reflect.Term])
    }

    PlaceholderSource.synthesizeCategorized(parts, holes)
      .left.map(error => Failure.template(error.message))
      .flatMap { synthesized =>
        val parserSource = synthesized.bindings.foldLeft(synthesized.source) { (source, binding) =>
          source.replace("$" + binding.name, "${" + binding.name + "}")
        }
        for
          tree <- parse(parserSource, dialect)
          _ <- validateExactCompiler(synthesized.source)
          typeHoles <- lowerTypeHoles(synthesized.bindings)
          term <- lowerTree(
            tree,
            synthesized.bindings.collect {
              case PlaceholderBinding(name, QuasiquoteHole.Term(term)) => name -> term
            }.toMap,
            synthesized.bindings.collect {
              case PlaceholderBinding(name, QuasiquoteHole.SelectedMemberNameSplice(selected)) =>
                name -> selected
            }.toMap,
            synthesized.bindings.collect {
              case PlaceholderBinding(name, QuasiquoteHole.ReflectedTypeSplice(reflectedType)) =>
                name -> reflectedType
            }.toMap,
            typeHoles,
            synthesized.literalCategorizedNames,
            Some(synthesized)
          )
        yield term
      }

  def lowerTree(using q: Quotes)(
      tree: scala.meta.Term,
      termHoles: Map[String, q.reflect.Term],
      selectedMemberNameHoles: Map[String, SelectedMemberName],
      reflectedTypeHoles: Map[String, q.reflect.TypeRepr],
      typeHoles: Map[String, q.reflect.TypeRepr],
      literalCategorizedNames: Set[String] = Set.empty,
      categorizedSource: Option[CategorizedPlaceholderSource[q.reflect.Term, q.reflect.TypeRepr]] = None
  ): Either[Failure, q.reflect.Term] =
    import q.reflect.*

    def unsupported(tree: scala.meta.Tree, detail: String): Either[Failure, Nothing] =
      Left(Failure.unsupported(tree, detail))

    def sequence[A](values: List[Either[Failure, A]]): Either[Failure, List[A]] =
      values.foldRight(Right(Nil): Either[Failure, List[A]]) { (next, accumulated) =>
        for
          head <- next
          tail <- accumulated
        yield head :: tail
      }

    def typeName(tpe: scala.meta.Type): Either[Failure, String] =
      tpe match
        case name: scala.meta.Type.Name => Right(name.value)
        case select: scala.meta.Type.Select => Right(select.syntax)
        case _ => unsupported(tpe, "only named ascription types are admitted")

    def lowerType(tpe: scala.meta.Type): Either[Failure, TypeTree] =
      typeName(tpe).flatMap { name =>
        if reflectedTypeHoles.contains(name) then
          Left(
            Failure.lowering(
              s"Reflected-Type splice `$name` is supported only as the complete type of a constructor expression."
            )
          )
        else if selectedMemberNameHoles.contains(name) then
          Left(
            Failure.selectedMember(
              tpe,
              "UNSUPPORTED_SELECTED_MEMBER_NAME_POSITION",
              "Selected-member name hole is supported only in an explicit selection name field."
            )
          )
        else typeHoles.get(name) match
          case Some(repr) => Right(Inferred(repr))
          case None =>
            name match
              case "Int" | "scala.Int" => Right(TypeTree.of[Int])
              case "String" | "scala.String" | "java.lang.String" => Right(TypeTree.of[String])
              case "Boolean" | "scala.Boolean" => Right(TypeTree.of[Boolean])
              case generated if PlaceholderSource.isCategorizedName(generated) && !literalCategorizedNames(generated) =>
                Left(Failure.lowering(s"unknown or misplaced type placeholder: $generated"))
              case _ => Left(Failure.lowering(s"unresolved type name: $name"))
      }

    def lowerP2DeclaredType(tpe: scala.meta.Type): Either[Failure, TypeTree] =
      lowerType(tpe).orElse(
        TypeNormalFormSource
          .fromSource(tpe.syntax)
          .left.map(error => Failure.lowering(error.message))
          .flatMap(TypeReprLowerer.lowerNormalForm(_).left.map(error => Failure.lowering(error.message)))
          .map(Inferred.apply)
      )

    def applyFunction(function: Term, arguments: List[Term]): Either[Failure, Term] =
      try Right(function.appliedToArgs(arguments))
      catch
        case NonFatal(first) =>
          try Right(Select.unique(function, "apply").appliedToArgs(arguments))
          catch
            case NonFatal(second) =>
              Left(Failure.lowering(s"application failed: ${first.getMessage}; ${second.getMessage}"))

    def selectMember(
        qualifier: Term,
        name: String,
        normalizeSelection: Boolean = true
    ): Either[Failure, Term] =
      try
        val selected = Select.unique(qualifier, name)
        if normalizeSelection then
          selected.tpe.widen match
            case method: MethodType if method.paramNames.isEmpty => Right(selected.appliedToNone)
            case _ => Right(selected)
        else Right(selected)
      catch
        case NonFatal(error) => Left(Failure.lowering(s"selection $name failed: ${error.getMessage}"))

    def selectDynamicMember(
        tree: scala.meta.Term.Select,
        qualifier: Term,
        name: String,
        normalizeSelection: Boolean = true
    ): Either[Failure, Term] =
      val selected =
        try Right(Select.unique(qualifier, name))
        catch
          case NonFatal(error) if isNonUniqueSelectionFailure(error) =>
            Left(
              Failure.selectedMember(
                tree,
                "SELECTED_MEMBER_NOT_UNIQUE",
                s"Selected member '$name' is not unique on the explicit receiver."
              )
            )
          case NonFatal(_) =>
            Left(
              Failure.selectedMember(
                tree,
                "SELECTED_MEMBER_MISSING_OR_INACCESSIBLE",
                s"Selected member '$name' is missing or inaccessible on the explicit receiver."
              )
            )

      selected.flatMap { term =>
        if term.symbol == Symbol.noSymbol ||
            term.symbol.flags.is(Flags.Private) ||
            term.symbol.flags.is(Flags.Protected)
        then
          Left(
            Failure.selectedMember(
              tree,
              "SELECTED_MEMBER_MISSING_OR_INACCESSIBLE",
              s"Selected member '$name' is missing or inaccessible on the explicit receiver."
            )
          )
        else try
          if normalizeSelection then
            term.tpe.widen match
              case method: MethodType if method.paramNames.isEmpty => Right(term.appliedToNone)
              case _ => Right(term)
          else Right(term)
        catch
          case NonFatal(_) =>
            Left(
              Failure.selectedMember(
                tree,
                "SELECTED_MEMBER_LOWERING_FAILURE",
                s"Selected member '$name' could not be lowered on the explicit receiver."
              )
            )
      }

    def isNonUniqueSelectionFailure(error: Throwable): Boolean =
      val detail = Option(error.getMessage).getOrElse("").toLowerCase(java.util.Locale.ROOT)
      detail.contains("overload") ||
        detail.contains("more than one") ||
        detail.contains("not unique") ||
        detail.contains("multiple alternative")

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

    def constructorName(tpe: scala.meta.Type): Either[Failure, String] =
      tpe match
        case name: scala.meta.Type.Name if selectedMemberNameHoles.contains(name.value) =>
          Left(
            Failure.selectedMember(
              name,
              "UNSUPPORTED_SELECTED_MEMBER_NAME_POSITION",
              "Selected-member name hole is supported only in an explicit selection name field."
            )
          )
        case name: scala.meta.Type.Name => Right(name.value)
        case select: scala.meta.Type.Select => Right(select.syntax)
        case _ => unsupported(tpe, "constructor type arguments are not supported")

    def reflectedConstructorType(
        tpe: scala.meta.Type
    ): Either[Failure, Option[TypeRepr]] =
      tpe match
        case name: scala.meta.Type.Name =>
          reflectedTypeHoles.get(name.value) match
            case some @ Some(_) => Right(some)
            case None if termHoles.contains(name.value) =>
              Left(
                Failure.lowering(
                  s"Term splice `${name.value}` is not valid as the complete type of a constructor expression."
                )
              )
            case None if typeHoles.contains(name.value) =>
              Left(
                Failure.lowering(
                  s"Constructed-type splice `${name.value}` is not supported as the complete type of a constructor expression; only the complete type of an expression ascription is supported."
                )
              )
            case None => Right(None)
        case other =>
          val nestedReflected = constructorTypeNames(other).find(reflectedTypeHoles.contains)
          nestedReflected match
            case Some(name) =>
              Left(
                Failure.lowering(
                  s"Reflected-Type splice `$name` is not supported inside partial or applied constructor type syntax; only the complete type of a constructor expression is supported."
                )
              )
            case None => Right(None)

    def constructorTypeNames(tpe: scala.meta.Type): List[String] =
      tpe match
        case name: scala.meta.Type.Name => name.value :: Nil
        case select: scala.meta.Type.Select =>
          constructorQualifierNames(select.qual) ++ (select.name.value :: Nil)
        case applied: scala.meta.Type.Apply =>
          constructorTypeNames(applied.tpe) ++ applied.args.flatMap(constructorTypeNames)
        case _ => Nil

    def constructorQualifierNames(term: scala.meta.Term): List[String] =
      term match
        case name: scala.meta.Term.Name => name.value :: Nil
        case select: scala.meta.Term.Select =>
          constructorQualifierNames(select.qual) ++ (select.name.value :: Nil)
        case _ => Nil

    def lowerConstructor(name: String, arguments: List[Term]): Either[Failure, Term] =
      val classSymbol =
        try
          Class.forName(name, false, ScalametaTermFrontend.getClass.getClassLoader)
          Right(Symbol.requiredClass(name))
        catch case NonFatal(error) => Left(Failure.lowering(s"unresolved constructor $name: ${error.getMessage}"))
      classSymbol.flatMap { symbol =>
        try Right(Select.overloaded(New(TypeTree.ref(symbol)), "<init>", Nil, arguments))
        catch case NonFatal(error) => Left(Failure.lowering(s"constructor $name failed: ${error.getMessage}"))
      }

    def lowerReflectedConstructor(
        constructorType: TypeRepr,
        arguments: List[Term]
    ): Either[Failure, Term] =
      val label =
        try
          val fullName = constructorType.typeSymbol.fullName
          if fullName.nonEmpty then fullName
          else Printer.TypeReprCode.show(constructorType)
        catch case NonFatal(_) => "<caller-owned TypeRepr>"

      try Right(Select.overloaded(New(Inferred(constructorType)), "<init>", Nil, arguments))
      catch
        case NonFatal(error) =>
          Left(
            Failure.lowering(
              s"constructor $label failed: ${Option(error.getMessage).getOrElse(error.getClass.getSimpleName)}"
            )
          )

    def loop(
        current: scala.meta.Term,
        boundTerms: List[(String, BinderId, Term)] = Nil,
        lambdaDepth: Int = 0,
        localValDepth: Int = 0,
        applicationFunction: Boolean = false
    ): Either[Failure, Term] =
      def lowerChild(child: scala.meta.Term): Either[Failure, Term] =
        loop(child, boundTerms, lambdaDepth, localValDepth)

      current match
        case name: scala.meta.Term.Name =>
          termHoles.get(name.value) match
            case Some(term) if localValDepth > 0 && containsOwnedDefinition(term) =>
              Left(Failure.lowering(P2LocalValDiagnosticMessages.OwnedDefinitionSplice))
            case Some(term) if lambdaDepth > 0 && containsOwnedDefinition(term) =>
              Left(Failure.lowering(Lambda1DiagnosticMessages.OwnedDefinitionSplice))
            case Some(term) => Right(term)
            case None if reflectedTypeHoles.contains(name.value) =>
              Left(
                Failure.lowering(
                  s"Reflected-Type splice `${name.value}` is not valid in term position."
                )
              )
            case None if selectedMemberNameHoles.contains(name.value) =>
              Left(
                Failure.selectedMember(
                  name,
                  "UNSUPPORTED_SELECTED_MEMBER_NAME_POSITION",
                  "Selected-member name hole is supported only in an explicit selection name field."
                )
              )
            case None if typeHoles.contains(name.value) =>
              Left(Failure.lowering(s"type placeholder used in term position: ${name.value}"))
            case None if PlaceholderSource.isCategorizedName(name.value) && !literalCategorizedNames(name.value) =>
              Left(Failure.lowering(s"unknown term placeholder: ${name.value}"))
            case None =>
              boundTerms.collectFirst { case (boundName, _, term) if boundName == name.value => term } match
                case Some(term) => Right(term)
                case None => IdentifierResolver.resolve(name.value).left.map(error => Failure.lowering(error.message))
        case function: scala.meta.Term.Function =>
          if lambdaDepth > 0 then unsupported(function, Lambda1DiagnosticMessages.NestedLambda)
          else
            function.paramClause.values match
              case parameter :: Nil if parameter.decltpe.nonEmpty && parameter.mods.isEmpty =>
                val parameterName = parameter.name.value
                val binderId = BinderId(boundTerms.size)
                lowerType(parameter.decltpe.get).flatMap { parameterTypeTree =>
                  val parameterType = parameterTypeTree.tpe
                  val previewSymbol = Symbol.newVal(
                    Symbol.spliceOwner,
                    parameterName,
                    parameterType,
                    Flags.EmptyFlags,
                    Symbol.noSymbol
                  )
                  val previewParameter = Ref(previewSymbol)
                  loop(
                    function.body,
                    (parameterName, binderId, previewParameter) :: boundTerms,
                    lambdaDepth + 1,
                    localValDepth
                  ).flatMap { previewBody =>
                    val methodType = MethodType(List(parameterName))(
                      _ => List(parameterType),
                      _ => previewBody.tpe.widen
                    )
                    var bodyFailure: Option[Failure] = None
                    val lambda = Lambda(
                      owner = Symbol.spliceOwner,
                      tpe = methodType,
                      rhsFn = (_, parameters) =>
                        val parameterTerm = parameters.head.asInstanceOf[Term]
                        loop(
                          function.body,
                          (parameterName, binderId, parameterTerm) :: boundTerms,
                          lambdaDepth + 1,
                          localValDepth
                        ) match
                          case Right(lowered) => lowered
                          case Left(failure) =>
                            bodyFailure = Some(failure)
                            Literal(UnitConstant())
                    )
                    bodyFailure.toLeft(lambda)
                  }
                }
              case _ :: Nil => unsupported(function, Lambda1DiagnosticMessages.ExplicitParameterType)
              case _ => unsupported(function, Lambda1DiagnosticMessages.ExactlyOneParameter)
        case Lit.Int(value) => Right(Literal(IntConstant(value)))
        case Lit.String(value) => Right(Literal(StringConstant(value)))
        case Lit.Boolean(value) => Right(Literal(BooleanConstant(value)))
        case select: scala.meta.Term.Select =>
          lowerChild(select.qual).flatMap { qualifier =>
            selectedMemberNameHoles.get(select.name.value) match
              case Some(selectedName) =>
                selectDynamicMember(
                  select,
                  qualifier,
                  selectedName.decoded,
                  normalizeSelection = !applicationFunction
                )
              case None =>
                selectMember(
                  qualifier,
                  select.name.value,
                  normalizeSelection = !applicationFunction
                )
          }
        case unary: scala.meta.Term.ApplyUnary if UnaryMethodByOperator.contains(unary.op.value) =>
          lowerChild(unary.arg).flatMap(selectMember(_, UnaryMethodByOperator(unary.op.value)))
        case fresh: scala.meta.Term.New =>
          val argumentLists = fresh.init.argss
          if argumentLists.size != 1 then unsupported(fresh, "multiple constructor argument lists are not supported")
          else if argumentLists.head.exists(_.isInstanceOf[scala.meta.Term.Assign]) then
            unsupported(fresh, "named constructor arguments are not supported")
          else
            for
              reflectedType <- reflectedConstructorType(fresh.init.tpe)
              arguments <- sequence(argumentLists.head.map(lowerChild))
              result <- reflectedType match
                case Some(constructorType) =>
                  lowerReflectedConstructor(constructorType, arguments)
                case None =>
                  for
                    name <- constructorName(fresh.init.tpe)
                    _ <- ConstructorNamePolicy.validate(name).left.map(Failure.lowering)
                    lowered <- lowerConstructor(name, arguments)
                  yield lowered
            yield result
        case application: scala.meta.Term.Apply =>
          for
            function <- loop(
              application.fun,
              boundTerms,
              lambdaDepth,
              localValDepth,
              applicationFunction = true
            )
            arguments <- sequence(application.args.map(lowerChild))
            result <- applyFunction(function, arguments)
          yield result
        case infix: scala.meta.Term.ApplyInfix if infix.argClause.values.size == 1 =>
          if selectedMemberNameHoles.contains(infix.op.value) then
            Left(
              Failure.selectedMember(
                infix.op,
                "UNSUPPORTED_SELECTED_MEMBER_NAME_POSITION",
                "Selected-member name hole is supported only in an explicit selection name field."
              )
            )
          else
            for
              left <- lowerChild(infix.lhs)
              right <- lowerChild(infix.argClause.values.head)
              result <-
                try Right(Select.overloaded(left, infix.op.value, Nil, right :: Nil))
                catch case NonFatal(error) => Left(Failure.lowering(s"infix ${infix.op.value} failed: ${error.getMessage}"))
            yield result
        case tuple: scala.meta.Term.Tuple =>
          sequence(tuple.args.map(lowerChild)).flatMap { elements =>
            if elements.size < 2 || elements.size > 22 then
              unsupported(tuple, s"unsupported tuple arity ${elements.size}")
            else
              try Right(Expr.ofTupleFromSeq(elements.map(_.asExpr)).asTerm)
              catch case NonFatal(error) => Left(Failure.lowering(s"tuple lowering failed: ${error.getMessage}"))
          }
        case conditional: scala.meta.Term.If =>
          for
            condition <- lowerChild(conditional.cond)
            thenBranch <- lowerChild(conditional.thenp)
            elseBranch <- lowerChild(conditional.elsep)
          yield If(condition, thenBranch, elseBranch)
        case block: scala.meta.Term.Block =>
          block.stats match
            case (result: scala.meta.Term) :: Nil => lowerChild(result)
            case (definition: scala.meta.Defn.Val) :: (result: scala.meta.Term) :: Nil =>
              lowerLocalValBlock(definition, result, boundTerms, lambdaDepth, localValDepth)
            case (definition: scala.meta.Defn.Def) :: (result: scala.meta.Term) :: Nil =>
              if block.asInstanceOf[AnyRef].eq(tree.asInstanceOf[AnyRef]) then
                lowerLocalDefBlock(definition, result, block)
              else
                Left(
                  Failure.p3Semantic(
                    block,
                    "P3 local identity construction is admitted only as the complete source root"
                  )
                )
            case stats if stats.size >= 2 && stats.forall(_.isInstanceOf[scala.meta.Term]) =>
              val terms = stats.map(_.asInstanceOf[scala.meta.Term])
              for
                prefix <- sequence(terms.init.map(lowerChild))
                result <- lowerChild(terms.last)
              yield Block(prefix, result)
            case stats =>
              stats.collectFirst {
                case _: scala.meta.Defn.Val => P2LocalValDiagnosticMessages.ExactlyOne
                case _: scala.meta.Defn.Var => P2LocalValDiagnosticMessages.Mutable
                case _: scala.meta.Defn.Def => P2LocalValDiagnosticMessages.LocalDef
                case stat if !stat.isInstanceOf[scala.meta.Term] =>
                  P1BlockDiagnosticMessages.UnsupportedStatement(stat.productPrefix)
              } match
                case Some(detail) => unsupported(block, detail)
                case None => unsupported(block, "P1 block requires at least one expression statement and a final result")
        case ascription: scala.meta.Term.Ascribe =>
          for
            expression <- lowerChild(ascription.expr)
            tpe <- lowerType(ascription.tpe)
          yield Typed(expression, tpe)
        case interpolation: scala.meta.Term.Interpolate if interpolation.prefix.value == "s" =>
          def interpolationArgument(argument: scala.meta.Term): Either[Failure, Term] =
            argument match
              case block: scala.meta.Term.Block =>
                block.stats match
                  case (term: scala.meta.Term) :: Nil => lowerChild(term)
                  case _ => unsupported(block, "interpolation argument blocks must contain one admitted term")
              case term => lowerChild(term)
          for
            arguments <- sequence(interpolation.args.map(interpolationArgument))
            parts <- sequence(interpolation.parts.map {
              case Lit.String(value) => Right(value)
              case other => unsupported(other, "non-string interpolation segment")
            })
          yield '{ StringContext(${Varargs(parts.map(Expr(_)))}*).s(${Varargs(arguments.map(_.asExpr))}*) }.asTerm
        case other => unsupported(other, "outside the bounded side-by-side term tranche")

    def lowerLocalValBlock(
        definition: scala.meta.Defn.Val,
        result: scala.meta.Term,
        boundTerms: List[(String, BinderId, Term)],
        lambdaDepth: Int,
        localValDepth: Int
    ): Either[Failure, Term] =
      definition.pats match
        case scala.meta.Pat.Var(name) :: Nil if definition.mods.exists(_.isInstanceOf[scala.meta.Mod.Lazy]) =>
          unsupported(definition, P2LocalValDiagnosticMessages.Lazy)
        case scala.meta.Pat.Var(name) :: Nil if definition.mods.nonEmpty =>
          unsupported(definition, P2LocalValDiagnosticMessages.Pattern)
        case scala.meta.Pat.Var(name) :: Nil =>
          definition.decltpe match
            case None => unsupported(definition, P2LocalValDiagnosticMessages.MissingExplicitType)
            case Some(declaredType) =>
              for
                typeTree <- lowerP2DeclaredType(declaredType)
                initializer <- loop(definition.rhs, boundTerms, lambdaDepth, localValDepth)
                binderId = BinderId(boundTerms.size)
                symbol = Symbol.newVal(
                  Symbol.spliceOwner,
                  name.value,
                  typeTree.tpe,
                  Flags.EmptyFlags,
                  Symbol.noSymbol
                )
                value = ValDef(symbol, Some(initializer))
                loweredResult <- loop(
                  result,
                  (name.value, binderId, Ref(symbol)) :: boundTerms,
                  lambdaDepth,
                  localValDepth + 1
                )
              yield Block(List(value), loweredResult)
        case _ => unsupported(definition, P2LocalValDiagnosticMessages.Pattern)

    def lowerLocalDefBlock(
        definition: scala.meta.Defn.Def,
        result: scala.meta.Term,
        block: scala.meta.Term.Block
    ): Either[Failure, Term] =
      def reject(tree: scala.meta.Tree, detail: String): Either[Failure, Nothing] =
        Left(Failure.p3Semantic(tree, detail))

      def requireCondition(
          condition: Boolean,
          tree: scala.meta.Tree,
          detail: String
      ): Either[Failure, Unit] =
        Either.cond(condition, (), Failure.p3Semantic(tree, detail))

      def fixedType(tpe: scala.meta.Type): Either[Failure, (TypeNormalForm, TypeRepr)] =
        for
          normal <- TypeNormalFormSource
            .fromSource(tpe.syntax)
            .left.map(error => Failure.p3Type(tpe, error.message))
          _ <- Either.cond(
            normal match
              case TypeNormalForm.STypeIdent("Int" | "String" | "Boolean") => true
              case _ => false,
            (),
            Failure.p3Type(
              tpe,
              s"P3 supports only fixed Int/String/Boolean Types, obtained ${normal.render}"
            )
          )
          repr <- TypeReprLowerer
            .lowerNormalForm(normal)
            .left.map(error => Failure.p3Type(tpe, error.message))
        yield normal -> repr

      def isSimpleBinderName(name: String): Boolean =
        name != "_" && name.matches("[A-Za-z_$][A-Za-z0-9_$]*")

      for
        source <- categorizedSource.toRight(
          Failure.p3Source(block, "P3 lowering requires categorized interpolation source metadata")
        )
        _ <- requireCondition(
          definition.mods.isEmpty,
          definition,
          "P3 local method modifiers or annotations are unsupported"
        )
        _ <- requireCondition(
          definition.name.syntax == definition.name.value && isSimpleBinderName(definition.name.value),
          definition.name,
          "P3 requires a simple local method binder"
        )
        group <- definition.paramClauseGroups match
          case value :: Nil => Right(value)
          case _ => reject(definition, "P3 requires one parameter-clause group")
        _ <- requireCondition(
          group.tparamClause.values.isEmpty,
          definition,
          "P3 type parameters are unsupported"
        )
        clause <- group.paramClauses match
          case value :: Nil if value.mod.isEmpty => Right(value)
          case _ => reject(definition, "P3 requires one ordinary parameter clause")
        parameter <- clause.values match
          case value :: Nil => Right(value)
          case _ => reject(definition, "P3 requires exactly one ordinary parameter")
        _ <- requireCondition(
          parameter.mods.isEmpty && parameter.default.isEmpty,
          parameter,
          "P3 requires one unmodified strict parameter without a default"
        )
        _ <- requireCondition(
          parameter.name.syntax == parameter.name.value && isSimpleBinderName(parameter.name.value),
          parameter.name,
          "P3 requires a simple parameter binder"
        )
        parameterTypeTree <- parameter.decltpe.toRight(
          Failure.p3Type(parameter, "P3 requires an explicit parameter Type")
        )
        resultTypeTree <- definition.decltpe.toRight(
          Failure.p3Type(definition, "P3 requires an explicit result Type")
        )
        parameterType <- fixedType(parameterTypeTree)
        resultType <- fixedType(resultTypeTree)
        _ <- requireCondition(
          parameterType._1 == resultType._1 && parameterType._2 =:= resultType._2,
          definition,
          "P3 parameter and result Types must be identical"
        )
        _ <- definition.body match
          case body: scala.meta.Term.Name if body.value == parameter.name.value => Right(())
          case _ => reject(definition.body, "P3 method body must be exactly its own parameter reference")
        argumentTree <- result match
          case application: scala.meta.Term.Apply =>
            application.fun match
              case callee: scala.meta.Term.Name
                  if callee.value == definition.name.value && application.args.size == 1 =>
                Right(application.args.head)
              case _ => reject(application, "P3 result must call its own local method with one ordinary argument")
          case _ => reject(result, "P3 result must be one ordinary Apply")
        binding <- argumentTree match
          case name: scala.meta.Term.Name =>
            source.bindings.collectFirst {
              case candidate @ PlaceholderBinding(bindingName, QuasiquoteHole.Term(_))
                  if bindingName == name.value => candidate
            }.toRight(
              Failure.p3Source(
                argumentTree,
                "P3 call argument must be the authoritative generated Term-hole transport"
              )
            )
          case _ => reject(argumentTree, "P3 call argument must be exactly one scalar Term splice")
        _ <- requireCondition(
          source.bindings.size == 1,
          block,
          "P3 requires exactly one scalar Term splice and no other interpolation arguments"
        )
        span = SourceSpan(argumentTree.pos.start, argumentTree.pos.end)
        _ <- source.originMap
          .originsFor(span)
          .collectFirst {
            case mapped @ _root_.quasiquotes.source.MappedOrigin(
                  generatedSpan,
                  SourceOrigin.InterpolationArgument(_, _, InterpolationCategory.TermSplice)
                ) if generatedSpan == span => mapped
          }
          .toRight(Failure.p3Source(argumentTree, "P3 Term-hole source origin is absent or inexact"))
        _ <- Either.cond(
          source.source.slice(span.start, span.end) == binding.name,
          (),
          Failure.p3Source(
            argumentTree,
            "P3 Term-hole lexical source does not match its categorized binding"
          )
        )
        argument = binding.hole.asInstanceOf[QuasiquoteHole.Term[q.reflect.Term]].term
        _ <- Either.cond(
          !(argument.asInstanceOf[AnyRef] eq null),
          (),
          Failure.p3Source(argumentTree, "null caller-owned Term arguments are unsupported")
        )
        _ <- requireCondition(
          termHoles.get(binding.name).exists(_.asInstanceOf[AnyRef].eq(argument.asInstanceOf[AnyRef])),
          argumentTree,
          "P3 Term-hole binding does not preserve the original caller Term identity"
        )
        _ <- requireCondition(
          !containsOwnedDefinition(argument),
          argumentTree,
          "P3 caller-owned Term splice must not contain owned definitions"
        )
        _ <- requireCondition(
          argument.tpe.widen <:< parameterType._2,
          argumentTree,
          s"P3 call argument Type ${Printer.TypeReprCode.show(argument.tpe.widen)} is not compatible with ${Printer.TypeReprCode.show(parameterType._2)}"
        )
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
                    Failure.p3Quotes(definition, "Quotes DefDef callback did not expose one parameter")
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
                Failure.p3Quotes(
                  definition,
                  Option(error.getMessage).getOrElse(error.getClass.getSimpleName)
                )
              )
      yield lowered

    P2LocalValScalametaAdmission
      .validate(tree)
      .left
      .map(violation => Failure.unsupported(tree, violation.message))
      .flatMap(_ => loop(tree))

  private def lowerTypeHoles(using q: Quotes)(
      bindings: Vector[PlaceholderBinding[q.reflect.Term, q.reflect.TypeRepr]]
  ): Either[Failure, Map[String, q.reflect.TypeRepr]] =
    bindings.foldLeft[Either[Failure, Map[String, q.reflect.TypeRepr]]](Right(Map.empty)) {
      case (result, PlaceholderBinding(name, QuasiquoteHole.ConstructedTypeSplice(constructed))) =>
        for
          accumulated <- result
          repr <- constructed.toTypeRepr.left.map(error => Failure.lowering(error.message))
        yield accumulated.updated(name, repr)
      case (result, _) => result
    }
