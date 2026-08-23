package quasiquotes.construct.hybrid

import scala.quoted.{Expr, Quotes, Varargs}
import scala.util.control.NonFatal

import scala.meta.*
import scala.meta.parsers.Parsed

import _root_.quasiquotes.construct.*
import _root_.quasiquotes.parser.{BinderId, ConstructorNamePolicy, Lambda1DiagnosticMessages, TinyTermParser}
import _root_.quasiquotes.parser.P1BlockDiagnosticMessages
import _root_.quasiquotes.parser.P2LocalValDiagnosticMessages
import _root_.quasiquotes.hybrid.TermQ3DialectPolicy
import _root_.quasiquotes.types.toTypeRepr
import _root_.quasiquotes.types.{TypeNormalFormSource, TypeReprLowerer}

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
      arguments: Seq[q.reflect.Term | QuasiTypeSplice],
      dialect: Dialect = TermQ3DialectPolicy.selected
  ): Either[Failure, q.reflect.Term] =
    val holes: Seq[QuasiquoteHole[q.reflect.Term]] = arguments.map {
      case splice: QuasiTypeSplice => QuasiquoteHole.ConstructedTypeSplice(splice.constructedType)
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
            typeHoles,
            synthesized.literalCategorizedNames
          )
        yield term
      }

  def lowerTree(using q: Quotes)(
      tree: scala.meta.Term,
      termHoles: Map[String, q.reflect.Term],
      typeHoles: Map[String, q.reflect.TypeRepr],
      literalCategorizedNames: Set[String] = Set.empty
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
        typeHoles.get(name) match
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

    def selectMember(qualifier: Term, name: String): Either[Failure, Term] =
      try
        val selected = Select.unique(qualifier, name)
        selected.tpe.widen match
          case method: MethodType if method.paramNames.isEmpty => Right(selected.appliedToNone)
          case _ => Right(selected)
      catch
        case NonFatal(error) => Left(Failure.lowering(s"selection $name failed: ${error.getMessage}"))

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
        case name: scala.meta.Type.Name => Right(name.value)
        case select: scala.meta.Type.Select => Right(select.syntax)
        case _ => unsupported(tpe, "constructor type arguments are not supported")

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

    def loop(
        current: scala.meta.Term,
        boundTerms: List[(String, BinderId, Term)] = Nil,
        lambdaDepth: Int = 0,
        localValDepth: Int = 0
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
          lowerChild(select.qual).flatMap(selectMember(_, select.name.value))
        case unary: scala.meta.Term.ApplyUnary if UnaryMethodByOperator.contains(unary.op.value) =>
          lowerChild(unary.arg).flatMap(selectMember(_, UnaryMethodByOperator(unary.op.value)))
        case fresh: scala.meta.Term.New =>
          val argumentLists = fresh.init.argss
          if argumentLists.size != 1 then unsupported(fresh, "multiple constructor argument lists are not supported")
          else if argumentLists.head.exists(_.isInstanceOf[scala.meta.Term.Assign]) then
            unsupported(fresh, "named constructor arguments are not supported")
          else
            for
              name <- constructorName(fresh.init.tpe)
              _ <- ConstructorNamePolicy.validate(name).left.map(Failure.lowering)
              arguments <- sequence(argumentLists.head.map(lowerChild))
              result <- lowerConstructor(name, arguments)
            yield result
        case application: scala.meta.Term.Apply =>
          for
            function <- lowerChild(application.fun)
            arguments <- sequence(application.args.map(lowerChild))
            result <- applyFunction(function, arguments)
          yield result
        case infix: scala.meta.Term.ApplyInfix if infix.argClause.values.size == 1 =>
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

    loop(tree)

  private def lowerTypeHoles(using q: Quotes)(
      bindings: Vector[PlaceholderBinding[q.reflect.Term]]
  ): Either[Failure, Map[String, q.reflect.TypeRepr]] =
    bindings.foldLeft[Either[Failure, Map[String, q.reflect.TypeRepr]]](Right(Map.empty)) {
      case (result, PlaceholderBinding(name, QuasiquoteHole.ConstructedTypeSplice(constructed))) =>
        for
          accumulated <- result
          repr <- constructed.toTypeRepr.left.map(error => Failure.lowering(error.message))
        yield accumulated.updated(name, repr)
      case (result, _) => result
    }
