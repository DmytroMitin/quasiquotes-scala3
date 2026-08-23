package quasiquotes.matching

import scala.quoted.Quotes
import scala.util.matching.Regex

import quasiquotes.parser.{BinderId, P2LocalValDiagnosticMessages, TermShape, TinyTermParser}
import quasiquotes.source.ReflectedPositionProvenance
import quasiquotes.terms.TermShapeTraversal
import quasiquotes.types.TargetTypeReprInspector

sealed trait TargetBlockStatementView[+T] derives CanEqual

sealed trait TargetTermView[+T] extends TargetBlockStatementView[T] derives CanEqual:
  def original: T
  final def render: String = TargetTermView.render(this)

object TargetBlockStatementView:
  private[quasiquotes] final case class LocalVal[T](
      binderId: BinderId,
      displayName: String,
      declaredType: String,
      binderSymbol: Any,
      initializer: TargetTermView[T],
      original: Any
  ) extends TargetBlockStatementView[T]

object TargetTermView:
  private val UnaryOperatorByMethod = Map(
    "unary_+" -> "+",
    "unary_-" -> "-",
    "unary_!" -> "!",
    "unary_~" -> "~"
  )

  final case class Identifier[T](name: String, original: T) extends TargetTermView[T]
  private[quasiquotes] final case class BoundReference[T](
      binderId: BinderId,
      displayName: String,
      original: T
  ) extends TargetTermView[T]
  private[quasiquotes] final case class Lambda1[T](
      binderId: BinderId,
      displayName: String,
      parameterType: String,
      binderSymbol: Any,
      body: TargetTermView[T],
      original: T
  ) extends TargetTermView[T]
  final case class Literal[T](value: String, original: T) extends TargetTermView[T]
  final case class Select[T](qualifier: TargetTermView[T], name: String, original: T) extends TargetTermView[T]
  final case class Apply[T](function: TargetTermView[T], arguments: List[TargetTermView[T]], original: T) extends TargetTermView[T]
  final case class New[T](constructor: String, arguments: List[TargetTermView[T]], original: T) extends TargetTermView[T]
  final case class Infix[T](left: TargetTermView[T], operator: String, right: TargetTermView[T], original: T) extends TargetTermView[T]
  final case class Unary[T](operator: String, operand: TargetTermView[T], original: T) extends TargetTermView[T]
  final case class InterpolatedString[T](
      prefix: String,
      parts: List[String],
      arguments: List[TargetTermView[T]],
      original: T
  ) extends TargetTermView[T]
  final case class Typed[T](expression: TargetTermView[T], typeName: String, original: T) extends TargetTermView[T]
  final case class Tuple[T](elements: List[TargetTermView[T]], original: T) extends TargetTermView[T]
  final case class If[T](condition: TargetTermView[T], thenBranch: TargetTermView[T], elseBranch: TargetTermView[T], original: T) extends TargetTermView[T]
  final case class Block[T](statements: List[TargetBlockStatementView[T]], result: TargetTermView[T], original: T) extends TargetTermView[T]

  def fromTerm(using q: Quotes)(term: q.reflect.Term): Either[MatchFailure, TargetTermView[q.reflect.Term]] =
    fromTermInScope(term, Nil)

  private[matching] def fromTermInScope(using q: Quotes)(
      term: q.reflect.Term,
      ambientScope: List[(BinderId, q.reflect.Symbol)]
  ): Either[MatchFailure, TargetTermView[q.reflect.Term]] =
    import q.reflect.*
    var nextBinderId = ambientScope.size

    // This extraction step removes compiler-introduced wrappers but does not perform
    // the higher-level normalization experiment introduced in Task 3.5.
    def extract(
        term: Term,
        scope: List[(BinderId, Symbol)]
    ): Either[MatchFailure, TargetTermView[Term]] =
      val current = unwrapWrappersUnlessBound(term, scope)
      sourceInterpolation(term) match
        case Some((prefix, parts, sourceArgumentCount)) =>
          typedInterpolationArguments(current, sourceArgumentCount) match
            case Some(arguments) =>
              sequence(arguments.map(extract(_, scope))).map(
                TargetTermView.InterpolatedString(prefix, parts, _, current)
              )
            case None =>
              Left(
                MatchFailure.UnsupportedTargetShape(
                  s"source-proven interpolation typed shape: ${current.show(using Printer.TreeStructure)}"
                )
              )
        case None => extractOrdinary(current, scope)

    def extractOrdinary(
        term: Term,
        scope: List[(BinderId, Symbol)]
    ): Either[MatchFailure, TargetTermView[Term]] =
      term match
        case block: q.reflect.Block if Lambda.unapply(block).nonEmpty =>
          if scope.nonEmpty then
            Left(MatchFailure.UnsupportedTargetShape("nested lambdas are outside the bounded Lambda1 tranche"))
          else
            Lambda.unapply(block) match
              case Some((parameter :: Nil, body)) =>
                val binderId = BinderId(nextBinderId)
                nextBinderId += 1
                extract(body, (binderId -> parameter.symbol) :: scope).map(
                  Lambda1(
                    binderId,
                    parameter.name,
                    renderType(parameter.tpt),
                    parameter.symbol,
                    _,
                    block
                  )
                )
              case Some((parameters, _)) =>
                Left(MatchFailure.UnsupportedTargetShape(s"Lambda1 requires exactly one parameter, found ${parameters.size}"))
              case None =>
                Left(MatchFailure.UnsupportedTargetShape(block.show(using Printer.TreeStructure)))
        case Ident(name) =>
          val current = unwrapWrappersUnlessBound(term, scope)
          scope.collectFirst { case (binderId, symbol) if current.symbol == symbol => binderId } match
            case Some(binderId) => Right(TargetTermView.BoundReference(binderId, name, current))
            case None => Right(TargetTermView.Identifier(name, current))
        case q.reflect.Literal(IntConstant(value)) =>
          val current = unwrapWrappers(term)
          Right(TargetTermView.Literal(value.toString, current))
        case q.reflect.Literal(StringConstant(value)) =>
          val current = unwrapWrappers(term)
          Right(TargetTermView.Literal("\"" + value + "\"", current))
        case q.reflect.Literal(BooleanConstant(value)) =>
          val current = unwrapWrappers(term)
          Right(TargetTermView.Literal(value.toString, current))
        case q.reflect.Select(operand, name) if UnaryOperatorByMethod.contains(name) =>
          val current = unwrapWrappers(term)
          extract(operand, scope).map(TargetTermView.Unary(UnaryOperatorByMethod(name), _, current))
        case q.reflect.Select(qualifier, name) =>
          val current = unwrapWrappers(term)
          extract(qualifier, scope).map(TargetTermView.Select(_, name, current))
        case q.reflect.Apply(q.reflect.Select(q.reflect.New(typeTree), "<init>"), arguments) =>
          val current = unwrapWrappers(term)
          val constructor = typeTree.tpe.typeSymbol.fullName
          sequence(arguments.map(extract(_, scope))).map(TargetTermView.New(constructor, _, current))
        case q.reflect.Apply(
              TypeApply(q.reflect.Select(q.reflect.New(typeTree), "<init>"), _),
              arguments
            ) =>
          val current = unwrapWrappers(term)
          val constructor = typeTree.tpe.typeSymbol.fullName
          sequence(arguments.map(extract(_, scope))).map(TargetTermView.New(constructor, _, current))
        case q.reflect.Apply(function, arguments) if tupleArity(function).contains(arguments.length) =>
          val current = unwrapWrappers(term)
          sequence(arguments.map(extract(_, scope))).map(TargetTermView.Tuple(_, current))
        case q.reflect.Apply(function, arguments) =>
          val current = unwrapWrappers(term)
          for
            extractedFunction <- extract(function, scope)
            extractedArguments <- sequence(arguments.map(extract(_, scope)))
          yield TargetTermView.Apply(extractedFunction, extractedArguments, current)
        case q.reflect.Typed(expression, typeTree) =>
          val current = unwrapWrappers(term)
          extract(expression, scope).map(TargetTermView.Typed(_, renderType(typeTree), current))
        case q.reflect.If(condition, thenBranch, elseBranch) =>
          val current = unwrapWrappers(term)
          for
            extractedCondition <- extract(condition, scope)
            extractedThenBranch <- extract(thenBranch, scope)
            extractedElseBranch <- extract(elseBranch, scope)
          yield TargetTermView.If(extractedCondition, extractedThenBranch, extractedElseBranch, current)
        case block @ q.reflect.Block(statements, result) =>
          statements match
            case (definition @ ValDef(_, _, _)) :: Nil =>
              extractLocalValBlock(definition, result, block, scope)
            case definitions if definitions.exists {
                  case ValDef(_, _, _) => true
                  case _ => false
                } =>
              Left(MatchFailure.UnsupportedTargetShape(P2LocalValDiagnosticMessages.ExactlyOne))
            case definitions if definitions.exists {
                  case DefDef(_, _, _, _) => true
                  case _ => false
                } =>
              Left(MatchFailure.UnsupportedTargetShape(P2LocalValDiagnosticMessages.LocalDef))
            case expressionStatements =>
              val terms = expressionStatements.collect { case term: Term => term }
              if terms.size != expressionStatements.size then
                Left(MatchFailure.UnsupportedTargetShape("block target contains an unsupported statement"))
              else
                for
                  extractedPrefix <- sequence(terms.map(extract(_, scope)))
                  extractedResult <- extract(result, scope)
                yield TargetTermView.Block(extractedPrefix, extractedResult, block)
        case other =>
          Left(MatchFailure.UnsupportedTargetShape(other.show(using Printer.TreeStructure)))

    def extractLocalValBlock(
        definition: ValDef,
        result: Term,
        block: Term,
        scope: List[(BinderId, Symbol)]
    ): Either[MatchFailure, TargetTermView[Term]] =
      if definition.symbol.flags.is(Flags.Mutable) then
        Left(MatchFailure.UnsupportedTargetShape(P2LocalValDiagnosticMessages.Mutable))
      else if definition.symbol.flags.is(Flags.Lazy) then
        Left(MatchFailure.UnsupportedTargetShape(P2LocalValDiagnosticMessages.Lazy))
      else if !isSimpleBinderName(definition.name) then
        Left(MatchFailure.UnsupportedTargetShape(P2LocalValDiagnosticMessages.Pattern))
      else if sourceBackedInferredType(definition) then
        Left(MatchFailure.UnsupportedTargetShape(P2LocalValDiagnosticMessages.MissingExplicitType))
      else
        TargetTypeReprInspector.inspect(definition.tpt.tpe) match
          case Left(_) =>
            Left(MatchFailure.UnsupportedTargetShape(P2LocalValDiagnosticMessages.UnsupportedType))
          case Right(normalForm) =>
            definition.rhs match
              case Some(initializer) =>
                val binderId = BinderId(nextBinderId)
                for
                  extractedInitializer <- extract(initializer, scope)
                  extractedResult <- extract(result, (binderId -> definition.symbol) :: scope)
                yield
                  nextBinderId += 1
                  TargetTermView.Block(
                    List(
                      TargetBlockStatementView.LocalVal(
                        binderId,
                        definition.name,
                        TermShapeTraversal.renderNormalForm(normalForm),
                        definition.symbol,
                        extractedInitializer,
                        definition
                      )
                    ),
                    extractedResult,
                    block
                  )
              case None =>
                Left(MatchFailure.UnsupportedTargetShape(P2LocalValDiagnosticMessages.UnsupportedInitializer))

    extract(term, ambientScope)

  def render(view: TargetTermView[?]): String =
    view match
      case Identifier(name, _) => s"Ident($name)"
      case BoundReference(_, displayName, _) => s"BoundRef($displayName)"
      case Lambda1(_, displayName, parameterType, _, body, _) =>
        s"Lambda1($displayName: $parameterType, ${render(body)})"
      case Literal(value, _) => s"Literal($value)"
      case Select(qualifier, name, _) => s"Select(${render(qualifier)}, $name)"
      case Apply(function, arguments, _) =>
        s"Apply(${render(function)}, [${arguments.map(render).mkString(", ")}])"
      case New(constructor, arguments, _) =>
        s"New($constructor, [${arguments.map(render).mkString(", ")}])"
      case Infix(left, operator, right, _) =>
        s"Infix(${render(left)}, $operator, ${render(right)})"
      case Unary(operator, operand, _) =>
        s"Unary($operator, ${render(operand)})"
      case InterpolatedString(prefix, parts, arguments, _) =>
        s"InterpolatedString($prefix, [${parts.map(quote).mkString(", ")}], [${arguments.map(render).mkString(", ")}])"
      case Typed(expression, typeName, _) =>
        s"Typed(${render(expression)}, Type($typeName))"
      case Tuple(elements, _) =>
        s"Tuple([${elements.map(render).mkString(", ")}])"
      case If(condition, thenBranch, elseBranch, _) =>
        s"If(${render(condition)}, ${render(thenBranch)}, ${render(elseBranch)})"
      case Block(statements, result, _) =>
        s"Block([${statements.map(renderStatement).mkString(", ")}], ${render(result)})"

  private def renderStatement(statement: TargetBlockStatementView[?]): String =
    statement match
      case TargetBlockStatementView.LocalVal(_, displayName, declaredType, _, initializer, _) =>
        s"LocalVal($displayName: $declaredType = ${render(initializer)})"
      case term: TargetTermView[?] => render(term)

  private def sourceInterpolation(using q: Quotes)(
      term: q.reflect.Term
  ): Option[(String, List[String], Int)] =
    import q.reflect.*

    def unwrap(shape: TermShape): TermShape =
      shape match
        case TermShape.Parenthesized(inner) => unwrap(inner)
        case other => other

    ReflectedPositionProvenance
      .sourceCode(term.pos)
      .flatMap(source => TinyTermParser.parse(source).toOption)
      .map(parsed => unwrap(parsed.shape))
      .collect {
        case TermShape.InterpolatedString("s", parts, arguments) =>
          ("s", parts, arguments.size)
      }

  private def typedInterpolationArguments(using q: Quotes)(
      term: q.reflect.Term,
      expectedCount: Int
  ): Option[List[q.reflect.Term]] =
    import q.reflect.*

    def loop(current: Term): Option[List[Term]] =
      def unpack(arguments: List[Term]): List[Term] =
        arguments match
          case Repeated(values, _) :: Nil => values
          case q.reflect.Typed(Repeated(values, _), _) :: Nil => values
          case other => other

      unwrapWrappers(current) match
        case q.reflect.Apply(q.reflect.Select(_, "s"), arguments)
            if unpack(arguments).size == expectedCount =>
          Some(unpack(arguments))
        case q.reflect.Apply(TypeApply(q.reflect.Select(_, "s"), _), arguments)
            if unpack(arguments).size == expectedCount =>
          Some(unpack(arguments))
        case _ if expectedCount == 0 => Some(Nil)
        case _ => None

    loop(term)

  private def quote(value: String): String =
    "\"" + value.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

  private def unwrapWrappers(using q: Quotes)(term: q.reflect.Term): q.reflect.Term =
    import q.reflect.*
    term match
      case Inlined(_, _, inner) => unwrapWrappers(inner)
      case q.reflect.Block(Nil, inner: Term) => unwrapWrappers(inner)
      case ident: Ident if ident.symbol.exists && ident.symbol.pos.nonEmpty =>
        ident.symbol.tree match
          case ValDef(_, _, Some(rhs)) => unwrapWrappers(rhs)
          case _ => term
      case _ => term

  private def unwrapWrappersUnlessBound(using q: Quotes)(
      term: q.reflect.Term,
      scope: List[(BinderId, q.reflect.Symbol)]
  ): q.reflect.Term =
    import q.reflect.*
    term match
      case q.reflect.Typed(inner, _)
          if ReflectedPositionProvenance.sourceCode(term.pos).forall(!_.contains(":")) =>
        unwrapWrappersUnlessBound(inner, scope)
      case _ if term.symbol.exists && scope.exists(_._2 == term.symbol) => term
      case _ => unwrapWrappers(term)

  private def sourceBackedInferredType(using q: Quotes)(definition: q.reflect.ValDef): Boolean =
    val definitionSource = ReflectedPositionProvenance.sourceCode(definition.pos)
    val typeSource = ReflectedPositionProvenance.sourceCode(definition.tpt.pos)
    definitionSource.nonEmpty && typeSource.forall(_.trim.isEmpty)

  private def isSimpleBinderName(name: String): Boolean =
    name != "_" && name.matches("[A-Za-z_$][A-Za-z0-9_$]*")

  private def renderType(using q: Quotes)(typeTree: q.reflect.TypeTree): String =
    import q.reflect.*
    normalizeTypeName(typeTree.tpe.show)

  private def normalizeTypeName(typeName: String): String =
    typeName match
      case "scala.Int" => "Int"
      case "scala.Predef.String" | "java.lang.String" | "scala.String" => "String"
      case "scala.Boolean" => "Boolean"
      case other => other

  private val TupleSymbol: Regex = """.*Tuple([2-9]|1[0-9]|2[0-2])(\.apply|\.<init>)?$""".r

  private def tupleArity(using q: Quotes)(term: q.reflect.Term): Option[Int] =
    import q.reflect.*

    def fromSymbol(term: Term): Option[Int] =
      if term.symbol.exists then
        term.symbol.fullName match
          case TupleSymbol(arity, _) => Some(arity.toInt)
          case _ => None
      else None

    fromSymbol(term).orElse {
      term match
        case q.reflect.TypeApply(function, _) => tupleArity(function)
        case q.reflect.Select(qualifier, _) => tupleArity(qualifier)
        case _ => None
    }

  private def sequence[A](values: List[Either[MatchFailure, A]]): Either[MatchFailure, List[A]] =
    values.foldRight(Right(Nil): Either[MatchFailure, List[A]]) { (next, acc) =>
      for
        head <- next
        tail <- acc
      yield head :: tail
    }
