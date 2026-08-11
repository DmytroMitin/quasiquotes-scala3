package quasiquotes.terms.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Symbols.NoSymbol
import dotty.tools.dotc.util.SourceFile
import dotty.tools.dotc.util.Spans.Span

import quasiquotes.parser.TermShape
import quasiquotes.terms.ConstructedTerm
import quasiquotes.types.{AppliedTypeConstructorPolicy, TypeNormalForm}

private[quasiquotes] object GeneratedOriginFragmentSupport:
  import ConstructedTermGeneratedOriginError.*

  private val DecimalInteger = "-?[0-9]+".r
  private val OperatorName = "[!#%&*+\\-/:<=>?@\\\\^|~]+".r
  private val SupportedUnaryOperators = Set("+", "-", "!", "~")

  private[dotty] enum NodeKind:
    case TermIdent
    case Literal
    case InterpolatedString
    case InterpolationSegment
    case BracedInterpolationArgument
    case Select
    case Apply
    case Infix
    case Prefix
    case Typed
    case TermTuple
    case If
    case Parens
    case OperatorIdent
    case TypeIdent
    case AppliedType
    case TypeTuple
    case FunctionType

  private[dotty] final case class NodePlan(
      kind: NodeKind,
      start: Int,
      end: Int,
      point: Int,
      children: Vector[NodePlan]
  ):
    def span: Span = Span(start, end, point)
    def shifted(offset: Int): NodePlan =
      copy(
        start = start + offset,
        end = end + offset,
        point = point + offset,
        children = children.map(_.shifted(offset))
      )

  private final case class RenderedPlan(
      source: String,
      root: NodePlan,
      consumedSidecars: Int
  )

  private[quasiquotes] final class TermFragment private[dotty] (
      val source: String,
      private[dotty] val root: NodePlan
  )

  private[quasiquotes] final class TypeFragment private[dotty] (
      val source: String,
      private[dotty] val root: NodePlan
  )

  def planTerm(
      constructed: ConstructedTerm
  ): Either[ConstructedTermGeneratedOriginError, TermFragment] =
    for
      rendered <- Planner(
        constructed.ascriptionTypes,
        compactDefinitionBodyRoot = false
      ).renderTerm(constructed.root)
      _ <- validatePlan(rendered.root, rendered.source.length)
    yield new TermFragment(rendered.source, rendered.root)

  def planDefinitionBody(
      constructed: ConstructedTerm
  ): Either[ConstructedTermGeneratedOriginError, TermFragment] =
    for
      rendered <- Planner(
        constructed.ascriptionTypes,
        compactDefinitionBodyRoot = true
      ).renderTerm(constructed.root)
      _ <- validatePlan(rendered.root, rendered.source.length)
    yield new TermFragment(rendered.source, rendered.root)

  def planType(
      normalForm: TypeNormalForm
  ): Either[ConstructedTermGeneratedOriginError, TypeFragment] =
    for
      rendered <- Planner(
        Vector.empty,
        compactDefinitionBodyRoot = false
      ).renderStandaloneType(normalForm)
      _ <- validatePlan(rendered.root, rendered.source.length)
    yield new TypeFragment(rendered.source, rendered.root)

  def positionTerm(
      raw: untpd.Tree,
      fragment: TermFragment,
      source: SourceFile,
      baseOffset: Int
  )(using Context): Either[ConstructedTermGeneratedOriginError, untpd.Tree] =
    position(raw, fragment.root.shifted(baseOffset), source)

  def positionType(
      raw: untpd.Tree,
      fragment: TypeFragment,
      source: SourceFile,
      baseOffset: Int
  )(using Context): Either[ConstructedTermGeneratedOriginError, untpd.Tree] =
    position(raw, fragment.root.shifted(baseOffset), source)

  def validatePositionedTree(
      tree: untpd.Tree,
      expectedSource: SourceFile,
      sourceStart: Int,
      sourceEnd: Int
  )(using Context): Either[ConstructedTermGeneratedOriginError, Unit] =
    validatePositioned(tree, expectedSource, sourceStart, sourceEnd)

  def validateVirtualSourceName(
      name: String
  ): Either[ConstructedTermGeneratedOriginError, Unit] =
    val invalid =
      if name.isEmpty then Some("the name is empty")
      else if name != name.trim then
        Some("leading or trailing whitespace is not permitted")
      else if name.exists(char => char == '\u0000' || char == '\r' || char == '\n')
      then Some("NUL, CR, and LF are not permitted")
      else
        val represented = SourceFile.virtual(name, "").path
        Option.when(represented != name)(
          s"`$name` is represented by the compiler as `$represented`"
        )
    invalid.toLeft(())
      .left
      .map(InvalidVirtualSourceName.apply)

  private final class Planner private (
      ascriptionTypes: Vector[TypeNormalForm],
      compactDefinitionBodyRoot: Boolean
  ):
    private val builder = new StringBuilder
    private var typedOrdinal = 0

    def renderTerm(
      rootShape: TermShape
    ): Either[ConstructedTermGeneratedOriginError, RenderedPlan] =
      for
        root <-
          if compactDefinitionBodyRoot then
            renderDefinitionBodyRoot(rootShape)
          else renderTermNode(rootShape)
        _ <- Either.cond(
          typedOrdinal == ascriptionTypes.size,
          (),
          UnconsumedTypeSidecars(
            typedOrdinal,
            ascriptionTypes.size
          )
        )
      yield RenderedPlan(builder.toString, root, typedOrdinal)

    private def renderDefinitionBodyRoot(
        shape: TermShape
    ): Either[ConstructedTermGeneratedOriginError, NodePlan] =
      shape match
        case TermShape.Parenthesized(
              TermShape.Typed(expression, _)
            ) =>
          renderCompactTyped(expression, parenthesized = true)
        case _ =>
          renderTermNode(shape)

    def renderStandaloneType(
        normalForm: TypeNormalForm
    ): Either[ConstructedTermGeneratedOriginError, RenderedPlan] =
      renderType(normalForm, sidecarOrdinal = 0).map { root =>
        RenderedPlan(builder.toString, root, consumedSidecars = 0)
      }

    private def renderTermNode(
        shape: TermShape
    ): Either[ConstructedTermGeneratedOriginError, NodePlan] =
      shape match
        case TermShape.Identifier(name, _) =>
          leaf(NodeKind.TermIdent, renderIdentifier("identifier", name))
        case TermShape.Literal(value) =>
          leaf(NodeKind.Literal, renderLiteral(value))
        case TermShape.Select(qualifier, name) =>
          val start = builder.length
          for
            rawQualifier <- renderChild(qualifier, precedence = 90)
            member <- renderIdentifier("selected member", name)
          yield
            builder.append('.')
            val point = builder.length
            builder.append(member)
            node(NodeKind.Select, start, point, Vector(rawQualifier))
        case TermShape.Apply(function, arguments) =>
          val start = builder.length
          for
            rawFunction <- renderChild(function, precedence = 90)
            rawArguments <- renderSeparated(arguments, "(", ", ", ")")
          yield node(
            NodeKind.Apply,
            start,
            rawFunction.end,
            rawFunction +: rawArguments
          )
        case TermShape.New(_, _) =>
          Left(UnsupportedTermNode("New"))
        case TermShape.Infix(left, operator, right) =>
          val start = builder.length
          for
            rawLeft <- renderChild(left, precedence = 61)
            renderedOperator <- renderOperator(operator)
            rawRight <-
              builder.append(' ')
              val operatorStart = builder.length
              builder.append(renderedOperator)
              val operatorEnd = builder.length
              builder.append(' ')
              renderChild(right, precedence = 61).map { planned =>
                val operatorPlan = NodePlan(
                  NodeKind.OperatorIdent,
                  operatorStart,
                  operatorEnd,
                  operatorStart,
                  Vector.empty
                )
                planned -> operatorPlan
              }
          yield
            val (rightPlan, operatorPlan) = rawRight
            node(
              NodeKind.Infix,
              start,
              operatorPlan.start,
              Vector(rawLeft, operatorPlan, rightPlan)
            )
        case TermShape.Unary(operator, operand) =>
          if !SupportedUnaryOperators(operator) then
            Left(UnsupportedUnaryOperator(operator))
          else
            val start = builder.length
            builder.append(operator)
            val operatorPlan =
              NodePlan(
                NodeKind.OperatorIdent,
                start,
                builder.length,
                start,
                Vector.empty
              )
            renderPrefixOperand(operand).map { rawOperand =>
              node(
                NodeKind.Prefix,
                start,
                start,
                Vector(operatorPlan, rawOperand)
              )
            }
        case TermShape.InterpolatedString(prefix, parts, arguments) =>
          renderInterpolation(prefix, parts, arguments)
        case TermShape.Typed(expression, _) =>
          val ordinal = typedOrdinal
          val sidecar =
            ascriptionTypes
              .lift(ordinal)
              .toRight(MissingTypeSidecar(ordinal))
          typedOrdinal += 1
          val start = builder.length
          for
            normalForm <- sidecar
            _ = builder.append('(')
            rawExpression <- renderTermNode(expression)
            _ = builder.append("): ")
            rawType <- renderAscriptionType(normalForm, ordinal)
          yield node(
            NodeKind.Typed,
            start,
            start,
            Vector(rawExpression, rawType)
          )
        case TermShape.Tuple(elements) =>
          val start = builder.length
          renderSeparated(elements, "(", ", ", ")").map { rawElements =>
            node(NodeKind.TermTuple, start, start, rawElements)
          }
        case TermShape.If(condition, thenBranch, elseBranch) =>
          val start = builder.length
          builder.append("if ")
          for
            rawCondition <- renderChild(condition, precedence = 21)
            _ = builder.append(" then ")
            rawThen <- renderChild(thenBranch, precedence = 21)
            _ = builder.append(" else ")
            rawElse <- renderChild(elseBranch, precedence = 21)
          yield node(
            NodeKind.If,
            start,
            start,
            Vector(rawCondition, rawThen, rawElse)
          )
        case TermShape.Parenthesized(expression) =>
          val start = builder.length
          builder.append('(')
          renderTermNode(expression).map { rawExpression =>
            builder.append(')')
            node(NodeKind.Parens, start, start, Vector(rawExpression))
          }
        case TermShape.Unsupported(nodeKind, _) =>
          Left(UnsupportedTermNode(nodeKind))

    private def renderInterpolation(
        prefix: String,
        parts: List[String],
        arguments: List[TermShape]
    ): Either[ConstructedTermGeneratedOriginError, NodePlan] =
      for
        _ <- validateInterpolation(prefix, parts, arguments)
        start = builder.length
        _ = builder.append("s\"")
        rawSegments <- parts.init
          .zip(arguments)
          .foldLeft[
            Either[ConstructedTermGeneratedOriginError, Vector[NodePlan]]
          ](Right(Vector.empty)) { case (accumulated, (part, argument)) =>
            accumulated.flatMap { values =>
              val literal = appendInterpolationPart(part)
              renderInterpolationSegment(literal, argument)
                .map(values :+ _)
            }
          }
        finalLiteral = appendInterpolationPart(parts.last)
        _ = builder.append('"')
      yield node(
        NodeKind.InterpolatedString,
        start,
        start,
        rawSegments :+ finalLiteral
      )

    private def renderInterpolationSegment(
        literal: NodePlan,
        argument: TermShape
    ): Either[ConstructedTermGeneratedOriginError, NodePlan] =
      if StandardSInterpolationEncoding.isDirectArgument(argument) then
        builder.append('$')
        renderTermNode(argument).map { rawArgument =>
          node(
            NodeKind.InterpolationSegment,
            literal.start,
            literal.start,
            Vector(literal, rawArgument)
          )
        }
      else
        builder.append('$')
        val wrapperStart = builder.length
        builder.append('{')
        renderInterpolationArgumentNode(argument).map { rawArgument =>
          builder.append('}')
          val wrapper =
            node(
              NodeKind.BracedInterpolationArgument,
              wrapperStart,
              wrapperStart,
              Vector(rawArgument)
            )
          node(
            NodeKind.InterpolationSegment,
            literal.start,
            literal.start,
            Vector(literal, wrapper)
          )
        }

    private def renderInterpolationArgumentNode(
        shape: TermShape
    ): Either[ConstructedTermGeneratedOriginError, NodePlan] =
      shape match
        case TermShape.Parenthesized(TermShape.Typed(expression, _)) =>
          renderCompactTyped(expression, parenthesized = true)
        case TermShape.Typed(expression, _) =>
          renderCompactTyped(expression, parenthesized = false)
        case _ =>
          renderTermNode(shape)

    private def renderCompactTyped(
        expression: TermShape,
        parenthesized: Boolean
    ): Either[ConstructedTermGeneratedOriginError, NodePlan] =
      val ordinal = typedOrdinal
      val sidecar =
        ascriptionTypes
          .lift(ordinal)
          .toRight(MissingTypeSidecar(ordinal))
      typedOrdinal += 1
      val parensStart = builder.length
      if parenthesized then builder.append('(')
      val typedStart = builder.length
      for
        normalForm <- sidecar
        rawExpression <- renderTermNode(expression)
        _ = builder.append(": ")
        rawType <- renderAscriptionType(normalForm, ordinal)
      yield
        val typed =
          node(
            NodeKind.Typed,
            typedStart,
            typedStart,
            Vector(rawExpression, rawType)
          )
        if parenthesized then
          builder.append(')')
          node(
            NodeKind.Parens,
            parensStart,
            parensStart,
            Vector(typed)
          )
        else typed

    private def appendInterpolationPart(value: String): NodePlan =
      val encoded = StandardSInterpolationEncoding.encodePart(value)
      val start = builder.length
      builder.append(encoded.source)
      NodePlan(
        NodeKind.Literal,
        start,
        start + encoded.rawLiteralValue.length,
        start,
        Vector.empty
      )

    private def validateInterpolation(
        prefix: String,
        parts: List[String],
        arguments: List[TermShape]
    ): Either[ConstructedTermGeneratedOriginError, Unit] =
      if prefix != "s" then
        Left(UnsupportedInterpolationPrefix(String.valueOf(prefix)))
      else if parts == null || arguments == null then
        Left(
          MalformedInterpolation(
            Option(parts).fold(-1)(_.size),
            Option(arguments).fold(-1)(_.size)
          )
        )
      else if parts.size != arguments.size + 1 then
        Left(MalformedInterpolation(parts.size, arguments.size))
      else
        parts.zipWithIndex.collectFirst { case (null, index) => index } match
          case Some(index) => Left(NullInterpolationPart(index))
          case None =>
            arguments.zipWithIndex.collectFirst {
              case (null, index) => index
            } match
              case Some(index) => Left(NullInterpolationArgument(index))
              case None => Right(())

    private def renderChild(
        shape: TermShape,
        precedence: Int
    ): Either[ConstructedTermGeneratedOriginError, NodePlan] =
      if termPrecedence(shape) < precedence then
        builder.append('(')
        renderTermNode(shape).map { plan =>
          builder.append(')')
          plan
        }
      else renderTermNode(shape)

    private def renderPrefixOperand(
        operand: TermShape
    ): Either[ConstructedTermGeneratedOriginError, NodePlan] =
      if prefixOperandNeedsLexicalBoundary(operand) then
        builder.append('(')
        renderTermNode(operand).map { plan =>
          builder.append(')')
          plan
        }
      else renderChild(operand, precedence = 81)

    private def renderSeparated(
        shapes: List[TermShape],
        open: String,
        separator: String,
        close: String
    ): Either[ConstructedTermGeneratedOriginError, Vector[NodePlan]] =
      builder.append(open)
      val result =
        shapes.zipWithIndex.foldLeft[
          Either[ConstructedTermGeneratedOriginError, Vector[NodePlan]]
        ](Right(Vector.empty)) { case (accumulated, (shape, index)) =>
          accumulated.flatMap { values =>
            if index > 0 then builder.append(separator)
            renderTermNode(shape).map(values :+ _)
          }
        }
      result.map { values =>
        builder.append(close)
        values
      }

    private def renderType(
        normalForm: TypeNormalForm,
        sidecarOrdinal: Int
    ): Either[ConstructedTermGeneratedOriginError, NodePlan] =
      normalForm match
        case TypeNormalForm.STypeIdent(name @ ("Int" | "String" | "Boolean")) =>
          leaf(NodeKind.TypeIdent, Right(name))
        case TypeNormalForm.STypeApply(
              TypeNormalForm.STypeIdent(name),
              arguments
            ) if AppliedTypeConstructorPolicy
              .forConstruction(name, arguments.size)
              .isDefined =>
          val start = builder.length
          for
            constructor <- leaf(NodeKind.TypeIdent, Right(name))
            _ = builder.append('[')
            rawArguments <- renderAppliedTypeArguments(arguments, sidecarOrdinal)
            _ = builder.append(']')
          yield node(
            NodeKind.AppliedType,
            start,
            constructor.start,
            constructor +: rawArguments
          )
        case TypeNormalForm.STypeTuple(elements)
            if elements.size == 2 || elements.size == 3 =>
          val start = builder.length
          builder.append('(')
          renderTypes(elements, sidecarOrdinal, ", ").map { rawElements =>
            builder.append(')')
            node(NodeKind.TypeTuple, start, start, rawElements)
          }
        case TypeNormalForm.STypeFunction(arguments, result)
            if arguments.size == 1 || arguments.size == 2 =>
          val start = builder.length
          for
            rawArguments <-
              if arguments.size == 1 then
                renderTypeFunctionChild(arguments.head, sidecarOrdinal)
                  .map(Vector(_))
              else
                builder.append('(')
                renderTypes(arguments, sidecarOrdinal, ", ").map { values =>
                  builder.append(')')
                  values
                }
            _ = builder.append(" => ")
            arrowPoint = builder.length - 3
            rawResult <- renderTypeFunctionChild(result, sidecarOrdinal)
          yield node(
            NodeKind.FunctionType,
            start,
            arrowPoint,
            rawArguments :+ rawResult
          )
        case unsupported =>
          Left(UnsupportedTypeSidecar(sidecarOrdinal, unsupported.render))

    private def renderTypeFunctionChild(
        normalForm: TypeNormalForm,
        sidecarOrdinal: Int
    ): Either[ConstructedTermGeneratedOriginError, NodePlan] =
      renderDelimitedFunctionType(normalForm, sidecarOrdinal)

    private def renderAscriptionType(
        normalForm: TypeNormalForm,
        sidecarOrdinal: Int
    ): Either[ConstructedTermGeneratedOriginError, NodePlan] =
      renderDelimitedFunctionType(normalForm, sidecarOrdinal)

    private def renderDelimitedFunctionType(
        normalForm: TypeNormalForm,
        sidecarOrdinal: Int
    ): Either[ConstructedTermGeneratedOriginError, NodePlan] =
      normalForm match
        case _: TypeNormalForm.STypeFunction =>
          val start = builder.length
          builder.append('(')
          renderType(normalForm, sidecarOrdinal).map { plan =>
            builder.append(')')
            plan.copy(start = start, end = builder.length)
          }
        case _ =>
          renderType(normalForm, sidecarOrdinal)

    private def renderTypes(
        normalForms: List[TypeNormalForm],
        sidecarOrdinal: Int,
        separator: String
    ): Either[ConstructedTermGeneratedOriginError, Vector[NodePlan]] =
      normalForms.zipWithIndex.foldLeft[
        Either[ConstructedTermGeneratedOriginError, Vector[NodePlan]]
      ](Right(Vector.empty)) { case (accumulated, (normalForm, index)) =>
        accumulated.flatMap { values =>
          if index > 0 then builder.append(separator)
          renderType(normalForm, sidecarOrdinal).map(values :+ _)
        }
      }

    private def renderAppliedTypeArguments(
        arguments: List[TypeNormalForm],
        sidecarOrdinal: Int
    ): Either[ConstructedTermGeneratedOriginError, Vector[NodePlan]] =
      arguments.zipWithIndex.foldLeft[
        Either[ConstructedTermGeneratedOriginError, Vector[NodePlan]]
      ](Right(Vector.empty)) { case (accumulated, (argument, index)) =>
        accumulated.flatMap { values =>
          if index > 0 then builder.append(", ")
          renderDelimitedFunctionType(argument, sidecarOrdinal)
            .map(values :+ _)
        }
      }

    private def leaf(
        kind: NodeKind,
        rendered: Either[ConstructedTermGeneratedOriginError, String]
    ): Either[ConstructedTermGeneratedOriginError, NodePlan] =
      rendered.map { text =>
        val start = builder.length
        builder.append(text)
        node(kind, start, start, Vector.empty)
      }

    private def node(
        kind: NodeKind,
        start: Int,
        point: Int,
        children: Vector[NodePlan]
    ): NodePlan =
      NodePlan(kind, start, builder.length, point, children)

  private object Planner:
    def apply(
        ascriptionTypes: Vector[TypeNormalForm],
        compactDefinitionBodyRoot: Boolean
    ): Planner =
      new Planner(ascriptionTypes, compactDefinitionBodyRoot)

  private def renderIdentifier(
      role: String,
      name: String
  ): Either[ConstructedTermGeneratedOriginError, String] =
    if StandardSInterpolationEncoding.isPlainIdentifier(name) then
      if StandardSInterpolationEncoding.isKeyword(name) then Right(s"`$name`")
      else Right(name)
    else Left(UnrenderableName(role, name))

  private def renderOperator(
      operator: String
  ): Either[ConstructedTermGeneratedOriginError, String] =
    operator match
      case OperatorName() => Right(operator)
      case _
          if StandardSInterpolationEncoding.isPlainIdentifier(operator) &&
            !StandardSInterpolationEncoding.isKeyword(operator) =>
        Right(operator)
      case _ => Left(UnrenderableName("infix operator", operator))

  private def renderLiteral(
      value: String
  ): Either[ConstructedTermGeneratedOriginError, String] =
    value match
      case "true" | "false" =>
        Right(value)
      case DecimalInteger() =>
        Right(value)
      case semanticString
          if semanticString.length >= 2 &&
            semanticString.head == '"' &&
            semanticString.last == '"' =>
        Right(quoted(semanticString.substring(1, semanticString.length - 1)))
      case unsupported =>
        Left(UnsupportedLiteral(unsupported))

  private def quoted(value: String): String =
    val builder = new StringBuilder("\"")
    value.foreach {
      case '\\' => builder.append("\\\\")
      case '"' => builder.append("\\\"")
      case '\n' => builder.append("\\n")
      case '\r' => builder.append("\\r")
      case '\t' => builder.append("\\t")
      case '\b' => builder.append("\\b")
      case '\f' => builder.append("\\f")
      case char if char < ' ' || char == '\u007f' =>
        builder.append(f"\\u${char.toInt}%04x")
      case char =>
        builder.append(char)
    }
    builder.append('"').toString

  private def termPrecedence(shape: TermShape): Int =
    shape match
      case TermShape.Identifier(_, _) | TermShape.Literal(_) |
          TermShape.Tuple(_) | TermShape.Parenthesized(_) |
          TermShape.InterpolatedString(_, _, _) =>
        100
      case TermShape.Select(_, _) | TermShape.Apply(_, _) =>
        90
      case TermShape.New(_, _) =>
        90
      case TermShape.Unary(_, _) =>
        80
      case TermShape.Infix(_, _, _) =>
        60
      case TermShape.Typed(_, _) =>
        40
      case TermShape.If(_, _, _) =>
        20
      case TermShape.Unsupported(_, _) =>
        0

  /** Prefix precedence alone is insufficient when a high-precedence operand
    * starts with a symbolic token: direct concatenation would merge the prefix
    * and operand token streams. Under the current admitted grammar, negative
    * decimal literals are the symbolic-leading atom; select/apply chains
    * preserve the leading token of their qualifier/function.
    */
  private def prefixOperandNeedsLexicalBoundary(
      operand: TermShape
  ): Boolean =
    termPrecedence(operand) >= 81 &&
      renderedLeadingTokenStartsSymbolic(operand)

  private def renderedLeadingTokenStartsSymbolic(
      shape: TermShape
  ): Boolean =
    shape match
      case TermShape.Literal(value) =>
        value.startsWith("-") && DecimalInteger.matches(value)
      case TermShape.Select(qualifier, _) =>
        termPrecedence(qualifier) >= 90 &&
          renderedLeadingTokenStartsSymbolic(qualifier)
      case TermShape.Apply(function, _) =>
        termPrecedence(function) >= 90 &&
          renderedLeadingTokenStartsSymbolic(function)
      case TermShape.Unary(_, _) =>
        true
      case _ =>
        false

  private def validatePlan(
      root: NodePlan,
      sourceLength: Int
  ): Either[ConstructedTermGeneratedOriginError, Unit] =
    def loop(plan: NodePlan): Vector[String] =
      val local =
        Vector.newBuilder[String]
      if plan.start < 0 || plan.start > plan.point || plan.point > plan.end ||
          plan.end > sourceLength
      then
        local +=
          s"${plan.kind} has invalid bounds ${plan.start}..${plan.point}..${plan.end} for source length $sourceLength"
      plan.children.foreach { child =>
        if child.start < plan.start || child.end > plan.end then
          local +=
            s"${plan.kind} ${plan.start}..${plan.end} does not contain ${child.kind} ${child.start}..${child.end}"
      }
      plan.children.zip(plan.children.drop(1)).foreach { case (left, right) =>
        if left.end > right.start then
          local +=
            s"${plan.kind} children ${left.kind} and ${right.kind} overlap or are out of source order"
      }
      local.result() ++ plan.children.flatMap(loop)

    val errors =
      Option.when(root.start != 0 || root.end != sourceLength)(
        s"root ${root.start}..${root.end} does not cover source length $sourceLength"
      ).toVector ++ loop(root)
    Either.cond(
      errors.isEmpty,
      (),
      InvalidStructuralPlan(errors.mkString("; "))
    )

  private def position(
      raw: untpd.Tree,
      plan: NodePlan,
      source: SourceFile
  )(using Context): Either[ConstructedTermGeneratedOriginError, untpd.Tree] =
    def attach(tree: untpd.Tree): untpd.Tree =
      tree.cloneIn(source).withSpan(plan.span)

    (raw, plan.kind) match
      case (tree: untpd.Ident, NodeKind.TermIdent | NodeKind.TypeIdent |
            NodeKind.OperatorIdent) =>
        Right(attach(tree))
      case (tree: untpd.Literal, NodeKind.Literal) =>
        Right(attach(tree))
      case (tree: untpd.Number, NodeKind.Literal) =>
        Right(attach(tree))
      case (
            untpd.InterpolatedString(prefix, segments),
            NodeKind.InterpolatedString
          ) =>
        positionAll(segments, plan.children, source).map { positioned =>
          attach(untpd.InterpolatedString(prefix, positioned))
        }
      case (
            untpd.Thicket(trees),
            NodeKind.InterpolationSegment
          ) =>
        positionAll(trees, plan.children, source).map { positioned =>
          // A Thicket derives its union span and source from its children.
          // Calling withSpan on it propagates that union span into each child.
          untpd.Thicket(positioned).cloneIn(source)
        }
      case (
            untpd.Block(Nil, expression),
            NodeKind.BracedInterpolationArgument
          ) =>
        oneChild(plan).flatMap(position(expression, _, source)).map {
          positioned =>
            attach(untpd.Block(Nil, positioned))
        }
      case (tree: untpd.Select, NodeKind.Select) =>
        oneChild(plan).flatMap(position(tree.qualifier, _, source)).map {
          qualifier =>
            attach(untpd.cpy.Select(tree)(qualifier, tree.name))
        }
      case (tree: untpd.Apply, NodeKind.Apply) =>
        splitHead(plan).flatMap { case (functionPlan, argumentPlans) =>
          for
            function <- position(tree.fun, functionPlan, source)
            arguments <- positionAll(tree.args, argumentPlans, source)
          yield attach(untpd.cpy.Apply(tree)(function, arguments))
        }
      case (tree: untpd.InfixOp, NodeKind.Infix) =>
        exactChildren(plan, 3).flatMap { children =>
          for
            left <- position(tree.left, children(0), source)
            operator <- position(tree.op, children(1), source)
            right <- position(tree.right, children(2), source)
          yield attach(
            untpd.cpy
              .InfixOp(tree)(
                left,
                operator.asInstanceOf[untpd.Ident],
                right
              )
          )
        }
      case (tree: untpd.PrefixOp, NodeKind.Prefix) =>
        exactChildren(plan, 2).flatMap { children =>
          for
            operator <- position(tree.op, children(0), source)
            operand <- position(tree.od, children(1), source)
          yield attach(
            untpd.cpy
              .PrefixOp(tree)(
                operator.asInstanceOf[untpd.Ident],
                operand
              )
          )
        }
      case (tree: untpd.Typed, NodeKind.Typed) =>
        exactChildren(plan, 2).flatMap { children =>
          for
            expression <- position(tree.expr, children(0), source)
            typeTree <- position(tree.tpt, children(1), source)
          yield attach(untpd.cpy.Typed(tree)(expression, typeTree))
        }
      case (tree: untpd.Tuple, NodeKind.TermTuple | NodeKind.TypeTuple) =>
        positionAll(tree.trees, plan.children, source).map { elements =>
          attach(untpd.cpy.Tuple(tree)(elements))
        }
      case (tree: untpd.If, NodeKind.If) =>
        exactChildren(plan, 3).flatMap { children =>
          for
            condition <- position(tree.cond, children(0), source)
            thenBranch <- position(tree.thenp, children(1), source)
            elseBranch <- position(tree.elsep, children(2), source)
          yield attach(
            untpd.cpy.If(tree)(condition, thenBranch, elseBranch)
          )
        }
      case (tree: untpd.Parens, NodeKind.Parens) =>
        oneChild(plan).flatMap(position(tree.t, _, source)).map { expression =>
          attach(untpd.cpy.Parens(tree)(expression))
        }
      case (tree: untpd.AppliedTypeTree, NodeKind.AppliedType) =>
        splitHead(plan).flatMap { case (constructorPlan, argumentPlans) =>
          for
            constructor <- position(tree.tpt, constructorPlan, source)
            arguments <- positionAll(tree.args, argumentPlans, source)
          yield attach(
            untpd.cpy.AppliedTypeTree(tree)(constructor, arguments)
          )
        }
      case (tree: untpd.Function, NodeKind.FunctionType) =>
        splitLast(plan).flatMap { case (argumentPlans, resultPlan) =>
          for
            arguments <- positionAll(tree.args, argumentPlans, source)
            result <- position(tree.body, resultPlan, source)
          yield attach(untpd.cpy.Function(tree)(arguments, result))
        }
      case _ =>
        Left(
          RawTreePlanMismatch(
            s"raw ${raw.getClass.getSimpleName} cannot consume ${plan.kind}"
          )
        )

  private def positionAll(
      trees: List[untpd.Tree],
      plans: Vector[NodePlan],
      source: SourceFile
  )(using Context): Either[
    ConstructedTermGeneratedOriginError,
    List[untpd.Tree]
  ] =
    if trees.size != plans.size then
      Left(
        RawTreePlanMismatch(
          s"child count ${trees.size} does not match plan count ${plans.size}"
        )
      )
    else
      trees
        .zip(plans)
        .foldRight[
          Either[
            ConstructedTermGeneratedOriginError,
            List[untpd.Tree]
          ]
        ](Right(Nil)) { case ((tree, plan), accumulated) =>
          for
            positioned <- position(tree, plan, source)
            rest <- accumulated
          yield positioned :: rest
        }

  private def validatePositioned(
      tree: untpd.Tree,
      source: SourceFile,
      sourceStart: Int,
      sourceEnd: Int
  )(using Context): Either[ConstructedTermGeneratedOriginError, Unit] =
    val rootErrors =
      Option.when(
        !tree.span.exists ||
          tree.span.start != sourceStart ||
          tree.span.end != sourceEnd
      )(
        s"root ${tree.getClass.getSimpleName} does not cover generated source $sourceStart..$sourceEnd"
      ).toVector
    val errors =
      rootErrors ++ allTrees(tree).flatMap { current =>
        val local = Vector.newBuilder[String]
        if !current.source.exists then
          local += s"${current.getClass.getSimpleName} has no source"
        else if current.source.path != source.path then
          local +=
            s"${current.getClass.getSimpleName} has source `${current.source.path}` instead of `${source.path}`"
        if !current.span.exists then
          local += s"${current.getClass.getSimpleName} has no span"
        else if current.span.start < 0 ||
            current.span.start > current.span.point ||
            current.span.point > current.span.end ||
            current.span.end > sourceEnd
        then
          local +=
            s"${current.getClass.getSimpleName} has out-of-bounds span ${current.span.start}..${current.span.point}..${current.span.end}"
        if current.symbol != NoSymbol then
          local += s"${current.getClass.getSimpleName} unexpectedly has a symbol"
        if current.isInstanceOf[untpd.TypedSplice] then
          local += "positioned result contains a TypedSplice"
        val children = directChildren(current)
        if current.span.exists then
          children.foreach { child =>
            if child.span.exists &&
                (child.span.start < current.span.start ||
                  child.span.end > current.span.end)
            then
              local +=
                s"${current.getClass.getSimpleName} span does not contain ${child.getClass.getSimpleName}"
          }
        children.zip(children.drop(1)).foreach { case (left, right) =>
          if left.span.exists && right.span.exists &&
              left.span.end > right.span.start
          then
            local +=
              s"${current.getClass.getSimpleName} children ${left.getClass.getSimpleName} " +
                s"${left.span.start}..${left.span.end} and ${right.getClass.getSimpleName} " +
                s"${right.span.start}..${right.span.end} overlap or are out of source order"
        }
        local.result()
      }
    Either.cond(
      errors.isEmpty,
      (),
      IncompletePositionMap(errors.mkString("; "))
    )

  private[quasiquotes] def allTrees(
      tree: untpd.Tree
  )(using Context): Vector[untpd.Tree] =
    val children = directChildren(tree)
    tree +: children.flatMap(allTrees)

  private[quasiquotes] def directChildren(
      tree: untpd.Tree
  )(using Context): Vector[untpd.Tree] =
    tree match
      case value: untpd.DefDef =>
        Vector(value.tpt, value.rhs)
      case value: untpd.ValDef =>
        Vector(value.tpt, value.rhs)
      case value: untpd.Select =>
        Vector(value.qualifier)
      case value: untpd.Apply =>
        value.fun +: value.args.toVector
      case value: untpd.InfixOp =>
        Vector(value.left, value.op, value.right)
      case value: untpd.PrefixOp =>
        Vector(value.op, value.od)
      case value: untpd.InterpolatedString =>
        value.segments.toVector
      case value: untpd.Thicket =>
        value.trees.toVector
      case untpd.Block(Nil, expression) =>
        Vector(expression)
      case value: untpd.Typed =>
        Vector(value.expr, value.tpt)
      case value: untpd.AppliedTypeTree =>
        value.tpt +: value.args.toVector
      case value: untpd.Tuple =>
        value.trees.toVector
      case value: untpd.Function =>
        value.args.toVector :+ value.body
      case value: untpd.If =>
        Vector(value.cond, value.thenp, value.elsep)
      case value: untpd.Parens =>
        Vector(value.t)
      case _ =>
        Vector.empty

  private def oneChild(
      plan: NodePlan
  ): Either[ConstructedTermGeneratedOriginError, NodePlan] =
    exactChildren(plan, 1).map(_.head)

  private def exactChildren(
      plan: NodePlan,
      count: Int
  ): Either[ConstructedTermGeneratedOriginError, Vector[NodePlan]] =
    Either.cond(
      plan.children.size == count,
      plan.children,
      RawTreePlanMismatch(
        s"${plan.kind} expected $count children but planned ${plan.children.size}"
      )
    )

  private def splitHead(
      plan: NodePlan
  ): Either[
    ConstructedTermGeneratedOriginError,
    (NodePlan, Vector[NodePlan])
  ] =
    plan.children.headOption
      .map(_ -> plan.children.tail)
      .toRight(RawTreePlanMismatch(s"${plan.kind} has no head child"))

  private def splitLast(
      plan: NodePlan
  ): Either[
    ConstructedTermGeneratedOriginError,
    (Vector[NodePlan], NodePlan)
  ] =
    plan.children.lastOption
      .map(plan.children.dropRight(1) -> _)
      .toRight(RawTreePlanMismatch(s"${plan.kind} has no result child"))
