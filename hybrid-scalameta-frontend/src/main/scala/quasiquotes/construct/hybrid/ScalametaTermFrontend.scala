package quasiquotes.construct.hybrid

import scala.quoted.{Expr, Quotes, Varargs}
import scala.util.control.NonFatal

import scala.meta.*
import scala.meta.parsers.Parsed

import _root_.quasiquotes.construct.*
import _root_.quasiquotes.parser.TinyTermParser
import _root_.quasiquotes.hybrid.TermQ3DialectPolicy
import _root_.quasiquotes.types.toTypeRepr

/** Public-Scalameta syntax parsing followed by project-owned Quotes lowering.
  * This lives only in the unpublished side-by-side module.
  */
object ScalametaTermFrontend:
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
        for
          tree <- parse(synthesized.source, dialect)
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

    def loop(current: scala.meta.Term): Either[Failure, Term] =
      current match
        case name: scala.meta.Term.Name =>
          termHoles.get(name.value) match
            case Some(term) => Right(term)
            case None if typeHoles.contains(name.value) =>
              Left(Failure.lowering(s"type placeholder used in term position: ${name.value}"))
            case None if PlaceholderSource.isCategorizedName(name.value) && !literalCategorizedNames(name.value) =>
              Left(Failure.lowering(s"unknown term placeholder: ${name.value}"))
            case None => IdentifierResolver.resolve(name.value).left.map(error => Failure.lowering(error.message))
        case Lit.Int(value) => Right(Literal(IntConstant(value)))
        case Lit.String(value) => Right(Literal(StringConstant(value)))
        case Lit.Boolean(value) => Right(Literal(BooleanConstant(value)))
        case select: scala.meta.Term.Select =>
          loop(select.qual).flatMap(selectMember(_, select.name.value))
        case application: scala.meta.Term.Apply =>
          for
            function <- loop(application.fun)
            arguments <- sequence(application.args.map(loop))
            result <- applyFunction(function, arguments)
          yield result
        case infix: scala.meta.Term.ApplyInfix if infix.argClause.values.size == 1 =>
          for
            left <- loop(infix.lhs)
            right <- loop(infix.argClause.values.head)
            result <-
              try Right(Select.overloaded(left, infix.op.value, Nil, right :: Nil))
              catch case NonFatal(error) => Left(Failure.lowering(s"infix ${infix.op.value} failed: ${error.getMessage}"))
          yield result
        case tuple: scala.meta.Term.Tuple =>
          sequence(tuple.args.map(loop)).flatMap { elements =>
            if elements.size < 2 || elements.size > 22 then
              unsupported(tuple, s"unsupported tuple arity ${elements.size}")
            else
              try Right(Expr.ofTupleFromSeq(elements.map(_.asExpr)).asTerm)
              catch case NonFatal(error) => Left(Failure.lowering(s"tuple lowering failed: ${error.getMessage}"))
          }
        case conditional: scala.meta.Term.If =>
          for
            condition <- loop(conditional.cond)
            thenBranch <- loop(conditional.thenp)
            elseBranch <- loop(conditional.elsep)
          yield If(condition, thenBranch, elseBranch)
        case ascription: scala.meta.Term.Ascribe =>
          for
            expression <- loop(ascription.expr)
            tpe <- lowerType(ascription.tpe)
          yield Typed(expression, tpe)
        case interpolation: scala.meta.Term.Interpolate if interpolation.prefix.value == "s" =>
          for
            arguments <- sequence(interpolation.args.map(loop))
            parts <- sequence(interpolation.parts.map {
              case Lit.String(value) => Right(value)
              case other => unsupported(other, "non-string interpolation segment")
            })
          yield '{ StringContext(${Varargs(parts.map(Expr(_)))}*).s(${Varargs(arguments.map(_.asExpr))}*) }.asTerm
        case other => unsupported(other, "outside the bounded side-by-side term tranche")

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
