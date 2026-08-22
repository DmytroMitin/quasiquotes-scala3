package quasiquotes.matching.hybrid

import scala.meta.*

import _root_.quasiquotes.construct.hybrid.ScalametaTermFrontend
import _root_.quasiquotes.matching.{PatternSource, TermPattern}
import _root_.quasiquotes.parser.BinderId
import _root_.quasiquotes.hybrid.TermQ3DialectPolicy
import _root_.quasiquotes.source.GeneratedHoleIndex

/** Scalameta syntax-to-existing-pattern-IR compiler for the bounded term slice. */
object ScalametaPatternFrontend:
  type Failure = ScalametaTermFrontend.Failure

  def compile(
      pattern: String,
      dialect: Dialect = TermQ3DialectPolicy.selected
  ): Either[Failure, TermPattern] =
    PatternSource.synthesizeMapped(pattern)
      .left.map(error => ScalametaTermFrontend.Failure.template(error.message))
      .flatMap { mapped =>
        for
          tree <- ScalametaTermFrontend.parse(mapped.patternSource.source, dialect)
          _ <- ScalametaTermFrontend.validateExactCompiler(mapped.patternSource.source)
          compiled <- compileTree(tree, mapped.generatedHoleIndex)
        yield compiled
      }

  def compileTree(
      tree: scala.meta.Term,
      holes: GeneratedHoleIndex
  ): Either[Failure, TermPattern] =
    def unsupported(tree: scala.meta.Tree, detail: String): Either[Failure, Nothing] =
      Left(
        ScalametaTermFrontend.Failure(
          "SCALAMETA_PATTERN_LOWERING_UNSUPPORTED",
          tree.pos.start,
          tree.pos.end,
          s"${tree.productPrefix}: $detail"
        )
      )

    def sequence[A](values: List[Either[Failure, A]]): Either[Failure, List[A]] =
      values.foldRight(Right(Nil): Either[Failure, List[A]]) { (next, accumulated) =>
        for
          head <- next
          tail <- accumulated
        yield head :: tail
      }

    def renderType(tpe: scala.meta.Type): Either[Failure, String] =
      tpe match
        case name: scala.meta.Type.Name => Right(normalizeType(name.value))
        case select: scala.meta.Type.Select => Right(normalizeType(select.syntax))
        case _ => unsupported(tpe, "ascription type must be a stable name")

    def literal(lit: Lit): Either[Failure, TermPattern] =
      lit match
        case Lit.Int(value) => Right(TermPattern.Literal(value.toString))
        case Lit.String(value) => Right(TermPattern.Literal("\"" + value + "\""))
        case Lit.Boolean(value) => Right(TermPattern.Literal(value.toString))
        case other => unsupported(other, "unsupported literal")

    def loop(
        current: scala.meta.Term,
        scope: List[(String, BinderId)] = Nil
    ): Either[Failure, TermPattern] =
      current match
        case name: scala.meta.Term.Name =>
          holes.semanticNameFor(name.value) match
            case Some(semanticName) => Right(TermPattern.Hole(semanticName))
            case None =>
              scope.collectFirst { case (boundName, id) if boundName == name.value => id } match
                case Some(id) => Right(TermPattern.BoundReference(id, name.value))
                case None => Right(TermPattern.Identifier(name.value))
        case value: Lit => literal(value)
        case select: scala.meta.Term.Select =>
          loop(select.qual, scope).map(TermPattern.Select(_, select.name.value))
        case application: scala.meta.Term.Apply =>
          for
            function <- loop(application.fun, scope)
            arguments <- sequence(application.args.map(loop(_, scope)))
          yield TermPattern.Apply(function, arguments)
        case infix: scala.meta.Term.ApplyInfix if infix.argClause.values.size == 1 =>
          for
            left <- loop(infix.lhs, scope)
            right <- loop(infix.argClause.values.head, scope)
          yield TermPattern.Infix(left, infix.op.value, right)
        case tuple: scala.meta.Term.Tuple =>
          sequence(tuple.args.map(loop(_, scope))).map(TermPattern.Tuple.apply)
        case conditional: scala.meta.Term.If =>
          for
            condition <- loop(conditional.cond, scope)
            thenBranch <- loop(conditional.thenp, scope)
            elseBranch <- loop(conditional.elsep, scope)
          yield TermPattern.If(condition, thenBranch, elseBranch)
        case ascription: scala.meta.Term.Ascribe =>
          for
            expression <- loop(ascription.expr, scope)
            tpe <- renderType(ascription.tpe)
          yield TermPattern.Typed(expression, tpe)
        case other => unsupported(other, "outside the bounded side-by-side term-pattern tranche")

    loop(tree)

  private def normalizeType(name: String): String =
    name match
      case "scala.Int" => "Int"
      case "scala.Predef.String" | "java.lang.String" | "scala.String" => "String"
      case "scala.Boolean" => "Boolean"
      case other => other
