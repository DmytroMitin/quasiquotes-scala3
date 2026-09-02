package quasiquotes.terms

import quasiquotes.parser.{BlockStatement, TermShape, TypeShape}
import quasiquotes.types.{TypeNormalForm, TypeTemplate}

private[quasiquotes] final class ConstructedTerm private (
    val root: TermShape,
    val ascriptionTypes: Vector[TypeNormalForm]
) derives CanEqual:
  private lazy val semanticRoot: TermShape =
    TermShapeTraversal.alphaNormalize(root)

  def render: String =
    s"ConstructedTerm(root=${root.render}, ascriptions=[${ascriptionTypes.map(_.render).mkString(", ")}])"

  override def equals(other: Any): Boolean =
    other match
      case that: ConstructedTerm =>
        semanticRoot == that.semanticRoot && ascriptionTypes == that.ascriptionTypes
      case _ =>
        false

  override def hashCode: Int =
    (semanticRoot, ascriptionTypes).hashCode

  override def toString: String =
    render

private[quasiquotes] object ConstructedTerm:
  def fromShape(shape: TermShape): Either[TermConstructionError, ConstructedTerm] =
    fromShapeUsing(shape, Vector.empty)

  def fromShapeInScope(
      shape: TermShape,
      binderId: quasiquotes.parser.BinderId
  ): Either[TermConstructionError, ConstructedTerm] =
    fromShapeUsing(shape, Vector(binderId))

  def fromShapeInScope(
      shape: TermShape,
      binderIds: Vector[quasiquotes.parser.BinderId]
  ): Either[TermConstructionError, ConstructedTerm] =
    fromShapeUsing(shape, binderIds)

  private def fromShapeUsing(
      shape: TermShape,
      enclosingBinderIds: Vector[quasiquotes.parser.BinderId]
  ): Either[TermConstructionError, ConstructedTerm] =
    for
      _ <- validateShape(shape, enclosingBinderIds)
      names = TermShapeTraversal.typedNames(shape)
      ascriptions <- deriveAscriptions(shape, names)
      constructed <- createUsing(shape, ascriptions, enclosingBinderIds)
    yield constructed

  def create(
      shape: TermShape,
      completedAscriptions: Vector[TypeNormalForm]
  ): Either[TermConstructionError, ConstructedTerm] =
    createUsing(shape, completedAscriptions, Vector.empty)

  def createInScope(
      shape: TermShape,
      completedAscriptions: Vector[TypeNormalForm],
      binderId: quasiquotes.parser.BinderId
  ): Either[TermConstructionError, ConstructedTerm] =
    createUsing(shape, completedAscriptions, Vector(binderId))

  def createInScope(
      shape: TermShape,
      completedAscriptions: Vector[TypeNormalForm],
      binderIds: Vector[quasiquotes.parser.BinderId]
  ): Either[TermConstructionError, ConstructedTerm] =
    createUsing(shape, completedAscriptions, binderIds)

  def validateInScope(
      term: ConstructedTerm,
      binderId: quasiquotes.parser.BinderId
  ): Either[TermConstructionError, Unit] =
    TermShapeTraversal.validateSupportedInScope(term.root, binderId)

  def validateInScope(
      term: ConstructedTerm,
      binderIds: Vector[quasiquotes.parser.BinderId]
  ): Either[TermConstructionError, Unit] =
    TermShapeTraversal.validateSupportedInScope(term.root, binderIds)

  def semanticInScope(
      term: ConstructedTerm,
      binderId: quasiquotes.parser.BinderId
  ): (TermShape, Vector[TypeNormalForm]) =
    (
      TermShapeTraversal.alphaNormalizeInScope(term.root, binderId),
      term.ascriptionTypes
    )

  def semanticInScope(
      term: ConstructedTerm,
      binderIds: Vector[quasiquotes.parser.BinderId]
  ): (TermShape, Vector[TypeNormalForm]) =
    (
      TermShapeTraversal.alphaNormalizeInScope(term.root, binderIds),
      term.ascriptionTypes
    )

  private def createUsing(
      shape: TermShape,
      completedAscriptions: Vector[TypeNormalForm],
      enclosingBinderIds: Vector[quasiquotes.parser.BinderId]
  ): Either[TermConstructionError, ConstructedTerm] =
    for
      _ <- validateShape(shape, enclosingBinderIds)
      canonicalRoot = TermShapeTraversal.canonicalizePlaceholders(shape)
      typedNames = TermShapeTraversal.typedNames(canonicalRoot)
      _ <- validateCount(typedNames.size, completedAscriptions.size)
      _ <- validateCompletedAscriptions(completedAscriptions)
      _ <- validateRendering(typedNames, completedAscriptions)
    yield new ConstructedTerm(canonicalRoot, completedAscriptions)

  private def validateShape(
      shape: TermShape,
      enclosingBinderIds: Vector[quasiquotes.parser.BinderId]
  ): Either[TermConstructionError, Unit] =
    if enclosingBinderIds.isEmpty then TermShapeTraversal.validateSupported(shape)
    else TermShapeTraversal.validateSupportedInScope(shape, enclosingBinderIds)

  private def deriveAscriptions(
      shape: TermShape,
      names: Vector[String]
  ): Either[TermConstructionError, Vector[TypeNormalForm]] =
    shape match
      case TermShape.Block((local: BlockStatement.LocalDef) :: Nil, _) =>
        for
          parameterType <- normalFormFromShape(local.parameterType, 0)
          resultType <- normalFormFromShape(local.resultType, 1)
          nestedTypes <- deriveSimpleAscriptions(names.drop(2), 2)
        yield Vector(parameterType, resultType) ++ nestedTypes
      case _ => deriveSimpleAscriptions(names, 0)

  private def normalFormFromShape(
      shape: TypeShape,
      typedOrdinal: Int
  ): Either[TermConstructionError, TypeNormalForm] =
    TypeNormalForm
      .fromShape(shape)
      .left
      .map(error =>
        TermConstructionError.InvalidTypeTemplateSidecar(
          typedOrdinal,
          error.message
        )
      )

  private def deriveSimpleAscriptions(
      names: Vector[String],
      startingOrdinal: Int
  ): Either[TermConstructionError, Vector[TypeNormalForm]] =
    names.zipWithIndex.foldLeft[
      Either[TermConstructionError, Vector[TypeNormalForm]]
    ](Right(Vector.empty)) { case (result, (name, index)) =>
      result.flatMap { values =>
        val typedOrdinal = startingOrdinal + index
        name match
          case "Int" | "String" | "Boolean" =>
            Right(values :+ TypeNormalForm.STypeIdent(name))
          case _ =>
            Left(
              TermConstructionError.InvalidTypeTemplateSidecar(
                typedOrdinal,
                s"`$name` requires the explicit completed-sidecar factory"
              )
            )
      }
    }

  private def validateCount(
      expected: Int,
      actual: Int
  ): Either[TermConstructionError, Unit] =
    Either.cond(
      expected == actual,
      (),
      TermConstructionError.TypedSidecarCountMismatch(expected, actual)
    )

  private def validateCompletedAscriptions(
      ascriptions: Vector[TypeNormalForm]
  ): Either[TermConstructionError, Unit] =
    ascriptions.zipWithIndex.foldLeft[Either[TermConstructionError, Unit]](
      Right(())
    ) { case (result, (normalForm, typedOrdinal)) =>
      result.flatMap { _ =>
        TypeTemplate
          .validateConstructed(normalForm)
          .left
          .map(error =>
            TermConstructionError.InvalidTypeTemplateSidecar(
              typedOrdinal,
              error.message
            )
          )
      }
    }

  private def validateRendering(
      typedNames: Vector[String],
      ascriptions: Vector[TypeNormalForm]
  ): Either[TermConstructionError, Unit] =
    typedNames
      .zip(ascriptions)
      .zipWithIndex
      .foldLeft[Either[TermConstructionError, Unit]](Right(())) {
        case (result, ((actual, normalForm), typedOrdinal)) =>
          result.flatMap { _ =>
            val expected = TermShapeTraversal.renderNormalForm(normalForm)
            Either.cond(
              actual == expected,
              (),
              TermConstructionError.TypedSidecarRenderingMismatch(
                typedOrdinal,
                expected,
                actual
              )
            )
          }
      }
