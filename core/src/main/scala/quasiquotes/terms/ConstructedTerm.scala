package quasiquotes.terms

import quasiquotes.parser.TermShape
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
    fromShapeUsing(shape, None)

  def fromShapeInScope(
      shape: TermShape,
      binderId: quasiquotes.parser.BinderId
  ): Either[TermConstructionError, ConstructedTerm] =
    fromShapeUsing(shape, Some(binderId))

  private def fromShapeUsing(
      shape: TermShape,
      enclosingBinderId: Option[quasiquotes.parser.BinderId]
  ): Either[TermConstructionError, ConstructedTerm] =
    for
      _ <- validateShape(shape, enclosingBinderId)
      names = TermShapeTraversal.typedNames(shape)
      ascriptions <- deriveSimpleAscriptions(names)
      constructed <- createUsing(shape, ascriptions, enclosingBinderId)
    yield constructed

  def create(
      shape: TermShape,
      completedAscriptions: Vector[TypeNormalForm]
  ): Either[TermConstructionError, ConstructedTerm] =
    createUsing(shape, completedAscriptions, None)

  def createInScope(
      shape: TermShape,
      completedAscriptions: Vector[TypeNormalForm],
      binderId: quasiquotes.parser.BinderId
  ): Either[TermConstructionError, ConstructedTerm] =
    createUsing(shape, completedAscriptions, Some(binderId))

  def validateInScope(
      term: ConstructedTerm,
      binderId: quasiquotes.parser.BinderId
  ): Either[TermConstructionError, Unit] =
    TermShapeTraversal.validateSupportedInScope(term.root, binderId)

  def semanticInScope(
      term: ConstructedTerm,
      binderId: quasiquotes.parser.BinderId
  ): (TermShape, Vector[TypeNormalForm]) =
    (
      TermShapeTraversal.alphaNormalizeInScope(term.root, binderId),
      term.ascriptionTypes
    )

  private def createUsing(
      shape: TermShape,
      completedAscriptions: Vector[TypeNormalForm],
      enclosingBinderId: Option[quasiquotes.parser.BinderId]
  ): Either[TermConstructionError, ConstructedTerm] =
    for
      _ <- validateShape(shape, enclosingBinderId)
      canonicalRoot = TermShapeTraversal.canonicalizePlaceholders(shape)
      typedNames = TermShapeTraversal.typedNames(canonicalRoot)
      _ <- validateCount(typedNames.size, completedAscriptions.size)
      _ <- validateCompletedAscriptions(completedAscriptions)
      _ <- validateRendering(typedNames, completedAscriptions)
    yield new ConstructedTerm(canonicalRoot, completedAscriptions)

  private def validateShape(
      shape: TermShape,
      enclosingBinderId: Option[quasiquotes.parser.BinderId]
  ): Either[TermConstructionError, Unit] =
    enclosingBinderId match
      case Some(binderId) =>
        TermShapeTraversal.validateSupportedInScope(shape, binderId)
      case None =>
        TermShapeTraversal.validateSupported(shape)

  private def deriveSimpleAscriptions(
      names: Vector[String]
  ): Either[TermConstructionError, Vector[TypeNormalForm]] =
    names.zipWithIndex.foldLeft[
      Either[TermConstructionError, Vector[TypeNormalForm]]
    ](Right(Vector.empty)) { case (result, (name, typedOrdinal)) =>
      result.flatMap { values =>
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
