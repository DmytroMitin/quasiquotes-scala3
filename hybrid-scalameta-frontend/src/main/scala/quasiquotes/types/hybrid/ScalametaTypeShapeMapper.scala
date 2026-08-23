package quasiquotes.types.hybrid

import scala.meta.*

import _root_.quasiquotes.parser.TypeShape

/** Direct public-Scalameta-Type to project-owned structural mapping. */
private[quasiquotes] object ScalametaTypeShapeMapper:
  def map(tree: scala.meta.Type): TypeShape =
    tree match
      case name: scala.meta.Type.Name =>
        TypeShape.Identifier(name.value)
      case applied: scala.meta.Type.Apply =>
        TypeShape.Apply(map(applied.tpe), applied.args.map(map))
      case tuple: scala.meta.Type.Tuple =>
        TypeShape.Tuple(tuple.args.map(map))
      case function: scala.meta.Type.Function =>
        TypeShape.Function(function.params.map(map), map(function.res))
      case selected: scala.meta.Type.Select =>
        TypeShape.Select(selectedQualifier(selected.qual), selected.name.value)
      case unsupported =>
        TypeShape.Unsupported(
          unsupported.productPrefix,
          "public Scalameta type node is outside the current Type Q3 matrix"
        )

  private def selectedQualifier(qualifier: scala.meta.Term): TypeShape =
    qualifier match
      case name: scala.meta.Term.Name => TypeShape.Identifier(name.value)
      case select: scala.meta.Term.Select =>
        TypeShape.Select(selectedQualifier(select.qual), select.name.value)
      case unsupported =>
        TypeShape.Unsupported(
          unsupported.productPrefix,
          "selected type qualifier is outside the current Type Q3 matrix"
        )
