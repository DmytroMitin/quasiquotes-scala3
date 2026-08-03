package quasiquotes.types

final case class ConstructedType(normalForm: TypeNormalForm):
  def source: String =
    ConstructedType.renderSource(normalForm)

object ConstructedType:
  def renderSource(normalForm: TypeNormalForm): String =
    normalForm match
      case TypeNormalForm.STypeIdent(name) => name
      case TypeNormalForm.STypeApply(TypeNormalForm.STypeIdent(name), argument :: Nil) =>
        s"$name[${renderSource(argument)}]"
      case TypeNormalForm.STypeApply(constructor, arguments) =>
        s"${renderSource(constructor)}[${arguments.map(renderSource).mkString(", ")}]"
      case TypeNormalForm.STypeTuple(first :: second :: Nil) =>
        s"(${renderSource(first)}, ${renderSource(second)})"
      case TypeNormalForm.STypeTuple(elements) =>
        s"(${elements.map(renderSource).mkString(", ")})"
      case TypeNormalForm.STypeFunction(argument :: Nil, result) =>
        s"${renderSource(argument)} => ${renderSource(result)}"
      case TypeNormalForm.STypeFunction(arguments, result) =>
        s"(${arguments.map(renderSource).mkString(", ")}) => ${renderSource(result)}"
