package quasiquotes.types

import scala.quoted.*

object AppliedTypeStructuralPreflight:
  inline def typedSnapshot[T]: String = ${ typedSnapshotImpl[T] }

  private def typedSnapshotImpl[T: Type](using Quotes): Expr[String] =
    import quotes.reflect.*

    def render(repr: TypeRepr): String =
      repr match
        case AppliedType(constructor, arguments) =>
          s"AppliedType(${render(constructor)}, [${arguments.map(render).mkString(", ")}])"
        case TypeRef(_, name) =>
          s"TypeRef($name)"
        case other =>
          s"${other.getClass.getSimpleName}(${other.show})"

    Expr(render(TypeRepr.of[T]))
