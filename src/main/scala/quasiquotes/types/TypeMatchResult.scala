package quasiquotes.types

final case class TypeMatchResult(bindings: Map[String, TypeNormalForm]):
  def binding(name: String): Option[TypeNormalForm] =
    bindings.get(TypeMatchResult.normalizeName(name))

  def bindingsSummary: String =
    bindings.toList.sortBy(_._1).map((name, normalForm) => s"$name=${normalForm.render}").mkString(", ")

object TypeMatchResult:
  private def normalizeName(name: String): String =
    if name.startsWith("$") then name.drop(1) else name
