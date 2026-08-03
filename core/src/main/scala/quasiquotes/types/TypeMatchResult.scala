package quasiquotes.types

final case class TypeMatchResult(bindings: Map[String, TypeNormalForm]):
  def binding(name: String): Option[TypeNormalForm] =
    bindings.get(TypeMatchResult.normalizeName(name))

  def requiredBinding(name: String): Either[TypeQuasiquoteError, TypeNormalForm] =
    val normalizedName = TypeMatchResult.normalizeName(name)
    binding(normalizedName).toRight(TypeQuasiquoteError(s"Missing type-hole binding `$normalizedName`; available bindings: ${availableBindingsSummary}"))

  def bindingsSummary: String =
    bindings.toList.sortBy(_._1).map((name, normalForm) => s"$name=${normalForm.render}").mkString(", ")

  private def availableBindingsSummary: String =
    if bindings.isEmpty then "<none>" else bindings.keys.toList.sorted.mkString(", ")

object TypeMatchResult:
  private def normalizeName(name: String): String =
    if name.startsWith("$") then name.drop(1) else name
