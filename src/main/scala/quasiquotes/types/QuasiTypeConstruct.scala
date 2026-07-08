package quasiquotes.types

object QuasiTypeConstruct:
  def fromTemplate(
      templateSource: String,
      bindings: Map[String, TypeNormalForm]
  ): Either[TypeQuasiquoteError, ConstructedType] =
    for
      template <- TypeTemplate.fromSource(templateSource)
      _ <- rejectExtraBindings(template, bindings)
      normalForm <- TypeTemplate.construct(template, bindings)
      _ <- TypeTemplate.validateConstructed(normalForm)
    yield ConstructedType(normalForm)

  def fromTemplate(
      templateSource: String,
      bindings: (String, TypeNormalForm)*
  ): Either[TypeQuasiquoteError, ConstructedType] =
    fromTemplate(templateSource, bindings.toMap)

  private def rejectExtraBindings(template: TypeTemplate, bindings: Map[String, TypeNormalForm]): Either[TypeQuasiquoteError, Unit] =
    val expectedNames = TypeTemplate.holeNames(template)
    val extraNames = bindings.keySet.diff(expectedNames).toList.sorted
    if extraNames.isEmpty then Right(())
    else Left(TypeQuasiquoteError(s"Extra type-construction binding(s): ${extraNames.mkString(", ")}"))
