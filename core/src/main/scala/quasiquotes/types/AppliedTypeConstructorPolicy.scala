package quasiquotes.types

private[quasiquotes] object AppliedTypeConstructorPolicy:
  final case class Constructor private[AppliedTypeConstructorPolicy] (
      name: String,
      requiredArity: Int,
      normalFormSourceAdmission: Boolean,
      constructionAdmission: Boolean
  )

  private val constructors = List(
    Constructor("List", 1, normalFormSourceAdmission = true, constructionAdmission = true),
    Constructor("Option", 1, normalFormSourceAdmission = true, constructionAdmission = true),
    Constructor("Either", 2, normalFormSourceAdmission = true, constructionAdmission = true)
  )

  def forNormalFormSource(name: String, actualArity: Int): Option[Constructor] =
    constructors.find(constructor =>
      constructor.name == name &&
        constructor.requiredArity == actualArity &&
        constructor.normalFormSourceAdmission
    )

  def forConstruction(name: String, actualArity: Int): Option[Constructor] =
    constructors.find(constructor =>
      constructor.name == name &&
        constructor.requiredArity == actualArity &&
        constructor.constructionAdmission
    )

  def named(name: String): Option[Constructor] =
    constructors.find(_.name == name)

  def forResolved(
      id: ResolvedTypeNameId,
      actualArity: Int
  ): Option[Constructor] =
    val expected =
      if id == StandardResolvedTypeNames.ListId then Some("List")
      else if id == StandardResolvedTypeNames.OptionId then Some("Option")
      else if id == StandardResolvedTypeNames.EitherId then Some("Either")
      else None
    expected.flatMap(name => forNormalFormSource(name, actualArity))
