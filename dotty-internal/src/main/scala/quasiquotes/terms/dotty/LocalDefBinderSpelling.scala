package quasiquotes.terms.dotty

private[dotty] object LocalDefBinderSpelling:
  def unavailableKeys(names: Iterable[String]): Set[String] =
    names.iterator.map { name =>
      if name.length >= 2 && name.head == '`' && name.last == '`' then
        name.substring(1, name.length - 1)
      else name
    }.toSet

  def freshen(preferred: String, unavailable: Set[String]): String =
    if !unavailable(preferred) then preferred
    else
      Iterator
        .from(1)
        .map(index => s"${preferred}_$index")
        .find(!unavailable(_))
        .get
