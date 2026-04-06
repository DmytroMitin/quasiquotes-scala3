package quasiquotes.parser

object Placeholder:
  private val HolePattern = "^__hole\\d+$".r

  def isPlaceholder(name: String): Boolean =
    HolePattern.matches(name)
