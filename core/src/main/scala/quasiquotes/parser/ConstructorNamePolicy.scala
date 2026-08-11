package quasiquotes.parser

private[quasiquotes] object ConstructorNamePolicy:
  private val Segment = "[A-Za-z_][A-Za-z0-9_]*".r

  def validate(name: String): Either[String, String] =
    val segments = Option(name).fold(Array.empty[String])(_.split("\\.", -1))
    if segments.length < 2 then
      Left("constructor names must be fully qualified with at least two plain identifier segments")
    else if segments.exists(segment => Segment.matches(segment) == false) then
      Left("constructor names must use plain identifier segments without backticks, type arguments, or binary-name spelling")
    else Right(name)
