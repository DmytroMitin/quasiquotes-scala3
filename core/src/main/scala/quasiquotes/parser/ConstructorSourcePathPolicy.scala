package quasiquotes.parser

private[quasiquotes] object ConstructorSourcePathPolicy:
  private val Segment = "[A-Za-z_][A-Za-z0-9_]*".r

  def validate(path: String): Either[String, String] =
    val segments = Option(path).fold(Array.empty[String])(_.split("\\.", -1))
    if segments.isEmpty || segments.exists(segment => Segment.matches(segment) == false) then
      Left("constructor names must use plain identifier segments without backticks, type arguments, or binary-name spelling")
    else Right(path)
