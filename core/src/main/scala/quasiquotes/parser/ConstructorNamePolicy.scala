package quasiquotes.parser

private[quasiquotes] object ConstructorNamePolicy:
  def validate(name: String): Either[String, String] =
    val segments = Option(name).fold(Array.empty[String])(_.split("\\.", -1))
    if segments.length < 2 then
      Left("constructor names must be fully qualified with at least two plain identifier segments")
    else ConstructorSourcePathPolicy.validate(name)
