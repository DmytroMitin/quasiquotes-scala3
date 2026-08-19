package external.consumer

// snippet:runtime-parser:start
import quasiquotes.parser.{ParsedExpression, TinyTermParser}

object RuntimeParserExample:
  val parsed: Either[Throwable, ParsedExpression] =
    TinyTermParser.parse("1 + 2")

  val summary: Either[Throwable, (String, String, String)] =
    parsed.map { result =>
      (
        result.source,
        result.shape.render,
        result.rawTree.getClass.getName
      )
    }
// snippet:runtime-parser:end
