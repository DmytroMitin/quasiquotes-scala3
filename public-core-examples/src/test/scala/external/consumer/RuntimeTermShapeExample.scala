package external.consumer

// snippet:runtime-term-shape:start
import quasiquotes.parser.TermShape

object RuntimeTermShapeExample:
  val tree: TermShape = TermShape.Infix(
    TermShape.Literal("1"),
    "+",
    TermShape.Literal("2")
  )

  val literalOperands: Option[(String, String)] =
    tree match
      case TermShape.Infix(
            TermShape.Literal(left),
            "+",
            TermShape.Literal(right)
          ) => Some((left, right))
      case _ => None
// snippet:runtime-term-shape:end
