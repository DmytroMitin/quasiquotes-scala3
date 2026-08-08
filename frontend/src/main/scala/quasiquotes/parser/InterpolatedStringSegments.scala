package quasiquotes.parser

import dotty.tools.dotc.ast.untpd

private[quasiquotes] final case class InterpolatedStringSegments(
    parts: List[String],
    arguments: List[untpd.Tree]
)

private[quasiquotes] object InterpolatedStringSegments:
  def decode(segments: List[untpd.Tree]): Either[String, InterpolatedStringSegments] =
    def unwrapArgument(tree: untpd.Tree): untpd.Tree =
      tree match
        case untpd.Block(Nil, expression) => expression
        case other => other

    def loop(
        remaining: List[untpd.Tree],
        parts: List[String],
        arguments: List[untpd.Tree]
    ): Either[String, InterpolatedStringSegments] =
      remaining match
        case untpd.Literal(constant) :: Nil if constant.value.isInstanceOf[String] =>
          Right(
            InterpolatedStringSegments(
              (constant.value.asInstanceOf[String] :: parts).reverse,
              arguments.reverse
            )
          )
        case untpd.Thicket(untpd.Literal(constant) :: argument :: Nil) :: tail
            if constant.value.isInstanceOf[String] =>
          loop(
            tail,
            constant.value.asInstanceOf[String] :: parts,
            unwrapArgument(argument) :: arguments
          )
        case tree :: _ =>
          Left(
            s"expected Thicket(literal-part, argument) followed by a final literal part, " +
              s"found ${tree.getClass.getSimpleName}: $tree"
          )
        case Nil => Left("missing final literal part")

    loop(segments, Nil, Nil).flatMap { decoded =>
      Either.cond(
        decoded.parts.size == decoded.arguments.size + 1,
        decoded,
        s"parts=${decoded.parts.size}, arguments=${decoded.arguments.size}"
      )
    }
