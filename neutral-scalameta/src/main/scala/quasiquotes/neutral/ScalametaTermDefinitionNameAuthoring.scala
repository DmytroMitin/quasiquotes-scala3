package quasiquotes.neutral

import _root_.quasiquotes.definitions.DefinitionName

import scala.meta.Term
import scala.util.control.NonFatal

/** Exact fresh Term.Name authoring shared by the narrow Definition authorers. */
private[quasiquotes] object ScalametaTermDefinitionNameAuthoring:
  def author(name: DefinitionName): Option[Term.Name] =
    Option(name).flatMap { expected =>
      Option(expected.decoded).flatMap { decoded =>
        try
          val authored = Term.Name(decoded)
          ScalametaDefinitionNameProjection.project(authored) match
            case Right(projected) if projected == expected => Some(authored)
            case _ => None
        catch case NonFatal(_) => None
      }
    }
