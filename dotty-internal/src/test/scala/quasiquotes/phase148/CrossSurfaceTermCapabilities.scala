package quasiquotes.phase148

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.util.{NoSource, SourceFile}

import quasiquotes.parser.TermShape

/** Tiny test-only associated-type algebra used to measure, not propose, a public API. */
private[quasiquotes] trait TermCapabilities:
  type Term

  def identifier(term: Term): Option[String]
  def select(term: Term): Option[(Term, String)]
  def application(term: Term): Option[(Term, List[Term])]
  def rebuildApplication(function: Term, arguments: List[Term]): Term
  def opaqueKind(term: Term): String

private[quasiquotes] object CrossSurfaceCallRewrite:
  def replaceFirstArgument(surface: TermCapabilities)(
      original: surface.Term,
      replacement: surface.Term
  ): Either[String, surface.Term] =
    surface.application(original) match
      case Some((function, _ :: tail)) =>
        Right(surface.rebuildApplication(function, replacement :: tail))
      case Some((_, Nil)) => Left("application has no argument to replace")
      case None => Left("term is not an application")

  def topology(surface: TermCapabilities)(term: surface.Term): List[String] =
    surface.identifier(term) match
      case Some(name) => List(s"Identifier($name)")
      case None =>
        surface.select(term) match
          case Some((qualifier, name)) =>
            s"Select($name)" :: topology(surface)(qualifier)
          case None =>
            surface.application(term) match
              case Some((function, arguments)) =>
                "Apply" :: (
                  topology(surface)(function) :::
                    arguments.flatMap(topology(surface))
                )
              case None => List(s"Opaque(${surface.opaqueKind(term)})")

private[quasiquotes] object NeutralTermCapabilities extends TermCapabilities:
  type Term = TermShape

  def identifier(term: Term): Option[String] =
    term match
      case TermShape.Identifier(name, _) => Some(name)
      case _ => None

  def select(term: Term): Option[(Term, String)] =
    term match
      case TermShape.Select(qualifier, name) => Some((qualifier, name))
      case _ => None

  def application(term: Term): Option[(Term, List[Term])] =
    term match
      case TermShape.Apply(function, arguments) => Some((function, arguments))
      case _ => None

  def rebuildApplication(function: Term, arguments: List[Term]): Term =
    TermShape.Apply(function, arguments)

  def opaqueKind(term: Term): String = term.getClass.getSimpleName
private[quasiquotes] object UntypedTermCapabilities extends TermCapabilities:
  type Term = untpd.Tree

  def identifier(term: Term): Option[String] =
    term match
      case untpd.Ident(name) => Some(name.toString)
      case _ => None

  def select(term: Term): Option[(Term, String)] =
    term match
      case untpd.Select(qualifier, name) => Some((qualifier, name.toString))
      case _ => None

  def application(term: Term): Option[(Term, List[Term])] =
    term match
      case untpd.Apply(function, arguments) => Some((function, arguments))
      case _ => None

  def rebuildApplication(function: Term, arguments: List[Term]): Term =
    given SourceFile = NoSource
    untpd.Apply(function, arguments)

  def opaqueKind(term: Term): String = term.getClass.getSimpleName
