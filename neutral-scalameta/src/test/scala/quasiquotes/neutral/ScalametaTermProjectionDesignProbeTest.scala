package quasiquotes.neutral

import scala.meta.*
import scala.meta.dialects.Scala3

import _root_.quasiquotes.parser.TermShape

/** Test-only Phase-131 probe. This is not a production projector API. */
final class ScalametaTermProjectionDesignProbeTest extends munit.FunSuite:
  test("literal infix projection reaches the existing public TermShape"):
    val source = q"1 + 1"

    assertEquals(
      TestOnlyScalametaTermProjection.project(source),
      Right(
        TermShape.Infix(
          TermShape.Literal("1"),
          "+",
          TermShape.Literal("1")
        )
      )
    )

  test("select, apply, and one-list new need no Quotes or Dotty value"):
    val selected = q"receiver.member"
    val applied = q"function(1, 2)"
    val fresh = q"new java.lang.StringBuilder(16)"

    assertEquals(
      TestOnlyScalametaTermProjection.project(selected),
      Right(TermShape.Select(TermShape.Identifier("receiver", false), "member"))
    )
    assertEquals(
      TestOnlyScalametaTermProjection.project(applied),
      Right(
        TermShape.Apply(
          TermShape.Identifier("function", false),
          List(TermShape.Literal("1"), TermShape.Literal("2"))
        )
      )
    )
    assertEquals(
      TestOnlyScalametaTermProjection.project(fresh),
      Right(
        TermShape.New(
          "java.lang.StringBuilder",
          List(TermShape.Literal("16"))
        )
      )
    )

private object TestOnlyScalametaTermProjection:
  def project(term: Term): Either[String, TermShape] =
    term match
      case name: Term.Name =>
        Right(TermShape.Identifier(name.value, isPlaceholder = false))
      case Lit.Int(value) =>
        Right(TermShape.Literal(value.toString))
      case select: Term.Select =>
        project(select.qual).map(TermShape.Select(_, select.name.value))
      case application: Term.Apply =>
        for
          function <- project(application.fun)
          arguments <- traverse(application.args)(project)
        yield TermShape.Apply(function, arguments)
      case infix: Term.ApplyInfix if infix.argClause.values.size == 1 =>
        for
          left <- project(infix.lhs)
          right <- project(infix.argClause.values.head)
        yield TermShape.Infix(left, infix.op.value, right)
      case fresh: Term.New if fresh.init.argss.size == 1 =>
        for
          constructor <- constructorName(fresh.init.tpe)
          arguments <- traverse(fresh.init.argss.head)(project)
        yield TermShape.New(constructor, arguments)
      case other =>
        Left(s"unsupported test-only Scalameta node: ${other.productPrefix}")

  private def constructorName(tpe: Type): Either[String, String] =
    tpe match
      case name: Type.Name => Right(name.value)
      case select: Type.Select =>
        constructorQualifier(select.qual)
          .map(prefix => s"$prefix.${select.name.value}")
      case other => Left(s"unsupported constructor type: ${other.productPrefix}")

  private def constructorQualifier(term: Term): Either[String, String] =
    term match
      case name: Term.Name => Right(name.value)
      case select: Term.Select =>
        constructorQualifier(select.qual)
          .map(prefix => s"$prefix.${select.name.value}")
      case other => Left(s"unsupported constructor qualifier: ${other.productPrefix}")

  private def traverse[A, B](
      values: List[A]
  )(projectValue: A => Either[String, B]): Either[String, List[B]] =
    values.foldRight(Right(Nil): Either[String, List[B]]) { (value, rest) =>
      for
        head <- projectValue(value)
        tail <- rest
      yield head :: tail
    }
