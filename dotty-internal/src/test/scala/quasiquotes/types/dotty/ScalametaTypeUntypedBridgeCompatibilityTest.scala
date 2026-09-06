package quasiquotes.types.dotty

import dotty.tools.dotc.ast.untpd

import quasiquotes.neutral.ScalametaTypeNormalFormProjection
import quasiquotes.terms.dotty.CompletedTypeUntypedLowerer

import scala.meta.*
import scala.meta.dialects.Scala3

final class ScalametaTypeUntypedBridgeCompatibilityTest extends munit.FunSuite:
  test("candidate bridge remains exactly equivalent to the pre-C030 composition"):
    val sources = List(
      "Int",
      "String",
      "Boolean",
      "List[Int]",
      "Option[(Int, String)]",
      "List[Int => String]",
      "Either[(Int, String), Boolean => Int]",
      "(Int, String, Boolean)",
      "(Int, String) => Boolean",
      "AnyVal",
      "Long",
      "pkg.Type",
      "pkg.List[Int]",
      "Map[Int, String]",
      "(Int, String, Boolean, Int)",
      "(Int, String, Boolean) => Int",
      "Tuple2[Int, String]",
      "Function1[Int, String]",
      "Function2[Int, String, Boolean]",
      "Int { type Out = String }",
      "Int | String"
    )

    val inputs: List[Type] = null :: sources.map(parseType)
    inputs.foreach { sourceType =>
      assertEquals(
        snapshot(ScalametaTypeUntypedBridge.lower(sourceType)),
        snapshot(legacyLower(sourceType)),
        clues(sourceType)
      )
    }

  private def legacyLower(
      sourceType: Type
  ): Either[ScalametaTypeUntypedBridge.Failure, untpd.Tree] =
    Option(sourceType)
      .toRight(
        ScalametaTypeUntypedBridge.Failure(
          "MISSING_INPUT",
          "the Scalameta Type must be present."
        )
      )
      .flatMap(present =>
        ScalametaTypeNormalFormProjection
          .project(present)
          .left
          .map(problem =>
            ScalametaTypeUntypedBridge.Failure(
              "NEUTRAL_PROJECTION_FAILED",
              s"${problem.code}: ${problem.detail}"
            )
          )
      )
      .flatMap(projected =>
        CompletedTypeUntypedLowerer
          .lower(projected.normalForm)
          .left
          .map(problem =>
            ScalametaTypeUntypedBridge.Failure(
              "EXACT_LOWERING_FAILED",
              problem.message
            )
          )
      )

  private def snapshot(
      result: Either[ScalametaTypeUntypedBridge.Failure, untpd.Tree]
  ): Either[(String, String), String] =
    result.left.map(problem => problem.code -> problem.detail).map(topology)

  private def topology(tree: untpd.Tree): String =
    tree match
      case untpd.Ident(name) => s"Ident($name)"
      case untpd.AppliedTypeTree(constructor, arguments) =>
        s"Applied(${topology(constructor)},[${arguments.map(topology).mkString(",")}])"
      case untpd.Tuple(elements) =>
        s"Tuple([${elements.map(topology).mkString(",")}])"
      case untpd.Function(arguments, result) =>
        s"Function([${arguments.map(topology).mkString(",")}],${topology(result)})"
      case other => s"Unexpected(${other.getClass.getName}:$other)"

  private def parseType(source: String): Type =
    Scala3(source).parse[Type].get
