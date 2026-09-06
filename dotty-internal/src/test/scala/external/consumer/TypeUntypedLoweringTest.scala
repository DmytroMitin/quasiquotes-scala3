package external.consumer

import dotty.tools.dotc.ast.untpd
import quasiquotes.types.{
  ResolvedTypeNameId,
  ResolvedTypeOwnerKind,
  ResolvedTypeOwnerSegment,
  TypeNormalForm
}
import quasiquotes.types.TypeNormalForm.*
import quasiquotes.types.dotty.TypeUntypedLowering

final class TypeUntypedLoweringTest extends munit.FunSuite:
  private val resolvedInt = ResolvedTypeNameId(
    Vector(ResolvedTypeOwnerSegment(ResolvedTypeOwnerKind.Package, "scala")),
    "Int"
  )

  test("external consumer lowers the complete context-free semantic Type intersection"):
    val fixtures = List(
      STypeIdent("Int") -> "Ident(Int)",
      STypeIdent("String") -> "Ident(String)",
      STypeIdent("Boolean") -> "Ident(Boolean)",
      STypeApply(STypeIdent("List"), List(STypeIdent("Int"))) ->
        "Applied(Ident(List),[Ident(Int)])",
      STypeApply(STypeIdent("Option"), List(STypeIdent("String"))) ->
        "Applied(Ident(Option),[Ident(String)])",
      STypeApply(
        STypeIdent("Either"),
        List(STypeIdent("Int"), STypeIdent("String"))
      ) -> "Applied(Ident(Either),[Ident(Int),Ident(String)])",
      STypeTuple(List(STypeIdent("Int"), STypeIdent("String"))) ->
        "Tuple([Ident(Int),Ident(String)])",
      STypeTuple(
        List(STypeIdent("Int"), STypeIdent("String"), STypeIdent("Boolean"))
      ) -> "Tuple([Ident(Int),Ident(String),Ident(Boolean)])",
      STypeFunction(List(STypeIdent("Int")), STypeIdent("String")) ->
        "Function([Ident(Int)],Ident(String))",
      STypeFunction(
        List(STypeIdent("Int"), STypeIdent("String")),
        STypeIdent("Boolean")
      ) -> "Function([Ident(Int),Ident(String)],Ident(Boolean))",
      STypeApply(
        STypeIdent("List"),
        List(STypeApply(STypeIdent("Option"), List(STypeIdent("Int"))))
      ) -> "Applied(Ident(List),[Applied(Ident(Option),[Ident(Int)])])",
      STypeApply(
        STypeIdent("Either"),
        List(
          STypeApply(STypeIdent("List"), List(STypeIdent("Int"))),
          STypeTuple(List(STypeIdent("String"), STypeIdent("Boolean")))
        )
      ) ->
        "Applied(Ident(Either),[Applied(Ident(List),[Ident(Int)]),Tuple([Ident(String),Ident(Boolean)])])",
      STypeFunction(
        List(
          STypeTuple(List(STypeIdent("Int"), STypeIdent("String")))
        ),
        STypeApply(
          STypeIdent("Option"),
          List(STypeFunction(List(STypeIdent("Boolean")), STypeIdent("Int")))
        )
      ) ->
        "Function([Tuple([Ident(Int),Ident(String)])],Applied(Ident(Option),[Function([Ident(Boolean)],Ident(Int))]))"
    )

    fixtures.foreach { case (semantic, expectedTopology) =>
      val raw = TypeUntypedLowering
        .lower(semantic)
        .fold(problem => fail(problem.message), identity)
      assertEquals(topology(raw), expectedTopology, clues(semantic))
      allTrees(raw).foreach { node =>
        assert(!node.source.exists, clues(semantic, node))
        assert(!node.span.exists, clues(semantic, node))
        assert(!node.isInstanceOf[untpd.TypedSplice], clues(semantic, node))
      }
    }

  test("external consumer receives stable unsupported classification"):
    val unsupported = List(
      STypeIdent("AnyVal"),
      STypeResolved(resolvedInt),
      STypeApply(STypeResolved(resolvedInt), List(STypeIdent("Int"))),
      STypeIdent("Long"),
      STypeApply(STypeIdent("Map"), List(STypeIdent("Int"), STypeIdent("String"))),
      STypeApply(STypeIdent("List"), Nil),
      STypeApply(STypeIdent("Option"), List(STypeIdent("Int"), STypeIdent("String"))),
      STypeApply(STypeIdent("Either"), List(STypeIdent("Int"))),
      STypeTuple(Nil),
      STypeTuple(List(STypeIdent("Int"))),
      STypeTuple(List.fill(4)(STypeIdent("Int"))),
      STypeFunction(Nil, STypeIdent("Int")),
      STypeFunction(List.fill(3)(STypeIdent("Int")), STypeIdent("Int"))
    )

    unsupported.foreach { semantic =>
      val failure = lowerFailure(semantic)
      assertEquals(failure.code, "UNSUPPORTED_SEMANTIC_VALUE", clues(semantic, failure))
      assert(failure.detail.nonEmpty, clues(semantic, failure))
    }

  test("external consumer receives stable malformed classification"):
    val malformed = List[TypeNormalForm](
      STypeIdent(null),
      STypeResolved(null),
      STypeApply(null, List(STypeIdent("Int"))),
      STypeApply(STypeIdent("List"), null),
      STypeApply(STypeIdent("List"), List(null)),
      STypeTuple(null),
      STypeTuple(List(STypeIdent("Int"), null)),
      STypeFunction(null, STypeIdent("Int")),
      STypeFunction(List(null), STypeIdent("Int")),
      STypeFunction(List(STypeIdent("Int")), null),
      STypeApply(
        STypeIdent("Option"),
        List(STypeFunction(List(STypeIdent("Int")), null))
      )
    )

    val missing = lowerFailure(null)
    assertEquals(missing.code, "MISSING_INPUT")
    assertEquals(missing.message, s"${missing.code}: ${missing.detail}")

    malformed.foreach { semantic =>
      val failure = lowerFailure(semantic)
      assertEquals(failure.code, "MALFORMED_SEMANTIC_VALUE", clues(semantic, failure))
      assert(failure.detail.nonEmpty, clues(semantic, failure))
    }

  private def lowerFailure(
      semantic: TypeNormalForm
  ): TypeUntypedLowering.Failure =
    TypeUntypedLowering
      .lower(semantic)
      .left
      .toOption
      .getOrElse(fail(s"semantic Type unexpectedly lowered: $semantic"))

  private def topology(tree: untpd.Tree): String =
    tree match
      case untpd.Ident(name) => s"Ident($name)"
      case untpd.AppliedTypeTree(constructor, arguments) =>
        s"Applied(${topology(constructor)},[${arguments.map(topology).mkString(",")}])"
      case untpd.Tuple(elements) =>
        s"Tuple([${elements.map(topology).mkString(",")}])"
      case untpd.Function(arguments, result) =>
        s"Function([${arguments.map(topology).mkString(",")}],${topology(result)})"
      case other => s"Unexpected(${other.getClass.getName})"

  private def allTrees(tree: untpd.Tree): List[untpd.Tree] =
    tree match
      case untpd.AppliedTypeTree(constructor, arguments) =>
        tree :: allTrees(constructor) ::: arguments.flatMap(allTrees)
      case untpd.Tuple(elements) => tree :: elements.flatMap(allTrees)
      case untpd.Function(arguments, result) =>
        tree :: arguments.flatMap(allTrees) ::: allTrees(result)
      case _ => tree :: Nil
