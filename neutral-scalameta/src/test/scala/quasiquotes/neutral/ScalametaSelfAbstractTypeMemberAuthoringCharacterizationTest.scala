package quasiquotes.neutral

import scala.annotation.nowarn
import scala.meta.*

@nowarn("cat=deprecation")
final class ScalametaSelfAbstractTypeMemberAuthoringCharacterizationTest
    extends munit.FunSuite:
  test("fresh direct constructors expose the exact self-member topology"):
    val declaration = directDeclaration("Self", "self", "Nat")

    assertEquals(declaration.mods, Nil)
    assertEquals(declaration.name.value, "Self")
    assertEquals(declaration.tparamClause.values, Nil)
    assertEquals(declaration.bounds.context, Nil)
    assertEquals(declaration.bounds.view, Nil)
    declaration.bounds.lo match
      case Some(Type.Singleton(lower: Term.Name)) =>
        assertEquals(lower.value, "self")
      case other => fail(s"expected exact singleton lower bound, found $other")
    declaration.bounds.hi match
      case Some(Type.Refine(Some(base: Type.Name), List(alias: Defn.Type))) =>
        assertEquals(base.value, "Nat")
        assertEquals(alias.mods, Nil)
        assertEquals(alias.name.value, "Self")
        assertEquals(alias.tparamClause.values, Nil)
        assertEquals(alias.bounds.lo, None)
        assertEquals(alias.bounds.hi, None)
        assertEquals(alias.bounds.context, Nil)
        assertEquals(alias.bounds.view, Nil)
        alias.body match
          case Type.Select(prefix: Term.Name, selected: Type.Name) =>
            assertEquals(prefix.value, "self")
            assertEquals(selected.value, "Self")
          case other => fail(s"expected direct selected-Type RHS, found $other")
      case other => fail(s"expected exact refined upper bound, found $other")

  test("fresh peer-collision aliases retain their exact values in both Term roles"):
    List("self$1", "self$12").foreach { selfAlias =>
      val declaration = directDeclaration("Element", selfAlias, "Domain")
      val Type.Singleton(lower: Term.Name) = declaration.bounds.lo.get: @unchecked
      val Type.Refine(_, List(alias: Defn.Type)) = declaration.bounds.hi.get: @unchecked
      val Type.Select(prefix: Term.Name, _) = alias.body: @unchecked

      assertEquals(lower.value, selfAlias)
      assertEquals(prefix.value, selfAlias)
      val projected = project(declaration, "Element", selfAlias, "Domain")
      assertEquals(projected.sourceSpan, None)
      assertEquals(projected.plan.selfAlias.source, selfAlias)
      assertEquals(projected.plan.lowerBound.alias.source, selfAlias)
      assertEquals(projected.plan.upperBound.rhs.alias.source, selfAlias)
    }

  test("direct trees are wholly fresh while the projector remains structurally spelling based"):
    val crossNamespace = directDeclaration("Member", "Member", "ExternalBase")
    assert(allTrees(crossNamespace).forall(_.pos == Position.None))
    assertEquals(
      project(crossNamespace, "Member", "Member", "ExternalBase").sourceSpan,
      None
    )

    val capturedUpperBase = directDeclaration("Member", "self", "Member")
    assert(allTrees(capturedUpperBase).forall(_.pos == Position.None))
    assertEquals(
      project(capturedUpperBase, "Member", "self", "Member").sourceSpan,
      None
    )

  private def directDeclaration(
      memberName: String,
      selfAlias: String,
      upperBaseName: String
  ): Decl.Type =
    val refinementAlias = Defn.Type(
      Nil,
      Type.Name(memberName),
      Type.ParamClause(Nil),
      Type.Select(Term.Name(selfAlias), Type.Name(memberName)),
      Type.Bounds.empty
    )
    Decl.Type(
      Nil,
      Type.Name(memberName),
      Type.ParamClause(Nil),
      Type.Bounds(
        Some(Type.Singleton(Term.Name(selfAlias))),
        Some(
          Type.Refine(
            Some(Type.Name(upperBaseName)),
            Stat.Block(List(refinementAlias))
          )
        ),
        Nil,
        Nil
      )
    )

  private def project(
      declaration: Decl.Type,
      memberName: String,
      selfAlias: String,
      upperBaseName: String
  ): ProjectedSelfAbstractTypeMember =
    ScalametaSelfAbstractTypeMemberProjection
      .project(declaration, memberName, selfAlias, upperBaseName)
      .fold(problem => fail(problem.message), identity)

  private def allTrees(root: Tree): List[Tree] =
    root :: root.children.toList.flatMap(allTrees)
