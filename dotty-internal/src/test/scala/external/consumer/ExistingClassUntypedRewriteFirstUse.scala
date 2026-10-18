package external.consumer

// snippet:existing-class-untyped-rewrite-first-use:start
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context

import quasiquotes.definitions.{DefinitionName, SemanticDefinition}
import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite
import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite.{Failure, MemberKind, Result}
import quasiquotes.parser.TermShape
import quasiquotes.types.{
  ResolvedTypeNameId,
  ResolvedTypeOwnerKind,
  ResolvedTypeOwnerSegment,
  TypeNormalForm
}

object ExistingClassUntypedRewriteFirstUse:
  private val IntType = TypeNormalForm.STypeIdent("Int")
  val canonicalOptionString: TypeNormalForm =
    val pkg = (name: String) =>
      ResolvedTypeOwnerSegment(ResolvedTypeOwnerKind.Package, name)
    TypeNormalForm.STypeApply(
      TypeNormalForm.STypeResolved(
        ResolvedTypeNameId(Vector(pkg("scala")), "Option")
      ),
      List(
        TypeNormalForm.STypeResolved(
          ResolvedTypeNameId(Vector(pkg("java"), pkg("lang")), "String")
        )
      )
    )

  private val GeneratedSibling =
    val name = DefinitionName.fromSource("bonus")
      .fold(error => throw new IllegalArgumentException(error.message), identity)
    SemanticDefinition.concreteMethod(name, Vector.empty, IntType)(
      _ => Right(TermShape.Literal("5"))
    ).fold(error => throw new IllegalArgumentException(error.message), identity)

  def rewrite(existingClass: untpd.Tree)(using Context): Either[Failure, Result] =
    for
      captured <- ExistingClassUntypedRewrite.capture(existingClass)
      member <- captured.member(0)
      method <- captured.method(member.ref)
      parameter = method.parameterClauses.head.parameters.head
      reference <- method.parameterScope.reference(parameter.ref)
      body = TermShape.Apply(
        TermShape.Select(TermShape.Identifier("Math", false), "abs"),
        List(reference)
      )
      p1 <- captured.emptyPlan.replaceParameterType(parameter.ref, IntType)
      p2 <- p1.replaceResultType(method.ref, IntType)
      p3 <- p2.replaceBody(method.ref, body)
      p4 <- p3.append(GeneratedSibling, "generated/ExistingClassBonus.scala")
      result <- ExistingClassUntypedRewrite(captured, p4)
    yield result

  def rewriteCanonicalOption(existingClass: untpd.Tree)(using Context)
      : Either[Failure, Result] =
    for
      captured <- ExistingClassUntypedRewrite.capture(existingClass)
      candidates = captured.members.filter(_.kind == MemberKind.Method)
      _ <- Either.cond(
        candidates.size == 1,
        (),
        Failure(
          "METHOD_SELECTION_FAILED",
          s"expected exactly one direct method; found ${candidates.size}"
        )
      )
      method <- captured.method(candidates.head.ref)
      parameter = method.parameterClauses.head.parameters.head
      reference <- method.parameterScope.reference(parameter.ref)
      p1 <- captured.emptyPlan.replaceParameterType(parameter.ref, canonicalOptionString)
      p2 <- p1.replaceResultType(method.ref, canonicalOptionString)
      p3 <- p2.replaceBody(method.ref, reference)
      result <- ExistingClassUntypedRewrite(captured, p3)
    yield result

  def contextualClauseKinds(existingClass: untpd.Tree)(using Context)
      : Either[Failure, (Vector[(String, Int)], Vector[(String, Int)])] =
    for
      captured <- ExistingClassUntypedRewrite.capture(existingClass)
      contextualMember <- captured.member(0)
      contextualMethod <- captured.method(contextualMember.ref)
      mixedMember <- captured.member(1)
      mixedMethod <- captured.method(mixedMember.ref)
    yield
      val contextual = contextualMethod.parameterClauses.zipWithIndex.map:
        (clause, clauseIndex) => clause.kind.code -> clauseIndex
      val mixed = mixedMethod.parameterClauses.zipWithIndex.map:
        (clause, clauseIndex) => clause.kind.code -> clauseIndex
      contextual -> mixed
// snippet:existing-class-untyped-rewrite-first-use:end
