package external.consumer

// snippet:existing-class-untyped-rewrite-first-use:start
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Contexts.Context

import quasiquotes.definitions.{DefinitionName, SemanticDefinition}
import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite
import quasiquotes.definitions.dotty.ExistingClassUntypedRewrite.{Failure, Result}
import quasiquotes.parser.TermShape
import quasiquotes.types.TypeNormalForm

object ExistingClassUntypedRewriteFirstUse:
  private val IntType = TypeNormalForm.STypeIdent("Int")
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
// snippet:existing-class-untyped-rewrite-first-use:end
