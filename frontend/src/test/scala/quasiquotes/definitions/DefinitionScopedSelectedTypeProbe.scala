package quasiquotes.definitions

import quasiquotes.parser.BinderId
import quasiquotes.types.ResolvedTypeReflection

import scala.quoted.*

private[definitions] final case class DefinitionScopedSelectedTypeEvidence(
    contextualAccepted: Boolean,
    ordinaryAccepted: Boolean,
    alphaRenamed: Boolean,
    rebuiltContextualAccepted: Boolean,
    rebuiltOrdinaryAccepted: Boolean,
    differentPrefixCode: Option[String],
    differentDeclarationCode: Option[String],
    externalPrefixCode: Option[String],
    nestedPrefixCode: Option[String]
) derives CanEqual

private[definitions] object DefinitionScopedSelectedTypeProbe:
  inline def inspect[A](inline expression: A): DefinitionScopedSelectedTypeEvidence =
    ${ inspectImpl('expression) }

  private def inspectImpl[A: Type](
      expression: Expr[A]
  )(using Quotes): Expr[DefinitionScopedSelectedTypeEvidence] =
    import quotes.reflect.*
    import scala.collection.mutable.ListBuffer

    val admittedNames = Set(
      "contextual",
      "renamedContextual",
      "ordinary",
      "firstPrefix",
      "otherDeclaration",
      "externalPrefix",
      "nestedPrefix"
    )
    val discovered = ListBuffer.empty[DefDef]
    val traversal = new TreeTraverser:
      override def traverseTree(tree: Tree)(owner: Symbol): Unit =
        tree match
          case definition: DefDef if admittedNames(definition.name) =>
            discovered += definition
          case _ => ()
        super.traverseTree(tree)(owner)
    traversal.traverseTree(expression.asTerm)(Symbol.spliceOwner)

    val definitions = discovered.toList.map(value => value.name -> value).toMap
    def definition(name: String): DefDef =
      definitions.getOrElse(name, report.errorAndAbort(s"Missing selected-Type probe definition `$name`."))

    def selectedTarget(value: DefDef): TypeRef =
      value.returnTpt.tpe match
        case selected: TypeRef => selected
        case other =>
          report.errorAndAbort(
            s"Expected selected TypeRef result for `${value.name}`, found `${other.getClass.getSimpleName}`."
          )

    def memberId(target: TypeRef) =
      ResolvedTypeReflection
        .deriveFromOwner(target.typeSymbol.owner, target.name)
        .fold(error => report.errorAndAbort(error.message), identity)

    def plan(
        value: DefDef,
        prefixPosition: Int,
        expectedMember: TypeRef
    ): DefinitionScopedSelectedTypePlan =
      val binders = value.termParamss.flatMap(_.params).indices.map(BinderId(_)).toVector
      DefinitionScopedSelectedTypePlan
        .create(binders, binders(prefixPosition), memberId(expectedMember))
        .fold(error => report.errorAndAbort(error.message), identity)

    def code(result: Either[DefinitionScopedSelectedTypeReflection.Error, Unit]): Option[String] =
      result.left.toOption.map(_.code)

    val contextual = definition("contextual")
    val renamed = definition("renamedContextual")
    val ordinary = definition("ordinary")
    val firstPrefix = definition("firstPrefix")
    val otherDeclaration = definition("otherDeclaration")
    val externalPrefix = definition("externalPrefix")
    val nestedPrefix = definition("nestedPrefix")

    val contextualTarget = selectedTarget(contextual)
    val renamedTarget = selectedTarget(renamed)
    val ordinaryTarget = selectedTarget(ordinary)
    val firstPrefixTarget = selectedTarget(firstPrefix)
    val otherDeclarationTarget = selectedTarget(otherDeclaration)

    val contextualPlan = plan(contextual, 0, contextualTarget)
    val renamedPlan = plan(renamed, 0, renamedTarget)
    val ordinaryPlan = plan(ordinary, 0, ordinaryTarget)
    val wrongPrefixPlan = plan(firstPrefix, 1, firstPrefixTarget)
    val wrongDeclarationPlan = plan(otherDeclaration, 0, contextualTarget)
    val externalPlan = plan(externalPrefix, 0, contextualTarget)
    val nestedPlan = plan(nestedPrefix, 0, contextualTarget)

    val contextualAccepted =
      DefinitionScopedSelectedTypeReflection
        .inspect(contextual, contextualPlan, contextualTarget)
        .isRight
    val ordinaryAccepted =
      DefinitionScopedSelectedTypeReflection
        .inspect(ordinary, ordinaryPlan, ordinaryTarget)
        .isRight

    val rebuiltContextual = DefinitionScopedSelectedTypeReflection
      .rebuild(contextual, contextualPlan, contextualTarget.typeSymbol)
      .flatMap(value =>
        DefinitionScopedSelectedTypeReflection.inspect(
          contextual,
          contextualPlan,
          value
        )
      )
      .isRight
    val rebuiltOrdinary = DefinitionScopedSelectedTypeReflection
      .rebuild(ordinary, ordinaryPlan, ordinaryTarget.typeSymbol)
      .flatMap(value =>
        DefinitionScopedSelectedTypeReflection.inspect(
          ordinary,
          ordinaryPlan,
          value
        )
      )
      .isRight

    val differentPrefixCode = code(
        DefinitionScopedSelectedTypeReflection.inspect(
          firstPrefix,
          wrongPrefixPlan,
          firstPrefixTarget
        )
      )
    val differentDeclarationCode = code(
        DefinitionScopedSelectedTypeReflection.inspect(
          otherDeclaration,
          wrongDeclarationPlan,
          otherDeclarationTarget
        )
      )
    val externalPrefixCode = code(
        DefinitionScopedSelectedTypeReflection.inspect(
          externalPrefix,
          externalPlan,
          selectedTarget(externalPrefix)
        )
      )
    val nestedPrefixCode = code(
        DefinitionScopedSelectedTypeReflection.inspect(
          nestedPrefix,
          nestedPlan,
          selectedTarget(nestedPrefix)
        )
      )

    '{
      DefinitionScopedSelectedTypeEvidence(
        ${ Expr(contextualAccepted) },
        ${ Expr(ordinaryAccepted) },
        ${ Expr(contextualPlan.alphaEquivalentTo(renamedPlan)) },
        ${ Expr(rebuiltContextual) },
        ${ Expr(rebuiltOrdinary) },
        ${ Expr(differentPrefixCode) },
        ${ Expr(differentDeclarationCode) },
        ${ Expr(externalPrefixCode) },
        ${ Expr(nestedPrefixCode) }
      )
    }
