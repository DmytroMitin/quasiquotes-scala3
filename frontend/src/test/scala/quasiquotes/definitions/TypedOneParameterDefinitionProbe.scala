package quasiquotes.definitions

import scala.quoted.*

private[definitions] final case class TypedOneParameterDefinitionEvidence(
    name: String,
    parameterClauseSizes: List[Int],
    parameterNames: List[String],
    parameterTypes: List[String],
    resultType: String,
    bodyKind: String,
    parameterOwnersAreDefinition: List[Boolean],
    boundReferenceNames: List[String],
    boundReferencesUseParameterSymbol: Boolean,
    treeStructure: String
) derives CanEqual

private[definitions] object TypedOneParameterDefinitionProbe:
  inline def inspect[A](inline expression: A): List[TypedOneParameterDefinitionEvidence] =
    ${ inspectImpl('expression) }

  private def inspectImpl[A: Type](
      expression: Expr[A]
  )(using Quotes): Expr[List[TypedOneParameterDefinitionEvidence]] =
    import quotes.reflect.*
    import scala.collection.mutable.ListBuffer

    def kind(tree: Tree): String =
      tree.getClass.getSimpleName.stripSuffix("$")

    val definitions = ListBuffer.empty[DefDef]
    val discovery = new TreeTraverser:
      override def traverseTree(tree: Tree)(owner: Symbol): Unit =
        tree match
          case definition: DefDef
              if definition.name == "id" || definition.name == "inc" =>
            definitions += definition
          case _ => ()
        super.traverseTree(tree)(owner)
    discovery.traverseTree(expression.asTerm)(Symbol.spliceOwner)

    def evidence(definition: DefDef): Expr[TypedOneParameterDefinitionEvidence] =
      val clauses = definition.termParamss
      val parameters = clauses.flatMap(_.params)
      val parameterSymbols = parameters.map(_.symbol)
      val references = ListBuffer.empty[Ident]
      val referenceDiscovery = new TreeTraverser:
        override def traverseTree(tree: Tree)(owner: Symbol): Unit =
          tree match
            case reference: Ident if parameterSymbols.contains(reference.symbol) =>
              references += reference
            case _ => ()
          super.traverseTree(tree)(owner)
      definition.rhs.foreach(referenceDiscovery.traverseTree(_)(definition.symbol))

      val clauseSizes = clauses.map(_.params.size)
      val parameterNames = parameters.map(_.name)
      val parameterTypes = parameters.map(_.tpt.tpe.show)
      val owners = parameters.map(_.symbol.owner == definition.symbol)
      val referenceNames = references.toList.map(_.name)
      val allReferencesLinked =
        references.nonEmpty && references.forall(reference =>
          parameterSymbols.contains(reference.symbol)
        )
      val bodyKind = definition.rhs.fold("<none>")(kind)

      '{
        TypedOneParameterDefinitionEvidence(
          ${ Expr(definition.name) },
          ${ Expr.ofList(clauseSizes.map(size => Expr(size))) },
          ${ Expr.ofList(parameterNames.map(Expr(_))) },
          ${ Expr.ofList(parameterTypes.map(Expr(_))) },
          ${ Expr(definition.returnTpt.tpe.show) },
          ${ Expr(bodyKind) },
          ${ Expr.ofList(owners.map(Expr(_))) },
          ${ Expr.ofList(referenceNames.map(Expr(_))) },
          ${ Expr(allReferencesLinked) },
          ${ Expr(definition.show(using Printer.TreeStructure)) }
        )
      }

    Expr.ofList(definitions.toList.map(evidence))
