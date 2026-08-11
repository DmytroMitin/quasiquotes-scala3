package quasiquotes.phase74

import scala.quoted.*

private[phase74] final case class TypedScopeProbeEvidence(
    source: String,
    rootKind: String,
    treeStructure: String,
    lambdaCount: Int,
    lambdaParameterNames: List[String],
    lambdaParameterTypes: List[String],
    lambdaBodyKinds: List[String],
    lambdaParameterOwnersAreMethods: List[Boolean],
    boundReferenceNames: List[String],
    freeReferenceNames: List[String],
    sameNameShadowingUsesDistinctSymbols: Boolean,
    regularBlockCount: Int,
    blockStatKinds: List[String],
    blockResultKinds: List[String],
    localValueNames: List[String],
    localDefNames: List[String],
    localBinderReferenceNames: List[String],
    localBinderOwnersExist: List[Boolean],
    sourceSpans: List[String]
)

private[phase74] object TypedBlockLambdaProbe:
  inline def inspect[A](inline expression: A): TypedScopeProbeEvidence =
    ${ inspectImpl('expression) }

  inline def hygienicAdder(inline external: Int): Int => Int =
    ${ hygienicAdderImpl('external) }

  private def inspectImpl[A: Type](expression: Expr[A])(using Quotes): Expr[TypedScopeProbeEvidence] =
    import quotes.reflect.*
    import scala.collection.mutable.ListBuffer

    final case class LambdaData(whole: Block, parameters: List[ValDef], body: Term)
    final case class BlockData(whole: Block, statements: List[Statement], result: Term)

    def unwrap(term: Term): Term =
      term match
        case Inlined(_, _, inner) => unwrap(inner)
        case other => other

    def kind(tree: Tree): String = tree.getClass.getSimpleName.stripSuffix("$")

    def span(label: String, tree: Tree): String =
      val position = tree.pos
      val source = position.sourceCode.getOrElse("<none>").replace("\n", "\\n")
      s"$label:${position.start}..${position.end}:$source"

    val lambdas = ListBuffer.empty[LambdaData]
    val regularBlocks = ListBuffer.empty[BlockData]
    val discovery = new TreeTraverser:
      override def traverseTree(tree: Tree)(owner: Symbol): Unit =
        tree match
          case block: Block =>
            Lambda.unapply(block) match
              case Some((parameters, body)) =>
                lambdas += LambdaData(block, parameters, body)
              case None =>
                regularBlocks += BlockData(block, block.statements, block.expr)
          case _ => ()
        super.traverseTree(tree)(owner)
    discovery.traverseTree(expression.asTerm)(Symbol.spliceOwner)

    val lambdaBinders = lambdas.toList.flatMap(_.parameters).map(_.symbol)
    val localDefinitions = regularBlocks.toList.flatMap(_.statements).collect {
      case definition: ValDef => definition.symbol
      case definition: DefDef => definition.symbol
    }
    val boundReferences = ListBuffer.empty[String]
    val freeReferences = ListBuffer.empty[String]
    val localReferences = ListBuffer.empty[String]
    val referenceDiscovery = new TreeTraverser:
      override def traverseTree(tree: Tree)(owner: Symbol): Unit =
        tree match
          case reference @ Ident(name) if reference.symbol.exists =>
            if lambdaBinders.contains(reference.symbol) then boundReferences += name
            else if localDefinitions.contains(reference.symbol) then localReferences += name
            else if name == "free" then freeReferences += name
          case reference @ Select(_, name) if reference.symbol.exists && name == "free" =>
            freeReferences += name
          case _ => ()
        super.traverseTree(tree)(owner)
    referenceDiscovery.traverseTree(expression.asTerm)(Symbol.spliceOwner)

    val sameNameGroups = lambdas.toList.flatMap(_.parameters).groupBy(_.name).values
    val shadowingDistinct = sameNameGroups
      .filter(_.size > 1)
      .forall(group => group.map(_.symbol).distinct.size == group.size)

    val lambdaSpans = lambdas.toList.flatMap { lambda =>
      span("lambda", lambda.whole) ::
        lambda.parameters.zipWithIndex.map { case (parameter, index) =>
          span(s"lambda-param-$index", parameter)
        } ::: List(span("lambda-body", lambda.body))
    }
    val blockSpans = regularBlocks.toList.flatMap { block =>
      span("block", block.whole) ::
        block.statements.zipWithIndex.map { case (statement, index) =>
          span(s"block-stat-$index", statement)
        } ::: List(span("block-result", block.result))
    }

    def strings(values: List[String]): Expr[List[String]] =
      Expr.ofList(values.map(Expr(_)))

    def booleans(values: List[Boolean]): Expr[List[Boolean]] =
      Expr.ofList(values.map(Expr(_)))

    val stripped = unwrap(expression.asTerm)
    val source = expression.asTerm.pos.sourceCode.getOrElse("<none>")
    val structure = expression.asTerm.show(using Printer.TreeStructure)
    val lambdaList = lambdas.toList
    val blockList = regularBlocks.toList
    val parameterList = lambdaList.flatMap(_.parameters)
    val localValues = blockList.flatMap(_.statements).collect { case definition: ValDef => definition }
    val localDefs = blockList.flatMap(_.statements).collect { case definition: DefDef => definition }

    '{
      TypedScopeProbeEvidence(
        ${ Expr(source) },
        ${ Expr(kind(stripped)) },
        ${ Expr(structure) },
        ${ Expr(lambdaList.size) },
        ${ strings(parameterList.map(_.name)) },
        ${ strings(parameterList.map(_.tpt.tpe.show)) },
        ${ strings(lambdaList.map(lambda => kind(lambda.body))) },
        ${ booleans(parameterList.map(parameter => parameter.symbol.owner.flags.is(Flags.Method))) },
        ${ strings(boundReferences.toList) },
        ${ strings(freeReferences.toList.distinct) },
        ${ Expr(shadowingDistinct) },
        ${ Expr(blockList.size) },
        ${ strings(blockList.flatMap(_.statements).map(kind)) },
        ${ strings(blockList.map(block => kind(block.result))) },
        ${ strings(localValues.map(_.name)) },
        ${ strings(localDefs.map(_.name)) },
        ${ strings(localReferences.toList) },
        ${ booleans((localValues.map(_.symbol) ++ localDefs.map(_.symbol)).map(_.owner.exists)) },
        ${ strings(lambdaSpans ++ blockSpans) }
      )
    }

  private def hygienicAdderImpl(external: Expr[Int])(using Quotes): Expr[Int => Int] =
    import quotes.reflect.*

    val methodType = MethodType(List("x"))(
      _ => List(TypeRepr.of[Int]),
      _ => TypeRepr.of[Int]
    )
    Lambda(
      owner = Symbol.spliceOwner,
      tpe = methodType,
      rhsFn = (_, parameters) =>
        Select.overloaded(
          external.asTerm,
          "+",
          Nil,
          List(parameters.head.asInstanceOf[Term]),
          TypeRepr.of[Int]
        )
    ).asExprOf[Int => Int]
