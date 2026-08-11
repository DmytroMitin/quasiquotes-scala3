package quasiquotes.matching

import scala.quoted.*

private[quasiquotes] final case class ConstructorNewTypedEvidence(
    sourceCode: Option[String],
    treeStructure: String,
    recursiveShape: String,
    constructorClassName: String,
    constructorSymbolFullName: String,
    resultType: String,
    argumentCount: Int,
    hasGenuineNew: Boolean,
    hasInitSelect: Boolean
)

private[quasiquotes] object ConstructorNewTypedPreflight:
  inline def inspect[A](inline expression: A): ConstructorNewTypedEvidence =
    ${ inspectImpl('expression) }

  private def inspectImpl[A: Type](expression: Expr[A])(using Quotes): Expr[ConstructorNewTypedEvidence] =
    import quotes.reflect.*

    final case class ConstructorFacts(
        className: String,
        symbolName: String,
        resultType: String,
        argumentCount: Int
    )

    def unwrap(term: Term): Term =
      term match
        case Inlined(_, _, inner) => unwrap(inner)
        case Block(Nil, inner) => unwrap(inner)
        case other => other

    def constructorFacts(term: Term): Option[ConstructorFacts] =
      unwrap(term) match
        case application @ Apply(selection @ Select(created @ New(typeTree), "<init>"), arguments) =>
          Some(
            ConstructorFacts(
              typeTree.tpe.typeSymbol.fullName,
              selection.symbol.fullName,
              application.tpe.show,
              arguments.size
            )
          )
        case application @ Apply(
              TypeApply(selection @ Select(created @ New(typeTree), "<init>"), _),
              arguments
            ) =>
          Some(
            ConstructorFacts(
              typeTree.tpe.typeSymbol.fullName,
              selection.symbol.fullName,
              application.tpe.show,
              arguments.size
            )
          )
        case Apply(function, arguments) =>
          constructorFacts(function).orElse(arguments.iterator.map(constructorFacts).collectFirst { case Some(value) => value })
        case Select(qualifier, _) => constructorFacts(qualifier)
        case If(condition, thenBranch, elseBranch) =>
          List(condition, thenBranch, elseBranch).iterator.map(constructorFacts).collectFirst { case Some(value) => value }
        case Typed(inner, _) => constructorFacts(inner)
        case _ => None

    def span(tree: Tree): String =
      val position = tree.pos
      if position.sourceCode.nonEmpty then s"${position.start}..${position.end}" else "NoPosition"

    def recursive(tree: Tree): String =
      val typeText = tree match
        case term: Term => term.tpe.show
        case typeTree: TypeTree => typeTree.tpe.show
        case _ => "<none>"
      val header =
        s"${tree.getClass.getName}(span=${span(tree)},source=${tree.pos.sourceCode.getOrElse("<none>")}," +
          s"symbol=${tree.symbol.fullName},type=$typeText)"
      val children = tree match
        case Inlined(_, bindings, expansion) => bindings :+ expansion
        case Block(statements, expression) => statements :+ expression
        case Apply(function, arguments) => function +: arguments
        case TypeApply(function, arguments) => function +: arguments
        case Select(qualifier, _) => qualifier :: Nil
        case New(typeTree) => typeTree :: Nil
        case If(condition, thenBranch, elseBranch) => List(condition, thenBranch, elseBranch)
        case Typed(inner, typeTree) => List(inner, typeTree)
        case _ => Nil
      if children.isEmpty then header
      else s"$header[${children.map(recursive).mkString(",")}]"

    val term = expression.asTerm
    val facts = constructorFacts(term).getOrElse {
      quotes.reflect.report.errorAndAbort(
        s"Constructor new typed preflight expected a genuine New/<init> application, found ${term.show(using Printer.TreeStructure)}"
      )
    }
    val structure = term.show(using Printer.TreeStructure)
    val summary = recursive(term)
    '{
      ConstructorNewTypedEvidence(
        ${ Expr(term.pos.sourceCode) },
        ${ Expr(structure) },
        ${ Expr(summary) },
        ${ Expr(facts.className) },
        ${ Expr(facts.symbolName) },
        ${ Expr(facts.resultType) },
        ${ Expr(facts.argumentCount) },
        ${ Expr(structure.contains("New(")) },
        ${ Expr(structure.contains("<init>")) }
      )
    }
