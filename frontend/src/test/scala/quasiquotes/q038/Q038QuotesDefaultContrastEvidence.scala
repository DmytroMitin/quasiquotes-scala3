package quasiquotes.q038

import scala.quoted.*

final case class Q038ContrastMethodEvidence(
    ownerSuffix: String,
    name: String,
    clauseKinds: List[String],
    clauseSizes: List[Int],
    symbolClauseSizes: List[Int],
    termDefaultFlags: List[List[Boolean]]
)

final case class Q038ContrastGetterEvidence(
    ownerSuffix: String,
    name: String,
    clauseKinds: List[String],
    clauseSizes: List[Int],
    resultType: String,
    rhsFamily: String,
    referencedTerms: List[String]
)

final case class Q038QuotesContrastSnapshot(
    methods: List[Q038ContrastMethodEvidence],
    getters: List[Q038ContrastGetterEvidence]
)

object Q038QuotesDefaultContrastEvidence:
  def inspect(using q: Quotes): Q038QuotesContrastSnapshot =
    import q.reflect.*

    def ownerSuffix(symbol: Symbol): String =
      List("$Contrast", "$Parent", "$Child").find(symbol.fullName.endsWith).getOrElse(symbol.fullName)

    def clauseKind(clause: ParamClause): String = clause match
      case _: TypeParamClause => "type"
      case term: TermParamClause if term.isImplicit => "implicit"
      case term: TermParamClause if term.isGiven => "using"
      case term: TermParamClause if term.isErased => "erased"
      case _: TermParamClause => "ordinary"

    def clauseSize(clause: ParamClause): Int = clause match
      case values: TypeParamClause => values.params.size
      case values: TermParamClause => values.params.size

    def rhsFamily(term: Term): String = term match
      case _: Literal => "literal"
      case _: Ident => "reference"
      case _: Select => "selection"
      case _: Apply => "call"
      case Inlined(_, _, expansion) => s"inlined-${rhsFamily(expansion)}"
      case Block(_, expression) => s"block-${rhsFamily(expression)}"
      case Typed(expression, _) => s"typed-${rhsFamily(expression)}"
      case _ => term.getClass.getSimpleName

    val definitions = scala.collection.mutable.ListBuffer.empty[DefDef]
    val traversal = new TreeTraverser:
      override def traverseTree(tree: Tree)(owner: Symbol): Unit =
        tree match
          case definition: DefDef => definitions += definition
          case _ => ()
        super.traverseTree(tree)(owner)

    traversal.traverseTree('{
      class Contrast:
        def multiple(x: Int = 1)(y: Int = 2): Int = x + y
        def depends(x: Int)(y: Int = x): Int = y
        def generic[A](x: A = null.asInstanceOf[A]): A = x
      trait Parent:
        def inherited(x: Int = 1): Int
      class Child extends Parent:
        override def inherited(x: Int): Int = x
      ()
    }.asTerm)(Symbol.spliceOwner)

    val sourceNames = Set("multiple", "depends", "generic", "inherited")
    val methods = definitions.iterator
      .filter(definition => sourceNames.contains(definition.name))
      .map { definition =>
        Q038ContrastMethodEvidence(
          ownerSuffix(definition.symbol.owner),
          definition.name,
          definition.paramss.map(clauseKind),
          definition.paramss.map(clauseSize),
          definition.symbol.paramSymss.map(_.size),
          definition.paramss.collect {
            case clause: TermParamClause =>
              clause.params.map(_.symbol.flags.is(Flags.HasDefault))
          }
        )
      }
      .toList
      .sortBy(row => (row.ownerSuffix, row.name))

    val getters = definitions.iterator
      .filter(_.name.contains("$default$"))
      .map { getter =>
        val references = scala.collection.mutable.ListBuffer.empty[String]
        val referenceTraversal = new TreeTraverser:
          override def traverseTree(tree: Tree)(owner: Symbol): Unit =
            tree match
              case identifier: Ident => references += identifier.name
              case _ => ()
            super.traverseTree(tree)(owner)
        getter.rhs.foreach(referenceTraversal.traverseTree(_)(getter.symbol))
        Q038ContrastGetterEvidence(
          ownerSuffix(getter.symbol.owner),
          getter.name,
          getter.paramss.map(clauseKind),
          getter.paramss.map(clauseSize),
          getter.returnTpt.tpe.show,
          getter.rhs.map(rhsFamily).getOrElse("absent"),
          references.toList.distinct.sorted
        )
      }
      .toList
      .sortBy(row => (row.ownerSuffix, row.name))

    Q038QuotesContrastSnapshot(methods, getters)
