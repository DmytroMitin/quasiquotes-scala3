package quasiquotes.q038

import scala.quoted.*

object Q038DefaultSources:
  val stable: Int = 7
  def call(): Int = 9

final case class Q038ParameterEvidence(
    method: String,
    index: Int,
    name: String,
    typeName: String,
    rhsFamily: String,
    symbolPresent: Boolean,
    ownerIsMethod: Boolean,
    hasDefault: Boolean,
    isImplicit: Boolean,
    isGiven: Boolean,
    isSynthetic: Boolean,
    isErased: Boolean,
    hasPosition: Boolean
)

final case class Q038MethodEvidence(
    name: String,
    clauseSizes: List[Int],
    symbolClauseSizes: List[Int],
    exactParamSymss: Boolean,
    parameters: List[Q038ParameterEvidence]
)

final case class Q038GetterEvidence(
    name: String,
    ownerName: String,
    isSynthetic: Boolean,
    isMethod: Boolean,
    clauseSizes: List[Int],
    resultType: String,
    rhsFamily: String,
    hasPosition: Boolean
)

final case class Q038QuotesSnapshot(
    methods: List[Q038MethodEvidence],
    getters: List[Q038GetterEvidence]
)

object Q038QuotesDefaultEvidence:
  def inspect(using q: Quotes): Q038QuotesSnapshot =
    import q.reflect.*

    def rhsFamily(term: Term): String =
      term match
        case _: Literal => "literal"
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
      class Fixture:
        def noDefault(x: Int): Int = x
        def oneDefault(x: Int = 1): Int = x
        def trailingDefault(x: Int, y: String = "x"): String = y
        def twoDefaults(x: Int = 1, y: String = "x"): String = y
        def nonliteralDefault(x: Int = 1 + 2): Int = x
        def stableSelectionDefault(x: Int = Q038DefaultSources.stable): Int = x
        def callExpressionDefault(x: Int = Q038DefaultSources.call()): Int = x
        def manyDefaults(
            a: Int = 1,
            b: Int,
            c: String = "c",
            d: Long = 4L
        ): Long = d
        def emptyOrdinary(): Int = 0
      ()
    }.asTerm)(Symbol.spliceOwner)

    val sourceNames = Set(
      "noDefault",
      "oneDefault",
      "trailingDefault",
      "twoDefaults",
      "nonliteralDefault",
      "stableSelectionDefault",
      "callExpressionDefault",
      "manyDefaults",
      "emptyOrdinary"
    )

    val methods = definitions.iterator
      .filter(definition => sourceNames.contains(definition.name))
      .map { definition =>
        val termClauses = definition.paramss.collect { case clause: TermParamClause => clause }
        val parameters = termClauses.flatMap(_.params)
        Q038MethodEvidence(
          definition.name,
          termClauses.map(_.params.size),
          definition.symbol.paramSymss.map(_.size),
          definition.symbol.paramSymss == termClauses.map(_.params.map(_.symbol)),
          parameters.zipWithIndex.map { (parameter, index) =>
            Q038ParameterEvidence(
              definition.name,
              index,
              parameter.name,
              parameter.tpt.tpe.show,
              parameter.rhs.map(rhsFamily).getOrElse("absent"),
              parameter.symbol != Symbol.noSymbol,
              parameter.symbol.owner == definition.symbol,
              parameter.symbol.flags.is(Flags.HasDefault),
              parameter.symbol.flags.is(Flags.Implicit),
              parameter.symbol.flags.is(Flags.Given),
              parameter.symbol.flags.is(Flags.Synthetic),
              parameter.symbol.flags.is(Flags.Erased),
              parameter.pos.start >= 0
            )
          }
        )
      }
      .toList
      .sortBy(_.name)

    val getters = definitions.iterator
      .filter(_.name.contains("$default$"))
      .map { getter =>
        Q038GetterEvidence(
          getter.name,
          getter.symbol.owner.fullName,
          getter.symbol.flags.is(Flags.Synthetic),
          getter.symbol.isDefDef,
          getter.paramss.collect { case clause: TermParamClause => clause.params.size },
          getter.returnTpt.tpe.show,
          getter.rhs.map(rhsFamily).getOrElse("absent"),
          getter.pos.start >= 0
        )
      }
      .toList
      .sortBy(_.name)

    Q038QuotesSnapshot(methods, getters)
