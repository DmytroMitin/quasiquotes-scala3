package quasiquotes.q042

import scala.quoted.*

final case class Q042ParameterEvidence(
    method: String,
    index: Int,
    name: String,
    valDefTypeFamily: String,
    symbolTypeFamily: String,
    methodTypeFamily: String,
    valDefByName: Boolean,
    symbolByName: Boolean,
    methodByName: Boolean,
    valDefElementFamily: Option[String],
    symbolElementFamily: Option[String],
    methodElementFamily: Option[String],
    elementsAgree: Boolean,
    symbolMatchesElement: Boolean,
    sameAsInt: Boolean,
    sameAsFunction0Int: Boolean,
    symbolPresent: Boolean,
    ownerIsMethod: Boolean,
    isParam: Boolean,
    isImplicit: Boolean,
    isGiven: Boolean,
    isSynthetic: Boolean,
    isErased: Boolean,
    hasDefault: Boolean,
    annotationCount: Int,
    positionStart: Int,
    positionEnd: Int
)

final case class Q042MethodEvidence(
    name: String,
    clauseKinds: List[String],
    clauseSizes: List[Int],
    symbolClauseSizes: List[Int],
    exactParamSymss: Boolean,
    methodTypeFamily: String,
    methodParameterFamilies: List[List[String]],
    resultTypeFamily: String,
    rhsPresent: Boolean,
    parameters: List[Q042ParameterEvidence]
)

object Q042QuotesByNameEvidence:
  def inspect(using q: Quotes): List[Q042MethodEvidence] =
    import q.reflect.*

    def typeFamily(tpe: TypeRepr): String = tpe match
      case ByNameType(_) => "by-name"
      case AppliedType(constructor, arguments)
          if constructor.typeSymbol == defn.RepeatedParamClass =>
        s"repeated-applied-${arguments.size}"
      case AppliedType(_, arguments) => s"applied-${arguments.size}"
      case AnnotatedType(_, _) => "annotated"
      case _: ParamRef => "parameter-reference"
      case _: TypeRef => "type-reference"
      case _: TermRef => "term-reference"
      case _ => "other-public-type"

    def methodParameterRows(tpe: TypeRepr): List[List[TypeRepr]] = tpe match
      case poly: PolyType => methodParameterRows(poly.resType)
      case method: MethodType => method.paramTypes :: methodParameterRows(method.resType)
      case _ => Nil

    def methodTypeFamily(tpe: TypeRepr): String = tpe match
      case _: PolyType => "poly-method"
      case _: MethodType => "method"
      case _ => "other-public-type"

    def clauseKind(clause: ParamClause): String = clause match
      case _: TypeParamClause => "type"
      case term: TermParamClause if term.isImplicit => "implicit"
      case term: TermParamClause if term.isGiven => "using"
      case term: TermParamClause if term.isErased => "erased"
      case _: TermParamClause => "ordinary"

    def clauseSize(clause: ParamClause): Int = clause match
      case values: TypeParamClause => values.params.size
      case values: TermParamClause => values.params.size

    def agree(values: List[Option[TypeRepr]]): Boolean =
      values match
        case List(Some(first), None, Some(third)) => first =:= third
        case List(None, None, None) => true
        case _ => false

    val definitions = scala.collection.mutable.ListBuffer.empty[DefDef]
    val traversal = new TreeTraverser:
      override def traverseTree(tree: Tree)(owner: Symbol): Unit =
        tree match
          case definition: DefDef => definitions += definition
          case _ => ()
        super.traverseTree(tree)(owner)

    traversal.traverseTree('{
      class Fixture:
        def strict(value: Int): Int = value
        def byName(value: => Int): Int = value
        def thunk(value: () => Int): Int = value()
        def nested(value: => List[Option[Int]]): Int = value.size
        def prefix(first: Int, delayed: => String): String = delayed
        def middle(first: Int, delayed: => String, last: Long): String = delayed
        def two(first: => Int, second: => String): String = second
        def cardinality(a: => Int, b: String, c: => Long, d: Boolean, e: => Double): Double = e
        def generic[A](value: => A): A = value
        def multiple(first: Int)(delayed: => String): String = delayed
        def defaulted(value: => Int = 1): Int = value
      ()
    }.asTerm)(Symbol.spliceOwner)

    val sourceNames = Set(
      "strict", "byName", "thunk", "nested", "prefix", "middle", "two",
      "cardinality", "generic", "multiple", "defaulted"
    )

    definitions.iterator
      .filter(definition => sourceNames.contains(definition.name))
      .map { definition =>
        val termRows = definition.paramss.collect { case clause: TermParamClause => clause.params }
        val parameters = termRows.flatten
        val methodType = definition.symbol.termRef.widen
        val methodRows = methodParameterRows(methodType)
        val methodTypes = methodRows.flatten
        Q042MethodEvidence(
          definition.name,
          definition.paramss.map(clauseKind),
          definition.paramss.map(clauseSize),
          definition.symbol.paramSymss.map(_.size),
          definition.symbol.paramSymss == definition.paramss.map {
            case clause: TypeParamClause => clause.params.map(_.symbol)
            case clause: TermParamClause => clause.params.map(_.symbol)
          },
          methodTypeFamily(methodType),
          methodRows.map(_.map(typeFamily)),
          typeFamily(definition.returnTpt.tpe),
          definition.rhs.nonEmpty,
          parameters.zipWithIndex.map { (parameter, index) =>
            val valDefElement = Q042ByNameOrdinaryCandidateFactory.valDefByNameElementType(parameter)
            val symbolElement = Q042ByNameOrdinaryCandidateFactory.symbolByNameElementType(parameter)
            val methodElement = methodTypes.lift(index).flatMap {
              case ByNameType(element) => Some(element)
              case _ => None
            }
            Q042ParameterEvidence(
              definition.name,
              index,
              parameter.name,
              typeFamily(parameter.tpt.tpe),
              typeFamily(parameter.symbol.termRef.widen),
              methodTypes.lift(index).map(typeFamily).getOrElse("missing"),
              valDefElement.nonEmpty,
              symbolElement.nonEmpty,
              methodElement.nonEmpty,
              valDefElement.map(typeFamily),
              symbolElement.map(typeFamily),
              methodElement.map(typeFamily),
              agree(List(valDefElement, symbolElement, methodElement)),
              valDefElement.exists(parameter.symbol.termRef.widen =:= _),
              parameter.tpt.tpe =:= TypeRepr.of[Int],
              parameter.tpt.tpe =:= TypeRepr.of[() => Int],
              parameter.symbol != Symbol.noSymbol,
              parameter.symbol.owner == definition.symbol,
              parameter.symbol.flags.is(Flags.Param),
              parameter.symbol.flags.is(Flags.Implicit),
              parameter.symbol.flags.is(Flags.Given),
              parameter.symbol.flags.is(Flags.Synthetic),
              parameter.symbol.flags.is(Flags.Erased),
              parameter.symbol.flags.is(Flags.HasDefault),
              parameter.symbol.annotations.size,
              parameter.pos.start,
              parameter.pos.end
            )
          }
        )
      }
      .toList
      .sortBy(_.name)
