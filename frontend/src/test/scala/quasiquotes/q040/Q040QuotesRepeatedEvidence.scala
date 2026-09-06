package quasiquotes.q040

import scala.quoted.*

final case class Q040ParameterEvidence(
    method: String,
    index: Int,
    name: String,
    typeFamily: String,
    symbolTypeFamily: String,
    repeatedByValDefStructure: Boolean,
    repeatedByAnnotationSymbol: Boolean,
    repeatedByMethodStructure: Boolean,
    valDefElementFamily: Option[String],
    methodElementFamily: Option[String],
    sameAsSeqInt: Boolean,
    symbolPresent: Boolean,
    ownerIsMethod: Boolean,
    isParam: Boolean,
    isImplicit: Boolean,
    isGiven: Boolean,
    isSynthetic: Boolean,
    isErased: Boolean,
    hasDefault: Boolean,
    positionStart: Int,
    positionEnd: Int
)

final case class Q040MethodEvidence(
    name: String,
    clauseKinds: List[String],
    clauseSizes: List[Int],
    symbolClauseSizes: List[Int],
    exactParamSymss: Boolean,
    methodTypeFamily: String,
    methodParameterFamilies: List[List[String]],
    parameters: List[Q040ParameterEvidence]
)

object Q040QuotesRepeatedEvidence:
  def inspect(using q: Quotes): List[Q040MethodEvidence] =
    import q.reflect.*

    def typeFamily(tpe: TypeRepr): String = tpe match
      case ByNameType(_) => "by-name"
      case AnnotatedType(_, annotation)
          if annotation.tpe.typeSymbol == defn.RepeatedAnnot =>
        "repeated-annotated"
      case AnnotatedType(_, _) => "annotated"
      case AppliedType(constructor, arguments)
          if constructor.typeSymbol == defn.RepeatedParamClass =>
        s"repeated-applied-${arguments.size}"
      case AppliedType(_, arguments) => s"applied-${arguments.size}"
      case _: ParamRef => "parameter-reference"
      case _: TypeRef => "type-reference"
      case _: TermRef => "term-reference"
      case _ => "other-public-type"

    def methodParameterFamilies(tpe: TypeRepr): List[List[String]] = tpe match
      case method: MethodType =>
        method.paramTypes.map(typeFamily) :: methodParameterFamilies(method.resType)
      case poly: PolyType => methodParameterFamilies(poly.resType)
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

    val definitions = scala.collection.mutable.ListBuffer.empty[DefDef]
    val traversal = new TreeTraverser:
      override def traverseTree(tree: Tree)(owner: Symbol): Unit =
        tree match
          case definition: DefDef => definitions += definition
          case _ => ()
        super.traverseTree(tree)(owner)

    traversal.traverseTree('{
      class Fixture:
        def plainSeq(xs: Seq[Int]): Int = xs.size
        def repeatedOnly(xs: Int*): Int = xs.size
        def prefixRepeated(head: Int, tail: String*): Int = head + tail.size
        def nestedRepeated(xs: List[Option[Int]]*): Int = xs.size
        def generic[A](xs: A*): Int = xs.size
        def multiple(prefix: Int)(tail: Long*): Int = prefix + tail.size
        def byName(value: => Seq[Int]): Int = value.size
        final def modified(xs: Int*): Int = xs.size
      ()
    }.asTerm)(Symbol.spliceOwner)

    val sourceNames = Set(
      "plainSeq",
      "repeatedOnly",
      "prefixRepeated",
      "nestedRepeated",
      "generic",
      "multiple",
      "byName",
      "modified"
    )

    definitions.iterator
      .filter(definition => sourceNames.contains(definition.name))
      .map { definition =>
        val parameters = definition.paramss.collect {
          case clause: TermParamClause => clause.params
        }.flatten
        val methodType = definition.symbol.termRef.widen
        Q040MethodEvidence(
          definition.name,
          definition.paramss.map(clauseKind),
          definition.paramss.map(clauseSize),
          definition.symbol.paramSymss.map(_.size),
          definition.symbol.paramSymss == definition.paramss.map {
            case clause: TypeParamClause => clause.params.map(_.symbol)
            case clause: TermParamClause => clause.params.map(_.symbol)
          },
          methodTypeFamily(methodType),
          methodParameterFamilies(methodType),
          parameters.zipWithIndex.map { (parameter, index) =>
            val repeatedElement = Q040RepeatedOrdinaryCandidateFactory.repeatedElementType(parameter)
            val methodRepeatedElement =
              Q040RepeatedOrdinaryCandidateFactory.methodRepeatedElementType(definition, index)
            val repeatedAnnotation = parameter.tpt.tpe match
              case AnnotatedType(_, annotation) =>
                annotation.tpe.typeSymbol == defn.RepeatedAnnot
              case _ => false
            Q040ParameterEvidence(
              definition.name,
              index,
              parameter.name,
              typeFamily(parameter.tpt.tpe),
              typeFamily(parameter.symbol.termRef.widen),
              repeatedElement.nonEmpty,
              repeatedAnnotation,
              methodRepeatedElement.nonEmpty,
              repeatedElement.map(typeFamily),
              methodRepeatedElement.map(typeFamily),
              parameter.tpt.tpe =:= TypeRepr.of[Seq[Int]],
              parameter.symbol != Symbol.noSymbol,
              parameter.symbol.owner == definition.symbol,
              parameter.symbol.flags.is(Flags.Param),
              parameter.symbol.flags.is(Flags.Implicit),
              parameter.symbol.flags.is(Flags.Given),
              parameter.symbol.flags.is(Flags.Synthetic),
              parameter.symbol.flags.is(Flags.Erased),
              parameter.symbol.flags.is(Flags.HasDefault),
              parameter.pos.start,
              parameter.pos.end
            )
          }
        )
      }
      .toList
      .sortBy(_.name)
