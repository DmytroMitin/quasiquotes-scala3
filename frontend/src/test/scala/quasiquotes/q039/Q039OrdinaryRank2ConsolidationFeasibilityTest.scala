package quasiquotes.q039

import scala.compiletime.testing.typeCheckErrors
import scala.quoted.*
import scala.quoted.staging.{Compiler, withQuotes}

final case class Q039ParameterEvidence(
    name: String,
    mode: String,
    treeMode: String,
    methodMode: String,
    elementsAgree: Boolean,
    symbolMatchesElement: Boolean,
    hasDefault: Boolean,
    rhsPresent: Boolean,
    ownerIsMethod: Boolean
)

final case class Q039MethodEvidence(
    name: String,
    clauseKinds: List[String],
    clauseSizes: List[Int],
    exactParamSymss: Boolean,
    resultPresent: Boolean,
    rhsPresent: Boolean,
    parameters: List[Q039ParameterEvidence]
)

final class Q039OrdinaryRank2ConsolidationFeasibilityTest extends munit.FunSuite:
  private def evidence(using q: Quotes): Map[String, Q039MethodEvidence] =
    import q.reflect.*

    def clauseKind(clause: ParamClause): String = clause match
      case _: TypeParamClause => "type"
      case term: TermParamClause if term.isImplicit => "implicit"
      case term: TermParamClause if term.isGiven => "using"
      case term: TermParamClause if term.isErased => "erased"
      case _: TermParamClause => "ordinary"

    def methodRows(tpe: TypeRepr): List[List[TypeRepr]] = tpe match
      case poly: PolyType => methodRows(poly.resType)
      case method: MethodType => method.paramTypes :: methodRows(method.resType)
      case _ => Nil

    def repeatedTreeElement(tpe: TypeRepr): Option[TypeRepr] = tpe match
      case AnnotatedType(AppliedType(_, List(element)), annotation)
          if annotation.tpe.typeSymbol == defn.RepeatedAnnot => Some(element)
      case AppliedType(constructor, List(element))
          if constructor.typeSymbol == defn.RepeatedParamClass => Some(element)
      case _ => None

    def repeatedMethodElement(tpe: TypeRepr): Option[TypeRepr] = tpe match
      case AppliedType(constructor, List(element))
          if constructor.typeSymbol == defn.RepeatedParamClass => Some(element)
      case _ => None

    def byNameElement(tpe: TypeRepr): Option[TypeRepr] = tpe match
      case ByNameType(element) => Some(element)
      case _ => None

    def mode(tpe: TypeRepr, methodPosition: TypeRepr): (String, String, Boolean, Boolean) =
      val treeByName = byNameElement(tpe)
      val methodByName = byNameElement(methodPosition)
      val treeRepeated = repeatedTreeElement(tpe)
      val methodRepeated = repeatedMethodElement(methodPosition)
      (treeByName, methodByName, treeRepeated, methodRepeated) match
        case (Some(tree), Some(method), None, None) =>
          ("by-name", "by-name", tree =:= method, true)
        case (None, None, Some(tree), Some(method)) =>
          ("repeated", "repeated", tree =:= method, true)
        case (None, None, None, None) =>
          ("strict", "strict", true, true)
        case _ => ("incoherent", "incoherent", false, false)

    val definitions = scala.collection.mutable.ListBuffer.empty[DefDef]
    val traversal = new TreeTraverser:
      override def traverseTree(tree: Tree)(owner: Symbol): Unit =
        tree match
          case definition: DefDef => definitions += definition
          case _ => ()
        super.traverseTree(tree)(owner)

    traversal.traverseTree('{
      class Fixture:
        def absent: Int = 0
        def empty(): Int = 0
        def strict(a: Int, b: String): Int = a
        def byNameMix(a: Int, b: => String, c: Long): String = b
        def repeatedMix(a: Int, b: String*): Int = a + b.size
        def byNameAndRepeated(a: => Int, b: String*): Int = a + b.size
        def multipleByName(a: => Int, b: String, c: => Long, d: Boolean, e: => Double): Double = e
        def defaultStrict(a: Int = 1): Int = a
        def strictThenDefault(a: Int, b: String = "x"): String = b
        def defaultThenByName(a: Int = 1, b: => String): String = b
        def twoClauses(a: Int)(b: => String)(c: Long*): String = b
        def generic[A](a: A): A = a
      ()
    }.asTerm)(Symbol.spliceOwner)

    val names = Set(
      "absent", "empty", "strict", "byNameMix", "repeatedMix",
      "byNameAndRepeated", "multipleByName", "defaultStrict",
      "strictThenDefault", "defaultThenByName", "twoClauses", "generic"
    )

    definitions.iterator.filter(d => names.contains(d.name)).map { definition =>
      val methodTypes = methodRows(definition.symbol.termRef.widen).flatten
      val parameters = definition.paramss.collect { case clause: TermParamClause => clause.params }.flatten
      definition.name -> Q039MethodEvidence(
        definition.name,
        definition.paramss.map(clauseKind),
        definition.paramss.map {
          case clause: TypeParamClause => clause.params.size
          case clause: TermParamClause => clause.params.size
        },
        definition.symbol.paramSymss == definition.paramss.map {
          case clause: TypeParamClause => clause.params.map(_.symbol)
          case clause: TermParamClause => clause.params.map(_.symbol)
        },
        definition.returnTpt.tpe != null,
        definition.rhs.nonEmpty,
        parameters.zip(methodTypes).map { (parameter, methodType) =>
          val (treeMode, methodMode, elementsAgree, _) = mode(parameter.tpt.tpe, methodType)
          val element = byNameElement(parameter.tpt.tpe).orElse(repeatedTreeElement(parameter.tpt.tpe))
          Q039ParameterEvidence(
            parameter.name,
            treeMode,
            treeMode,
            methodMode,
            elementsAgree,
            element.forall(parameter.symbol.termRef.widen =:= _),
            parameter.symbol.flags.is(Flags.HasDefault),
            parameter.rhs.nonEmpty,
            parameter.symbol.owner == definition.symbol
          )
        }
      )
    }.toMap

  test("mixed strict by-name repeated and default structure composes on public Quotes"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val rows = withQuotes(evidence)
    rows.values.toList.sortBy(_.name).foreach(row => println(s"Q039_QUOTES $row"))

    assert(rows.values.forall(_.exactParamSymss), rows)
    assert(rows.values.forall(_.rhsPresent), rows)
    assert(rows.values.flatMap(_.parameters).forall(_.ownerIsMethod), rows)
    assert(rows.values.flatMap(_.parameters).forall(!_.rhsPresent), rows)
    assertEquals(rows("strict").parameters.map(_.mode), List("strict", "strict"))
    assertEquals(rows("byNameMix").parameters.map(_.mode), List("strict", "by-name", "strict"))
    assertEquals(rows("repeatedMix").parameters.map(_.mode), List("strict", "repeated"))
    assertEquals(rows("byNameAndRepeated").parameters.map(_.mode), List("by-name", "repeated"))
    assertEquals(rows("multipleByName").parameters.map(_.mode), List("by-name", "strict", "by-name", "strict", "by-name"))
    assert(rows.values.flatMap(_.parameters).forall(_.elementsAgree), rows)
    assert(rows.values.flatMap(_.parameters).filter(_.mode != "repeated").forall(_.symbolMatchesElement), rows)
    assert(rows.values.flatMap(_.parameters).filter(_.mode == "repeated").forall(!_.symbolMatchesElement), rows)
    assertEquals(rows("defaultStrict").parameters.map(_.hasDefault), List(true))
    assertEquals(rows("strictThenDefault").parameters.map(_.hasDefault), List(false, true))
    assertEquals(rows("defaultThenByName").parameters.map(_.hasDefault), List(true, false))
    assertEquals(rows("empty").clauseSizes, List(0))
    assertEquals(rows("absent").clauseSizes, Nil)
    assertEquals(rows("twoClauses").clauseSizes, List(1, 1, 1))
    assertEquals(rows("generic").clauseKinds, List("type", "ordinary"))

  test("current production rank-3 admits recoverable modes and rejects defaults while rank-2 is closed"):
    import quasiquotes.matching.DefinitionPattern.dqq

    given Compiler = Compiler.make(getClass.getClassLoader)
    val result = withQuotes:
      val q = summon[Quotes]
      import q.reflect.*
      val found = scala.collection.mutable.Map.empty[String, DefDef]
      val traversal = new TreeTraverser:
        override def traverseTree(tree: Tree)(owner: Symbol): Unit =
          tree match
            case definition: DefDef => found.update(definition.name, definition)
            case _ => ()
          super.traverseTree(tree)(owner)
      traversal.traverseTree('{
        def strict(a: Int, b: String): Int = a
        def defaulted(a: Int = 1): Int = a
        def byName(a: Int, b: => String): String = b
        def repeated(a: Int, b: String*): Int = a + b.size
        def combined(a: => Int, b: String*): Int = a + b.size
        def multiple(a: Int)(b: => String)(c: Long*): String = b
        ()
      }.asTerm)(Symbol.spliceOwner)
      val extractor = dqq(StringContext("", " def ", "(...", "): ", " = ", ""))(using q)
      List("strict", "defaulted", "byName", "repeated", "combined", "multiple").map { name =>
        val target = found(name)
        name -> extractor.unapply(target).map(captured =>
          captured._3.flatten.zip(target.paramss.collect { case c: TermParamClause => c.params }.flatten)
            .forall((left, right) => left eq right) && captured._5.eq(target.rhs.get)
        )
      }
    println(s"Q039_STANDARD_PRODUCTION $result")
    assertEquals(result.toMap, Map(
      "strict" -> Some(true),
      "defaulted" -> None,
      "byName" -> Some(true),
      "repeated" -> Some(true),
      "combined" -> Some(true),
      "multiple" -> Some(true)
    ))

    val rankTwo = typeCheckErrors(
      """import scala.quoted.*; import quasiquotes.matching.DefinitionPattern.dqq
         def f(using q: Quotes)(d: q.reflect.DefDef) = d match
           case dqq\"$mods def $name(..$params): $result = $body\" => ()
           case _ => ()"""
    )
    assert(rankTwo.nonEmpty, rankTwo)

  test("required mixed sources compile and default-repeated interaction is rejected"):
    inline def errors(inline source: String): List[String] = typeCheckErrors(source).map(_.message)
    val valid = List(
      errors("def strict(a: Int, b: String): Int = a"),
      errors("def byNameMix(a: Int, b: => String, c: Long): String = b"),
      errors("def repeatedMix(a: Int, b: String*): Int = a + b.size"),
      errors("def byNameAndRepeated(a: => Int, b: String*): Int = a + b.size"),
      errors("def defaultStrict(a: Int = 1): Int = a"),
      errors("def strictThenDefault(a: Int, b: String = \"x\"): String = b"),
      errors("def defaultThenByName(a: Int = 1, b: => String): String = b")
    )
    val invalid = errors("def defaultRepeated(a: Int = 1, b: String* = Seq.empty): Int = a + b.size")
    assert(valid.forall(_.isEmpty), valid)
    assert(invalid.nonEmpty, invalid)
