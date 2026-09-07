package quasiquotes.q044

import scala.annotation.StaticAnnotation
import scala.quoted.*

import quasiquotes.matching.{DefinitionModifiers, RankedDefinitionPatternExtractor}

final class Q044ParameterAnnotation extends StaticAnnotation

final case class Q044OrdinaryRank2Report(
    positive: Map[String, Boolean],
    modes: Map[String, List[String]],
    negative: Map[String, Boolean],
    rank3: Map[String, Boolean]
)

object Q044UnifiedOrdinaryRank2Harness:
  def inspect(using q: Quotes)(
      rank2: RankedDefinitionPatternExtractor[
        q.reflect.DefDef,
        (
          DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term],
          String,
          Seq[q.reflect.ValDef],
          q.reflect.TypeRepr,
          q.reflect.Term
        )
      ],
      rank3: RankedDefinitionPatternExtractor[
        q.reflect.DefDef,
        (
          DefinitionModifiers[q.reflect.Flags, q.reflect.TypeRepr, q.reflect.Term],
          String,
          Seq[Seq[q.reflect.ValDef]],
          q.reflect.TypeRepr,
          q.reflect.Term
        )
      ]
  ): Q044OrdinaryRank2Report =
    import q.reflect.*

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
        def strict1(a: Int): Int = a
        def strict2(a: Int, b: String): String = b
        def strict5(a: Int, b: String, c: Long, d: Boolean, e: Double): Double = e
        def strictByName(a: Int, b: => String): String = b
        def multipleByName(a: => Int, b: String, c: => Long, d: Boolean, e: => Double): Double = e
        def strictRepeated(a: Int, b: String*): Int = a + b.size
        def byNameRepeated(a: => Int, b: String*): Int = a + b.size
        def allModes(a: Int, b: => List[Option[String]], c: Either[Int, String]*): Int = a + b.size + c.size
        def foreignModes(a: Int, b: => List[Option[String]], c: Either[Int, String]*): Int = a + b.size + c.size
        def nestedByName(a: => Either[Int, List[String]]): Int = a.fold(identity, _.size)
        def nestedRepeated(a: List[Option[Int]]*): Int = a.size
        final def modified(a: Int, b: => String, c: Long*): String = b
        def plainSeq(a: Seq[String]): Int = a.size
        def defaultOnly(a: Int = 1): Int = a
        def defaultStrict(a: Int, b: String = "x"): String = b
        def defaultByName(a: Int = 1, b: => String): String = b
        def generic[A](a: A): A = a
        def multiple(a: Int)(b: => String)(c: Long*): String = b
        def namedUsing(using a: Int): Int = a
        def scala2Implicit(implicit a: Int): Int = a
        def annotated(@Q044ParameterAnnotation a: Int): Int = a
      class ConstructorProbe(value: Int)
      extension (value: Int) def expanded(a: Int): Int = value + a
      given namedGiven(using value: Int): Int = value
      ()
    }.asTerm)(Symbol.spliceOwner)

    def named(name: String): DefDef =
      definitions.find(_.name == name).getOrElse(
        throw new IllegalStateException(s"missing Q044 fixture $name")
      )

    def ordinaryParameters(target: DefDef): List[ValDef] = target.paramss match
      case List(clause: TermParamClause) => clause.params
      case _ => Nil

    def methodTypes(target: DefDef): List[TypeRepr] = target.symbol.termRef.widen match
      case method: MethodType => method.paramTypes
      case _ => Nil

    def byNameElement(tpe: TypeRepr): Option[TypeRepr] = tpe match
      case ByNameType(element) => Some(element)
      case _ => None

    def treeRepeatedElement(tpe: TypeRepr): Option[TypeRepr] = tpe match
      case AnnotatedType(AppliedType(_, List(element)), annotation)
          if annotation.tpe.typeSymbol == defn.RepeatedAnnot => Some(element)
      case AppliedType(constructor, List(element))
          if constructor.typeSymbol == defn.RepeatedParamClass => Some(element)
      case _ => None

    def methodRepeatedElement(tpe: TypeRepr): Option[TypeRepr] = tpe match
      case AppliedType(constructor, List(element))
          if constructor.typeSymbol == defn.RepeatedParamClass => Some(element)
      case _ => None

    def mode(parameter: ValDef, methodType: TypeRepr): (String, Boolean) =
      val treeByName = byNameElement(parameter.tpt.tpe)
      val methodByName = byNameElement(methodType)
      val treeRepeated = treeRepeatedElement(parameter.tpt.tpe)
      val methodRepeated = methodRepeatedElement(methodType)
      (treeByName, methodByName, treeRepeated, methodRepeated) match
        case (Some(tree), Some(method), None, None) =>
          ("by-name", tree =:= method && parameter.symbol.termRef.widen =:= tree)
        case (None, None, Some(tree), Some(method)) =>
          ("repeated", tree =:= method)
        case (None, None, None, None) =>
          ("strict", parameter.tpt.tpe =:= methodType && parameter.symbol.termRef.widen =:= parameter.tpt.tpe)
        case _ => ("incoherent", false)

    def sameScope(left: Option[TypeRepr], right: Option[TypeRepr]): Boolean =
      (left, right) match
        case (None, None) => true
        case (Some(a), Some(b)) => a =:= b
        case _ => false

    val expectedModes = Map(
      "empty" -> Nil,
      "strict1" -> List("strict"),
      "strict2" -> List("strict", "strict"),
      "strict5" -> List.fill(5)("strict"),
      "strictByName" -> List("strict", "by-name"),
      "multipleByName" -> List("by-name", "strict", "by-name", "strict", "by-name"),
      "strictRepeated" -> List("strict", "repeated"),
      "byNameRepeated" -> List("by-name", "repeated"),
      "allModes" -> List("strict", "by-name", "repeated"),
      "nestedByName" -> List("by-name"),
      "nestedRepeated" -> List("repeated"),
      "modified" -> List("strict", "by-name", "repeated"),
      "plainSeq" -> List("strict")
    )

    val positiveRows = expectedModes.toList.map { (name, expected) =>
      val target = named(name)
      val original = ordinaryParameters(target)
      val extracted = rank2.unapply(target)
      val actualModes = original.zip(methodTypes(target)).map(mode)
      val valid = extracted.exists { captured =>
        val parameters = captured._3.toList
        captured._1.flags == target.symbol.flags &&
          sameScope(captured._1.privateWithin, target.symbol.privateWithin) &&
          sameScope(captured._1.protectedWithin, target.symbol.protectedWithin) &&
          captured._1.annotations.size == target.symbol.annotations.size &&
          captured._1.annotations.zip(target.symbol.annotations).forall((left, right) => left eq right) &&
          captured._2 == target.name &&
          parameters.size == original.size &&
          parameters.zip(original).forall((left, right) => left eq right) &&
          parameters.map(_.symbol) == original.map(_.symbol) &&
          parameters.map(_.symbol).forall(_ != Symbol.noSymbol) &&
          parameters.map(_.symbol).distinct.size == parameters.size &&
          parameters.forall(_.symbol.owner == target.symbol) &&
          target.symbol.paramSymss == List(parameters.map(_.symbol)) &&
          actualModes.map(_._1) == expected &&
          actualModes.forall(_._2) &&
          captured._4 =:= target.returnTpt.tpe &&
          target.rhs.exists(_ eq captured._5)
      }
      (name, valid, actualModes.map(_._1))
    }

    val exact = named("allModes")
    val foreign = named("foreignModes")
    val exactParameters = ordinaryParameters(exact)
    val byNameTreeMismatch = ValDef.copy(exactParameters(1))(
      exactParameters(1).name,
      TypeTree.of[String],
      None
    )
    val repeatedTreeMismatch = ValDef.copy(exactParameters(2))(
      exactParameters(2).name,
      TypeTree.of[Seq[Either[Int, String]]],
      None
    )
    val erasedReplacement = ValDef(
      Symbol.newVal(
        exact.symbol,
        "erasedReplacement",
        TypeRepr.of[Int],
        Flags.Param | Flags.Erased,
        Symbol.noSymbol
      ),
      None
    )

    def copied(parameters: List[ValDef], rhs: Option[Term] = exact.rhs): DefDef =
      DefDef.copy(exact)(
        exact.name,
        List(TermParamClause(parameters)),
        exact.returnTpt,
        rhs
      )

    def flaggedAccessor(name: String, flags: Flags): DefDef =
      val symbol = Symbol.newMethod(
        Symbol.spliceOwner,
        name,
        MethodType(Nil)(_ => Nil, _ => TypeRepr.of[String]),
        flags,
        Symbol.noSymbol
      )
      DefDef(symbol, _ => Some(Literal(StringConstant("value"))))

    final class NullTargetHolder:
      var value: DefDef = scala.compiletime.uninitialized

    val constructor = definitions.find(definition =>
      definition.symbol.isClassConstructor && definition.symbol.owner.name == "ConstructorProbe"
    ).get
    val negativeTargets = List(
      "absent" -> named("absent"),
      "default-only" -> named("defaultOnly"),
      "default-strict" -> named("defaultStrict"),
      "default-by-name" -> named("defaultByName"),
      "generic" -> named("generic"),
      "multiple" -> named("multiple"),
      "named-using" -> named("namedUsing"),
      "scala2-implicit" -> named("scala2Implicit"),
      "erased" -> copied(exactParameters.updated(0, erasedReplacement)),
      "parameter-annotation" -> named("annotated"),
      "nonfinal-repeated-corruption" -> copied(List(exactParameters(2), exactParameters(0), exactParameters(1))),
      "incoherent-by-name" -> copied(exactParameters.updated(1, byNameTreeMismatch)),
      "incoherent-repeated" -> copied(exactParameters.updated(2, repeatedTreeMismatch)),
      "foreign-owner" -> DefDef.copy(exact)(exact.name, foreign.paramss, exact.returnTpt, exact.rhs),
      "duplicate" -> copied(List(exactParameters(0), exactParameters(1), exactParameters(1))),
      "reordered" -> copied(exactParameters.reverse),
      "paramss-mismatch" -> copied(exactParameters.dropRight(1)),
      "missing-rhs" -> copied(exactParameters, None),
      "constructor" -> constructor,
      "extension" -> named("expanded"),
      "field-accessor" -> flaggedAccessor("fieldAccessor", Flags.FieldAccessor),
      "param-accessor" -> flaggedAccessor("paramAccessor", Flags.ParamAccessor),
      "case-accessor" -> flaggedAccessor("caseAccessor", Flags.CaseAccessor),
      "given" -> named("namedGiven")
    )
    val negativeRows = negativeTargets.map((label, target) => label -> rank2.unapply(target).isEmpty) :+
      ("null" -> rank2.unapply(new NullTargetHolder().value).isEmpty)

    val rank3Expected = Map(
      "strict2" -> true,
      "strictByName" -> true,
      "strictRepeated" -> true,
      "allModes" -> true,
      "multiple" -> true,
      "defaultOnly" -> false
    )
    val rank3Rows = rank3Expected.map { (name, expectedMatch) =>
      val target = named(name)
      val extracted = rank3.unapply(target)
      val identity = extracted.exists(captured =>
        val original = target.paramss.collect { case clause: TermParamClause => clause.params }
        captured._3.map(_.toList).zip(original).forall((capturedClause, originalClause) =>
          capturedClause.zip(originalClause).forall((left, right) => left eq right)
        ) && target.rhs.exists(_ eq captured._5)
      )
      name -> (extracted.nonEmpty == expectedMatch && (!expectedMatch || identity))
    }

    Q044OrdinaryRank2Report(
      positiveRows.map((name, valid, _) => name -> valid).toMap,
      positiveRows.map((name, _, actualModes) => name -> actualModes).toMap,
      negativeRows.toMap,
      rank3Rows
    )
