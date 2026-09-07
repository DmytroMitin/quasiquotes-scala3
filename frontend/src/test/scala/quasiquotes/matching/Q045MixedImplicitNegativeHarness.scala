package quasiquotes.matching

import scala.quoted.*
import scala.language.experimental.erasedDefinitions

object Q045MixedImplicitNegativeHarness:
  def inspect(using q: Quotes)(extractor: RankedDefinitionPatternExtractor[
    q.reflect.DefDef,
    (String, Seq[q.reflect.ValDef], Seq[q.reflect.ValDef], q.reflect.TypeRepr, q.reflect.Term)
  ]): List[(String, Boolean)] =
    import q.reflect.*

    def definition(expression: Expr[Any], expectedName: String): DefDef =
      val found = scala.collection.mutable.ListBuffer.empty[DefDef]
      val traversal = new TreeTraverser:
        override def traverseTree(tree: Tree)(owner: Symbol): Unit =
          tree match
            case value: DefDef if value.name == expectedName => found += value
            case _ => ()
          super.traverseTree(tree)(owner)
      traversal.traverseTree(expression.asTerm)(Symbol.spliceOwner)
      found.head

    val exact = definition('{ def exact(x: Int, y: Int)(implicit first: Ordering[Int], second: Numeric[Int]): Int = x; () }, "exact")
    val foreign = definition('{ def foreign(x: Int, y: Int)(implicit first: Ordering[Int], second: Numeric[Int]): Int = x; () }, "foreign")
    val repeated = definition('{ def repeated(values: Int*): Int = values.size; () }, "repeated")
    val List(ordinary: TermParamClause, contextual: TermParamClause) = exact.paramss: @unchecked
    val List(foreignOrdinary: TermParamClause, foreignContextual: TermParamClause) = foreign.paramss: @unchecked
    val repeatedParameter = repeated.paramss.head.asInstanceOf[TermParamClause].params.head
    val repeatedInt = defn.RepeatedParamClass.typeRef.appliedTo(TypeRepr.of[Int])
    val repeatedImplicitSymbol = Symbol.newVal(
      exact.symbol,
      "repeatedImplicit",
      repeatedInt,
      Flags.Param | Flags.Implicit,
      Symbol.noSymbol
    )
    val repeatedImplicit = TermParamClause(List(ValDef(repeatedImplicitSymbol, None)))
    val erasedImplicitSymbol = Symbol.newVal(
      exact.symbol,
      "erasedImplicit",
      TypeRepr.of[Ordering[Int]],
      Flags.Param | Flags.Implicit | Flags.Erased,
      Symbol.noSymbol
    )
    val erasedImplicit = TermParamClause(List(ValDef(erasedImplicitSymbol, None)))
    val constructor = definition('{ class Sample(x: Int)(implicit ord: Ordering[Int]); () }, "<init>")
    val extension = definition('{ extension (x: Int) def expanded(y: Int)(implicit ord: Ordering[Int]): Int = x + y; () }, "expanded")
    val provided = definition('{ given provided(using ord: Ordering[Int]): Int = 1; () }, "provided")
    def flaggedAccessor(flags: Flags): DefDef =
      val symbol = Symbol.newMethod(
        Symbol.spliceOwner,
        "accessor",
        MethodType(Nil)(_ => Nil, _ => TypeRepr.of[String]),
        flags,
        Symbol.noSymbol
      )
      DefDef(symbol, _ => Some(Literal(StringConstant("value"))))

    val targets = List(
      "no-clauses" -> definition('{ def noClauses: Int = 0; () }, "noClauses"),
      "ordinary-only" -> definition('{ def ordinary(x: Int): Int = x; () }, "ordinary"),
      "implicit-only" -> definition('{ def old(implicit ord: Ordering[Int]): Int = 1; () }, "old"),
      "using-only" -> definition('{ def onlyUsing(using ord: Ordering[Int]): Int = 1; () }, "onlyUsing"),
      "ordinary-using" -> definition('{ def mixedUsing(x: Int)(using ord: Ordering[Int]): Int = x; () }, "mixedUsing"),
      "using-ordinary" -> definition('{ def wrongOrder(using ord: Ordering[Int])(x: Int): Int = x; () }, "wrongOrder"),
      "implicit-ordinary-structural" -> DefDef.copy(exact)(exact.name, List(contextual, ordinary), exact.returnTpt, exact.rhs),
      "implicit-implicit-structural" -> DefDef.copy(exact)(exact.name, List(contextual, contextual), exact.returnTpt, exact.rhs),
      "ordinary-ordinary" -> definition('{ def twoOrdinary(x: Int)(y: Int): Int = x + y; () }, "twoOrdinary"),
      "third-clause" -> DefDef.copy(exact)(exact.name, List(ordinary, contextual, ordinary), exact.returnTpt, exact.rhs),
      "two-contextual" -> DefDef.copy(exact)(exact.name, List(contextual, contextual), exact.returnTpt, exact.rhs),
      "anonymous-using" -> definition('{ def anonymous(x: Int)(using Ordering[Int]): Int = x; () }, "anonymous"),
      "context-bound-only" -> definition('{ def cbOnly[A: Ordering]: Int = 1; () }, "cbOnly"),
      "context-bound-ordinary" -> definition('{ def cbOrdinary[A: Ordering](value: A): A = value; () }, "cbOrdinary"),
      "context-bound-implicit" -> definition('{ def cbImplicit[A: Ordering](implicit marker: Q037Marker): Int = 1; () }, "cbImplicit"),
      "context-bound-ordinary-implicit" -> definition('{ def cb[A: Ordering](value: A)(implicit marker: Q037Marker): A = value; () }, "cb"),
      "generic" -> definition('{ def generic[A](value: A)(implicit marker: Q037Marker): A = value; () }, "generic"),
      "default-ordinary" -> definition('{ def defaultOrdinary(x: Int = 1)(implicit ord: Ordering[Int]): Int = x; () }, "defaultOrdinary"),
      "default-implicit" -> definition('{ def defaultImplicit(x: Int)(implicit ord: Ordering[Int] = null): Int = x; () }, "defaultImplicit"),
      "repeated-ordinary-structural" -> DefDef.copy(exact)(exact.name, List(TermParamClause(List(repeatedParameter)), contextual), exact.returnTpt, exact.rhs),
      "repeated-implicit-structural" -> DefDef.copy(exact)(exact.name, List(ordinary, repeatedImplicit), exact.returnTpt, exact.rhs),
      "by-name-ordinary" -> definition('{ def byNameOrdinary(x: => Int)(implicit ord: Ordering[Int]): Int = x; () }, "byNameOrdinary"),
      "by-name-implicit" -> definition('{ def byNameImplicit(x: Int)(implicit delayed: => Int): Int = delayed; () }, "byNameImplicit"),
      "erased-ordinary" -> definition('{ def erasedOrdinary(erased x: Int)(implicit ord: Ordering[Int]): Int = 1; () }, "erasedOrdinary"),
      "erased-implicit-structural" -> DefDef.copy(exact)(exact.name, List(ordinary, erasedImplicit), exact.returnTpt, exact.rhs),
      "annotated-ordinary" -> definition('{ def annotatedOrdinary(@Q037ParameterAnnotation x: Int)(implicit ord: Ordering[Int]): Int = x; () }, "annotatedOrdinary"),
      "annotated-implicit" -> definition('{ def annotatedImplicit(x: Int)(implicit @Q037ParameterAnnotation ord: Ordering[Int]): Int = x; () }, "annotatedImplicit"),
      "foreign-ordinary-owner" -> DefDef.copy(exact)(exact.name, List(foreignOrdinary, contextual), exact.returnTpt, exact.rhs),
      "foreign-implicit-owner" -> DefDef.copy(exact)(exact.name, List(ordinary, foreignContextual), exact.returnTpt, exact.rhs),
      "duplicate-ordinary" -> DefDef.copy(exact)(exact.name, List(TermParamClause(List(ordinary.params.head, ordinary.params.head)), contextual), exact.returnTpt, exact.rhs),
      "reordered-ordinary" -> DefDef.copy(exact)(exact.name, List(TermParamClause(ordinary.params.reverse), contextual), exact.returnTpt, exact.rhs),
      "duplicate-implicit" -> DefDef.copy(exact)(exact.name, List(ordinary, TermParamClause(List(contextual.params.head, contextual.params.head))), exact.returnTpt, exact.rhs),
      "reordered-implicit" -> DefDef.copy(exact)(exact.name, List(ordinary, TermParamClause(contextual.params.reverse)), exact.returnTpt, exact.rhs),
      "cross-clause-duplicate" -> DefDef.copy(exact)(exact.name, List(ordinary, TermParamClause(List(ordinary.params.head))), exact.returnTpt, exact.rhs),
      "param-symss-mismatch" -> DefDef.copy(exact)(exact.name, List(TermParamClause(List(ordinary.params.head)), contextual), exact.returnTpt, exact.rhs),
      "missing-rhs" -> DefDef.copy(exact)(exact.name, exact.paramss, exact.returnTpt, None),
      "constructor" -> constructor,
      "extension" -> extension,
      "field-accessor" -> flaggedAccessor(Flags.FieldAccessor),
      "param-accessor" -> flaggedAccessor(Flags.ParamAccessor),
      "case-accessor" -> flaggedAccessor(Flags.CaseAccessor),
      "given-definition" -> provided,
      "null" -> null.asInstanceOf[DefDef]
    )
    targets.map((label, target) => label -> extractor.unapply(target).isEmpty)
