package external.consumer

import scala.quoted.*

import quasiquotes.construct.Quasiquotes.*
import quasiquotes.matching.DefinitionPattern

object DefinitionPatternFirstUseMacros:
  inline def matchDqr(value: Int): Int = ${ matchDqrImpl('value) }
  inline def matchIndependent(value: Int): Int = ${ matchIndependentImpl('value) }
  inline def preservesMixedReferences(value: Int): Int = ${ preservesMixedReferencesImpl('value) }
  inline def mismatchesAreNone: Boolean = ${ mismatchesAreNoneImpl }
  inline def dqqMatchDqr(value: Int): Int = ${ dqqMatchDqrImpl('value) }
  inline def dqqMatchIndependent(value: Int): Int = ${ dqqMatchIndependentImpl('value) }
  inline def dqqPreservesMixedReferences(value: Int): Int = ${ dqqPreservesMixedReferencesImpl('value) }
  inline def dqqMismatchesFallThrough: Boolean = ${ dqqMismatchesFallThroughImpl }
  inline def dqqSelectiveImport(value: Int): Int = ${ dqqSelectiveImportImpl('value) }
  inline def dqqWildcardImports(value: Int): Int = ${ dqqWildcardImportsImpl('value) }

  inline def dqqNullContext: Int = ${ rejectedDqq(0) }
  inline def dqqNullLiteralPart: Int = ${ rejectedDqq(1) }
  inline def dqqZeroSlots: Int = ${ rejectedDqq(2) }
  inline def dqqTwoSlots: Int = ${ rejectedDqq(3) }
  inline def dqqMethodNameSlot: Int = ${ rejectedDqq(4) }
  inline def dqqParameterNameSlot: Int = ${ rejectedDqq(5) }
  inline def dqqParameterTypeSlot: Int = ${ rejectedDqq(6) }
  inline def dqqResultTypeSlot: Int = ${ rejectedDqq(7) }
  inline def dqqPartialRhsSlot: Int = ${ rejectedDqq(8) }
  inline def dqqTwoParameters: Int = ${ rejectedDqq(9) }
  inline def dqqContextualParameter: Int = ${ rejectedDqq(10) }
  inline def dqqTypeParameter: Int = ${ rejectedDqq(11) }
  inline def dqqWrongDefinitionCategory: Int = ${ rejectedDqq(12) }
  inline def dqqMalformed: Int = ${ rejectedDqq(13) }

  private def matchDqrImpl(value: Expr[Int])(using q: Quotes): Expr[Int] =
    import q.reflect.*

    val parameterType = TypeRepr.of[Int]
    val resultType = TypeRepr.of[Int]
    val definition = dqr"def boundedIdentity(value: $parameterType): $resultType = value"
    val matched = requiredPattern(
      "def boundedIdentity(value: Int): Int = $body"
    ).matchDefinition(definition).getOrElse {
      report.errorAndAbort("DefinitionPattern did not match the dqr definition")
    }
    val parameter = firstParameter(definition)
    val body = definition.rhs.get

    if matched.methodName != "boundedIdentity" ||
        matched.parameterName != "value" ||
        !(matched.parameterType.asInstanceOf[AnyRef] eq parameter.tpt.tpe.asInstanceOf[AnyRef]) ||
        !(matched.resultType.asInstanceOf[AnyRef] eq definition.returnTpt.tpe.asInstanceOf[AnyRef]) ||
        !(matched.body eq body) ||
        matched.body.symbol != parameter.symbol
    then report.errorAndAbort("DefinitionPattern did not preserve the exact dqr payload")

    Block(List(definition), Apply(Ref(definition.symbol), List(value.asTerm))).asExprOf[Int]

  private def matchIndependentImpl(value: Expr[Int])(using q: Quotes): Expr[Int] =
    import q.reflect.*

    val authored = '{
      def independent(input: String): Int = input.length
      independent(${ value }.toString)
    }.asTerm
    val definition = firstDefinition(authored)
    val matched = requiredPattern(
      "def independent(input: String): Int = $body"
    ).matchDefinition(definition).getOrElse {
      report.errorAndAbort("DefinitionPattern did not match an independently authored definition")
    }
    val parameter = firstParameter(definition)

    if matched.methodName != "independent" ||
        matched.parameterName != "input" ||
        !(matched.parameterType.asInstanceOf[AnyRef] eq parameter.tpt.tpe.asInstanceOf[AnyRef]) ||
        !(matched.resultType.asInstanceOf[AnyRef] eq definition.returnTpt.tpe.asInstanceOf[AnyRef]) ||
        !(matched.body eq definition.rhs.get)
    then report.errorAndAbort("DefinitionPattern changed the independently authored payload")

    authored.asExprOf[Int]

  private def preservesMixedReferencesImpl(value: Expr[Int])(using q: Quotes): Expr[Int] =
    import q.reflect.*

    val free = Expr(40)
    val authored = '{
      def mixed(bound: Int): Int = bound + $free
      mixed($value)
    }.asTerm
    val definition = firstDefinition(authored)
    val originalBody = definition.rhs.get
    val matched = requiredPattern(
      "def mixed(bound: Int): Int = $body"
    ).matchDefinition(definition).getOrElse {
      report.errorAndAbort("DefinitionPattern did not match a mixed-reference body")
    }

    if !(matched.body eq originalBody) then
      report.errorAndAbort("DefinitionPattern rebuilt a mixed-reference body")

    authored.asExprOf[Int]

  private def mismatchesAreNoneImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*

    def definitionOf(term: Term): DefDef = firstDefinition(term)

    val exact = definitionOf('{ def selected(value: Int): String = value.toString; selected(1) }.asTerm)
    val wrongName = definitionOf('{ def other(value: Int): String = value.toString; other(1) }.asTerm)
    val wrongParameterName = definitionOf('{ def selected(other: Int): String = other.toString; selected(1) }.asTerm)
    val wrongParameterType = definitionOf('{ def selected(value: String): String = value; selected("x") }.asTerm)
    val wrongResultType = definitionOf('{ def selected(value: Int): Int = value; selected(1) }.asTerm)
    val parameterless = definitionOf('{ def selected: String = "x"; selected }.asTerm)
    val twoParameters = definitionOf('{ def selected(value: Int, other: Int): String = (value + other).toString; selected(1, 2) }.asTerm)
    val contextual = definitionOf('{ def selected(value: Int)(using String): String = value.toString; given String = "x"; selected(1) }.asTerm)
    val polymorphic = definitionOf('{ def selected[A](value: Int): String = value.toString; selected[String](1) }.asTerm)
    val defaulted = definitionOf('{ def selected(value: Int = 1): String = value.toString; selected() }.asTerm)
    val repeated = definitionOf('{ def selected(value: Int*): String = value.mkString; selected(1) }.asTerm)
    val pattern = requiredPattern("def selected(value: Int): String = $body")

    val acceptedOnlyExact =
      pattern.matchDefinition(exact).nonEmpty &&
        pattern.matchDefinition(wrongName).isEmpty &&
        pattern.matchDefinition(wrongParameterName).isEmpty &&
        pattern.matchDefinition(wrongParameterType).isEmpty &&
        pattern.matchDefinition(wrongResultType).isEmpty &&
        pattern.matchDefinition(parameterless).isEmpty &&
        pattern.matchDefinition(twoParameters).isEmpty &&
        pattern.matchDefinition(contextual).isEmpty &&
        pattern.matchDefinition(polymorphic).isEmpty &&
        pattern.matchDefinition(defaulted).isEmpty &&
        pattern.matchDefinition(repeated).isEmpty &&
        pattern.matchDefinition(null.asInstanceOf[q.reflect.DefDef]).isEmpty

    Expr(acceptedOnlyExact)

  private def dqqMatchDqrImpl(value: Expr[Int])(using q: Quotes): Expr[Int] =
    import q.reflect.*
    import quasiquotes.matching.DefinitionPattern.*

    val parameterType = TypeRepr.of[Int]
    val resultType = TypeRepr.of[Int]
    val definition = dqr"def boundedIdentity(value: $parameterType): $resultType = value"
    definition match
      case dqq"def boundedIdentity(value: Int): Int = $body" =>
        val captured: q.reflect.Term = body
        val parameter = firstParameter(definition)
        if !(captured eq definition.rhs.get) || captured.symbol != parameter.symbol then
          report.errorAndAbort("dqq did not preserve the exact dqr body")
      case _ => report.errorAndAbort("dqq did not match the dqr definition")

    Block(List(definition), Apply(Ref(definition.symbol), List(value.asTerm))).asExprOf[Int]

  private def dqqMatchIndependentImpl(value: Expr[Int])(using q: Quotes): Expr[Int] =
    import q.reflect.*
    import quasiquotes.matching.DefinitionPattern.*

    val authored = '{
      def independent(input: String): Int = input.length
      independent(${ value }.toString)
    }.asTerm
    val definition = firstDefinition(authored)
    definition match
      case dqq"def independent(input: String): Int = $body" =>
        val captured: q.reflect.Term = body
        if !(captured eq definition.rhs.get) then
          report.errorAndAbort("dqq rebuilt an independently authored body")
      case _ => report.errorAndAbort("dqq did not match an independently authored definition")

    authored.asExprOf[Int]

  private def dqqPreservesMixedReferencesImpl(value: Expr[Int])(using q: Quotes): Expr[Int] =
    import q.reflect.*
    import quasiquotes.matching.DefinitionPattern.*

    val free = Expr(40)
    val authored = '{
      def mixed(bound: Int): Int = bound + $free
      mixed($value)
    }.asTerm
    val definition = firstDefinition(authored)
    val originalBody = definition.rhs.get
    definition match
      case dqq"def mixed(bound: Int): Int = $body" =>
        val captured: q.reflect.Term = body
        if !(captured eq originalBody) then
          report.errorAndAbort("dqq rebuilt a mixed-reference body")
      case _ => report.errorAndAbort("dqq did not match a mixed-reference definition")

    authored.asExprOf[Int]

  private def dqqMismatchesFallThroughImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import quasiquotes.matching.DefinitionPattern.*

    def definitionOf(term: Term): DefDef = firstDefinition(term)
    def matches(target: DefDef): Boolean =
      target match
        case dqq"def selected(value: Int): String = $body" =>
          val captured: q.reflect.Term = body
          captured eq target.rhs.get
        case _ => false

    val exact = definitionOf('{ def selected(value: Int): String = value.toString; selected(1) }.asTerm)
    val wrongName = definitionOf('{ def other(value: Int): String = value.toString; other(1) }.asTerm)
    val wrongParameterName = definitionOf('{ def selected(other: Int): String = other.toString; selected(1) }.asTerm)
    val wrongParameterType = definitionOf('{ def selected(value: String): String = value; selected("x") }.asTerm)
    val wrongResultType = definitionOf('{ def selected(value: Int): Int = value; selected(1) }.asTerm)
    val parameterless = definitionOf('{ def selected: String = "x"; selected }.asTerm)
    val twoParameters = definitionOf('{ def selected(value: Int, other: Int): String = (value + other).toString; selected(1, 2) }.asTerm)
    val contextual = definitionOf('{ def selected(value: Int)(using String): String = value.toString; given String = "x"; selected(1) }.asTerm)
    val polymorphic = definitionOf('{ def selected[A](value: Int): String = value.toString; selected[String](1) }.asTerm)
    val defaulted = definitionOf('{ def selected(value: Int = 1): String = value.toString; selected() }.asTerm)

    Expr(
      matches(exact) &&
        !matches(wrongName) &&
        !matches(wrongParameterName) &&
        !matches(wrongParameterType) &&
        !matches(wrongResultType) &&
        !matches(parameterless) &&
        !matches(twoParameters) &&
        !matches(contextual) &&
        !matches(polymorphic) &&
        !matches(defaulted)
    )

  private def dqqSelectiveImportImpl(value: Expr[Int])(using q: Quotes): Expr[Int] =
    import q.reflect.*
    import quasiquotes.matching.DefinitionPattern.dqq

    val authored = '{ def selected(value: Int): Int = value; selected($value) }.asTerm
    val definition = firstDefinition(authored)
    definition match
      case dqq"def selected(value: Int): Int = $body" =>
        val captured: q.reflect.Term = body
        if !(captured eq definition.rhs.get) then report.errorAndAbort("selective dqq import changed the body")
      case _ => report.errorAndAbort("selective dqq import did not match")
    authored.asExprOf[Int]

  private def dqqWildcardImportsImpl(value: Expr[Int])(using q: Quotes): Expr[Int] =
    import q.reflect.*
    import quasiquotes.construct.Quasiquotes.*
    import quasiquotes.matching.DefinitionPattern.*
    import quasiquotes.matching.QuasiPattern.*
    import quasiquotes.types.QuasiTypequotes.*

    val authored = '{ def selected(value: Int): Int = value; selected($value) }.asTerm
    val definition = firstDefinition(authored)
    definition match
      case dqq"def selected(value: Int): Int = $body" =>
        val captured: q.reflect.Term = body
        if !(captured eq definition.rhs.get) then report.errorAndAbort("wildcard imports changed the dqq body")
      case _ => report.errorAndAbort("wildcard imports made dqq unavailable")
    authored.asExprOf[Int]

  private def rejectedDqq(kind: Int)(using q: Quotes): Expr[Int] =
    import quasiquotes.matching.DefinitionPattern.*

    val sc = kind match
      case 0 => null.asInstanceOf[StringContext]
      case 1 => StringContext(null)
      case 2 => StringContext("def selected(value: Int): Int = value")
      case 3 => StringContext("def selected(value: ", "): Int = ", "")
      case 4 => StringContext("def ", "(value: Int): Int = value")
      case 5 => StringContext("def selected(", ": Int): Int = value")
      case 6 => StringContext("def selected(value: ", "): Int = value")
      case 7 => StringContext("def selected(value: Int): ", " = value")
      case 8 => StringContext("def selected(value: Int): Int = 1 + ", "")
      case 9 => StringContext("def selected(value: Int, other: Int): Int = ", "")
      case 10 => StringContext("def selected(using value: Int): Int = ", "")
      case 11 => StringContext("def selected[A](value: Int): Int = ", "")
      case 12 => StringContext("val selected: Int = ", "")
      case 13 => StringContext("def selected(value: Int): = ", "")
      case _ => StringContext("", "")
    sc.dqq
    Expr(0)

  private def firstDefinition(using q: Quotes)(term: q.reflect.Term): q.reflect.DefDef =
    import q.reflect.*

    term match
      case Inlined(_, _, nested) => firstDefinition(nested)
      case Block(statements, _) =>
        statements.collectFirst { case definition: DefDef => definition }.getOrElse {
          report.errorAndAbort("test fixture did not contain a definition")
        }
      case _ => report.errorAndAbort("test fixture did not lower to a block")

  private def firstParameter(using q: Quotes)(
      definition: q.reflect.DefDef
  ): q.reflect.ValDef =
    import q.reflect.*

    definition.paramss match
      case List(clause: TermParamClause) => clause.params.head
      case _ => report.errorAndAbort("test fixture did not contain one term-parameter clause")

  private def requiredPattern(using q: Quotes)(source: String) =
    DefinitionPattern.singleParameter(source).fold(
      error => q.reflect.report.errorAndAbort(error.message),
      identity
    )
