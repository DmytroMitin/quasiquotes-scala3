package external.consumer

import scala.quoted.*

import quasiquotes.construct.Quasiquotes.*
import quasiquotes.matching.DefinitionPattern

object DefinitionPatternFirstUseMacros:
  inline def matchDqr(value: Int): Int = ${ matchDqrImpl('value) }
  inline def matchIndependent(value: Int): Int = ${ matchIndependentImpl('value) }
  inline def preservesMixedReferences(value: Int): Int = ${ preservesMixedReferencesImpl('value) }
  inline def mismatchesAreNone: Boolean = ${ mismatchesAreNoneImpl }

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
