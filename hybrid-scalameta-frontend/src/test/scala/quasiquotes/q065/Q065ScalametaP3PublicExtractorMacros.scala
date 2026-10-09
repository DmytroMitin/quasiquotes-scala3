package quasiquotes.q065

import scala.quoted.*

import quasiquotes.matching.TermMatcher
import quasiquotes.scalameta.{ScalametaQuasiPattern, TermFrontend}
import quasiquotes.scalameta.ScalametaQuasiPattern.qq

object Q065ScalametaP3PublicExtractorMacros:
  final case class IntEvidence(
      value: Int,
      exactArgumentIdentity: Boolean,
      distinctBinderSymbols: Boolean,
      bodyUsesParameterSymbol: Boolean,
      resultUsesMethodSymbol: Boolean
  )

  inline def intEvidence(inline expression: Int): IntEvidence =
    ${ intEvidenceImpl('expression) }

  inline def captureString(inline expression: String): String =
    ${ captureStringImpl('expression) }

  inline def captureBoolean(inline expression: Boolean): Boolean =
    ${ captureBooleanImpl('expression) }

  inline def nestedLambdaMatches(inline expression: Int): Boolean =
    ${ nestedLambdaMatchesImpl('expression) }

  inline def captureCollisionSafe(inline expression: Int): Int =
    ${ captureCollisionSafeImpl('expression) }

  inline def negativeTargetMatches: List[Boolean] =
    ${ negativeTargetMatchesImpl }

  inline def repeatedHoleMatches: Boolean =
    ${ repeatedHoleMatchesImpl }

  inline def repeatedHoleMismatchRejects: Boolean =
    ${ repeatedHoleMismatchRejectsImpl }

  inline def escapedParameterMatches: Boolean =
    ${ escapedParameterMatchesImpl }

  private def unwrap(using q: Quotes)(term: q.reflect.Term): q.reflect.Term =
    import q.reflect.*
    term match
      case Inlined(_, _, inner) => unwrap(inner)
      case Typed(inner, _) => unwrap(inner)
      case q.reflect.Block(Nil, inner: Term) => unwrap(inner)
      case other => other

  private def intEvidenceImpl(expression: Expr[Int])(using q: Quotes): Expr[IntEvidence] =
    import q.reflect.*

    val target = unwrap(expression.asTerm)
    target match
      case q.reflect.Block(
            List(definition: DefDef),
            q.reflect.Apply(function, List(rawArgument))
          ) =>
        definition.paramss match
          case List(clause: TermParamClause) if clause.params.size == 1 =>
            val parameter = clause.params.head
            expression.asTerm match
              case qq"""{
                def id(value: Int): Int = value
                id($argument)
              }""" =>
                val exact = argument.asInstanceOf[AnyRef].eq(rawArgument.asInstanceOf[AnyRef])
                val bodyUsesParameter =
                  definition.rhs.exists(body => unwrap(body).symbol == parameter.symbol)
                val resultUsesMethod = unwrap(function).symbol == definition.symbol
                '{
                  IntEvidence(
                    ${ argument.asExprOf[Int] },
                    ${ Expr(exact) },
                    ${ Expr(definition.symbol != parameter.symbol) },
                    ${ Expr(bodyUsesParameter) },
                    ${ Expr(resultUsesMethod) }
                  )
                }
              case _ => '{ IntEvidence(-1, false, false, false, false) }
          case _ => '{ IntEvidence(-1, false, false, false, false) }
      case _ => '{ IntEvidence(-1, false, false, false, false) }

  private def captureStringImpl(expression: Expr[String])(using q: Quotes): Expr[String] =
    import q.reflect.*
    expression.asTerm match
      case qq"""{ def id(value: String): String = value; id($argument) }""" =>
        argument.asExprOf[String]
      case _ => Expr("")

  private def captureBooleanImpl(expression: Expr[Boolean])(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    expression.asTerm match
      case qq"""{ def id(value: Boolean): Boolean = value; id($argument) }""" =>
        argument.asExprOf[Boolean]
      case _ => Expr(false)

  private def nestedLambdaMatchesImpl(expression: Expr[Int])(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    expression.asTerm match
      case qq"""{ def id(value: Int): Int = value; id($argument) }""" => Expr(true)
      case _ => Expr(false)

  private def captureCollisionSafeImpl(expression: Expr[Int])(using q: Quotes): Expr[Int] =
    import q.reflect.*
    expression.asTerm match
      case qq"""{
        def id(value: Int): Int = value
        id((Q065ScalametaP3Targets.__qqhole_argument, $argument)._2)
      }""" => argument.asExprOf[Int]
      case _ => Expr(-1)

  private def negativeTargetMatchesImpl(using q: Quotes): Expr[List[Boolean]] =
    import q.reflect.*

    def matches(target: Term): Boolean =
      target match
        case qq"""{ def id(value: Int): Int = value; id($argument) }""" => true
        case _ => false

    val results = List(
      matches('{ 41 }.asTerm),
      matches('{
        def different(x: Int): Int = 1
        different(41)
      }.asTerm),
      matches('{
        def different(x: Int): Int = x
        identity(41)
      }.asTerm),
      matches('{
        def different(x: String): String = x
        different("value")
      }.asTerm),
      matches('{
        def first(x: Int): Int = x
        def second(x: Int): Int = x
        first(41)
      }.asTerm),
      matches('{
        val seed: Int = 41
        def different(x: Int): Int = x
        different(seed)
      }.asTerm),
      matches('{
        def different(x: Int): Int = x
        different
      }.asTerm)
    )
    Expr.ofList(results.map(Expr(_)))

  private def repeatedHoleMatchesImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    val source = "{ def id(value: Int): Int = value; id(($same, $same)._1) }"
    val target = '{
      def renamed(x: Int): Int = x
      renamed((Q065ScalametaP3Targets.ambient, Q065ScalametaP3Targets.ambient)._1)
    }.asTerm
    val matched = TermFrontend
      .compile(source)
      .flatMap(compiled => TermMatcher.matchTerm(compiled.pattern, target).left.map(error =>
        TermFrontend.Failure("MATCH_FAILURE", 0, 0, error.toString)
      ))
      .isRight
    Expr(matched)

  private def repeatedHoleMismatchRejectsImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    val source = "{ def id(value: Int): Int = value; id(($same, $same)._1) }"
    val target = '{
      def renamed(x: Int): Int = x
      renamed((41, 42)._1)
    }.asTerm
    val rejected = TermFrontend
      .compile(source)
      .toOption
      .exists(compiled => TermMatcher.matchTerm(compiled.pattern, target).isLeft)
    Expr(rejected)

  private def escapedParameterMatchesImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    val target = unwrap('{
      def different(x: Int): Int = x
      different(41)
    }.asTerm)
    val escaped = target match
      case q.reflect.Block(List(definition: DefDef), q.reflect.Apply(function, List(_))) =>
        definition.paramss match
          case List(clause: TermParamClause) if clause.params.size == 1 =>
            q.reflect.Block(
              List(definition),
              q.reflect.Apply(function, List(q.reflect.Ref(clause.params.head.symbol)))
            )
          case _ => target
      case _ => target
    val extractor = ScalametaQuasiPattern.qq(
      StringContext("{ def id(value: Int): Int = value; id(", ") }")
    )(using q)
    Expr(extractor.unapplySeq(escaped).nonEmpty)

private object Q065ScalametaP3Targets:
  val ambient: Int = 41
  val __qqhole_argument: Int = 7
