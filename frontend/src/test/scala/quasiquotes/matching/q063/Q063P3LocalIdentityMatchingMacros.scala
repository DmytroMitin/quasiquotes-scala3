package quasiquotes.matching.q063

import scala.quoted.*

import quasiquotes.matching.QuasiPattern.*

object Q063P3LocalIdentityMatchingMacros:
  final case class Evidence(
      value: Int,
      exactArgumentIdentity: Boolean,
      distinctBinderSymbols: Boolean,
      bodyUsesParameterSymbol: Boolean,
      resultUsesMethodSymbol: Boolean
  )

  inline def captureInt(inline expression: Int): Int =
    ${ captureIntImpl('expression) }

  inline def evidence(inline expression: Int): Evidence =
    ${ evidenceImpl('expression) }

  inline def captureString(inline expression: String): String =
    ${ captureStringImpl('expression) }

  inline def captureBoolean(inline expression: Boolean): Boolean =
    ${ captureBooleanImpl('expression) }

  inline def captureRepeated(inline expression: Int): Int =
    ${ captureRepeatedImpl('expression) }

  inline def matchesInt[T](inline expression: T): Boolean =
    ${ matchesIntImpl('expression) }

  inline def escapedParameterMatches: Boolean =
    ${ escapedParameterMatchesImpl }

  private def captureIntImpl(expression: Expr[Int])(using q: Quotes): Expr[Int] =
    import q.reflect.*

    expression.asTerm match
      case qq"""{
        def id(value: Int): Int = value
        id($argument)
      }""" => argument.asExprOf[Int]
      case _ => Expr(-1)

  private def evidenceImpl(expression: Expr[Int])(using q: Quotes): Expr[Evidence] =
    import q.reflect.*

    def unwrap(term: Term): Term =
      term match
        case Inlined(_, _, inner) => unwrap(inner)
        case q.reflect.Block(Nil, inner: Term) => unwrap(inner)
        case other => other

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
                val exact =
                  argument.asInstanceOf[AnyRef].eq(rawArgument.asInstanceOf[AnyRef])
                val bodyUsesParameter =
                  definition.rhs.exists(body => unwrap(body).symbol == parameter.symbol)
                val resultUsesMethod = unwrap(function).symbol == definition.symbol
                '{
                  Evidence(
                    ${ argument.asExprOf[Int] },
                    ${ Expr(exact) },
                    ${ Expr(definition.symbol != parameter.symbol) },
                    ${ Expr(bodyUsesParameter) },
                    ${ Expr(resultUsesMethod) }
                  )
                }
              case _ => '{ Evidence(-1, false, false, false, false) }
          case _ => '{ Evidence(-1, false, false, false, false) }
      case _ => '{ Evidence(-1, false, false, false, false) }

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

  private def captureRepeatedImpl(expression: Expr[Int])(using q: Quotes): Expr[Int] =
    import q.reflect.*
    quasiquotes.matching.QuasiPattern
      .term("{ def id(value: Int): Int = value; id(($same, $same)._1) }")
      .flatMap(_.matchTerm(expression.asTerm))
      .toOption
      .flatMap(_.bindings.get("same"))
      .map(_.asExprOf[Int])
      .getOrElse(Expr(-1))

  private def matchesIntImpl[T: Type](expression: Expr[T])(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    expression.asTerm match
      case qq"""{ def id(value: Int): Int = value; id($argument) }""" =>
        Expr(true)
      case _ => Expr(false)

  private def escapedParameterMatchesImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*

    def unwrap(term: Term): Term =
      term match
        case Inlined(_, _, inner) => unwrap(inner)
        case q.reflect.Block(Nil, inner: Term) => unwrap(inner)
        case other => other

    val target = unwrap('{
      def different(x: Int): Int = x
      different(41)
    }.asTerm)
    val matched = target match
      case q.reflect.Block(
            List(definition: DefDef),
            q.reflect.Apply(function, List(_))
          ) =>
        definition.paramss match
          case List(clause: TermParamClause) if clause.params.size == 1 =>
            val escapedResult =
              q.reflect.Apply(function, List(q.reflect.Ref(clause.params.head.symbol)))
            val escapedTarget = q.reflect.Block(List(definition), escapedResult)
            quasiquotes.matching.QuasiPattern
              .term("{ def id(value: Int): Int = value; id($argument) }")
              .toOption
              .flatMap(_.matchTerm(escapedTarget).toOption)
              .nonEmpty
          case _ => false
      case _ => false
    Expr(matched)
