package quasiquotes.q050

import scala.quoted.*

object Q050ScalametaQqSequenceCaptureMacros:
  inline def applyArguments(inline expression: Any): List[Int] =
    ${ applyArgumentsImpl('expression) }

  inline def applyTail(inline expression: Any): (Int, List[Int]) =
    ${ applyTailImpl('expression) }

  inline def applyInit(inline expression: Any): (List[Int], Int) =
    ${ applyInitImpl('expression) }

  inline def applyMiddle(inline expression: Any): Option[(Int, List[Int], Int)] =
    ${ applyMiddleImpl('expression) }

  inline def applyFixedEndsMatch(inline expression: Any): Boolean =
    ${ applyFixedEndsMatchImpl('expression) }

  inline def newArguments(inline expression: Any): List[Int] =
    ${ newArgumentsImpl('expression) }

  inline def newTail(inline expression: Any): (Int, List[Int]) =
    ${ newTailImpl('expression) }

  inline def newInit(inline expression: Any): (List[Int], Int) =
    ${ newInitImpl('expression) }

  inline def newMiddle(inline expression: Any): Option[(Int, List[Int], Int)] =
    ${ newMiddleImpl('expression) }

  inline def newFixedEndsMatch(inline expression: Any): Boolean =
    ${ newFixedEndsMatchImpl('expression) }

  inline def scalarDirect(inline left: Int, inline right: Int): (Int, Int) =
    ${ scalarDirectImpl('left, 'right) }

  inline def scalarUmbrella(inline left: Int, inline right: Int): (Int, Int) =
    ${ scalarUmbrellaImpl('left, 'right) }

  inline def dynamicScalarFallback(inline left: Int, inline right: Int): (Int, Int) =
    ${ dynamicScalarFallbackImpl('left, 'right) }

  inline def rankedIdentityAndEngine: (Boolean, Boolean, String) =
    ${ rankedIdentityAndEngineImpl }

  private def applyArgumentsImpl(expression: Expr[Any])(using q: Quotes): Expr[List[Int]] =
    import q.reflect.*
    import quasiquotes.scalameta.ScalametaQuasiPattern.qq

    expression.asTerm match
      case qq"$function(..$arguments)" =>
        val _: q.reflect.Term = function
        val _: Seq[q.reflect.Term] = arguments
        Expr.ofList(arguments.toList.map(_.asExprOf[Int]))
      case _ => '{ Nil }

  private def applyTailImpl(expression: Expr[Any])(using q: Quotes): Expr[(Int, List[Int])] =
    import q.reflect.*
    import quasiquotes.scalameta.ScalametaQuasiPattern.qq

    expression.asTerm match
      case qq"$function($head, ..$tail)" =>
        val _: q.reflect.Term = function
        val _: q.reflect.Term = head
        val _: Seq[q.reflect.Term] = tail
        '{ (${ head.asExprOf[Int] }, ${ Expr.ofList(tail.toList.map(_.asExprOf[Int])) }) }
      case _ => '{ (-1, Nil) }

  private def applyInitImpl(expression: Expr[Any])(using q: Quotes): Expr[(List[Int], Int)] =
    import q.reflect.*
    import quasiquotes.scalameta.ScalametaQuasiPattern.qq

    expression.asTerm match
      case qq"$function(..$init, $last)" =>
        val _: q.reflect.Term = function
        val _: Seq[q.reflect.Term] = init
        val _: q.reflect.Term = last
        '{ (${ Expr.ofList(init.toList.map(_.asExprOf[Int])) }, ${ last.asExprOf[Int] }) }
      case _ => '{ (Nil, -1) }

  private def applyMiddleImpl(
      expression: Expr[Any]
  )(using q: Quotes): Expr[Option[(Int, List[Int], Int)]] =
    import q.reflect.*
    import quasiquotes.scalameta.ScalametaQuasiPattern.qq

    expression.asTerm match
      case qq"$function($first, ..$middle, $last)" =>
        val _: q.reflect.Term = function
        val _: q.reflect.Term = first
        val _: Seq[q.reflect.Term] = middle
        val _: q.reflect.Term = last
        '{
          Some(
            (
              ${ first.asExprOf[Int] },
              ${ Expr.ofList(middle.toList.map(_.asExprOf[Int])) },
              ${ last.asExprOf[Int] }
            )
          )
        }
      case _ => '{ None }

  private def applyFixedEndsMatchImpl(expression: Expr[Any])(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import quasiquotes.scalameta.ScalametaQuasiPattern.qq

    expression.asTerm match
      case qq"$function(1, ..$middle, 9)" =>
        val _: q.reflect.Term = function
        val _: Seq[q.reflect.Term] = middle
        Expr(true)
      case _ => Expr(false)

  private def newArgumentsImpl(expression: Expr[Any])(using q: Quotes): Expr[List[Int]] =
    import q.reflect.*
    import quasiquotes.scalameta.ScalametaQuasiPattern.qq

    expression.asTerm match
      case qq"new quasiquotes.q050.Q050Constructor(..$arguments)" =>
        val _: Seq[q.reflect.Term] = arguments
        Expr.ofList(arguments.toList.map(_.asExprOf[Int]))
      case _ => '{ Nil }

  private def newTailImpl(expression: Expr[Any])(using q: Quotes): Expr[(Int, List[Int])] =
    import q.reflect.*
    import quasiquotes.scalameta.ScalametaQuasiPattern.qq

    expression.asTerm match
      case qq"new quasiquotes.q050.Q050Constructor($head, ..$tail)" =>
        val _: q.reflect.Term = head
        val _: Seq[q.reflect.Term] = tail
        '{ (${ head.asExprOf[Int] }, ${ Expr.ofList(tail.toList.map(_.asExprOf[Int])) }) }
      case _ => '{ (-1, Nil) }

  private def newInitImpl(expression: Expr[Any])(using q: Quotes): Expr[(List[Int], Int)] =
    import q.reflect.*
    import quasiquotes.scalameta.ScalametaQuasiPattern.qq

    expression.asTerm match
      case qq"new quasiquotes.q050.Q050Constructor(..$init, $last)" =>
        val _: Seq[q.reflect.Term] = init
        val _: q.reflect.Term = last
        '{ (${ Expr.ofList(init.toList.map(_.asExprOf[Int])) }, ${ last.asExprOf[Int] }) }
      case _ => '{ (Nil, -1) }

  private def newMiddleImpl(
      expression: Expr[Any]
  )(using q: Quotes): Expr[Option[(Int, List[Int], Int)]] =
    import q.reflect.*
    import quasiquotes.scalameta.ScalametaQuasiPattern.qq

    expression.asTerm match
      case qq"new quasiquotes.q050.Q050Constructor($first, ..$middle, $last)" =>
        val _: q.reflect.Term = first
        val _: Seq[q.reflect.Term] = middle
        val _: q.reflect.Term = last
        '{
          Some(
            (
              ${ first.asExprOf[Int] },
              ${ Expr.ofList(middle.toList.map(_.asExprOf[Int])) },
              ${ last.asExprOf[Int] }
            )
          )
        }
      case _ => '{ None }

  private def newFixedEndsMatchImpl(expression: Expr[Any])(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import quasiquotes.scalameta.ScalametaQuasiPattern.qq

    expression.asTerm match
      case qq"new quasiquotes.q050.Q050Constructor(1, ..$middle, 9)" =>
        val _: Seq[q.reflect.Term] = middle
        Expr(true)
      case _ => Expr(false)

  private def scalarDirectImpl(
      leftOperand: Expr[Int],
      rightOperand: Expr[Int]
  )(using q: Quotes): Expr[(Int, Int)] =
    import q.reflect.*
    import quasiquotes.scalameta.ScalametaQuasiPattern.qq

    '{ $leftOperand + $rightOperand }.asTerm match
      case qq"$left + $right" =>
        val _: q.reflect.Term = left
        val _: q.reflect.Term = right
        '{ (${ left.asExprOf[Int] }, ${ right.asExprOf[Int] }) }
      case _ => '{ (-1, -1) }

  private def scalarUmbrellaImpl(
      leftOperand: Expr[Int],
      rightOperand: Expr[Int]
  )(using q: Quotes): Expr[(Int, Int)] =
    import q.reflect.*
    import quasiquotes.scalameta.Quasiquotes.qq

    '{ $leftOperand + $rightOperand }.asTerm match
      case qq"$left + $right" =>
        val _: q.reflect.Term = left
        val _: q.reflect.Term = right
        '{ (${ left.asExprOf[Int] }, ${ right.asExprOf[Int] }) }
      case _ => '{ (-1, -1) }

  private def dynamicScalarFallbackImpl(
      leftOperand: Expr[Int],
      rightOperand: Expr[Int]
  )(using q: Quotes): Expr[(Int, Int)] =
    import q.reflect.*
    import quasiquotes.scalameta.ScalametaQuasiPattern.qq

    val dynamicContext = StringContext("", " + ", "")
    val extractor: quasiquotes.scalameta.ScalametaTermPatternExtractor[q.reflect.Term] =
      dynamicContext.qq
    extractor.unapplySeq('{ $leftOperand + $rightOperand }.asTerm) match
      case Some(Seq(left, right)) =>
        '{ (${ left.asExprOf[Int] }, ${ right.asExprOf[Int] }) }
      case _ => '{ (-1, -1) }

  private def rankedIdentityAndEngineImpl(using q: Quotes): Expr[(Boolean, Boolean, String)] =
    import q.reflect.*
    import quasiquotes.scalameta.ScalametaQuasiPattern.qq

    def stripCompilerShells(term: Term): Term =
      term match
        case Inlined(_, _, inner) => stripCompilerShells(inner)
        case Typed(inner, _) => stripCompilerShells(inner)
        case Block(Nil, inner) => stripCompilerShells(inner)
        case other => other

    val applyTarget = stripCompilerShells(
      '{ Q050Targets.many(1, Q050Targets.one(2).head, 3) }.asTerm
    )
    val applyChildren = applyTarget match
      case Apply(_, arguments) => arguments
      case other => report.errorAndAbort(s"expected Apply target, obtained ${other.show}")
    val capturedApply = applyTarget match
      case qq"$function(..$arguments)" => arguments
      case _ => report.errorAndAbort("ranked Scalameta Apply identity pattern did not match")
    val applyIdentity =
      capturedApply.size == applyChildren.size &&
        capturedApply.zip(applyChildren).forall((captured, original) =>
          captured.asInstanceOf[AnyRef] eq original.asInstanceOf[AnyRef]
        )

    val newTarget = stripCompilerShells('{ new Q050Constructor(1, 2, 3) }.asTerm)
    val newChildren = newTarget match
      case Apply(Select(New(_), "<init>"), arguments) => arguments
      case other => report.errorAndAbort(s"expected New target, obtained ${other.show}")
    val capturedNew = newTarget match
      case qq"new quasiquotes.q050.Q050Constructor(..$arguments)" => arguments
      case _ => report.errorAndAbort("ranked Scalameta New identity pattern did not match")
    val newIdentity =
      capturedNew.size == newChildren.size &&
        capturedNew.zip(newChildren).forall((captured, original) =>
          captured.asInstanceOf[AnyRef] eq original.asInstanceOf[AnyRef]
        )

    val engine = quasiquotes.scalameta.TermFrontend
      .compile("f($qqCapture0)")
      .fold(failure => report.errorAndAbort(failure.message), _.engine.toString)
    '{ (${ Expr(applyIdentity) }, ${ Expr(newIdentity) }, ${ Expr(engine) }) }
