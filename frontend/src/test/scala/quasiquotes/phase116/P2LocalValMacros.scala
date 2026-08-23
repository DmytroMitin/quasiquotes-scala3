package quasiquotes.phase116

import scala.quoted.*

import quasiquotes.construct.{QuasiquoteBuilder, Quasiquotes}
import quasiquotes.matching.{QuasiPattern, TargetTermView}

object P2LocalValMacros:
  inline def construct(inline initializer: Int): Int =
    ${ constructImpl('initializer) }

  inline def constructList(inline initializer: List[Int]): List[Int] =
    ${ constructListImpl('initializer) }

  inline def symbolOwnerEvidence(inline initializer: Int): (Boolean, Boolean, Boolean, Boolean) =
    ${ symbolOwnerEvidenceImpl('initializer) }

  inline def sameDisplayNameExternal(inline x: Int): (Int, Boolean) =
    ${ sameDisplayNameExternalImpl('x) }

  inline def ownedDefinitionSpliceRejection: String =
    ${ ownedDefinitionSpliceRejectionImpl }

  inline def alphaMatches(inline expression: Int): Boolean =
    ${ alphaMatchesImpl('expression) }

  inline def captureInitializerIdentity(inline expression: Int): (Int, Boolean) =
    ${ captureInitializerIdentityImpl('expression) }

  inline def boundPatternRejectsFreeSameText(inline expression: Int): Boolean =
    ${ boundPatternRejectsFreeSameTextImpl('expression) }

  inline def targetRejectionEvidence: (Boolean, Boolean, Boolean, Boolean, Boolean, Boolean) =
    ${ targetRejectionEvidenceImpl }

  inline def constructionScopeBoundaryEvidence: (String, String, String, String, String, String) =
    ${ constructionScopeBoundaryEvidenceImpl }

  inline def targetScopeBoundaryEvidence: (String, String, String) =
    ${ targetScopeBoundaryEvidenceImpl }

  inline def distinctNameMixedBinderEvidence: (Boolean, Boolean, Boolean, Boolean) =
    ${ distinctNameMixedBinderEvidenceImpl }

  private def constructImpl(initializer: Expr[Int])(using q: Quotes): Expr[Int] =
    import q.reflect.*
    import Quasiquotes.*
    val init = initializer.asTerm
    qr"{ val x: Int = $init; x }".asExprOf[Int]

  private def constructListImpl(initializer: Expr[List[Int]])(using q: Quotes): Expr[List[Int]] =
    import q.reflect.*
    import Quasiquotes.*
    qr"{ val items: List[Int] = ${initializer.asTerm}; items }".asExprOf[List[Int]]

  private def symbolOwnerEvidenceImpl(
      initializer: Expr[Int]
  )(using q: Quotes): Expr[(Boolean, Boolean, Boolean, Boolean)] =
    import q.reflect.*
    import Quasiquotes.*

    val external = initializer.asTerm
    val built = qr"{ val x: Int = $external; x }"
    built match
      case Block((definition: ValDef) :: Nil, result: Ident) =>
        var initializerUsesLocal = false
        val traverser = new TreeTraverser:
          override def traverseTree(tree: Tree)(owner: Symbol): Unit =
            tree match
              case ident: Ident if ident.symbol == definition.symbol => initializerUsesLocal = true
              case _ => super.traverseTree(tree)(owner)
        definition.rhs.foreach(traverser.traverseTree(_)(definition.symbol.owner))
        Expr(
          (
            definition.symbol.exists,
            definition.tpt.tpe =:= TypeRepr.of[Int],
            definition.symbol.owner == Symbol.spliceOwner && result.symbol == definition.symbol,
            !initializerUsesLocal && external.symbol != definition.symbol
          )
        )
      case other =>
        report.errorAndAbort(
          s"expected one local ValDef and one bound result reference, obtained ${other.show(using Printer.TreeStructure)}"
        )

  private def sameDisplayNameExternalImpl(
      external: Expr[Int]
  )(using q: Quotes): Expr[(Int, Boolean)] =
    import q.reflect.*
    import Quasiquotes.*

    val externalTerm = external.asTerm
    val built = qr"{ val x: Int = 1; $externalTerm }"
    val preserved = built match
      case Block((definition: ValDef) :: Nil, result: Term) =>
        result.symbol == externalTerm.symbol && result.symbol != definition.symbol
      case _ => false
    '{ (${built.asExprOf[Int]}, ${Expr(preserved)}) }

  private def ownedDefinitionSpliceRejectionImpl(using q: Quotes): Expr[String] =
    import q.reflect.*

    val owned = '{
      val external = 1
      external
    }.asTerm
    Expr(
      QuasiquoteBuilder
        .build(Seq("{ val x: Int = 0; ", " }"), Seq(owned))
        .left
        .toOption
        .map(_.message)
        .getOrElse("accepted")
    )

  private def alphaMatchesImpl(expression: Expr[Int])(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import QuasiPattern.*

    expression.asTerm match
      case qq"{ val x: Int = $initializer; x }" => Expr(true)
      case _ => Expr(false)

  private def captureInitializerIdentityImpl(
      expression: Expr[Int]
  )(using q: Quotes): Expr[(Int, Boolean)] =
    import q.reflect.*
    import QuasiPattern.*

    def unwrap(term: Term): Term = term match
      case Inlined(_, _, inner) => unwrap(inner)
      case other => other

    val target = unwrap(expression.asTerm)
    val originalInitializer = target match
      case Block((definition: ValDef) :: Nil, _) => definition.rhs.get
      case other => report.errorAndAbort(s"expected local-val block target, obtained ${other.show(using Printer.TreeStructure)}")
    target match
      case qq"{ val x: Int = $initializer; x }" =>
        val same = initializer.asInstanceOf[AnyRef].eq(originalInitializer.asInstanceOf[AnyRef])
        '{ (${initializer.asExprOf[Int]}, ${Expr(same)}) }
      case _ =>
        val detail = quasiquotes.matching.QuasiPattern
          .termOrThrow("{ val x: Int = $initializer; x }")
          .matchTerm(target)
        report.errorAndAbort(s"local-val qq pattern did not match: $detail")

  private def boundPatternRejectsFreeSameTextImpl(
      expression: Expr[Int]
  )(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import QuasiPattern.*

    expression.asTerm match
      case qq"{ val x: Int = $initializer; x }" => Expr(false)
      case _ => Expr(true)

  private def targetRejectionEvidenceImpl(using q: Quotes): Expr[(Boolean, Boolean, Boolean, Boolean, Boolean, Boolean)] =
    import q.reflect.*

    val multiple = '{
      val x: Int = 1
      val y: Int = 2
      y
    }.asTerm
    val mutable = '{
      var x: Int = 1
      x
    }.asTerm
    val localDef = '{
      def x: Int = 1
      x
    }.asTerm
    val inferred = '{
      val x = 1
      x
    }.asTerm
    val lazyValue = '{
      lazy val x: Int = 1
      x
    }.asTerm
    val pattern = '{
      val (x, y) = (1, 2)
      x
    }.asTerm
    Expr(
      (
        TargetTermView.fromTerm(multiple).isLeft,
        TargetTermView.fromTerm(mutable).isLeft,
        TargetTermView.fromTerm(localDef).isLeft,
        TargetTermView.fromTerm(inferred).isLeft,
        TargetTermView.fromTerm(lazyValue).isLeft,
        TargetTermView.fromTerm(pattern).isLeft
      )
    )

  private def constructionScopeBoundaryEvidenceImpl(
      using q: Quotes
  ): Expr[(String, String, String, String, String, String)] =
    def outcome(source: String): String =
      QuasiquoteBuilder
        .build(Seq(source), Nil)
        .fold(_.message, _ => "accepted")

    Expr(
      (
        outcome("{ val x: Int = 1; { val y: Int = 2; y } }"),
        outcome("(x: Int) => { val x: Int = 1; x }"),
        outcome("{ val x: Int = 1; (x: Int) => x }"),
        outcome("{ val x: Int = { val y: Int = 2; y }; x }"),
        outcome("{ val x: Int = 1; ({ val y: Int = 2; y }) }"),
        outcome("{ val x: Int = 1; { { val y: Int = 2; y }; x } }")
      )
    )

  private def targetScopeBoundaryEvidenceImpl(using q: Quotes): Expr[(String, String, String)] =
    import q.reflect.*

    val nested = '{
      val x: Int = 1
      {
        val y: Int = 2
        y
      }
    }.asTerm
    val p2ShadowsLambda = '{ (x: Int) =>
      val x: Int = 1
      x
    }.asTerm
    val lambdaShadowsP2 = '{
      val x: Int = 1
      (x: Int) => x
    }.asTerm

    def outcome(term: Term): String =
      TargetTermView.fromTerm(term).fold(_.message, _ => "accepted")

    Expr((outcome(nested), outcome(p2ShadowsLambda), outcome(lambdaShadowsP2)))

  private def distinctNameMixedBinderEvidenceImpl(
      using q: Quotes
  ): Expr[(Boolean, Boolean, Boolean, Boolean)] =
    import q.reflect.*

    val p2InsideLambda = "(outer: Int) => { val x: Int = 1; x }"
    val lambdaInsideP2 = "{ val x: Int = 1; (inner: Int) => inner }"
    val p2InsideLambdaTarget = '{ (outer: Int) =>
      val x: Int = 1
      x
    }.asTerm
    val lambdaInsideP2Target = '{
      val x: Int = 1
      (inner: Int) => inner
    }.asTerm

    Expr(
      (
        QuasiquoteBuilder.build(Seq(p2InsideLambda), Nil).isRight,
        QuasiquoteBuilder.build(Seq(lambdaInsideP2), Nil).isRight,
        TargetTermView.fromTerm(p2InsideLambdaTarget).isRight,
        TargetTermView.fromTerm(lambdaInsideP2Target).isRight
      )
    )
