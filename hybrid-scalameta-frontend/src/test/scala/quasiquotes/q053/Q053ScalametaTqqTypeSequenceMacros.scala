package quasiquotes.q053

import scala.quoted.*

object Q053ScalametaTqqTypeSequenceMacros:
  inline def allArguments: (List[String], Boolean) = ${ allArgumentsImpl }
  inline def tailArgument: (String, List[String], Boolean) = ${ tailArgumentImpl }
  inline def initArgument: (List[String], String, Boolean) = ${ initArgumentImpl }
  inline def emptyMiddle: (String, List[String], String, Boolean) = ${ emptyMiddleImpl }
  inline def boundaries: Boolean = ${ boundariesImpl }
  inline def nestedIdentity: Boolean = ${ nestedIdentityImpl }
  inline def repeatedSemanticIdentity: Boolean = ${ repeatedSemanticIdentityImpl }
  inline def scalarDirect: Boolean = ${ scalarDirectImpl }
  inline def scalarUmbrella: Boolean = ${ scalarUmbrellaImpl }
  inline def dynamicScalarFallback: Boolean = ${ dynamicScalarFallbackImpl }
  inline def rankedEngine: (String, Boolean) = ${ rankedEngineImpl }

  private def allArgumentsImpl(using q: Quotes): Expr[(List[String], Boolean)] =
    import q.reflect.*
    import quasiquotes.scalameta.ScalametaQuasiPattern.tqq

    val target = TypeRepr.of[Either[Int, String]]
    val expected = appliedArguments(target)
    target match
      case tqq"Either[..$arguments]" =>
        val _: Seq[q.reflect.TypeRepr] = arguments
        tuple2Expr(arguments.toList.map(render), sameReferences(arguments, expected))
      case _ => '{ (Nil, false) }

  private def tailArgumentImpl(using q: Quotes): Expr[(String, List[String], Boolean)] =
    import q.reflect.*
    import quasiquotes.scalameta.ScalametaQuasiPattern.tqq

    val target = TypeRepr.of[Either[Int, String]]
    val expected = appliedArguments(target)
    target match
      case tqq"Either[$head, ..$tail]" =>
        val _: q.reflect.TypeRepr = head
        val _: Seq[q.reflect.TypeRepr] = tail
        '{
          (
            ${ Expr(render(using q)(head)) },
            ${ Expr.ofList(tail.toList.map(value => Expr(render(using q)(value)))) },
            ${ Expr(sameReference(using q)(head, expected.head) && sameReferences(using q)(tail, expected.tail)) }
          )
        }
      case _ => '{ ("no-match", Nil, false) }

  private def initArgumentImpl(using q: Quotes): Expr[(List[String], String, Boolean)] =
    import q.reflect.*
    import quasiquotes.scalameta.ScalametaQuasiPattern.tqq

    val target = TypeRepr.of[Either[Int, String]]
    val expected = appliedArguments(target)
    target match
      case tqq"Either[..$init, $last]" =>
        val _: Seq[q.reflect.TypeRepr] = init
        val _: q.reflect.TypeRepr = last
        '{
          (
            ${ Expr.ofList(init.toList.map(value => Expr(render(using q)(value)))) },
            ${ Expr(render(using q)(last)) },
            ${ Expr(sameReferences(using q)(init, expected.init) && sameReference(using q)(last, expected.last)) }
          )
        }
      case _ => '{ (Nil, "no-match", false) }

  private def emptyMiddleImpl(using q: Quotes): Expr[(String, List[String], String, Boolean)] =
    import q.reflect.*
    import quasiquotes.scalameta.ScalametaQuasiPattern.tqq

    val target = TypeRepr.of[Either[Int, String]]
    val expected = appliedArguments(target)
    target match
      case tqq"Either[$first, ..$middle, $last]" =>
        val _: q.reflect.TypeRepr = first
        val _: Seq[q.reflect.TypeRepr] = middle
        val _: q.reflect.TypeRepr = last
        '{
          (
            ${ Expr(render(using q)(first)) },
            ${ Expr.ofList(middle.toList.map(value => Expr(render(using q)(value)))) },
            ${ Expr(render(using q)(last)) },
            ${ Expr(sameReference(using q)(first, expected.head) && middle.isEmpty && sameReference(using q)(last, expected.last)) }
          )
        }
      case _ => '{ ("no-match", Nil, "no-match", false) }

  private def boundariesImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import quasiquotes.scalameta.ScalametaQuasiPattern.tqq

    val listTarget = TypeRepr.of[List[Int]]
    val optionTarget = TypeRepr.of[Option[String]]
    val eitherTarget = TypeRepr.of[Either[Int, String]]
    val eitherConstructor = appliedConstructor(eitherTarget)
    val insufficient = AppliedType(eitherConstructor, List(TypeRepr.of[Int]))

    def matchesEither(target: TypeRepr): Boolean =
      target match
        case tqq"Either[..$arguments]" =>
          val _: Seq[q.reflect.TypeRepr] = arguments
          true
        case _ => false

    val listMatches = listTarget match
      case tqq"List[..$arguments]" => sameReferences(arguments, appliedArguments(listTarget))
      case _ => false
    val optionMatches = optionTarget match
      case tqq"Option[..$arguments]" => sameReferences(arguments, appliedArguments(optionTarget))
      case _ => false
    val prefixMismatch = eitherTarget match
      case tqq"Either[String, ..$tail]" => true
      case _ => false
    val suffixMismatch = eitherTarget match
      case tqq"Either[..$init, Int]" => true
      case _ => false

    Expr(
      listMatches && optionMatches && !prefixMismatch && !suffixMismatch &&
        !matchesEither(insufficient) && !matchesEither(TypeRepr.of[Map[Int, String]]) &&
        !matchesEither(null.asInstanceOf[TypeRepr])
    )

  private def nestedIdentityImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import quasiquotes.scalameta.ScalametaQuasiPattern.tqq

    val target = TypeRepr.of[Either[List[Int], Option[String]]]
    val expected = appliedArguments(target)
    val nested = appliedArguments(expected.head).head
    Expr(
      target match
        case tqq"Either[List[$head], ..$tail]" =>
          val _: q.reflect.TypeRepr = head
          val _: Seq[q.reflect.TypeRepr] = tail
          sameReference(head, nested) && tail.size == 1 && sameReference(tail.head, expected(1))
        case _ => false
    )

  private def repeatedSemanticIdentityImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import quasiquotes.scalameta.ScalametaQuasiPattern.tqq

    val listConstructor = appliedConstructor(TypeRepr.of[List[Any]])
    val eitherConstructor = appliedConstructor(TypeRepr.of[Either[Any, Any]])
    val left = TypeRepr.of[List[Int]]
    val right = AppliedType(listConstructor, List(TypeRepr.of[Int]))
    val target = AppliedType(eitherConstructor, List(left, right))
    val expected = appliedArguments(target)
    Expr(
      target match
        case tqq"Either[..$arguments]" =>
          !sameReference(expected.head, expected(1)) && sameReferences(arguments, expected)
        case _ => false
    )

  private def scalarDirectImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import quasiquotes.scalameta.ScalametaQuasiPattern.tqq

    val target = TypeRepr.of[List[Int]]
    val expected = appliedArguments(target).head
    Expr(
      target match
        case tqq"List[$argument]" =>
          val _: q.reflect.TypeRepr = argument
          sameReference(argument, expected)
        case _ => false
    )

  private def scalarUmbrellaImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import quasiquotes.scalameta.Quasiquotes.tqq

    val target = TypeRepr.of[Either[Int, String]]
    val expected = appliedArguments(target)
    Expr(
      target match
        case tqq"Either[$head, ..$tail]" =>
          val _: q.reflect.TypeRepr = head
          val _: Seq[q.reflect.TypeRepr] = tail
          sameReference(head, expected.head) && sameReferences(tail, expected.tail)
        case _ => false
    )

  private def dynamicScalarFallbackImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import quasiquotes.scalameta.ScalametaQuasiPattern.*

    val target = TypeRepr.of[List[Int]]
    val expected = appliedArguments(target).head
    val context = StringContext("List[", "]")
    val extractor: quasiquotes.scalameta.ScalametaTypePatternExtractor[q.reflect.TypeRepr] =
      context.tqq
    Expr(
      extractor.unapplySeq(target) match
        case Some(Seq(argument)) => sameReference(argument, expected)
        case _ => false
    )

  private def rankedEngineImpl(using q: Quotes): Expr[(String, Boolean)] =
    import q.reflect.*
    import quasiquotes.scalameta.{ScalametaQuasiPattern, TypeFrontend}

    val compiled = TypeFrontend.compileRanked(Seq("Either[", "]"), 0).fold(
      failure => report.errorAndAbort(failure.message),
      identity
    )
    val scalar = TypeFrontend.compile(Seq("List[", "]")).fold(
      failure => report.errorAndAbort(failure.message),
      identity
    )
    '{
      (
        ${ Expr(compiled.engine.toString) },
        ${ Expr(compiled.primaryFailure.isEmpty && scalar.engine == TypeFrontend.Engine.Scalameta) }
      )
    }

  private def appliedArguments(using q: Quotes)(value: q.reflect.TypeRepr): List[q.reflect.TypeRepr] =
    import q.reflect.*
    value match
      case AppliedType(_, arguments) => arguments
      case other => report.errorAndAbort(s"expected AppliedType, observed ${other.show}")

  private def appliedConstructor(using q: Quotes)(value: q.reflect.TypeRepr): q.reflect.TypeRepr =
    import q.reflect.*
    value match
      case AppliedType(constructor, _) => constructor
      case other => report.errorAndAbort(s"expected AppliedType, observed ${other.show}")

  private def render(using q: Quotes)(value: q.reflect.TypeRepr): String =
    quasiquotes.types.TargetTypeReprInspector.inspect(value).fold(_.message, _.render)

  private def sameReference(using q: Quotes)(left: q.reflect.TypeRepr, right: q.reflect.TypeRepr): Boolean =
    left.asInstanceOf[AnyRef] eq right.asInstanceOf[AnyRef]

  private def sameReferences(using q: Quotes)(left: Seq[q.reflect.TypeRepr], right: Seq[q.reflect.TypeRepr]): Boolean =
    left.size == right.size && left.zip(right).forall((a, b) => sameReference(a, b))

  private def tuple2Expr(using Quotes)(rendered: List[String], identity: Boolean): Expr[(List[String], Boolean)] =
    '{ (${ Expr.ofList(rendered.map(Expr(_))) }, ${ Expr(identity) }) }
