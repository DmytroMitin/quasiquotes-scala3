package quasiquotes.types.q051

import scala.quoted.*

import quasiquotes.types.{TargetTypeReprInspector, TypeNormalForm}

object Q051TypeArgumentSequenceCaptureMacros:
  inline def allArguments: (List[String], Boolean) = ${ allArgumentsImpl }
  inline def tailArgument: (String, List[String], Boolean) = ${ tailArgumentImpl }
  inline def initArgument: (List[String], String, Boolean) = ${ initArgumentImpl }
  inline def emptyMiddle: (String, List[String], String, Boolean) =
    ${ emptyMiddleImpl }
  inline def prefixMismatch: Boolean = ${ prefixMismatchImpl }
  inline def suffixMismatch: Boolean = ${ suffixMismatchImpl }
  inline def unaryFixedConstructors: Boolean = ${ unaryFixedConstructorsImpl }
  inline def targetBoundaryFailures: Boolean = ${ targetBoundaryFailuresImpl }
  inline def scalarCompatibility: Boolean = ${ scalarCompatibilityImpl }
  inline def nestedIdentity: Boolean = ${ nestedIdentityImpl }
  inline def repeatedSemanticIdentity: Boolean = ${ repeatedSemanticIdentityImpl }
  inline def diagnostic(inline name: String): String = ${ diagnosticImpl('name) }

  private def allArgumentsImpl(using q: Quotes): Expr[(List[String], Boolean)] =
    import q.reflect.*
    import Q051TypePatternProbe.*

    val target = TypeRepr.of[Either[Int, String]]
    val expected = appliedArguments(target)
    target match
      case tqq"Either[..$arguments]" =>
        val _: Seq[q.reflect.TypeRepr] = arguments
        tuple2Expr(
          arguments.toList.map(render),
          sameReferences(arguments, expected)
        )
      case _ => '{ (Nil, false) }

  private def tailArgumentImpl(using q: Quotes): Expr[(String, List[String], Boolean)] =
    import q.reflect.*
    import Q051TypePatternProbe.*

    val target = TypeRepr.of[Either[Int, String]]
    val expected = appliedArguments(target)
    target match
      case tqq"Either[$head, ..$tail]" =>
        val _: q.reflect.TypeRepr = head
        val _: Seq[q.reflect.TypeRepr] = tail
        val headRendered = render(using q)(head)
        val tailRendered = tail.toList.map(render(using q))
        val identities =
          sameReference(using q)(head, expected.head) &&
            sameReferences(using q)(tail, expected.tail)
        '{
          (
            ${ Expr(headRendered) },
            ${ Expr.ofList(tailRendered.map(Expr(_))) },
            ${ Expr(identities) }
          )
        }
      case _ => '{ ("no-match", Nil, false) }

  private def initArgumentImpl(using q: Quotes): Expr[(List[String], String, Boolean)] =
    import q.reflect.*
    import Q051TypePatternProbe.*

    val target = TypeRepr.of[Either[Int, String]]
    val expected = appliedArguments(target)
    target match
      case tqq"Either[..$init, $last]" =>
        val _: Seq[q.reflect.TypeRepr] = init
        val _: q.reflect.TypeRepr = last
        val initRendered = init.toList.map(render(using q))
        val lastRendered = render(using q)(last)
        val identities =
          sameReferences(using q)(init, expected.init) &&
            sameReference(using q)(last, expected.last)
        '{
          (
            ${ Expr.ofList(initRendered.map(Expr(_))) },
            ${ Expr(lastRendered) },
            ${ Expr(identities) }
          )
        }
      case _ => '{ (Nil, "no-match", false) }

  private def emptyMiddleImpl(using q: Quotes): Expr[(String, List[String], String, Boolean)] =
    import q.reflect.*
    import Q051TypePatternProbe.*

    val target = TypeRepr.of[Either[Int, String]]
    val expected = appliedArguments(target)
    target match
      case tqq"Either[$first, ..$middle, $last]" =>
        val _: q.reflect.TypeRepr = first
        val _: Seq[q.reflect.TypeRepr] = middle
        val _: q.reflect.TypeRepr = last
        val firstRendered = render(using q)(first)
        val middleRendered = middle.toList.map(render(using q))
        val lastRendered = render(using q)(last)
        val identities =
          sameReference(using q)(first, expected.head) &&
            middle.isEmpty &&
            sameReference(using q)(last, expected.last)
        '{
          (
            ${ Expr(firstRendered) },
            ${ Expr.ofList(middleRendered.map(Expr(_))) },
            ${ Expr(lastRendered) },
            ${ Expr(identities) }
          )
        }
      case _ => '{ ("no-match", Nil, "no-match", false) }

  private def prefixMismatchImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import Q051TypePatternProbe.*

    Expr(
      TypeRepr.of[Either[Int, String]] match
        case tqq"Either[String, ..$tail]" =>
          val _: Seq[q.reflect.TypeRepr] = tail
          true
        case _ => false
    )

  private def suffixMismatchImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import Q051TypePatternProbe.*

    Expr(
      TypeRepr.of[Either[Int, String]] match
        case tqq"Either[..$init, Int]" =>
          val _: Seq[q.reflect.TypeRepr] = init
          true
        case _ => false
    )

  private def unaryFixedConstructorsImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import Q051TypePatternProbe.*

    val listTarget = TypeRepr.of[List[Int]]
    val listExpected = appliedArguments(listTarget)
    val listMatches =
      listTarget match
        case tqq"List[..$arguments]" =>
          arguments.size == 1 && sameReferences(arguments, listExpected)
        case _ => false

    val optionTarget = TypeRepr.of[Option[String]]
    val optionExpected = appliedArguments(optionTarget)
    val optionMatches =
      optionTarget match
        case tqq"Option[..$arguments]" =>
          arguments.size == 1 && sameReferences(arguments, optionExpected)
        case _ => false

    Expr(listMatches && optionMatches)

  private def targetBoundaryFailuresImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import Q051TypePatternProbe.*

    val eitherConstructor = TypeRepr.of[Either[Any, Any]] match
      case AppliedType(constructor, _) => constructor
      case other => report.errorAndAbort(s"expected Either AppliedType, observed ${other.show}")
    val insufficient = AppliedType(eitherConstructor, List(TypeRepr.of[Int]))

    def matches(target: TypeRepr): Boolean =
      target match
        case tqq"Either[..$arguments]" =>
          val _: Seq[q.reflect.TypeRepr] = arguments
          true
        case _ => false

    Expr(
      !matches(insufficient) &&
        !matches(TypeRepr.of[Map[Int, String]]) &&
        !matches(null.asInstanceOf[TypeRepr])
    )

  private def scalarCompatibilityImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import Q051TypePatternProbe.*

    val target = TypeRepr.of[List[Int]]
    val expected = appliedArguments(target).head
    Expr(
      target match
        case tqq"List[$argument]" =>
          val _: q.reflect.TypeRepr = argument
          sameReference(argument, expected)
        case _ => false
    )

  private def nestedIdentityImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import Q051TypePatternProbe.*

    val target = TypeRepr.of[Either[List[Int], Option[String]]]
    val expected = appliedArguments(target)
    val nested = appliedArguments(expected.head).head
    Expr(
      target match
        case tqq"Either[List[$head], ..$tail]" =>
          val _: q.reflect.TypeRepr = head
          val _: Seq[q.reflect.TypeRepr] = tail
          sameReference(head, nested) &&
            tail.size == 1 &&
            sameReference(tail.head, expected(1))
        case _ => false
    )

  private def repeatedSemanticIdentityImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import Q051TypePatternProbe.*

    val listConstructor = TypeRepr.of[List[Any]] match
      case AppliedType(constructor, _) => constructor
      case other => report.errorAndAbort(s"expected List AppliedType, observed ${other.show}")
    val eitherConstructor = TypeRepr.of[Either[Any, Any]] match
      case AppliedType(constructor, _) => constructor
      case other => report.errorAndAbort(s"expected Either AppliedType, observed ${other.show}")
    val left = TypeRepr.of[List[Int]]
    val right = AppliedType(listConstructor, List(TypeRepr.of[Int]))
    val target = AppliedType(eitherConstructor, List(left, right))
    val expected = appliedArguments(target)
    Expr(
      target match
        case tqq"Either[..$arguments]" =>
          val _: Seq[q.reflect.TypeRepr] = arguments
          expected.size == 2 &&
            !sameReference(expected.head, expected(1)) &&
            sameReferences(arguments, expected)
        case _ => false
    )

  private def diagnosticImpl(name: Expr[String])(using Quotes): Expr[String] =
    val (parts, sequenceIndex) = name.valueOrAbort match
      case "multiple" => (List("Either[..", ", ..", "]"), 0)
      case "rank3" => (List("Either[...", "]"), 0)
      case "orphan" => (List("Either[.. ]"), 0)
      case "root" => (List("..", ""), 0)
      case "tuple" => (List("(..", ", Int)"), 0)
      case "function" => (List("(..", ") => Int"), 0)
      case "unsupported" => (List("Map[..", "]"), 0)
      case "selected" => (List("scala.Either[..", "]"), 0)
      case "dynamic" => (List("", "[..", "]"), 1)
      case "exceeds-arity" =>
        (List("Either[Int, ..", ", String, Boolean]"), 0)
      case other => quotes.reflect.report.errorAndAbort(s"unknown diagnostic probe: $other")
    Expr(
      Q051TypePatternProbe
        .compile(parts, sequenceIndex)
        .fold(identity, _ => "<no-error>")
    )

  private def appliedArguments(using q: Quotes)(
      value: q.reflect.TypeRepr
  ): List[q.reflect.TypeRepr] =
    import q.reflect.*
    value match
      case AppliedType(_, arguments) => arguments
      case other => report.errorAndAbort(s"expected AppliedType, observed ${other.show}")

  private def render(using q: Quotes)(value: q.reflect.TypeRepr): String =
    TargetTypeReprInspector
      .inspect(value)
      .fold(_.message, _.render)

  private def sameReference(using q: Quotes)(
      left: q.reflect.TypeRepr,
      right: q.reflect.TypeRepr
  ): Boolean =
    left.asInstanceOf[AnyRef] eq right.asInstanceOf[AnyRef]

  private def sameReferences(using q: Quotes)(
      left: Seq[q.reflect.TypeRepr],
      right: Seq[q.reflect.TypeRepr]
  ): Boolean =
    left.size == right.size &&
      left.zip(right).forall((leftValue, rightValue) =>
        sameReference(using q)(leftValue, rightValue)
      )

  private def tuple2Expr(using Quotes)(
      rendered: List[String],
      identity: Boolean
  ): Expr[(List[String], Boolean)] =
    '{ (${ Expr.ofList(rendered.map(Expr(_))) }, ${ Expr(identity) }) }
