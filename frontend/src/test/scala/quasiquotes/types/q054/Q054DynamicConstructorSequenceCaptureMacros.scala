package quasiquotes.types.q054

import scala.quoted.*

import quasiquotes.types.{
  Q005HigherKindedBox,
  ReflectedTypeApplication,
  RuntimeTypeApplicationFixture,
  TargetTypeReprInspector,
  TypeNormalForm
}

final case class Q054ConstructorDomainResult(
    list: Boolean,
    either: Boolean,
    box: Boolean,
    higherKinded: Boolean,
    instanceInner: Boolean,
    q046ProperRoundTrips: Boolean,
    higherKindedCapturedArgumentRejectedByQ046: Boolean,
    instanceInnerRejectedByQ046: Boolean
)

object Q054DynamicConstructorSequenceCaptureMacros:
  inline def wholeArguments: (String, List[String], Boolean) =
    ${ wholeArgumentsImpl }
  inline def layouts: Boolean = ${ layoutsImpl }
  inline def constructorDomains: Q054ConstructorDomainResult =
    ${ constructorDomainsImpl }
  inline def identityCases: Boolean = ${ identityCasesImpl }
  inline def targetBoundaries: Boolean = ${ targetBoundariesImpl }
  inline def diagnostic(inline name: String): String = ${ diagnosticImpl('name) }

  private def wholeArgumentsImpl(using q: Quotes): Expr[(String, List[String], Boolean)] =
    import q.reflect.*
    import Q054TypePatternProbe.*

    val target = TypeRepr.of[Either[Int, String]]
    val expected = application(target)
    target match
      case tqq"$constructor[..$arguments]" =>
        val _: q.reflect.TypeRepr = constructor
        val _: Seq[q.reflect.TypeRepr] = arguments
        val rendered = arguments.toList.map(render)
        val exact = sameReference(constructor, expected._1) &&
          sameReferences(arguments, expected._2)
        '{
          (
            ${ Expr(constructorName(using q)(constructor)) },
            ${ Expr.ofList(rendered.map(Expr(_))) },
            ${ Expr(exact) }
          )
        }
      case _ => '{ ("no-match", Nil, false) }

  private def layoutsImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import Q054TypePatternProbe.*

    val target = TypeRepr.of[Either[Int, String]]
    val (expectedConstructor, expectedArguments) = application(target)

    val whole = target match
      case tqq"$constructor[..$arguments]" =>
        sameReference(constructor, expectedConstructor) &&
          sameReferences(arguments, expectedArguments)
      case _ => false

    val headTail = target match
      case tqq"$constructor[$head, ..$tail]" =>
        val _: TypeRepr = constructor
        val _: TypeRepr = head
        val _: Seq[TypeRepr] = tail
        sameReference(constructor, expectedConstructor) &&
          sameReference(head, expectedArguments.head) &&
          sameReferences(tail, expectedArguments.tail)
      case _ => false

    val initLast = target match
      case tqq"$constructor[..$init, $last]" =>
        val _: TypeRepr = constructor
        val _: Seq[TypeRepr] = init
        val _: TypeRepr = last
        sameReference(constructor, expectedConstructor) &&
          sameReferences(init, expectedArguments.init) &&
          sameReference(last, expectedArguments.last)
      case _ => false

    val firstMiddleLast = target match
      case tqq"$constructor[$first, ..$middle, $last]" =>
        val _: TypeRepr = constructor
        val _: TypeRepr = first
        val _: Seq[TypeRepr] = middle
        val _: TypeRepr = last
        sameReference(constructor, expectedConstructor) &&
          sameReference(first, expectedArguments.head) &&
          middle.isEmpty &&
          sameReference(last, expectedArguments.last)
      case _ => false

    val fixedConstraint = target match
      case tqq"$constructor[Int, ..$tail]" =>
        val _: TypeRepr = constructor
        val _: Seq[TypeRepr] = tail
        sameReference(constructor, expectedConstructor) &&
          sameReferences(tail, expectedArguments.tail)
      case _ => false

    Expr(whole && headTail && initLast && firstMiddleLast && fixedConstraint)

  private def constructorDomainsImpl(using q: Quotes): Expr[Q054ConstructorDomainResult] =
    import q.reflect.*
    import Q054TypePatternProbe.*

    def captured(target: TypeRepr): Option[(TypeRepr, Seq[TypeRepr])] =
      target match
        case tqq"$constructor[..$arguments]" => Some(constructor -> arguments)
        case _ => None

    def exactCapture(target: TypeRepr): Boolean =
      val expected = application(target)
      captured(target).exists { case (constructor, arguments) =>
        sameReference(constructor, expected._1) &&
          sameReferences(arguments, expected._2)
      }

    def q046RoundTrip(target: TypeRepr): Boolean =
      captured(target).exists { case (constructor, arguments) =>
        ReflectedTypeApplication
          .build(using q)(constructor, arguments)
          .exists(result =>
            result =:= target &&
              (application(result) match
                case (found, elements) =>
                  sameReference(found, constructor) &&
                    sameReferences(elements, arguments))
          )
      }

    val list = TypeRepr.of[List[Int]]
    val either = TypeRepr.of[Either[Int, String]]
    val box = TypeRepr.of[Q054Box[Int]]
    val higherKinded = TypeRepr.of[Q005HigherKindedBox[List]]
    val instanceInner = TypeRepr.of[RuntimeTypeApplicationFixture.outer.Inner[Int]]
    val properRoundTrips = List(list, either, box)

    val result = Q054ConstructorDomainResult(
      exactCapture(list),
      exactCapture(either),
      exactCapture(box),
      exactCapture(higherKinded),
      exactCapture(instanceInner),
      properRoundTrips.forall(q046RoundTrip),
      captured(higherKinded).exists { case (constructor, arguments) =>
        ReflectedTypeApplication.build(using q)(constructor, arguments).isLeft
      },
      captured(instanceInner).exists { case (constructor, arguments) =>
        ReflectedTypeApplication.build(using q)(constructor, arguments).isLeft
      }
    )
    '{
      Q054ConstructorDomainResult(
        ${ Expr(result.list) },
        ${ Expr(result.either) },
        ${ Expr(result.box) },
        ${ Expr(result.higherKinded) },
        ${ Expr(result.instanceInner) },
        ${ Expr(result.q046ProperRoundTrips) },
        ${ Expr(result.higherKindedCapturedArgumentRejectedByQ046) },
        ${ Expr(result.instanceInnerRejectedByQ046) }
      )
    }

  private def identityCasesImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import Q054TypePatternProbe.*

    def exact(target: TypeRepr): Boolean =
      val expected = application(target)
      target match
        case tqq"$constructor[..$arguments]" =>
          sameReference(constructor, expected._1) &&
            sameReferences(arguments, expected._2)
        case _ => false

    val nested = TypeRepr.of[Either[List[Int], Option[String]]]
    val listConstructor = application(TypeRepr.of[List[Any]])._1
    val eitherConstructor = application(TypeRepr.of[Either[Any, Any]])._1
    val left = TypeRepr.of[List[Int]]
    val right = AppliedType(listConstructor, List(TypeRepr.of[Int]))
    val assembled = AppliedType(eitherConstructor, List(left, right))
    val aliasExpanded = TypeRepr.of[Q054Alias[Int]]
    val equalDistinct =
      val arguments = application(assembled)._2
      arguments.size == 2 &&
        (arguments.head =:= arguments(1)) &&
        !sameReference(arguments.head, arguments(1))

    Expr(exact(nested) && exact(assembled) && equalDistinct && exact(aliasExpanded))

  private def targetBoundariesImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import Q054TypePatternProbe.*

    def matches(target: TypeRepr): Boolean =
      target match
        case tqq"$constructor[..$arguments]" =>
          val _: TypeRepr = constructor
          val _: Seq[TypeRepr] = arguments
          true
        case _ => false

    val typeLambda = TypeRepr.of[[A] =>> Either[A, A]]
    val refinement = TypeRepr.of[AnyRef { type Value = Int }]
    Expr(
      !matches(TypeRepr.of[Int]) &&
        !matches(null.asInstanceOf[TypeRepr]) &&
        !matches(typeLambda) &&
        !matches(refinement)
    )

  private def diagnosticImpl(name: Expr[String])(using Quotes): Expr[String] =
    val (parts, sequenceIndex) = name.valueOrAbort match
      case "missing-rank" => (List("", "[", "]"), 1)
      case "rank3" => (List("", "[...", "]"), 1)
      case "two-ranked" => (List("", "[..", ", ..", "]"), 1)
      case "constructor-outside-apply" => (List("", ""), 0)
      case "unsupported-fixed" =>
        (List("", "[Map[Int, String], ..", "]"), 1)
      case other => quotes.reflect.report.errorAndAbort(s"unknown Q054 diagnostic: $other")
    val result = Q054TypePatternProbe.compile(parts, sequenceIndex).fold(identity, _ => "<no-error>")
    Expr(result)

  private def application(using q: Quotes)(
      value: q.reflect.TypeRepr
  ): (q.reflect.TypeRepr, List[q.reflect.TypeRepr]) =
    import q.reflect.*
    value match
      case AppliedType(constructor, arguments) => constructor -> arguments
      case other => report.errorAndAbort(s"expected AppliedType, observed ${other.show}")

  private def constructorName(using q: Quotes)(value: q.reflect.TypeRepr): String =
    import q.reflect.*
    value match
      case TypeRef(_, name) => name
      case other => other.show

  private def render(using q: Quotes)(value: q.reflect.TypeRepr): String =
    TargetTypeReprInspector.inspect(value) match
      case Right(TypeNormalForm.STypeIdent(name)) => name
      case Right(normalForm) => normalForm.render
      case Left(error) => error.message

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
