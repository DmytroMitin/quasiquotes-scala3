package quasiquotes.types

import scala.annotation.experimental
import scala.quoted.staging.{Compiler, withQuotes}

final class Q005HigherKindedBox[F[_]]

/** Q005 test-only characterization of the public `AppliedType` boundary.
  *
  * This is deliberately not a quasiquote implementation. It records which
  * invalid applications are rejected by public reflection itself on every
  * supported compiler lane.
  */
@experimental
class Q005AppliedTypeFeasibilityTest extends munit.FunSuite:
  private final case class Attempt(
      constructed: Boolean,
      typechecked: Boolean,
      failureClass: Option[String],
      failureMessage: Option[String],
      rendered: Option[String]
  )

  private final case class Evidence(
      validUnary: Boolean,
      validBinary: Boolean,
      dynamicNested: Boolean,
      validHigherKindedArgument: Boolean,
      invalid: Map[String, Attempt],
      constructorIdentityPreserved: Boolean,
      argumentIdentityPreserved: Boolean,
      constructorArities: Map[String, Int],
      constructorParameterInfo: Map[String, List[String]]
  )

  test("public AppliedType preserves valid dynamic inputs but leaves invalid kind and arity unchecked"):
    given Compiler = Compiler.make(getClass.getClassLoader)
    val evidence = withQuotes:
      val q = summon[scala.quoted.Quotes]
      import q.reflect.*

      def constructorOf(applied: TypeRepr): TypeRepr =
        applied match
          case AppliedType(constructor, _) => constructor
          case other => report.errorAndAbort(s"Q005 expected an AppliedType, obtained ${other.show}")

      def attempt(constructor: TypeRepr, arguments: List[TypeRepr]): Attempt =
        try
          val result = AppliedType(constructor, arguments)
          try
            result.asType match
              case '[t] => Attempt(true, true, None, None, Some(result.show))
          catch
            case failure: Throwable =>
              Attempt(
                constructed = true,
                typechecked = false,
                failureClass = Some(failure.getClass.getName),
                failureMessage = Option(failure.getMessage),
                rendered = Some(result.show)
              )
        catch
          case failure: Throwable =>
            Attempt(
              constructed = false,
              typechecked = false,
              failureClass = Some(failure.getClass.getName),
              failureMessage = Option(failure.getMessage),
              rendered = None
            )

      val listConstructor = constructorOf(TypeRepr.of[List[Any]])
      val eitherConstructor = constructorOf(TypeRepr.of[Either[Any, Any]])
      val optionConstructor = constructorOf(TypeRepr.of[Option[Any]])
      val unaryArgument = TypeRepr.of[Int]
      val binaryArguments = List(TypeRepr.of[Int], TypeRepr.of[String])

      // Values are assembled as ordinary collections during macro expansion;
      // neither constructor choice nor list length is encoded in source syntax.
      val constructors = List(listConstructor, eitherConstructor)
      val argumentLists = List(List(unaryArgument), binaryArguments)
      val dynamicResults = constructors.zip(argumentLists).map(AppliedType(_, _))

      val nested = AppliedType(
        eitherConstructor,
        List(
          AppliedType(listConstructor, List(TypeRepr.of[Int])),
          AppliedType(optionConstructor, List(TypeRepr.of[String]))
        )
      )

      val validHigherKindedArgument =
        val higherKindedConstructor = constructorOf(TypeRepr.of[Q005HigherKindedBox[List]])
        val result = AppliedType(higherKindedConstructor, List(listConstructor))
        result =:= TypeRepr.of[Q005HigherKindedBox[List]]

      val identityArgument = TypeRepr.of[Boolean]
      val identityResult = AppliedType(listConstructor, List(identityArgument))
      val (returnedConstructor, returnedArgument) = identityResult match
        case AppliedType(foundConstructor, foundArgument :: Nil) =>
          (foundConstructor, foundArgument)
        case other => report.errorAndAbort(s"Q005 identity probe obtained ${other.show}")

      Evidence(
        validUnary = dynamicResults.head =:= TypeRepr.of[List[Int]],
        validBinary = dynamicResults(1) =:= TypeRepr.of[Either[Int, String]],
        dynamicNested = nested =:= TypeRepr.of[Either[List[Int], Option[String]]],
        validHigherKindedArgument = validHigherKindedArgument,
        invalid = Map(
          "zero" -> attempt(listConstructor, Nil),
          "too-few" -> attempt(eitherConstructor, List(TypeRepr.of[Int])),
          "too-many" -> attempt(listConstructor, List(TypeRepr.of[Int], TypeRepr.of[String])),
          "proper-type-constructor" -> attempt(TypeRepr.of[Int], List(TypeRepr.of[String])),
          "wrong-kind" -> attempt(listConstructor, List(listConstructor)),
          "higher-kinded-wrong-kind" -> attempt(
            constructorOf(TypeRepr.of[Q005HigherKindedBox[List]]),
            List(TypeRepr.of[Int])
          )
        ),
        constructorIdentityPreserved =
          returnedConstructor.asInstanceOf[AnyRef] eq listConstructor.asInstanceOf[AnyRef],
        argumentIdentityPreserved =
          returnedArgument.asInstanceOf[AnyRef] eq identityArgument.asInstanceOf[AnyRef],
        constructorArities = Map(
          "List" -> listConstructor.typeSymbol.primaryConstructor.paramSymss.flatten.count(_.isTypeParam),
          "Either" -> eitherConstructor.typeSymbol.primaryConstructor.paramSymss.flatten.count(_.isTypeParam),
          "HigherKindedBox" -> constructorOf(TypeRepr.of[Q005HigherKindedBox[List]])
            .typeSymbol.primaryConstructor.paramSymss.flatten.count(_.isTypeParam)
        ),
        constructorParameterInfo = Map(
          "List" -> listConstructor.typeSymbol.primaryConstructor.paramSymss.flatten
            .filter(_.isTypeParam).map(_.info.show),
          "Either" -> eitherConstructor.typeSymbol.primaryConstructor.paramSymss.flatten
            .filter(_.isTypeParam).map(_.info.show),
          "HigherKindedBox" -> constructorOf(TypeRepr.of[Q005HigherKindedBox[List]])
            .typeSymbol.primaryConstructor.paramSymss.flatten.filter(_.isTypeParam).map(_.info.show)
        )
      )

    assert(evidence.validUnary)
    assert(evidence.validBinary)
    assert(evidence.dynamicNested)
    assert(evidence.validHigherKindedArgument)
    assert(evidence.constructorIdentityPreserved)
    assert(evidence.argumentIdentityPreserved)
    assertEquals(evidence.constructorArities, Map("List" -> 1, "Either" -> 2, "HigherKindedBox" -> 1))
    assertEquals(evidence.constructorParameterInfo.keySet, Set("List", "Either", "HigherKindedBox"))
    println(s"Q005_CONSTRUCTOR_PARAMETER_INFO=${evidence.constructorParameterInfo}")
    evidence.invalid.foreach { case (label, attempt) =>
      assert(attempt.constructed, s"$label was unexpectedly rejected at AppliedType: $attempt")
      assert(attempt.typechecked, s"$label was unexpectedly rejected by asType: $attempt")
    }
