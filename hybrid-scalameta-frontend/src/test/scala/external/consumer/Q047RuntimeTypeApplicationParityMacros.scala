package external.consumer

import scala.quoted.*

import quasiquotes.types.Q005HigherKindedBox

object Q047RuntimeTypeApplicationParityMacros:
  inline def direct: Boolean = ${ directImpl }
  inline def umbrella: Boolean = ${ umbrellaImpl }

  private def directImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*

    def constructor(tpe: TypeRepr): TypeRepr = tpe match
      case AppliedType(found, _) => found

    def standard(found: TypeRepr, arguments: Seq[TypeRepr]): TypeRepr =
      import quasiquotes.types.QuasiTypequotes.tqr
      tqr"$found[..$arguments]"

    def optIn(found: TypeRepr, arguments: Seq[TypeRepr]): TypeRepr =
      import quasiquotes.scalameta.ScalametaQuasiquotes.tqr
      tqr"$found[..$arguments]"

    def sameObject(left: TypeRepr, right: TypeRepr): Boolean =
      left.asInstanceOf[AnyRef] eq right.asInstanceOf[AnyRef]

    def checked(found: TypeRepr, arguments: Seq[TypeRepr], expected: TypeRepr): Boolean =
      val standardResult = standard(found, arguments)
      val optInResult = optIn(found, arguments)
      def preservesInputs(result: TypeRepr): Boolean = result match
        case AppliedType(resultConstructor, resultArguments) =>
          sameObject(resultConstructor, found) &&
            resultArguments.size == arguments.size &&
            resultArguments.zip(arguments).forall(sameObject)
        case _ => false
      standardResult =:= optInResult &&
        optInResult =:= expected &&
        preservesInputs(standardResult) &&
        preservesInputs(optInResult)

    val list = constructor(TypeRepr.of[List[Int]])
    val option = constructor(TypeRepr.of[Option[String]])
    val either = constructor(TypeRepr.of[Either[Int, String]])
    val box = constructor(TypeRepr.of[Q005HigherKindedBox[List]])
    val int = TypeRepr.of[Int]
    val string = TypeRepr.of[String]

    val listResult = optIn(list, Vector(int))
    val optionResult = optIn(option, Vector(string))
    val checks = Vector(
      checked(list, Vector(int), TypeRepr.of[List[Int]]),
      checked(either, Vector(int, string), TypeRepr.of[Either[Int, String]]),
      checked(
        Vector(list, either).head,
        Vector(Vector(int), Vector(int, string)).head,
        TypeRepr.of[List[Int]]
      ),
      checked(either, Vector(listResult, optionResult), TypeRepr.of[Either[List[Int], Option[String]]]),
      checked(box, Vector(list), TypeRepr.of[Q005HigherKindedBox[List]])
    )
    Expr(checks.forall(identity))

  private def umbrellaImpl(using q: Quotes): Expr[Boolean] =
    import q.reflect.*
    import quasiquotes.scalameta.Quasiquotes.tqr

    val constructor = TypeRepr.of[Either[Int, String]] match
      case AppliedType(found, _) => found
    val arguments: Seq[TypeRepr] = Vector(TypeRepr.of[Int], TypeRepr.of[String])
    val result: q.reflect.TypeRepr = tqr"$constructor[..$arguments]"
    Expr(result =:= TypeRepr.of[Either[Int, String]])
