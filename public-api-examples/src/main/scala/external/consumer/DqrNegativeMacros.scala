package external.consumer

import scala.quoted.*

object DqrNegativeMacros:
  inline def nullLiteralPart: Int = ${ nullLiteralPartImpl }
  inline def malformed: Int = ${ malformedImpl }
  inline def parameterless: Int = ${ parameterlessImpl }
  inline def twoParameters: Int = ${ twoParametersImpl }
  inline def multipleClauses: Int = ${ multipleClausesImpl }
  inline def contextualParameter: Int = ${ contextualParameterImpl }
  inline def typeParameter: Int = ${ typeParameterImpl }
  inline def bodyHole: Int = ${ bodyHoleImpl }
  inline def nameHole: Int = ${ nameHoleImpl }
  inline def wholeDefinitionHole: Int = ${ wholeDefinitionHoleImpl }
  inline def wrongArity: Int = ${ wrongArityImpl }
  inline def unsupportedType: Int = ${ unsupportedTypeImpl }
  inline def unequalTypes: Int = ${ unequalTypesImpl }
  inline def wrongBodyBinder: Int = ${ wrongBodyBinderImpl }
  inline def constructorSyntax: Int = ${ constructorSyntaxImpl }
  inline def otherDefinitionSyntax: Int = ${ otherDefinitionSyntaxImpl }
  inline def sequenceShapedHoles: Int = ${ sequenceShapedHolesImpl }

  private def nullLiteralPartImpl(using q: Quotes): Expr[Int] =
    import q.reflect.*

    rejected(StringContext(null, "): ", " = value"), TypeRepr.of[Int], TypeRepr.of[Int])

  private def malformedImpl(using q: Quotes): Expr[Int] =
    import q.reflect.*
    rejected(StringContext("not a definition ", " then ", ""), TypeRepr.of[Int], TypeRepr.of[Int])

  private def parameterlessImpl(using q: Quotes): Expr[Int] =
    import q.reflect.*
    rejected(StringContext("def id: ", " = ", ""), TypeRepr.of[Int], TypeRepr.of[Int])

  private def twoParametersImpl(using q: Quotes): Expr[Int] =
    import q.reflect.*
    rejected(StringContext("def id(x: ", ", y: Int): ", " = x"), TypeRepr.of[Int], TypeRepr.of[Int])

  private def multipleClausesImpl(using q: Quotes): Expr[Int] =
    import q.reflect.*
    rejected(StringContext("def id(x: ", ")(y: Int): ", " = x"), TypeRepr.of[Int], TypeRepr.of[Int])

  private def contextualParameterImpl(using q: Quotes): Expr[Int] =
    import q.reflect.*
    rejected(StringContext("def id(using x: ", "): ", " = x"), TypeRepr.of[Int], TypeRepr.of[Int])

  private def typeParameterImpl(using q: Quotes): Expr[Int] =
    import q.reflect.*
    rejected(StringContext("def id[A](x: ", "): ", " = x"), TypeRepr.of[Int], TypeRepr.of[Int])

  private def bodyHoleImpl(using q: Quotes): Expr[Int] =
    import q.reflect.*
    rejected(StringContext("def id(x: ", "): ", " = ", ""), TypeRepr.of[Int], TypeRepr.of[Int], TypeRepr.of[Int])

  private def nameHoleImpl(using q: Quotes): Expr[Int] =
    import q.reflect.*
    rejected(StringContext("def ", "(x: ", "): ", " = x"), TypeRepr.of[Int], TypeRepr.of[Int], TypeRepr.of[Int])

  private def wholeDefinitionHoleImpl(using q: Quotes): Expr[Int] =
    import q.reflect.*
    rejected(StringContext("", "", ""), TypeRepr.of[Int], TypeRepr.of[Int])

  private def wrongArityImpl(using q: Quotes): Expr[Int] =
    import q.reflect.*
    rejected(StringContext("def id(x: ", "): ", " = x"), TypeRepr.of[Int])

  private def unsupportedTypeImpl(using q: Quotes): Expr[Int] =
    import q.reflect.*
    rejected(StringContext("def id(x: ", "): ", " = x"), TypeRepr.of[Map[Int, String]], TypeRepr.of[Map[Int, String]])

  private def unequalTypesImpl(using q: Quotes): Expr[Int] =
    import q.reflect.*
    rejected(StringContext("def id(x: ", "): ", " = x"), TypeRepr.of[Int], TypeRepr.of[String])

  private def wrongBodyBinderImpl(using q: Quotes): Expr[Int] =
    import q.reflect.*
    rejected(StringContext("def id(x: ", "): ", " = y"), TypeRepr.of[Int], TypeRepr.of[Int])

  private def constructorSyntaxImpl(using q: Quotes): Expr[Int] =
    import q.reflect.*
    rejected(StringContext("def this(x: ", "): ", " = x"), TypeRepr.of[Int], TypeRepr.of[Int])

  private def otherDefinitionSyntaxImpl(using q: Quotes): Expr[Int] =
    import q.reflect.*
    rejected(StringContext("val x: ", " = ", ""), TypeRepr.of[Int], TypeRepr.of[Int])

  private def sequenceShapedHolesImpl(using q: Quotes): Expr[Int] =
    import q.reflect.*
    rejected(StringContext("def id(x: ", "", "): ", " = x"), TypeRepr.of[Int], TypeRepr.of[Int], TypeRepr.of[Int])

  private def rejected(using q: Quotes)(
      sc: StringContext,
      arguments: q.reflect.TypeRepr*
  ): Expr[Int] =
    import quasiquotes.construct.Quasiquotes.*

    sc.dqr(arguments*)
    Expr(0)
