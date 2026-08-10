package quasiquotes.types

import quasiquotes.parser.TypeShape

/** Centralized presentation text for the bounded experimental type surface. */
private[quasiquotes] object TypeDiagnosticMessages:
  val SupportedConstructors: String = "List/1, Option/1, Either/2"
  val SupportedNormalFormIdentifiers: String = "Int, String, Boolean, AnyVal"
  val SupportedConstructionIdentifiers: String = "Int, String, Boolean"

  def unsupportedAppliedConstructor(
      name: String,
      actualArity: Int
  ): String =
    AppliedTypeConstructorPolicy.named(name) match
      case Some(constructor) =>
        val noun = if constructor.requiredArity == 1 then "argument" else "arguments"
        s"Expected exactly ${constructor.requiredArity} type $noun for `$name`, but found $actualArity."
      case None =>
        s"Unsupported applied type constructor `$name`; supported constructors are $SupportedConstructors."

  def selectedType(qualifier: TypeShape, name: String): String =
    val selected = s"${selectedPrefix(qualifier)}.$name"
    s"Selected type syntax `$selected` is not supported; use unqualified `$name` in the current experimental surface."

  def selectedConstructor(qualifier: TypeShape, name: String): String =
    val selected = s"${selectedPrefix(qualifier)}.$name[...]"
    s"Selected type constructor syntax `$selected` is not supported; use unqualified `$name[...]` in the current experimental surface."

  def constructorHole(name: String): String =
    s"Type-constructor hole `$$$name[...]` is not supported; use one of the fixed constructors $SupportedConstructors."

  def unsupportedNormalFormIdentifier(name: String): String =
    s"Unsupported type identifier `$name`; supported identifiers are $SupportedNormalFormIdentifiers."

  def unsupportedConstructionIdentifier(name: String): String =
    s"Unsupported type-construction identifier `$name`; supported identifiers are $SupportedConstructionIdentifiers."

  def unsupportedTypeSyntax(operation: String): String =
    s"Unsupported type syntax for $operation; supported applied constructors are $SupportedConstructors."

  def unsupportedTupleArity(operation: String, actualArity: Int): String =
    s"Unsupported tuple arity for $operation: expected 2 or 3 elements, but found $actualArity."

  def unsupportedFunctionArity(operation: String, actualArity: Int): String =
    s"Unsupported function arity for $operation: expected 1 or 2 parameters, but found $actualArity."

  val unsupportedTargetType: String =
    s"Unsupported target type representation; supported targets use $SupportedNormalFormIdentifiers, $SupportedConstructors, Tuple2/3, or Function1/2."

  private def selectedPrefix(shape: TypeShape): String =
    shape match
      case TypeShape.Identifier(name) => name
      case TypeShape.Select(qualifier, name) =>
        s"${selectedPrefix(qualifier)}.$name"
      case _ => "<selected>"
