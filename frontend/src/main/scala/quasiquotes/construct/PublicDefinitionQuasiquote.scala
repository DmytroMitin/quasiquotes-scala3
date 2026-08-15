package quasiquotes.construct

import scala.quoted.*
import scala.util.matching.Regex

import quasiquotes.definitions.DefinitionName
import quasiquotes.publicapi.CompletedTerm
import quasiquotes.publicapi.CompletedType
import quasiquotes.publicapi.DefinitionConstruction
import quasiquotes.types.TargetTypeReprInspector
import quasiquotes.types.TypeNormalForm

private[quasiquotes] object PublicDefinitionQuasiquote:
  private val DiagnosticPrefix = "Invalid dqr definition template:"
  private val Prefix: Regex =
    raw"""\s*def\s+([A-Za-z_][A-Za-z0-9_]*)\s*\(\s*([A-Za-z_][A-Za-z0-9_]*)\s*:\s*""".r
  private val BetweenTypes: Regex = raw"""\s*\)\s*:\s*""".r
  private val Suffix: Regex =
    raw"""\s*=\s*([A-Za-z_][A-Za-z0-9_]*)\s*""".r

  def build(using q: Quotes)(
      sc: StringContext,
      args: Seq[q.reflect.TypeRepr]
  ): q.reflect.DefDef =
    import q.reflect.*

    def abort(detail: String): Nothing =
      report.errorAndAbort(s"$DiagnosticPrefix $detail")

    if sc == null then abort("StringContext must not be null.")
    val parts = sc.parts
    if parts == null || parts.isEmpty then
      abort("StringContext must contain exactly three literal parts.")
    if parts.exists(_ == null) then
      abort("StringContext literal parts must not be null.")
    if parts.size != 3 then
      abort("expected exactly two TypeRepr splice positions.")
    if args == null || args.size != 2 then
      abort(s"expected exactly two TypeRepr splices, but received ${Option(args).fold(0)(_.size)}.")

    val (methodNameSource, parameterNameSource) = parts(0) match
      case Prefix(methodName, parameterName) => (methodName, parameterName)
      case _ =>
        abort("expected `def method(parameter: $parameterType): $resultType = parameter`.")

    parts(1) match
      case BetweenTypes() => ()
      case _ =>
        abort("expected one ordinary parameter clause followed by the result type splice.")

    val bodyNameSource = parts(2) match
      case Suffix(bodyName) => bodyName
      case _ => abort("the body must be the literal declared parameter reference.")

    val methodName = DefinitionName.plain(methodNameSource).fold(
      _ => abort("the method name must be a valid ordinary Scala identifier."),
      identity
    )
    val parameterName = DefinitionName.plain(parameterNameSource).fold(
      _ => abort("the parameter name must be a valid ordinary Scala identifier."),
      identity
    )
    if bodyNameSource != parameterName.source then
      abort("the body must reference the declared parameter name exactly.")

    val inspectedTypes = args.zipWithIndex.map { (argument, index) =>
      TargetTypeReprInspector.inspect(argument).fold(
        error => abort(s"unsupported TypeRepr splice at ordinal $index: ${error.message}"),
        identity
      )
    }
    if inspectedTypes(0) != inspectedTypes(1) then
      abort("the parameter and result TypeRepr splices must have equal normalized types.")

    val completedType = toCompletedType(inspectedTypes.head).fold(abort, identity)
    val completedBody = CompletedTerm
      .definitionParameterReference(parameterName.decoded)
      .fold(error => abort(error.message), identity)
    DefinitionConstruction
      .singleParameterMethod(
        methodName.decoded,
        parameterName.decoded,
        completedType,
        completedType,
        completedBody
      )
      .fold(error => abort(error.message), _ => ())

    val methodType = MethodType(List(parameterName.decoded))(
      _ => List(args.head),
      _ => args(1)
    )
    val methodSymbol = Symbol.newMethod(
      Symbol.spliceOwner,
      methodName.decoded,
      methodType
    )
    DefDef(methodSymbol, parameterClauses =>
      parameterClauses match
        case List(List(parameter)) => Some(Ref(parameter.symbol))
        case _ => abort("generated method parameters violated the single-parameter contract.")
    )

  private def toCompletedType(normalForm: TypeNormalForm): Either[String, CompletedType] =
    normalForm match
      case TypeNormalForm.STypeIdent(name) => named(name)
      case TypeNormalForm.STypeApply(constructor, arguments) =>
        for
          completedConstructor <- toCompletedType(constructor)
          completedArguments <- collect(arguments.map(toCompletedType))
          result <- CompletedType
            .applied(completedConstructor, completedArguments.toVector)
            .left
            .map(_.message)
        yield result
      case TypeNormalForm.STypeTuple(elements) =>
        applied(s"Tuple${elements.size}", elements)
      case TypeNormalForm.STypeFunction(arguments, result) =>
        applied(s"Function${arguments.size}", arguments :+ result)

  private def named(name: String): Either[String, CompletedType] =
    CompletedType.named(name).left.map(_.message)

  private def applied(
      constructor: String,
      arguments: List[TypeNormalForm]
  ): Either[String, CompletedType] =
    for
      completedConstructor <- named(constructor)
      completedArguments <- collect(arguments.map(toCompletedType))
      result <- CompletedType
        .applied(completedConstructor, completedArguments.toVector)
        .left
        .map(_.message)
    yield result

  private def collect[A](values: List[Either[String, A]]): Either[String, List[A]] =
    values.foldRight[Either[String, List[A]]](Right(Nil)) { (value, accumulated) =>
      for
        head <- value
        tail <- accumulated
      yield head :: tail
    }
