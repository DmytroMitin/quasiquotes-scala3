package quasiquotes.hybrid.q036

import scala.quoted.*
import scala.meta.*
import scala.meta.parsers.Parsed
import scala.util.control.NonFatal

import _root_.quasiquotes.hybrid.TermQ3DialectPolicy
import _root_.quasiquotes.q036.Q036MixedClauseCandidateFactory

object Q036ScalametaDefinitionPatternMacros:
  def extractor(
      context: Expr[StringContext],
      callerQuotes: Expr[Quotes]
  )(using Quotes): Expr[Any] =
    val parts = context match
      case '{ StringContext(${ Varargs(partExpressions) }*) } =>
        val values = partExpressions.toList.map(_.value)
        Option.when(values.forall(_.nonEmpty))(values.flatten)
      case _ => None

    parts match
      case Some(values) =>
        validatePatternParts(values) match
          case Right(_) =>
            '{ Q036MixedClauseCandidateFactory.capturedModifiers(using $callerQuotes) }
          case Left(detail) =>
            quotes.reflect.report.errorAndAbort(
              s"Invalid Q036 typed-Scalameta dqq Definition template: $detail.",
              context
            )
      case None =>
        quotes.reflect.report.errorAndAbort(
          "Q036 typed-Scalameta mixed-clause Definition template must be statically known.",
          context
        )

  private def validatePatternParts(parts: List[String]): Either[String, Unit] =
    parts match
      case List(beforeModifiers, beforeName, beforeOrdinary, beforeImplicit, beforeResult, beforeBody, suffix)
          if beforeModifiers.trim.isEmpty &&
            beforeName.matches("(?s)\\s+def\\s+") &&
            beforeOrdinary.matches("(?s)\\s*\\(\\s*\\.\\.\\s*") &&
            beforeImplicit.matches("(?s)\\s*\\)\\s*\\(\\s*implicit\\s+\\.\\.\\s*") &&
            beforeResult.matches("(?s)\\s*\\)\\s*:\\s*") &&
            beforeBody.matches("(?s)\\s*=\\s*") &&
            suffix.trim.isEmpty =>
        val literalSource = parts.mkString
        def fresh(prefix: String, occupied: Set[String]): String =
          Iterator.from(0).map(index => s"$prefix$index").find(name =>
            !literalSource.contains(name) && !occupied.contains(name)
          ).get
        val method = fresh("__q036_method_", Set.empty)
        val ordinaryOne = fresh("__q036_ordinary_", Set(method))
        val ordinaryTwo = fresh("__q036_ordinary_", Set(method, ordinaryOne))
        val implicitOne = fresh("__q036_implicit_", Set(method, ordinaryOne, ordinaryTwo))
        val implicitTwo = fresh("__q036_implicit_", Set(method, ordinaryOne, ordinaryTwo, implicitOne))
        val result = fresh("__q036_result_", Set(method, ordinaryOne, ordinaryTwo, implicitOne, implicitTwo))
        val body = fresh("__q036_body_", Set(method, ordinaryOne, ordinaryTwo, implicitOne, implicitTwo, result))
        val ordinaryMarker = beforeOrdinary.lastIndexOf("..")
        val implicitMarker = beforeImplicit.lastIndexOf("..")
        val source =
          "@deprecated(\"q036\", \"\") private[quasiquotes] final" + beforeName + method +
            beforeOrdinary.substring(0, ordinaryMarker) + s"$ordinaryOne: Int, $ordinaryTwo: String" +
            beforeImplicit.substring(0, implicitMarker) +
            s"$implicitOne: Ordering[Int], $implicitTwo: Numeric[Int]" + beforeResult + result +
            beforeBody + body + suffix
        try
          TermQ3DialectPolicy.selected(source).parse[Stat] match
            case Parsed.Success(definition: Defn.Def) =>
              val groups = definition.paramClauseGroups
              val clauses = groups.flatMap(_.paramClauses)
              val modifierSyntax = definition.mods.map(_.syntax)
              val valid =
                definition.name.value == method &&
                  modifierSyntax.exists(_.startsWith("@deprecated")) &&
                  modifierSyntax.contains("private[quasiquotes]") &&
                  modifierSyntax.contains("final") &&
                  groups.size == 1 && groups.head.tparamClause.values.isEmpty &&
                  clauses.size == 2 && clauses.head.mod.isEmpty &&
                  clauses(1).mod.exists(_.syntax == "implicit") &&
                  clauses.head.values.map(_.name.value) == List(ordinaryOne, ordinaryTwo) &&
                  clauses.head.values.forall(_.mods.isEmpty) &&
                  clauses(1).values.map(_.name.value) == List(implicitOne, implicitTwo) &&
                  clauses(1).values.forall(_.mods.map(_.syntax) == List("implicit")) &&
                  clauses.flatten.forall(_.default.isEmpty) &&
                  definition.decltpe.exists(_.syntax == result) &&
                  (definition.body match
                    case value: Term.Name => value.value == body
                    case _ => false)
              Either.cond(valid, (), "Scalameta sentinels did not preserve the exact mixed implicit structure")
            case Parsed.Success(other) => Left(s"Scalameta parsed ${other.productPrefix} instead of Defn.Def")
            case error: Parsed.Error => Left(s"Scalameta rejected sentinel source: ${error.message}")
        catch
          case NonFatal(error) => Left(s"Scalameta failure: ${error.getClass.getSimpleName}")
      case _ =>
        Left("only `$mods def $name(..$params)(implicit ..$implicitParams): $result = $body` is selected")

object Q036ScalametaDefinitionPattern:
  extension (inline context: StringContext)
    transparent inline def dqq(using q: Quotes) =
      ${ Q036ScalametaDefinitionPatternMacros.extractor('context, 'q) }
