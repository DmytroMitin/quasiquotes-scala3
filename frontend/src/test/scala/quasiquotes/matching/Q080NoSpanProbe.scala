package quasiquotes.matching

import scala.quoted.{Expr, Quotes, Varargs, staging}
import quasiquotes.source.ReflectedPositionProvenance

object Q080NoSpanProbe:
  def route(partValues: List[String]): String =
    given staging.Compiler =
      staging.Compiler.make(getClass.getClassLoader)

    staging.withQuotes:
      val q = summon[Quotes]
      import q.reflect.*

      val stringContext = Ref(Symbol.requiredModule("scala.StringContext"))
      val constructor = Select.unique(stringContext, "apply")
      val parts = Repeated(
        partValues.map(value => Literal(StringConstant(value))),
        TypeTree.of[String]
      )
      val generatedContext: Expr[StringContext] = Apply(
        constructor,
        List(Typed(parts, Inferred(TypeRepr.of[Seq[String]])))
      ).asExprOf[StringContext]
      val source = ReflectedPositionProvenance.sourceCode(using q)(generatedContext.asTerm.pos)
      val staticParts = generatedContext match
        case '{ StringContext(${ Varargs(partExpressions) }*) } =>
          partExpressions.toList.map(_.valueOrAbort)
        case _ => Nil
      val routing =
        try
          QuasiPatternMacro.extractor(
            generatedContext,
            '{ null.asInstanceOf[Quotes] }
          )(using q)
          "legacy-routed"
        catch
          case error: Throwable =>
            "failure:" + error.getClass.getName + ":" +
              Option(error.getMessage).getOrElse("<no message>")

      s"source=${source.fold("none")(_ => "present")};static=${staticParts.nonEmpty};$routing"
