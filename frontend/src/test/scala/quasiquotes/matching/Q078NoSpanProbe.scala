package quasiquotes.matching

import scala.quoted.{Expr, Quotes, Varargs, staging}
import quasiquotes.source.ReflectedPositionProvenance

object Q078NoSpanProbe:
  def route(compile: (Expr[StringContext], Expr[Quotes], Quotes) => Expr[Any]): String =
    given staging.Compiler = staging.Compiler.make(getClass.getClassLoader)
    staging.withQuotes:
      val q = summon[Quotes]
      import q.reflect.*
      val constructor = Select.unique(Ref(Symbol.requiredModule("scala.StringContext")), "apply")
      val values = Repeated(List("", ".", ".", ".", "").map(value => Literal(StringConstant(value))), TypeTree.of[String])
      val context = Apply(constructor, List(Typed(values, Inferred(TypeRepr.of[Seq[String]])))).asExprOf[StringContext]
      val sourceAbsent = ReflectedPositionProvenance.sourceCode(using q)(context.asTerm.pos).isEmpty
      val static = context match
        case '{ StringContext(${ Varargs(parts) }*) } => parts.size == 5
        case _ => false
      val diagnostic =
        try
          compile(context, '{ throw new IllegalStateException("unused caller") }, q)
          "unexpected-success"
        catch
          case error: Throwable => error.toString
      s"sourceAbsent=$sourceAbsent;static=$static;$diagnostic"

  def standard: String = route((context, caller, q) => Q078StandardMacro.extractor(context, caller)(using q))
