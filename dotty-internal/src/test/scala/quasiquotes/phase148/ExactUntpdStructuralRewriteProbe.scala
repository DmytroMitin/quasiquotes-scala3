package quasiquotes.phase148

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.CompilationUnit
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.parsing.Parsers
import dotty.tools.dotc.reporting.StoreReporter
import dotty.tools.dotc.util.{NoSource, SourceFile}

/** Test-only exact-compiler probe. This is deliberately not a production U API. */
private[quasiquotes] object ExactUntpdStructuralRewriteProbe:
  final case class Result(
      originalClass: untpd.TypeDef,
      originalTemplate: untpd.Template,
      originalAnnotation: untpd.Tree,
      originalKeep: untpd.DefDef,
      originalChange: untpd.DefDef,
      originalOpaque: untpd.ValDef,
      originalCall: untpd.DefDef,
      prefix: List[untpd.Tree],
      suffix: List[untpd.Tree],
      rebuiltClass: untpd.TypeDef,
      rebuiltTemplate: untpd.Template,
      rebuiltAnnotation: untpd.Tree,
      rebuiltKeep: untpd.Tree,
      replacementChange: untpd.DefDef,
      replacementBody: untpd.Tree,
      rebuiltOpaque: untpd.Tree,
      rebuiltCall: untpd.Tree,
      bodyNames: List[String],
      rebuiltBodyNames: List[String]
  ):
    val provenanceKinds: List[String] =
      List("preserved", "reconstructed", "opaque-preserved")

  def rewrite(source: String)(using Context): Result =
    val originalClass = parseClass(source)
    val originalTemplate = originalClass.rhs match
      case value: untpd.Template => value
      case other =>
        throw new AssertionError(
          s"expected Template, found ${other.getClass.getSimpleName}"
        )
    val originalAnnotation = originalClass.mods.annotations match
      case value :: Nil => value
      case other =>
        throw new AssertionError(s"expected one class annotation, found $other")

    val originalKeep = methodNamed(originalTemplate.body, "keep")
    val originalChange = methodNamed(originalTemplate.body, "change")
    val originalOpaque = valueNamed(originalTemplate.body, "opaque")
    val originalCall = methodNamed(originalTemplate.body, "call")
    val changeIndex = originalTemplate.body.indexWhere(_.eq(originalChange))
    if changeIndex < 0 then
      throw new AssertionError("change method is not in the template body")
    val (prefix, changeAndSuffix) = originalTemplate.body.splitAt(changeIndex)
    val suffix = changeAndSuffix match
      case head :: tail if head.eq(originalChange) => tail
      case other => throw new AssertionError(s"unexpected body split: $other")

    given SourceFile = NoSource
    val replacementBody = untpd.Number("20", untpd.NumberKind.Whole(10))
    val replacementChange = untpd
      .DefDef(
        originalChange.name,
        Nil,
        originalChange.tpt,
        replacementBody
      )
      .withMods(originalChange.mods)
    val rebuiltBody = prefix ::: replacementChange :: suffix
    val rebuiltTemplate = untpd.Template(
      originalTemplate.constr,
      originalTemplate.parentsOrDerived,
      originalTemplate.derived,
      originalTemplate.self,
      rebuiltBody
    )
    val rebuiltClass = untpd
      .TypeDef(originalClass.name, rebuiltTemplate)
      .withMods(originalClass.mods)
    val rebuiltAnnotation = rebuiltClass.mods.annotations match
      case value :: Nil => value
      case other =>
        throw new AssertionError(
          s"expected one rebuilt class annotation, found $other"
        )

    Result(
      originalClass,
      originalTemplate,
      originalAnnotation,
      originalKeep,
      originalChange,
      originalOpaque,
      originalCall,
      prefix,
      suffix,
      rebuiltClass,
      rebuiltTemplate,
      rebuiltAnnotation,
      rebuiltBody.head,
      replacementChange,
      replacementBody,
      rebuiltBody(2),
      rebuiltBody(3),
      names(originalTemplate.body),
      names(rebuiltBody)
    )

  def parseTerm(source: String)(using outerContext: Context): untpd.Tree =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("Phase148Term.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parser = new Parsers.Parser(unit.source)
    val parsed = parser.expr()
    val messages = reporter.pendingMessages.map(_.message).toList
    if messages.nonEmpty then
      throw new AssertionError(s"term parser diagnostics: $messages")
    parsed

  private def parseClass(
      source: String
  )(using outerContext: Context): untpd.TypeDef =
    val reporter = new StoreReporter(null)
    val unit = CompilationUnit("Phase148Class.scala", source)
    given Context = outerContext.fresh.setCompilationUnit(unit).setReporter(reporter)
    val parser = new Parsers.Parser(unit.source)
    val parsed = parser.parse()
    val messages = reporter.pendingMessages.map(_.message).toList
    if messages.nonEmpty then
      throw new AssertionError(s"class parser diagnostics: $messages")
    parsed match
      case packageDef: untpd.PackageDef =>
        packageDef.stats match
          case (value: untpd.TypeDef) :: Nil => value
          case other =>
            throw new AssertionError(s"expected one TypeDef, found $other")
      case other =>
        throw new AssertionError(
          s"expected PackageDef, found ${other.getClass.getSimpleName}"
        )

  private def methodNamed(
      body: List[untpd.Tree],
      expected: String
  ): untpd.DefDef =
    body.collectFirst {
      case value: untpd.DefDef if value.name.toString == expected => value
    }.getOrElse(throw new AssertionError(s"missing method $expected"))

  private def valueNamed(
      body: List[untpd.Tree],
      expected: String
  ): untpd.ValDef =
    body.collectFirst {
      case value: untpd.ValDef if value.name.toString == expected => value
    }.getOrElse(throw new AssertionError(s"missing value $expected"))

  private def names(body: List[untpd.Tree]): List[String] =
    body.map {
      case value: untpd.DefDef => value.name.toString
      case value: untpd.ValDef => value.name.toString
      case other => other.getClass.getSimpleName
    }
