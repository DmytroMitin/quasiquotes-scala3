package quasiquotes.terms.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.util.SourceFile

private[quasiquotes] final class GeneratedOriginTermResult private[dotty] (
    val tree: untpd.Tree,
    val generatedSource: String,
    val sourceFile: SourceFile
):
  def virtualSourceName: String = sourceFile.path

  override def toString: String =
    s"GeneratedOriginTermResult(source=${sourceFile.path}, length=${generatedSource.length})"
