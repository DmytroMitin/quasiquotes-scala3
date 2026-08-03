package quasiquotes.definitions.dotty

import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.util.SourceFile

private[quasiquotes] final class GeneratedOriginDefinitionResult private[dotty] (
    val tree: untpd.Tree,
    val generatedSource: String,
    val sourceFile: SourceFile
):
  def virtualSourceName: String = sourceFile.path

  override def toString: String =
    s"GeneratedOriginDefinitionResult(source=${sourceFile.path}, length=${generatedSource.length})"
