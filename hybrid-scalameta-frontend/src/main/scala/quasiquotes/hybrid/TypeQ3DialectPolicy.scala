package quasiquotes.hybrid

import scala.meta.Dialect
import scala.meta.dialects

/** Compiler-line policy for the unpublished side-by-side type frontend. */
private[quasiquotes] object TypeQ3DialectPolicy:
  val compilerVersion: String =
    _root_.dotty.tools.dotc.config.Properties.versionNumberString

  val selected: Dialect =
    if compilerVersion.startsWith("3.8") then dialects.Scala38
    else dialects.Scala3

  val selectedName: String =
    if compilerVersion.startsWith("3.8") then "Scala38"
    else "Scala3"
