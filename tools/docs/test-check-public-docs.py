#!/usr/bin/env python3
"""Behavior tests for the public documentation boundary checker."""

from pathlib import Path
import subprocess
import sys
import tempfile
import unittest


CHECKER = Path(__file__).with_name("check-public-docs.py")


class PublicDocsCheckTest(unittest.TestCase):
    def make_fixture(
        self,
        root: Path,
        *,
        matrix_link: str = "docs/SYNTAX_SUPPORT_MATRIX.md",
        dqr_status: str = "Public now, exact bounded shape",
    ) -> None:
        docs = root / "docs"
        docs.mkdir(parents=True)
        definitions = root / "frontend/src/main/scala/quasiquotes/definitions"
        construct = root / "frontend/src/main/scala/quasiquotes/construct"
        matching = root / "frontend/src/main/scala/quasiquotes/matching"
        types = root / "frontend/src/main/scala/quasiquotes/types"
        definitions.mkdir(parents=True)
        construct.mkdir(parents=True)
        matching.mkdir(parents=True)
        types.mkdir(parents=True)

        (root / "README.md").write_text(
            f"[matrix]({matrix_link})\n"
            "<!-- public-surface-table:start -->\n"
            "| Role | Interpolated syntax | Interpolator availability | Programmatic API | Function/API availability |\n"
            "| --- | --- | --- | --- | --- |\n"
            "| Term construction | `qr\"...\"` | Public now | `QuasiquoteBuilder.build(...)` | Public now |\n"
            "| Term pattern matching | `case qq\"...\"` | Public now | `QuasiPattern.term(...)`, `termOrThrow(...)` | Public now |\n"
            "| Type construction | `tqr\"...\"` | Public now | `QuasiTypequotes.tqr(...)` | Public research API |\n"
            "| Type pattern matching | `case tqq\"...\"` | Public now | `QuasiTypequotes.tqq(...)` / `QuasiTypePattern.*` | Public research API |\n"
            f"| Definition construction | `dqr\"def id(x: $parameterType): $resultType = x\"` | {dqr_status} | `DefinitionConstruction.*` | Public bounded compiler-free API |\n"
            "| Definition pattern matching | `case dqq\"...\"` | TODO / not implemented | — | Not yet |\n"
            "<!-- public-surface-table:end -->\n",
            encoding="utf-8",
        )
        (docs / "SYNTAX_SUPPORT_MATRIX.md").write_text(
            "# Syntax support matrix\n\n"
            "SUPPORTED BOUNDED INTERNAL NOT_YET NOT_PLANNED\n\n"
            "https://docs.scala-lang.org/overviews/quasiquotes/syntax-summary.html\n"
            "https://scalameta.org/docs/trees/quasiquotes\n\n"
            "Any change that adds, removes, or materially alters a term, type, or definition syntax family must update this matrix.\n",
            encoding="utf-8",
        )
        (docs / "PUBLIC_API_BASELINE.tsv").write_text(
            "module\towner\tkind\tname\tsignature\n"
            "frontend\tquasiquotes.construct.Quasiquotes\tdef\tqr\tqr signature\n"
            "frontend\tquasiquotes.construct.QuasiquoteBuilder\tdef\tbuild\tbuild signature\n"
            "frontend\tquasiquotes.matching.QuasiPattern\tdef\tterm\tterm signature\n"
            "frontend\tquasiquotes.matching.QuasiPattern\tdef\ttermOrThrow\ttermOrThrow signature\n"
            "frontend\tquasiquotes.matching.QuasiPattern\tdef\tqq\tqq(using q: Quotes): TermPatternExtractor[q.reflect.Term]\n"
            "frontend\tquasiquotes.matching.TermPatternExtractor\tdef\tunapplySeq\tunapply signature\n"
            "frontend\tquasiquotes.types.QuasiTypequotes\tdef\ttqr\ttqr signature\n"
            "frontend\tquasiquotes.types.QuasiTypequotes\tdef\ttqq\ttqq signature\n"
            "frontend\tquasiquotes.types.TypePatternExtractor\tdef\tunapplySeq\tunapply signature\n"
            "frontend\tquasiquotes.construct.Quasiquotes\tdef\tdqr\tdqr signature\n"
            "core\tquasiquotes.publicapi.DefinitionConstruction\tdef\ttwoParameterMethod\ttwo signature\n",
            encoding="utf-8",
        )
        (definitions / "DefinitionQuasiquotes.scala").write_text(
            "private[quasiquotes] object DefinitionQuasiquotes:\n  def dqr = ()\n",
            encoding="utf-8",
        )
        (construct / "Quasiquotes.scala").write_text(
            "object Quasiquotes:\n"
            "  def dqr(using q: Quotes)(args: q.reflect.TypeRepr*): q.reflect.DefDef = ???\n",
            encoding="utf-8",
        )
        (matching / "QuasiPattern.scala").write_text(
            "object QuasiPattern:\n  def term = ()\n  def termOrThrow = ()\n"
            "  def qq(using q: Quotes): TermPatternExtractor[q.reflect.Term] = ???\n",
            encoding="utf-8",
        )
        (matching / "TermPatternExtractor.scala").write_text(
            "final class TermPatternExtractor[T]:\n  def unapplySeq(value: T): Option[Seq[T]] = ???\n",
            encoding="utf-8",
        )
        (types / "QuasiTypequotes.scala").write_text(
            "object QuasiTypequotes:\n"
            "  def tqr(using q: Quotes)(args: q.reflect.TypeRepr*): q.reflect.TypeRepr = ???\n"
            "  def tqq(using q: Quotes): TypePatternExtractor[q.reflect.TypeRepr] = ???\n",
            encoding="utf-8",
        )
        (types / "TypePatternExtractor.scala").write_text(
            "final class TypePatternExtractor[T]:\n"
            "  def unapplySeq(value: T): Option[Seq[T]] = ???\n",
            encoding="utf-8",
        )

    def run_checker(self, root: Path) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            [sys.executable, str(CHECKER), "--root", str(root)],
            check=False,
            capture_output=True,
            text=True,
        )

    def test_accepts_truthful_surface_table_matrix_and_links(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertIn("PUBLIC_DOCUMENTATION_BOUNDARY_PASS", result.stdout)

    def test_rejects_missing_relative_documentation_link(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root, matrix_link="docs/MISSING.md")

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("missing relative link", result.stderr)

    def test_rejects_stale_internal_only_dqr_claim(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root, dqr_status="Internal research, not public")

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("surface row mismatch", result.stderr)

    def test_ignores_markdown_link_shapes_inside_code(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            readme = root / "README.md"
            readme.write_text(
                readme.read_text(encoding="utf-8")
                + "\nInline: `def apply[A](using value: Show[A]) = value`\n"
                + "```scala\ndef apply[A](using value: Show[A]) = value\n```\n",
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 0, result.stderr)


if __name__ == "__main__":
    unittest.main()
