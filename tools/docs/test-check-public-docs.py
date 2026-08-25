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
        readme_core_count: int = 1,
        readme_frontend_count: int = 14,
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
            "| Definition pattern matching | `case dqq\"def id(x: Int): Int = $body\"` | Public now, exact bounded shape | `DefinitionPattern.singleParameter(...)` | Public now, exact bounded shape |\n"
            "<!-- public-surface-table:end -->\n\n"
            "The machine-readable [0.2.0 public API baseline](docs/api-baselines/0.2.0.tsv)\n"
            f"contains {readme_core_count} core and {readme_frontend_count} frontend Scaladoc-visible entries.\n",
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
        baselines = docs / "api-baselines"
        baselines.mkdir()
        (baselines / "0.2.0.tsv").write_text(
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
            "frontend\tquasiquotes.matching.DefinitionPattern\tdef\tsingleParameter\tsingle signature\n"
            "frontend\tquasiquotes.matching.DefinitionPattern\tdef\tdqq\tdqq signature\n"
            "frontend\tquasiquotes.matching.SingleParameterDefinitionPattern\tdef\tmatchDefinition\tmatch signature\n"
            "frontend\tquasiquotes.matching.SingleParameterDefinitionPattern\tdef\tunapply\tunapply signature\n"
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
        (matching / "DefinitionPattern.scala").write_text(
            "object DefinitionPattern:\n"
            "  extension (sc: StringContext)\n"
            "    def dqq(using q: Quotes): SingleParameterDefinitionPattern = ???\n"
            "  def singleParameter(source: String) = ???\n"
            "final class SingleParameterDefinitionPattern:\n"
            "  def matchDefinition(using q: Quotes)(target: q.reflect.DefDef) = ???\n"
            "  def unapply(using q: Quotes)(target: q.reflect.DefDef) = ???\n",
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

    def test_rejects_stale_frontend_api_count(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root, readme_frontend_count=13)

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("public API count drift", result.stderr)
            self.assertIn("README core=1 frontend=13", result.stderr)
            self.assertIn("baseline core=1 frontend=14", result.stderr)

    def test_rejects_stale_core_api_count(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root, readme_core_count=2)

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("public API count drift", result.stderr)
            self.assertIn("README core=2 frontend=14", result.stderr)
            self.assertIn("baseline core=1 frontend=14", result.stderr)

    def test_rejects_baseline_growth_without_readme_count_update(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            baseline = root / "docs/api-baselines/0.2.0.tsv"
            baseline.write_text(
                baseline.read_text(encoding="utf-8")
                + "core\tquasiquotes.publicapi.AddedApi\tdef\tadded\tadded signature\n",
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("public API count drift", result.stderr)
            self.assertIn("README core=1 frontend=14", result.stderr)
            self.assertIn("baseline core=2 frontend=14", result.stderr)

    def test_rejects_unexpected_public_api_module(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            baseline = root / "docs/api-baselines/0.2.0.tsv"
            baseline.write_text(
                baseline.read_text(encoding="utf-8")
                + "experimental\tquasiquotes.experimental.Api\tdef\tvalue\tvalue signature\n",
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn(
                "public API count contract requires exactly core and frontend modules",
                result.stderr,
            )
            self.assertIn("experimental", result.stderr)


if __name__ == "__main__":
    unittest.main()
