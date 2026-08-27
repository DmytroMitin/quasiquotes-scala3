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
            "[architecture](docs/ARCHITECTURE.md)\n"
            "[Macro-Paradise](https://github.com/DmytroMitin/macroparadise-scala3)\n"
            "[AUXify](https://github.com/DmytroMitin/AUXify-scala3)\n"
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
        (root / "ROADMAP.md").write_text(
            "[north-star checkpoints](docs/NORTH_STAR_QUASIQUOTE_EXAMPLES.md)\n\n"
            "| N1 | `DESIGN_REQUIRED`, `IMPLEMENTATION_REQUIRED` |\n"
            "| N2 | `DESIGN_REQUIRED`, `IMPLEMENTATION_REQUIRED` |\n"
            "| N3 | `DESIGN_REQUIRED`, `IMPLEMENTATION_REQUIRED` |\n"
            "| N4 | `DESIGN_REQUIRED`, `IMPLEMENTATION_REQUIRED` |\n"
            "| N5 | `DESIGN_REQUIRED`, `IMPLEMENTATION_REQUIRED` |\n",
            encoding="utf-8",
        )
        (docs / "ARCHITECTURE.md").write_text(
            "# Architecture\n\n"
            "The project owns one compiler-free semantic model. The current-Dotty frontend\n"
            "is the released default and the reference oracle; the typed Scalameta route is\n"
            "an explicit, unpublished opt-in. The two typed routes must agree wherever they\n"
            "both advertise support. In the hybrid route, only a Scalameta parse failure may fall back.\n\n"
            "The compiler-free model is symbol-free, and no public symbol-quasiquote family\n"
            "is currently planned. Symbols and owners that are derivable from syntax belong to\n"
            "the typed backend's lowering plan. A pre-typer `untpd` backend must\n"
            "emit syntax without fabricating typed symbols.\n",
            encoding="utf-8",
        )
        (docs / "WHY_QUASIQUOTES.md").write_text(
            "[future checkpoints](NORTH_STAR_QUASIQUOTE_EXAMPLES.md)\n",
            encoding="utf-8",
        )
        (docs / "HYBRID_SCALAMETA_TERM_FRONTEND_EXPERIMENT.md").write_text(
            "The [canonical architecture](ARCHITECTURE.md) owns the shared status.\n",
            encoding="utf-8",
        )
        (docs / "NORTH_STAR_QUASIQUOTE_EXAMPLES.md").write_text(
            "# North-star quasiquote examples\n\n"
            "All conceptual quasiquote syntax in this document is future, non-current\n"
            "notation.\n"
            "CURRENT_MANUAL_BASELINE_PROVED\n"
            "DESIGN_REQUIRED\n"
            "IMPLEMENTATION_REQUIRED\n\n"
            + "".join(
                f"## N{checkpoint}\n\n"
                "### Manual/current baseline\n\nBaseline.\n\n"
                "### Desired source-like shape\n\nShape.\n\n"
                "### Required missing capabilities\n\nCapabilities.\n\n"
                "### Checkpoint criterion\n\nCriterion.\n\n"
                for checkpoint in range(1, 6)
            ),
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

    def test_rejects_missing_related_project_link(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            readme = root / "README.md"
            readme.write_text(
                readme.read_text(encoding="utf-8").replace(
                    "https://github.com/DmytroMitin/AUXify-scala3",
                    "https://example.invalid/auxify",
                ),
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("README missing related-project link", result.stderr)

    def test_rejects_missing_canonical_architecture_fact(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            architecture = root / "docs/ARCHITECTURE.md"
            architecture.write_text(
                architecture.read_text(encoding="utf-8").replace(
                    "only a Scalameta parse failure may fall back",
                    "fallback policy omitted",
                ),
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("canonical architecture missing fact", result.stderr)

    def test_rejects_missing_north_star_checkpoint(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            north_star = root / "docs/NORTH_STAR_QUASIQUOTE_EXAMPLES.md"
            north_star.write_text(
                north_star.read_text(encoding="utf-8").replace("## N4", "## omitted"),
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("north-star document missing checkpoint", result.stderr)

    def test_rejects_checkpoint_without_required_structure(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            north_star = root / "docs/NORTH_STAR_QUASIQUOTE_EXAMPLES.md"
            north_star.write_text(
                north_star.read_text(encoding="utf-8").replace(
                    "### Checkpoint criterion\n\nCriterion.",
                    "### Criterion omitted\n\nOmitted.",
                    1,
                ),
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("north-star checkpoint N1 missing section", result.stderr)

    def test_rejects_silently_completed_roadmap_checkpoint(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            roadmap = root / "ROADMAP.md"
            roadmap.write_text(
                roadmap.read_text(encoding="utf-8").replace(
                    "| N5 | `DESIGN_REQUIRED`, `IMPLEMENTATION_REQUIRED` |",
                    "| N5 | `CHECKPOINT_COMPLETE` |",
                ),
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("roadmap checkpoint N5 has invalid status", result.stderr)

    def test_rejects_scalameta_status_doc_without_canonical_link(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            experiment = root / "docs/HYBRID_SCALAMETA_TERM_FRONTEND_EXPERIMENT.md"
            experiment.write_text("standalone status narrative\n", encoding="utf-8")

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("Scalameta experiment missing canonical architecture link", result.stderr)

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
