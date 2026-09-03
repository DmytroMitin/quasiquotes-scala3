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
            "Static exact-one `dqq` returns `SingleParameterDefinitionPattern`; "
            "static structural templates return `DefinitionPatternExtractor`; "
            "dynamic/non-static `dqq` retains the single-parameter fallback. "
            "There is no `dqq2`, `dqq3`, or `dqq4` public API.\n\n"
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
            "| Sequence-Term arguments | Apply and New sequence support |\n"
            "| Ordered term capture extractor | Scalar slots bind q.reflect.Term; exactly one "
            "direct Apply or New sequence slot binds Seq[q.reflect.Term] |\n\n"
            "https://docs.scala-lang.org/overviews/quasiquotes/syntax-summary.html\n"
            "https://scalameta.org/docs/trees/quasiquotes\n\n"
            "Any change that adds, removes, or materially alters a term, type, or definition syntax family must update this matrix.\n",
            encoding="utf-8",
        )
        (docs / "SUPPORTED_SYNTAX_AND_LIMITATIONS.md").write_text(
            "# Supported syntax and limitations\n\n"
            "Same-spelling `dqq` uses structural specialization. Static exact-one "
            "templates retain `SingleParameterDefinitionPattern`; static structural "
            "templates use scalable `DefinitionPatternExtractor`; dynamic/non-static "
            "calls retain the single-parameter fallback.\n\n"
            "## Bounded term-pattern extractor\n\n"
            "Scalar slots bind `q.reflect.Term`; exactly one direct sequence slot in ordinary "
            "`Apply.arguments` or fixed one-list `New.arguments` binds "
            "`Seq[q.reflect.Term]`.\n",
            encoding="utf-8",
        )
        (docs / "COMPATIBILITY.md").write_text(
            "# Compatibility\n\n"
            "The Scala/TASTy `dqq` declaration is now a transparent-inline selector. "
            "The historical erased JVM descriptor returning "
            "`SingleParameterDefinitionPattern` is preserved by a source-hidden bridge. "
            "The selector replacement is a new experimental 0.x-minor-class change.\n",
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
            "object QuasiPattern:\n"
            "  def term = ()\n"
            "  def termOrThrow = ()\n"
            "  private[matching] def scalarExtractor(sc: StringContext)"
            "(using q: Quotes): TermPatternExtractor[q.reflect.Term] = ???\n"
            "  @targetName(\"qq\")\n"
            "  private[matching] def qqLegacy(sc: StringContext)"
            "(using q: Quotes): TermPatternExtractor[q.reflect.Term] =\n"
            "    scalarExtractor(sc)\n"
            "  extension (inline sc: StringContext)\n"
            "    transparent inline def qq(using q: Quotes) = extractor(sc)\n",
            encoding="utf-8",
        )
        (matching / "QuasiPatternMacro.scala").write_text(
            "object QuasiPatternMacro:\n"
            "  def scalar = QuasiPattern.scalarExtractor(context)\n"
            "  def ranked = RankedTermPatternExtractorFactory.extractor(context)\n",
            encoding="utf-8",
        )
        (matching / "TermPatternExtractor.scala").write_text(
            "final class TermPatternExtractor[T]:\n  def unapplySeq(value: T): Option[Seq[T]] = ???\n",
            encoding="utf-8",
        )
        (matching / "RankedTermPatternExtractor.scala").write_text(
            "final class RankedTermPatternExtractor[T, Captures <: Tuple](extract: T => Option[Captures]):\n"
            "  def unapply(value: T): Option[Captures] = extract(value)\n",
            encoding="utf-8",
        )
        (matching / "DefinitionPattern.scala").write_text(
            "object DefinitionPattern:\n"
            "  private[matching] def singleParameterExtractor(sc: StringContext)"
            "(using q: Quotes): SingleParameterDefinitionPattern = ???\n"
            "  private[matching] def twoParameterExtractor(sc: StringContext)"
            "(using q: Quotes): DefinitionPatternExtractor = ???\n"
            "  @targetName(\"dqq\")\n"
            "  private[matching] def dqqLegacy(sc: StringContext)"
            "(using q: Quotes): SingleParameterDefinitionPattern =\n"
            "    singleParameterExtractor(sc)\n"
            "  extension (inline sc: StringContext)\n"
            "    transparent inline def dqq(using q: Quotes) = extractor(sc)\n"
            "  def singleParameter(source: String) = ???\n"
            "final class SingleParameterDefinitionPattern:\n"
            "  def matchDefinition(using q: Quotes)(target: q.reflect.DefDef) = ???\n"
            "  def unapply(using q: Quotes)(target: q.reflect.DefDef) = ???\n",
            encoding="utf-8",
        )
        (matching / "DefinitionPatternMacro.scala").write_text(
            "object DefinitionPatternMacro:\n"
            "  def single = DefinitionPattern.singleParameterExtractor(context)\n"
            "  def two = DefinitionPattern.twoParameterExtractor(context)\n",
            encoding="utf-8",
        )
        (matching / "DefinitionPatternExtractor.scala").write_text(
            "final class DefinitionPatternExtractor private (expected: Any):\n"
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

    def test_rejects_missing_transparent_inline_qq_selector(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            source = root / "frontend/src/main/scala/quasiquotes/matching/QuasiPattern.scala"
            source.write_text(
                source.read_text(encoding="utf-8").replace(
                    "transparent inline def qq(using q: Quotes) = extractor(sc)",
                    "def qqCurrent(using q: Quotes) = extractor(sc)",
                ),
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("transparent-inline selector contract", result.stderr)

    def test_rejects_missing_scalar_qq_extractor_route(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            source = root / "frontend/src/main/scala/quasiquotes/matching/QuasiPatternMacro.scala"
            source.write_text(
                source.read_text(encoding="utf-8").replace(
                    "QuasiPattern.scalarExtractor", "QuasiPattern.removedScalarRoute"
                ),
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("scalar extractor route contract", result.stderr)

    def test_rejects_missing_legacy_qq_jvm_bridge(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            source = root / "frontend/src/main/scala/quasiquotes/matching/QuasiPattern.scala"
            source.write_text(
                source.read_text(encoding="utf-8").replace(
                    "@targetName(\"qq\")", "@targetName(\"qqRemoved\")"
                ),
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("legacy JVM bridge contract", result.stderr)

    def test_rejects_missing_ranked_qq_extractor_contract(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            source = root / "frontend/src/main/scala/quasiquotes/matching/RankedTermPatternExtractor.scala"
            source.write_text(
                source.read_text(encoding="utf-8").replace(
                    "final class RankedTermPatternExtractor", "final class RemovedRankedExtractor"
                ),
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("ranked extractor contract", result.stderr)

    def test_rejects_stale_pre_q002_qq_source_shape_as_current_evidence(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            source = root / "frontend/src/main/scala/quasiquotes/matching/QuasiPattern.scala"
            source.write_text(
                "object QuasiPattern:\n"
                "  def term = ()\n"
                "  def termOrThrow = ()\n"
                "  def qq(using q: Quotes): TermPatternExtractor[q.reflect.Term] = ???\n",
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("transparent-inline selector contract", result.stderr)
            self.assertIn("legacy JVM bridge contract", result.stderr)

    def test_rejects_missing_ranked_qq_documentation_contract(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            limitations = root / "docs/SUPPORTED_SYNTAX_AND_LIMITATIONS.md"
            limitations.write_text(
                "# Supported syntax and limitations\n\nScalar qq matching only.\n",
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("scalar/ranked documentation contract", result.stderr)

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

    def test_rejects_stale_one_parameter_only_dqq_documentation(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            readme = root / "README.md"
            readme.write_text(
                readme.read_text(encoding="utf-8").replace(
                    "static structural templates return `DefinitionPatternExtractor`; ",
                    "static structural templates are undocumented; ",
                ),
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("Q012RR definition-pattern documentation contract", result.stderr)

    def test_rejects_missing_dqq_jvm_and_tasty_compatibility_accounting(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root)
            compatibility = root / "docs/COMPATIBILITY.md"
            compatibility.write_text(
                "# Compatibility\n\nDefinition compatibility is undocumented.\n",
                encoding="utf-8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("Q012RR dqq compatibility accounting", result.stderr)

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
