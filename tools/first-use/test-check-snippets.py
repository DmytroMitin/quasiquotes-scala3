#!/usr/bin/env python3
"""Behavior tests for the public first-use documentation drift guard."""

from pathlib import Path
import subprocess
import sys
import tempfile
import unittest


CHECKER = Path(__file__).with_name("check-snippets.py")


class CheckSnippetsTest(unittest.TestCase):
    def make_fixture(
        self,
        root: Path,
        documented_lambda: str,
        documented_quick_start: str = "val quickStart = 5",
        documented_two_parameter: str = "val twoParameter = 7",
        documented_qq_extractor: str = "val qqExtractor = 8",
        documented_type_interpolator: str = "val typeInterpolator = 9",
        documented_dqr: str = "val dqr = 10",
        documented_definition_pattern: str = "val definitionPattern = 11",
        documented_runtime_term_shape: str = "val runtimeTermShape = 12",
        documented_runtime_parser: str = "val runtimeParser = 13",
        documented_p1_block: str = "val p1Block = 14",
        documented_p2_local_val: str = "val p2LocalVal = 15",
        documented_source_owned_local_def: str = "val sourceOwnedLocalDef = 16",
        documented_why: str = "val why = 17",
        documented_c028_term_type: str = "val c028TermType = 18",
        documented_c028_semantic_definition: str = "val c028Definition = 19",
        documented_c028_dotty_source_free: str = "val c028SourceFree = 20",
        documented_c028_dotty_generated_origin: str = "val c028Generated = 21",
        documented_c028_generic_specialized: str = "val c028Specialized = 22",
    ) -> None:
        docs = root / "docs"
        sources = root / "public-api-examples/src/test/scala/external/consumer"
        core_sources = root / "public-core-examples/src/test/scala/external/consumer"
        docs.mkdir(parents=True)
        sources.mkdir(parents=True)
        core_sources.mkdir(parents=True)

        (core_sources / "CoreFirstUseSnippet.scala").write_text(
            "// snippet:core-first-use:start\nval core = 1\n"
            "// snippet:core-first-use:end\n",
            encoding="utf-8",
        )
        (core_sources / "DefinitionFirstUseSnippet.scala").write_text(
            "// snippet:definition-first-use:start\nval definition = 4\n"
            "// snippet:definition-first-use:end\n",
            encoding="utf-8",
        )
        (core_sources / "TwoParameterDefinitionFirstUseSnippet.scala").write_text(
            "// snippet:two-parameter-definition-first-use:start\n"
            "val twoParameter = 7\n"
            "// snippet:two-parameter-definition-first-use:end\n",
            encoding="utf-8",
        )
        (sources / "FrontendFirstUseSnippet.scala").write_text(
            "// snippet:frontend-first-use:start\nval frontend = 2\n"
            "// snippet:frontend-first-use:end\n",
            encoding="utf-8",
        )
        (sources / "Lambda1FirstUseSnippet.scala").write_text(
            "// snippet:lambda1-first-use:start\nval lambda = 3\n"
            "// snippet:lambda1-first-use:end\n",
            encoding="utf-8",
        )
        (sources / "P1BlockFirstUseSnippet.scala").write_text(
            "// snippet:p1-block-first-use:start\nval p1Block = 14\n"
            "// snippet:p1-block-first-use:end\n",
            encoding="utf-8",
        )
        (sources / "P2LocalValFirstUseSnippet.scala").write_text(
            "// snippet:p2-local-val-first-use:start\nval p2LocalVal = 15\n"
            "// snippet:p2-local-val-first-use:end\n",
            encoding="utf-8",
        )
        (sources / "SourceOwnedLocalDefFirstUseSnippet.scala").write_text(
            "// snippet:source-owned-local-def-first-use:start\n"
            "val sourceOwnedLocalDef = 16\n"
            "// snippet:source-owned-local-def-first-use:end\n",
            encoding="utf-8",
        )
        (sources / "QqExtractorFirstUseSnippet.scala").write_text(
            "// snippet:qq-extractor-first-use:start\nval qqExtractor = 8\n"
            "// snippet:qq-extractor-first-use:end\n",
            encoding="utf-8",
        )
        (sources / "TypeInterpolatorFirstUseSnippet.scala").write_text(
            "// snippet:type-interpolator-first-use:start\n"
            "val typeInterpolator = 9\n"
            "// snippet:type-interpolator-first-use:end\n",
            encoding="utf-8",
        )
        (sources / "DqrFirstUseSnippet.scala").write_text(
            "// snippet:dqr-first-use:start\nval dqr = 10\n"
            "// snippet:dqr-first-use:end\n",
            encoding="utf-8",
        )
        (sources / "DefinitionPatternFirstUseSnippet.scala").write_text(
            "// snippet:definition-pattern-first-use:start\n"
            "val definitionPattern = 11\n"
            "// snippet:definition-pattern-first-use:end\n",
            encoding="utf-8",
        )
        (core_sources / "RuntimeTermShapeExample.scala").write_text(
            "// snippet:runtime-term-shape:start\nval runtimeTermShape = 12\n"
            "// snippet:runtime-term-shape:end\n",
            encoding="utf-8",
        )
        (sources / "RuntimeParserExample.scala").write_text(
            "// snippet:runtime-parser:start\nval runtimeParser = 13\n"
            "// snippet:runtime-parser:end\n",
            encoding="utf-8",
        )
        (sources / "ReadmeQuickStart.scala").write_text(
            "// snippet:readme-quick-start:start\nval quickStart = 5\n"
            "// snippet:readme-quick-start:end\n",
            encoding="utf-8",
        )
        (sources / "WhyQuasiquotesCurrentExamples.scala").write_text(
            "// snippet:why-quasiquotes-current:start\nval why = 17\n"
            "// snippet:why-quasiquotes-current:end\n",
            encoding="utf-8",
        )
        neutral_sources = root / "neutral-scalameta/src/test/scala/external/consumer"
        dotty_sources = root / "dotty-internal/src/test/scala/external/consumer"
        neutral_sources.mkdir(parents=True)
        dotty_sources.mkdir(parents=True)
        (neutral_sources / "C028TermTypeHelloWorld.scala").write_text(
            "// snippet:c028-term-type:start\nval c028TermType = 18\n"
            "// snippet:c028-term-type:end\n",
            encoding="utf-8",
        )
        (core_sources / "C028SemanticDefinitionHelloWorld.scala").write_text(
            "// snippet:c028-semantic-definition:start\nval c028Definition = 19\n"
            "// snippet:c028-semantic-definition:end\n",
            encoding="utf-8",
        )
        (dotty_sources / "C028DottyBridgeHelloWorld.scala").write_text(
            "// snippet:c028-dotty-source-free:start\nval c028SourceFree = 20\n"
            "// snippet:c028-dotty-source-free:end\n"
            "// snippet:c028-dotty-generated-origin:start\nval c028Generated = 21\n"
            "// snippet:c028-dotty-generated-origin:end\n"
            "// snippet:c028-generic-specialized-definition:start\n"
            "val c028Specialized = 22\n"
            "// snippet:c028-generic-specialized-definition:end\n",
            encoding="utf-8",
        )
        (root / "README.md").write_text(
            "<!-- snippet:readme-quick-start:start -->\n```scala\n"
            + documented_quick_start
            + "\n```\n<!-- snippet:readme-quick-start:end -->\n",
            encoding="utf-8",
        )
        (docs / "GETTING_STARTED.md").write_text(
            "<!-- snippet:core-first-use:start -->\n```scala\nval core = 1\n```\n"
            "<!-- snippet:core-first-use:end -->\n"
            "<!-- snippet:definition-first-use:start -->\n"
            "```scala\nval definition = 4\n```\n"
            "<!-- snippet:definition-first-use:end -->\n"
            "<!-- snippet:two-parameter-definition-first-use:start -->\n"
            "```scala\n"
            + documented_two_parameter
            + "\n```\n<!-- snippet:two-parameter-definition-first-use:end -->\n"
            "<!-- snippet:frontend-first-use:start -->\n```scala\nval frontend = 2\n```\n"
            "<!-- snippet:frontend-first-use:end -->\n"
            "<!-- snippet:lambda1-first-use:start -->\n```scala\n"
            + documented_lambda
            + "\n```\n<!-- snippet:lambda1-first-use:end -->\n"
            "<!-- snippet:p1-block-first-use:start -->\n```scala\n"
            + documented_p1_block
            + "\n```\n<!-- snippet:p1-block-first-use:end -->\n"
            "<!-- snippet:p2-local-val-first-use:start -->\n```scala\n"
            + documented_p2_local_val
            + "\n```\n<!-- snippet:p2-local-val-first-use:end -->\n"
            "<!-- snippet:source-owned-local-def-first-use:start -->\n```scala\n"
            + documented_source_owned_local_def
            + "\n```\n<!-- snippet:source-owned-local-def-first-use:end -->\n"
            "<!-- snippet:qq-extractor-first-use:start -->\n```scala\n"
            + documented_qq_extractor
            + "\n```\n<!-- snippet:qq-extractor-first-use:end -->\n"
            + "<!-- snippet:type-interpolator-first-use:start -->\n```scala\n"
            + documented_type_interpolator
            + "\n```\n<!-- snippet:type-interpolator-first-use:end -->\n"
            + "<!-- snippet:dqr-first-use:start -->\n```scala\n"
            + documented_dqr
            + "\n```\n<!-- snippet:dqr-first-use:end -->\n"
            + "<!-- snippet:definition-pattern-first-use:start -->\n```scala\n"
            + documented_definition_pattern
            + "\n```\n<!-- snippet:definition-pattern-first-use:end -->\n",
            encoding="utf-8",
        )
        (docs / "EXECUTION_ENVIRONMENTS_AND_AST_REPRESENTATIONS.md").write_text(
            "<!-- snippet:runtime-term-shape:start -->\n```scala\n"
            + documented_runtime_term_shape
            + "\n```\n<!-- snippet:runtime-term-shape:end -->\n"
            + "<!-- snippet:runtime-parser:start -->\n```scala\n"
            + documented_runtime_parser
            + "\n```\n<!-- snippet:runtime-parser:end -->\n",
            encoding="utf-8",
        )
        (docs / "WHY_QUASIQUOTES.md").write_text(
            "<!-- snippet:why-quasiquotes-current:start -->\n```scala\n"
            + documented_why
            + "\n```\n<!-- snippet:why-quasiquotes-current:end -->\n",
            encoding="utf-8",
        )
        (docs / "SEMANTIC_MODELS_AND_CONVERSIONS.md").write_text(
            "<!-- snippet:c028-term-type:start -->\n```scala\n"
            + documented_c028_term_type
            + "\n```\n<!-- snippet:c028-term-type:end -->\n"
            + "<!-- snippet:c028-semantic-definition:start -->\n```scala\n"
            + documented_c028_semantic_definition
            + "\n```\n<!-- snippet:c028-semantic-definition:end -->\n"
            + "<!-- snippet:c028-dotty-source-free:start -->\n```scala\n"
            + documented_c028_dotty_source_free
            + "\n```\n<!-- snippet:c028-dotty-source-free:end -->\n"
            + "<!-- snippet:c028-dotty-generated-origin:start -->\n```scala\n"
            + documented_c028_dotty_generated_origin
            + "\n```\n<!-- snippet:c028-dotty-generated-origin:end -->\n"
            + "<!-- snippet:c028-generic-specialized-definition:start -->\n```scala\n"
            + documented_c028_generic_specialized
            + "\n```\n<!-- snippet:c028-generic-specialized-definition:end -->\n",
            encoding="utf-8",
        )

    def run_checker(self, root: Path) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            [sys.executable, str(CHECKER), "--root", str(root)],
            check=False,
            capture_output=True,
            text=True,
        )

    def test_accepts_exact_compiled_snippets(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root, "val lambda = 3")

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertIn(
                "First-use snippets aligned: core-first-use, definition-first-use, two-parameter-definition-first-use, frontend-first-use, lambda1-first-use, p1-block-first-use, p2-local-val-first-use, source-owned-local-def-first-use, qq-extractor-first-use, type-interpolator-first-use, dqr-first-use, definition-pattern-first-use, runtime-term-shape, runtime-parser, readme-quick-start, why-quasiquotes-current, c028-term-type, c028-semantic-definition, c028-dotty-source-free, c028-dotty-generated-origin, c028-generic-specialized-definition",
                result.stdout,
            )

    def test_rejects_c028_semantic_definition_drift(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(
                root,
                documented_lambda="val lambda = 3",
                documented_c028_semantic_definition="val c028Definition = 20",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn(
                "First-use snippet drift: c028-semantic-definition",
                result.stderr,
            )

    def test_rejects_p1_block_documentation_drift(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(
                root,
                documented_lambda="val lambda = 3",
                documented_p1_block="val p1Block = 15",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("First-use snippet drift: p1-block-first-use", result.stderr)

    def test_rejects_runtime_parser_documentation_drift(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(
                root,
                documented_lambda="val lambda = 3",
                documented_runtime_parser="val runtimeParser = 14",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("First-use snippet drift: runtime-parser", result.stderr)

    def test_rejects_documentation_drift(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root, "val lambda = 4")

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("First-use snippet drift: lambda1-first-use", result.stderr)

    def test_rejects_readme_quick_start_drift(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(
                root,
                documented_lambda="val lambda = 3",
                documented_quick_start="val quickStart = 6",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("First-use snippet drift: readme-quick-start", result.stderr)

    def test_rejects_two_parameter_definition_drift(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(
                root,
                documented_lambda="val lambda = 3",
                documented_two_parameter="val twoParameter = 8",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn(
                "First-use snippet drift: two-parameter-definition-first-use",
                result.stderr,
            )

    def test_rejects_type_interpolator_drift(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(
                root,
                documented_lambda="val lambda = 3",
                documented_type_interpolator="val typeInterpolator = 10",
            )

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn(
                "First-use snippet drift: type-interpolator-first-use",
                result.stderr,
            )


if __name__ == "__main__":
    unittest.main()
