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
                "First-use snippets aligned: core-first-use, definition-first-use, two-parameter-definition-first-use, frontend-first-use, lambda1-first-use, qq-extractor-first-use, type-interpolator-first-use, dqr-first-use, definition-pattern-first-use, runtime-term-shape, runtime-parser, readme-quick-start",
                result.stdout,
            )

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
