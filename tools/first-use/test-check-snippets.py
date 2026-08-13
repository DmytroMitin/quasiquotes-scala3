#!/usr/bin/env python3
"""Behavior tests for the public first-use documentation drift guard."""

from pathlib import Path
import subprocess
import sys
import tempfile
import unittest


CHECKER = Path(__file__).with_name("check-snippets.py")


class CheckSnippetsTest(unittest.TestCase):
    def make_fixture(self, root: Path, documented_lambda: str) -> None:
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
        (docs / "GETTING_STARTED.md").write_text(
            "<!-- snippet:core-first-use:start -->\n```scala\nval core = 1\n```\n"
            "<!-- snippet:core-first-use:end -->\n"
            "<!-- snippet:definition-first-use:start -->\n"
            "```scala\nval definition = 4\n```\n"
            "<!-- snippet:definition-first-use:end -->\n"
            "<!-- snippet:frontend-first-use:start -->\n```scala\nval frontend = 2\n```\n"
            "<!-- snippet:frontend-first-use:end -->\n"
            "<!-- snippet:lambda1-first-use:start -->\n```scala\n"
            + documented_lambda
            + "\n```\n<!-- snippet:lambda1-first-use:end -->\n",
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
                "First-use snippets aligned: core-first-use, definition-first-use, frontend-first-use, lambda1-first-use",
                result.stdout,
            )

    def test_rejects_documentation_drift(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            self.make_fixture(root, "val lambda = 4")

            result = self.run_checker(root)

            self.assertEqual(result.returncode, 1)
            self.assertIn("First-use snippet drift: lambda1-first-use", result.stderr)


if __name__ == "__main__":
    unittest.main()
