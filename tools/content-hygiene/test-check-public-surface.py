#!/usr/bin/env python3
"""Executable behavior tests for the public-surface hygiene checker."""

from __future__ import annotations

import subprocess
import tempfile
import unittest
from pathlib import Path


CHECKER = Path(__file__).with_name("check-public-surface.py")


class PublicSurfaceHygieneTest(unittest.TestCase):
    def run_checker(self, files: dict[str, str]) -> subprocess.CompletedProcess[str]:
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            for relative, text in files.items():
                path = root / relative
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text(text)
            return subprocess.run(
                ["python3", str(CHECKER), str(root)],
                text=True,
                capture_output=True,
                check=False,
            )

    def test_accepts_phase_neutral_policy_and_read_only_pinned_ci(self) -> None:
        result = self.run_checker(
            {
                "README.md": "# Project\n\nExperimental research software.\n",
                "SECURITY.md": (
                    "# Security\n\nNo private reporting channel is currently offered. "
                    "A channel may be added later.\n"
                ),
                ".github/workflows/test.yml": (
                    "name: Test\n"
                    "permissions:\n  contents: read\n"
                    "jobs:\n  test:\n    steps:\n"
                    "      - uses: actions/checkout@"
                    "d23441a48e516b6c34aea4fa41551a30e30af803\n"
                ),
            }
        )

        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout, "PUBLIC_CONTENT_AND_WORKFLOW_HYGIENE_PASS\n")

    def test_rejects_private_delivery_markers_and_security_channel_prerequisite(self) -> None:
        result = self.run_checker(
            {
                "README.md": (
                    "Pha" + "se 75 is complete. Exact next action is Pha" + "se 76.\n"
                ),
                "SECURITY.md": (
                    "A private security-reporting channel remains a required "
                    "public-visibility gate.\n"
                ),
                ".github/workflows/test.yml": (
                    "name: Test\npermissions:\n  contents: read\njobs: {}\n"
                ),
            }
        )

        self.assertEqual(result.returncode, 1)
        self.assertIn("PRIVATE_DELIVERY_CHRONOLOGY", result.stderr)
        self.assertIn("STALE_PRIVATE_CHANNEL_PREREQUISITE", result.stderr)

    def test_rejects_mutable_actions_write_permissions_and_secrets(self) -> None:
        result = self.run_checker(
            {
                "README.md": "# Project\n",
                ".github/workflows/test.yml": (
                    "name: Test\npermissions:\n  contents: write\njobs:\n  test:\n"
                    "    steps:\n      - uses: actions/checkout@v6\n"
                    "      - run: echo ${{ secrets.RELEASE_TOKEN }}\n"
                ),
            }
        )

        self.assertEqual(result.returncode, 1)
        self.assertIn("MUTABLE_THIRD_PARTY_ACTION", result.stderr)
        self.assertIn("WORKFLOW_WRITE_PERMISSION", result.stderr)
        self.assertIn("WORKFLOW_SECRET_REFERENCE", result.stderr)

    def test_rejects_workflow_without_explicit_read_only_permissions(self) -> None:
        result = self.run_checker(
            {
                "README.md": "# Project\n",
                ".github/workflows/test.yml": (
                    "name: Test\njobs:\n  test:\n    steps:\n"
                    "      - uses: actions/checkout@"
                    "d23441a48e516b6c34aea4fa41551a30e30af803\n"
                ),
            }
        )

        self.assertEqual(result.returncode, 1)
        self.assertIn("WORKFLOW_READ_ONLY_PERMISSION_MISSING", result.stderr)


if __name__ == "__main__":
    unittest.main(verbosity=2)
