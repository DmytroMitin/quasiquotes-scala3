#!/usr/bin/env python3
"""Synthetic S0-S10 contract tests for the exact inventory diff."""

from __future__ import annotations

import importlib.util
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path


TOOL_ROOT = Path(__file__).resolve().parent
sys.dont_write_bytecode = True
SPEC = importlib.util.spec_from_file_location("diff_inventory", TOOL_ROOT / "diff-inventory.py")
assert SPEC and SPEC.loader
diff_inventory = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = diff_inventory
SPEC.loader.exec_module(diff_inventory)

HEADER = "module\towner\tkind\tname\tsignature\n"
BASE = (
    ("core", "example.Api", "class", "Api", "Api()"),
    ("core", "example.Api", "def", "run", "run(x: Int): String"),
    ("core", "example.Api", "def", "run", "run(x: String): String"),
    (
        "frontend",
        "example.syntax",
        "extension",
        "render",
        "render(value: Api): String",
    ),
)


def write_inventory(path: Path, rows: tuple[tuple[str, ...], ...]) -> None:
    path.write_text(HEADER + "".join("\t".join(row) + "\n" for row in rows), encoding="utf-8")


class SyntheticMatrixTest(unittest.TestCase):
    def setUp(self) -> None:
        self.temp = tempfile.TemporaryDirectory()
        self.root = Path(self.temp.name)
        self.baseline = self.root / "baseline.tsv"
        write_inventory(self.baseline, BASE)

    def tearDown(self) -> None:
        self.temp.cleanup()

    def compare(self, rows: tuple[tuple[str, ...], ...]):
        candidate = self.root / "candidate.tsv"
        write_inventory(candidate, rows)
        return diff_inventory.compare(
            diff_inventory.load_inventory(self.baseline),
            diff_inventory.load_inventory(candidate),
        )

    def test_s0_reorder_only(self) -> None:
        result = self.compare(tuple(reversed(BASE)))
        self.assertEqual(diff_inventory.NO_DELTA, result.classification)
        self.assertEqual((), result.deltas)

    def test_s1_new_public_member(self) -> None:
        result = self.compare(BASE + (("core", "example.Api", "def", "fresh", "fresh: Api"),))
        self.assertEqual(diff_inventory.ADDITIVE, result.classification)
        self.assertEqual("ADDED_SYMBOL_OR_GROUP", result.deltas[0][0])

    def test_s2_removed_public_member(self) -> None:
        result = self.compare(BASE[:-1])
        self.assertEqual(diff_inventory.BREAKING, result.classification)
        self.assertEqual("REMOVED_SYMBOL_OR_GROUP", result.deltas[0][0])

    def test_s3_signature_replacement(self) -> None:
        changed = tuple(row for row in BASE if row[-1] != "run(x: Int): String") + (
            ("core", "example.Api", "def", "run", "run(x: Long): String"),
        )
        result = self.compare(changed)
        self.assertEqual(diff_inventory.BREAKING, result.classification)
        self.assertEqual(
            {"OVERLOAD_OR_SIGNATURE_ADDED", "OVERLOAD_OR_SIGNATURE_REMOVED"},
            {delta[0] for delta in result.deltas},
        )

    def test_s4_overload_addition(self) -> None:
        result = self.compare(
            BASE + (("core", "example.Api", "def", "run", "run(x: Long): String"),)
        )
        self.assertEqual(diff_inventory.ADDITIVE, result.classification)
        self.assertEqual(1, result.overload_or_signature_additions)

    def test_s5_overload_removal(self) -> None:
        result = self.compare(tuple(row for row in BASE if row[-1] != "run(x: String): String"))
        self.assertEqual(diff_inventory.BREAKING, result.classification)
        self.assertEqual(1, result.overload_or_signature_removals)

    def test_s6_owner_move(self) -> None:
        moved = tuple(row for row in BASE if row[3] != "render") + (
            ("frontend", "example.syntax2", "extension", "render", "render(value: Api): String"),
        )
        result = self.compare(moved)
        self.assertEqual(diff_inventory.BREAKING, result.classification)
        self.assertEqual(1, result.added_groups)
        self.assertEqual(1, result.removed_groups)

    def test_s7_module_move(self) -> None:
        moved = tuple(row for row in BASE if row[3] != "render") + (
            ("core", "example.syntax", "extension", "render", "render(value: Api): String"),
        )
        result = self.compare(moved)
        self.assertEqual(diff_inventory.BREAKING, result.classification)
        self.assertEqual(1, result.added_groups)
        self.assertEqual(1, result.removed_groups)

    def test_s8_kind_change(self) -> None:
        moved = tuple(row for row in BASE if row[3] != "render") + (
            ("frontend", "example.syntax", "def", "render", "render(value: Api): String"),
        )
        result = self.compare(moved)
        self.assertEqual(diff_inventory.BREAKING, result.classification)
        self.assertEqual(1, result.added_groups)
        self.assertEqual(1, result.removed_groups)

    def test_s9_malformed_header_fails_closed(self) -> None:
        candidate = self.root / "bad.tsv"
        candidate.write_text("wrong\theader\n", encoding="utf-8")
        with self.assertRaisesRegex(diff_inventory.InventoryError, "header"):
            diff_inventory.load_inventory(candidate)
        self.assertEqual(4, self.run_cli(candidate, "report"))

    def test_s10_duplicate_exact_row_fails_closed(self) -> None:
        candidate = self.root / "duplicate.tsv"
        write_inventory(candidate, BASE + (BASE[0],))
        with self.assertRaisesRegex(diff_inventory.InventoryError, "row 6"):
            diff_inventory.load_inventory(candidate)
        self.assertEqual(4, self.run_cli(candidate, "current-minor"))

    def test_gate_exit_and_deterministic_reports(self) -> None:
        candidate = self.root / "additive.tsv"
        write_inventory(candidate, tuple(reversed(BASE)) + (("core", "example.Api", "def", "fresh", "fresh: Api"),))
        self.assertEqual(2, self.run_cli(candidate, "current-minor", "one"))
        self.assertEqual(0, self.run_cli(candidate, "report", "two"))
        self.assertEqual(
            (self.root / "one.delta").read_bytes(),
            (self.root / "two.delta").read_bytes(),
        )
        self.assertEqual(
            (self.root / "one.summary").read_bytes(),
            (self.root / "two.summary").read_bytes(),
        )

    def run_cli(self, candidate: Path, mode: str, prefix: str = "run") -> int:
        result = subprocess.run(
            [
                sys.executable,
                str(TOOL_ROOT / "diff-inventory.py"),
                str(self.baseline),
                str(candidate),
                str(self.root / f"{prefix}.delta"),
                str(self.root / f"{prefix}.summary"),
                "--mode",
                mode,
            ],
            check=False,
            capture_output=True,
            text=True,
        )
        return result.returncode


if __name__ == "__main__":
    unittest.main(verbosity=2)
