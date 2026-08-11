#!/usr/bin/env python3
"""Exact, overload-aware diff for deterministic public API inventory TSVs."""

from __future__ import annotations

import argparse
import csv
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path


HEADER = ("module", "owner", "kind", "name", "signature")
DELTA_HEADER = (
    "change",
    "module",
    "owner",
    "kind",
    "name",
    "baseline_signature",
    "candidate_signature",
)
ALLOWED_MODULES = {"core", "frontend"}
ALLOWED_KINDS = {
    "class",
    "def",
    "enum",
    "enumcase",
    "extension",
    "given",
    "object",
    "trait",
    "type",
    "val",
    "var",
}
NO_DELTA = "NO_PUBLIC_API_DELTA"
ADDITIVE = "ADDITIVE_API_SHAPE_DELTA_REVIEW_REQUIRED"
BREAKING = "BREAKING_API_SHAPE_DELTA_REQUIRES_NEW_0X_MINOR"
MALFORMED = "MALFORMED_OR_UNSUPPORTED_API_INVENTORY"

Row = tuple[str, str, str, str, str]
GroupKey = tuple[str, str, str, str]
Delta = tuple[str, str, str, str, str, str, str]


class InventoryError(ValueError):
    def __init__(self, code: str, detail: str) -> None:
        super().__init__(detail)
        self.code = code
        self.detail = detail


@dataclass(frozen=True)
class DiffResult:
    classification: str
    baseline_rows: int
    candidate_rows: int
    baseline_groups: int
    candidate_groups: int
    unchanged_rows: int
    unchanged_groups: int
    added_groups: int
    removed_groups: int
    added_signatures: int
    removed_signatures: int
    overload_or_signature_additions: int
    overload_or_signature_removals: int
    deltas: tuple[Delta, ...]


def load_inventory(path: Path) -> tuple[Row, ...]:
    with path.open(encoding="utf-8", newline="") as stream:
        reader = csv.reader(stream, delimiter="\t", strict=True)
        try:
            header = tuple(next(reader))
        except StopIteration as error:
            raise InventoryError("EMPTY_INPUT", "inventory is empty") from error
        if header != HEADER:
            raise InventoryError("BAD_HEADER", "inventory header is unsupported")

        rows: list[Row] = []
        seen: set[Row] = set()
        for line_number, fields in enumerate(reader, start=2):
            if len(fields) != len(HEADER):
                raise InventoryError(
                    "BAD_FIELD_COUNT", f"row {line_number} has {len(fields)} fields"
                )
            row = tuple(fields)
            module, owner, kind, name, signature = row
            if module not in ALLOWED_MODULES:
                raise InventoryError("UNSUPPORTED_MODULE", f"row {line_number}")
            if kind not in ALLOWED_KINDS:
                raise InventoryError("UNSUPPORTED_KIND", f"row {line_number}")
            if not owner or not name or not signature:
                raise InventoryError("EMPTY_IDENTITY_FIELD", f"row {line_number}")
            if row in seen:
                raise InventoryError("DUPLICATE_EXACT_ROW", f"row {line_number}")
            seen.add(row)
            rows.append(row)
    return tuple(sorted(rows))


def group_rows(rows: tuple[Row, ...]) -> dict[GroupKey, frozenset[str]]:
    groups: defaultdict[GroupKey, set[str]] = defaultdict(set)
    for module, owner, kind, name, signature in rows:
        groups[(module, owner, kind, name)].add(signature)
    return {key: frozenset(signatures) for key, signatures in groups.items()}


def compare(baseline: tuple[Row, ...], candidate: tuple[Row, ...]) -> DiffResult:
    baseline_groups = group_rows(baseline)
    candidate_groups = group_rows(candidate)
    deltas: list[Delta] = []
    added_groups = removed_groups = 0
    added_signatures = removed_signatures = 0
    overload_additions = overload_removals = 0

    for key in sorted(set(baseline_groups) | set(candidate_groups)):
        module, owner, kind, name = key
        old = baseline_groups.get(key, frozenset())
        new = candidate_groups.get(key, frozenset())
        if not old:
            added_groups += 1
            for signature in sorted(new):
                added_signatures += 1
                deltas.append(
                    ("ADDED_SYMBOL_OR_GROUP", module, owner, kind, name, "", signature)
                )
        elif not new:
            removed_groups += 1
            for signature in sorted(old):
                removed_signatures += 1
                deltas.append(
                    ("REMOVED_SYMBOL_OR_GROUP", module, owner, kind, name, signature, "")
                )
        else:
            for signature in sorted(old - new):
                removed_signatures += 1
                overload_removals += 1
                deltas.append(
                    (
                        "OVERLOAD_OR_SIGNATURE_REMOVED",
                        module,
                        owner,
                        kind,
                        name,
                        signature,
                        "",
                    )
                )
            for signature in sorted(new - old):
                added_signatures += 1
                overload_additions += 1
                deltas.append(
                    (
                        "OVERLOAD_OR_SIGNATURE_ADDED",
                        module,
                        owner,
                        kind,
                        name,
                        "",
                        signature,
                    )
                )

    if removed_signatures:
        classification = BREAKING
    elif added_signatures:
        classification = ADDITIVE
    else:
        classification = NO_DELTA

    unchanged_rows = len(set(baseline) & set(candidate))
    unchanged_groups = sum(
        1
        for key in set(baseline_groups) & set(candidate_groups)
        if baseline_groups[key] == candidate_groups[key]
    )
    return DiffResult(
        classification=classification,
        baseline_rows=len(baseline),
        candidate_rows=len(candidate),
        baseline_groups=len(baseline_groups),
        candidate_groups=len(candidate_groups),
        unchanged_rows=unchanged_rows,
        unchanged_groups=unchanged_groups,
        added_groups=added_groups,
        removed_groups=removed_groups,
        added_signatures=added_signatures,
        removed_signatures=removed_signatures,
        overload_or_signature_additions=overload_additions,
        overload_or_signature_removals=overload_removals,
        deltas=tuple(sorted(deltas)),
    )


def policy_recommendation(classification: str) -> str:
    if classification == NO_DELTA:
        return "CURRENT_0X_MINOR_SHAPE_GATE_PASS"
    if classification == ADDITIVE:
        return "HUMAN_REVIEW_REQUIRED_NOT_AUTOMATICALLY_PATCH_SAFE"
    if classification == BREAKING:
        return "NEW_0X_MINOR_REQUIRED"
    return "FAIL_CLOSED_REPAIR_INVENTORY"


def write_delta(path: Path, deltas: tuple[Delta, ...]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8", newline="\n") as stream:
        stream.write("\t".join(DELTA_HEADER) + "\n")
        for delta in deltas:
            stream.write("\t".join(delta) + "\n")


def write_summary(path: Path, result: DiffResult, error_code: str = "NONE") -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    fields = (
        ("schema", "public-api-diff-v1"),
        ("baseline_rows", str(result.baseline_rows)),
        ("candidate_rows", str(result.candidate_rows)),
        ("baseline_groups", str(result.baseline_groups)),
        ("candidate_groups", str(result.candidate_groups)),
        ("unchanged_rows", str(result.unchanged_rows)),
        ("unchanged_groups", str(result.unchanged_groups)),
        ("added_groups", str(result.added_groups)),
        ("removed_groups", str(result.removed_groups)),
        ("added_signatures", str(result.added_signatures)),
        ("removed_signatures", str(result.removed_signatures)),
        (
            "overload_or_signature_additions",
            str(result.overload_or_signature_additions),
        ),
        (
            "overload_or_signature_removals",
            str(result.overload_or_signature_removals),
        ),
        ("top_level_classification", result.classification),
        ("policy_recommendation", policy_recommendation(result.classification)),
        ("error_code", error_code),
        (
            "non_guarantees",
            "binary,TASTy,source-overload-resolution,given-search,semantic-behavior,"
            "compiler-internal,runtime-serialization",
        ),
    )
    with path.open("w", encoding="utf-8", newline="\n") as stream:
        for key, value in fields:
            stream.write(f"{key}\t{value}\n")


def malformed_result() -> DiffResult:
    return DiffResult(MALFORMED, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, ())


def exit_code(classification: str, mode: str) -> int:
    if classification == MALFORMED:
        return 4
    if mode == "report":
        return 0
    return {NO_DELTA: 0, ADDITIVE: 2, BREAKING: 3}[classification]


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("baseline", type=Path)
    parser.add_argument("candidate", type=Path)
    parser.add_argument("delta", type=Path)
    parser.add_argument("summary", type=Path)
    parser.add_argument("--mode", choices=("report", "current-minor"), default="report")
    args = parser.parse_args()

    try:
        result = compare(load_inventory(args.baseline), load_inventory(args.candidate))
        write_delta(args.delta, result.deltas)
        write_summary(args.summary, result)
    except (InventoryError, csv.Error, UnicodeError, OSError) as error:
        if isinstance(error, InventoryError):
            code = error.code
        elif isinstance(error, OSError):
            code = "UNREADABLE_INPUT"
        else:
            code = "UNREADABLE_TSV"
        result = malformed_result()
        write_delta(args.delta, ())
        write_summary(args.summary, result, code)
    print(result.classification)
    return exit_code(result.classification, args.mode)


if __name__ == "__main__":
    raise SystemExit(main())
