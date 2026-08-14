#!/usr/bin/env python3
"""Check public documentation links and the quasiquote surface boundary."""

import argparse
import csv
from pathlib import Path
import re
import sys
from urllib.parse import unquote


SURFACES = {
    '`qr"..."`': "Public now",
    "`QuasiPattern.term(...)`": "Public now",
    "`QuasiTypequotes.tqr(...)`": "Public research API",
    "`QuasiTypequotes.tqq(...)`": "Public research API",
    "`DefinitionConstruction`": "Public now",
    '`dqr"..."`': "Internal research",
    "`dqq`": "Not yet",
}

PUBLIC_API = {
    ("quasiquotes.construct.Quasiquotes", "qr"),
    ("quasiquotes.matching.QuasiPattern", "term"),
    ("quasiquotes.types.QuasiTypequotes", "tqr"),
    ("quasiquotes.types.QuasiTypequotes", "tqq"),
    ("quasiquotes.publicapi.DefinitionConstruction", "twoParameterMethod"),
}


def marked(text: str, start: str, end: str, path: Path) -> str:
    if text.count(start) != 1 or text.count(end) != 1:
        raise ValueError(f"{path}: expected exactly one surface-table marker pair")
    return text.split(start, 1)[1].split(end, 1)[0]


def relative_link_findings(root: Path) -> list[str]:
    findings = []
    link = re.compile(r"\[[^\]]+\]\(([^)]+)\)")
    for markdown in sorted(root.rglob("*.md")):
        text = markdown.read_text(encoding="utf-8")
        prose = re.sub(r"```.*?```", "", text, flags=re.DOTALL)
        prose = re.sub(r"`[^`\n]*`", "", prose)
        for target in link.findall(prose):
            target = target.strip().strip("<>").split("#", 1)[0]
            if not target or target.startswith(("http://", "https://", "mailto:")):
                continue
            resolved = (markdown.parent / unquote(target)).resolve()
            if not resolved.exists():
                findings.append(
                    f"missing relative link: {markdown.relative_to(root)} -> {target}"
                )
    return findings


def table_findings(root: Path) -> list[str]:
    readme = root / "README.md"
    section = marked(
        readme.read_text(encoding="utf-8"),
        "<!-- public-surface-table:start -->",
        "<!-- public-surface-table:end -->",
        readme,
    )
    rows = {}
    for line in section.splitlines():
        if not line.startswith("|") or line.startswith("| ---"):
            continue
        cells = [cell.strip() for cell in line.strip("|").split("|")]
        if len(cells) >= 3 and cells[0] != "Surface":
            rows[cells[0]] = cells[2]

    return [
        f"surface status mismatch: {surface} expected {status}, found {rows.get(surface)}"
        for surface, status in SURFACES.items()
        if rows.get(surface) != status
    ]


def api_findings(root: Path) -> list[str]:
    baseline = root / "docs/PUBLIC_API_BASELINE.tsv"
    with baseline.open(encoding="utf-8", newline="") as handle:
        rows = list(csv.DictReader(handle, delimiter="\t"))
    available = {(row["owner"], row["name"]) for row in rows}
    findings = [
        f"missing public API inventory entry: {owner}.{name}"
        for owner, name in sorted(PUBLIC_API - available)
    ]
    for forbidden in ("dqr", "dqq"):
        if any(name == forbidden for _, name in available):
            findings.append(f"internal/not-yet surface leaked into public API: {forbidden}")
    return findings


def source_findings(root: Path) -> list[str]:
    definition_source = (
        root
        / "frontend/src/main/scala/quasiquotes/definitions/DefinitionQuasiquotes.scala"
    ).read_text(encoding="utf-8")
    pattern_source = (
        root / "frontend/src/main/scala/quasiquotes/matching/QuasiPattern.scala"
    ).read_text(encoding="utf-8")
    findings = []
    if "private[quasiquotes] object DefinitionQuasiquotes" not in definition_source:
        findings.append("dqr owner is no longer package-private")
    if "def dqr" not in definition_source:
        findings.append("documented internal dqr implementation is absent")
    if "def qq: Nothing" not in pattern_source or "UnsupportedOperationException" not in pattern_source:
        findings.append("reserved qq boundary no longer matches documentation")
    for source in sorted((root / "core/src/main").rglob("*.scala")) + sorted(
        (root / "frontend/src/main").rglob("*.scala")
    ):
        if re.search(r"\bdef\s+dqq\b", source.read_text(encoding="utf-8")):
            findings.append(f"dqq implementation exists but is documented not yet: {source}")
    return findings


def matrix_findings(root: Path) -> list[str]:
    matrix = (root / "docs/SYNTAX_SUPPORT_MATRIX.md").read_text(encoding="utf-8")
    required = (
        "SUPPORTED",
        "BOUNDED",
        "INTERNAL",
        "NOT_YET",
        "NOT_PLANNED",
        "https://docs.scala-lang.org/overviews/quasiquotes/syntax-summary.html",
        "https://scalameta.org/docs/trees/quasiquotes",
        "Any change that adds, removes, or materially alters a term, type, or definition",
    )
    return [f"syntax matrix missing required contract: {value}" for value in required if value not in matrix]


def check(root: Path) -> list[str]:
    return sorted(
        relative_link_findings(root)
        + table_findings(root)
        + api_findings(root)
        + source_findings(root)
        + matrix_findings(root)
    )


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--root", type=Path, default=Path(__file__).resolve().parents[2])
    arguments = parser.parse_args()
    root = arguments.root.resolve()
    try:
        findings = check(root)
    except (OSError, ValueError, KeyError) as error:
        print(error, file=sys.stderr)
        return 2
    if findings:
        print("\n".join(findings), file=sys.stderr)
        return 1
    print("PUBLIC_DOCUMENTATION_BOUNDARY_PASS")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
