#!/usr/bin/env python3
"""Check public documentation links and the quasiquote surface boundary."""

import argparse
from collections import Counter
import csv
from pathlib import Path
import re
import sys
from urllib.parse import unquote


SURFACE_ROWS = {
    "Term construction": (
        '`qr"..."`',
        "Public now",
        "`QuasiquoteBuilder.build(...)`",
        "Public now",
    ),
    "Term pattern matching": (
        '`case qq"..."`',
        "Public now",
        "`QuasiPattern.term(...)`, `termOrThrow(...)`",
        "Public now",
    ),
    "Type construction": (
        '`tqr"..."`',
        "Public now",
        "`QuasiTypequotes.tqr(...)`",
        "Public research API",
    ),
    "Type pattern matching": (
        '`case tqq"..."`',
        "Public now",
        "`QuasiTypequotes.tqq(...)` / `QuasiTypePattern.*`",
        "Public research API",
    ),
    "Definition construction": (
        '`dqr"def id(x: $parameterType): $resultType = x"`',
        "Public now, exact bounded shape",
        "`DefinitionConstruction.*`",
        "Public bounded compiler-free API",
    ),
    "Definition pattern matching": (
        '`case dqq"def id(x: Int): Int = $body"`',
        "Public now, exact bounded shape",
        "`DefinitionPattern.singleParameter(...)`",
        "Public now, exact bounded shape",
    ),
}

PUBLIC_API = {
    ("quasiquotes.construct.Quasiquotes", "qr"),
    ("quasiquotes.construct.QuasiquoteBuilder", "build"),
    ("quasiquotes.matching.QuasiPattern", "term"),
    ("quasiquotes.matching.QuasiPattern", "termOrThrow"),
    ("quasiquotes.matching.QuasiPattern", "qq"),
    ("quasiquotes.matching.TermPatternExtractor", "unapplySeq"),
    ("quasiquotes.types.QuasiTypequotes", "tqr"),
    ("quasiquotes.types.QuasiTypequotes", "tqq"),
    ("quasiquotes.types.TypePatternExtractor", "unapplySeq"),
    ("quasiquotes.construct.Quasiquotes", "dqr"),
    ("quasiquotes.matching.DefinitionPattern", "singleParameter"),
    ("quasiquotes.matching.DefinitionPattern", "dqq"),
    ("quasiquotes.matching.SingleParameterDefinitionPattern", "matchDefinition"),
    ("quasiquotes.matching.SingleParameterDefinitionPattern", "unapply"),
    ("quasiquotes.publicapi.DefinitionConstruction", "twoParameterMethod"),
}

PUBLIC_API_MODULES = {"core", "frontend"}
API_COUNT_STATEMENT = re.compile(
    r"The machine-readable \[0\.2\.0 public API baseline\]\(docs/api-baselines/0\.2\.0\.tsv\)\s+"
    r"contains (?P<core>\d+) core and (?P<frontend>\d+) frontend "
    r"Scaladoc-visible entries\."
)


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
        if len(cells) == 5 and cells[0] != "Role":
            rows[cells[0]] = tuple(cells[1:])

    return [
        f"surface row mismatch: {role} expected {expected}, found {rows.get(role)}"
        for role, expected in SURFACE_ROWS.items()
        if rows.get(role) != expected
    ]


def api_rows(root: Path) -> list[dict[str, str]]:
    baseline = root / "docs/api-baselines/0.2.0.tsv"
    with baseline.open(encoding="utf-8", newline="") as handle:
        return list(csv.DictReader(handle, delimiter="\t"))


def api_findings(rows: list[dict[str, str]]) -> list[str]:
    available = {(row["owner"], row["name"]) for row in rows}
    findings = [
        f"missing public API inventory entry: {owner}.{name}"
        for owner, name in sorted(PUBLIC_API - available)
    ]
    return findings


def api_count_findings(root: Path, rows: list[dict[str, str]]) -> list[str]:
    counts = Counter(row["module"] for row in rows)
    if set(counts) != PUBLIC_API_MODULES:
        found = ", ".join(sorted(module or "<empty>" for module in counts))
        return [
            "public API count contract requires exactly core and frontend modules; "
            f"found: {found or '<none>'}"
        ]

    readme = (root / "README.md").read_text(encoding="utf-8")
    matches = list(API_COUNT_STATEMENT.finditer(readme))
    if len(matches) != 1:
        return [
            "README public API count statement must appear exactly once; "
            f"found {len(matches)}"
        ]

    documented = {
        module: int(matches[0].group(module)) for module in PUBLIC_API_MODULES
    }
    if any(documented[module] != counts[module] for module in PUBLIC_API_MODULES):
        return [
            "public API count drift: "
            f"README core={documented['core']} frontend={documented['frontend']}; "
            f"baseline core={counts['core']} frontend={counts['frontend']}"
        ]
    return []


def source_findings(root: Path) -> list[str]:
    definition_source = (
        root
        / "frontend/src/main/scala/quasiquotes/definitions/DefinitionQuasiquotes.scala"
    ).read_text(encoding="utf-8")
    pattern_source = (
        root / "frontend/src/main/scala/quasiquotes/matching/QuasiPattern.scala"
    ).read_text(encoding="utf-8")
    extractor_source = (
        root / "frontend/src/main/scala/quasiquotes/matching/TermPatternExtractor.scala"
    ).read_text(encoding="utf-8")
    type_surface_source = (
        root / "frontend/src/main/scala/quasiquotes/types/QuasiTypequotes.scala"
    ).read_text(encoding="utf-8")
    type_extractor_source = (
        root / "frontend/src/main/scala/quasiquotes/types/TypePatternExtractor.scala"
    ).read_text(encoding="utf-8")
    construct_surface_source = (
        root / "frontend/src/main/scala/quasiquotes/construct/Quasiquotes.scala"
    ).read_text(encoding="utf-8")
    definition_pattern_source = (
        root / "frontend/src/main/scala/quasiquotes/matching/DefinitionPattern.scala"
    ).read_text(encoding="utf-8")
    findings = []
    if "private[quasiquotes] object DefinitionQuasiquotes" not in definition_source:
        findings.append("dqr owner is no longer package-private")
    if "def dqr" not in definition_source:
        findings.append("documented internal dqr implementation is absent")
    if "def dqr(using q: Quotes)(args: q.reflect.TypeRepr*): q.reflect.DefDef" not in construct_surface_source:
        findings.append("public dqr signature no longer matches documentation")
    if "def qq(using q: Quotes): TermPatternExtractor[q.reflect.Term]" not in pattern_source:
        findings.append("public qq signature no longer matches documentation")
    if "def unapplySeq(value: T): Option[Seq[T]]" not in extractor_source:
        findings.append("public qq extractor protocol no longer matches documentation")
    if "def tqr(using q: Quotes)(args: q.reflect.TypeRepr*): q.reflect.TypeRepr" not in type_surface_source:
        findings.append("public tqr interpolator signature no longer matches documentation")
    if "def tqq(using q: Quotes): TypePatternExtractor[q.reflect.TypeRepr]" not in type_surface_source:
        findings.append("public tqq extractor signature no longer matches documentation")
    if "def unapplySeq(value: T): Option[Seq[T]]" not in type_extractor_source:
        findings.append("public tqq extractor protocol no longer matches documentation")
    if "def singleParameter(" not in definition_pattern_source:
        findings.append("public definition-pattern factory no longer matches documentation")
    if "def matchDefinition(using q: Quotes)(" not in definition_pattern_source:
        findings.append("public definition matcher no longer matches documentation")
    if "def dqq(using q: Quotes): SingleParameterDefinitionPattern" not in definition_pattern_source:
        findings.append("public dqq signature no longer matches documentation")
    if "def unapply(using q: Quotes)(" not in definition_pattern_source:
        findings.append("public definition extractor protocol no longer matches documentation")
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
    rows = api_rows(root)
    return sorted(
        relative_link_findings(root)
        + table_findings(root)
        + api_findings(rows)
        + api_count_findings(root, rows)
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
