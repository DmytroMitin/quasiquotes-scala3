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
RELATED_PROJECT_URLS = (
    "https://github.com/DmytroMitin/macroparadise-scala3",
    "https://github.com/DmytroMitin/AUXify-scala3",
)
ARCHITECTURE_FACT_MARKERS = (
    "The project owns one compiler-free semantic model.",
    "current-Dotty frontend\nis the released default and the reference oracle",
    "typed Scalameta route is\nan explicit, unpublished opt-in",
    "must agree wherever they\nboth advertise support",
    "only a Scalameta parse failure may fall back",
    "no public symbol-quasiquote family\nis currently planned",
    "typed backend's lowering plan.",
    "The compiler-free model is symbol-free",
    "pre-typer `untpd` backend must\nemit syntax without fabricating typed symbols",
)
NORTH_STAR_STATUS_MARKERS = (
    "All conceptual quasiquote syntax in this document is future, non-current",
    "CURRENT_MANUAL_BASELINE_PROVED",
    "DESIGN_REQUIRED",
    "IMPLEMENTATION_REQUIRED",
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


def source_contract_present(source: str, pattern: str) -> bool:
    return re.search(pattern, source, flags=re.DOTALL) is not None


def qq_contract_markers_present(source: str, count_marker: str) -> bool:
    normalized = " ".join(source.split()).lower()
    return all(
        marker.lower() in normalized
        for marker in (
            "scalar",
            "q.reflect.Term",
            "sequence",
            "Seq[q.reflect.Term]",
            count_marker,
            "Apply",
            "New",
        )
    )


def qq_matrix_contract_present(source: str) -> bool:
    wanted_roles = {"Sequence-Term arguments", "Ordered term capture extractor"}
    relevant_rows = []
    for line in source.splitlines():
        if not line.lstrip().startswith("|"):
            continue
        cells = [cell.strip() for cell in line.strip().strip("|").split("|")]
        if cells and cells[0] in wanted_roles:
            relevant_rows.append(line)
    return qq_contract_markers_present("\n".join(relevant_rows), "exactly one")


def qq_limitations_contract_present(source: str) -> bool:
    section = re.search(
        r"(?ms)^## Bounded term-pattern extractor\s*$.*?(?=^## |\Z)", source
    )
    return section is not None and qq_contract_markers_present(
        section.group(0), "one direct"
    )


def source_findings(root: Path) -> list[str]:
    definition_source = (
        root
        / "frontend/src/main/scala/quasiquotes/definitions/DefinitionQuasiquotes.scala"
    ).read_text(encoding="utf-8")
    pattern_source = (
        root / "frontend/src/main/scala/quasiquotes/matching/QuasiPattern.scala"
    ).read_text(encoding="utf-8")
    pattern_macro_source = (
        root / "frontend/src/main/scala/quasiquotes/matching/QuasiPatternMacro.scala"
    ).read_text(encoding="utf-8")
    extractor_source = (
        root / "frontend/src/main/scala/quasiquotes/matching/TermPatternExtractor.scala"
    ).read_text(encoding="utf-8")
    ranked_extractor_source = (
        root
        / "frontend/src/main/scala/quasiquotes/matching/RankedTermPatternExtractor.scala"
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
    syntax_matrix = (root / "docs/SYNTAX_SUPPORT_MATRIX.md").read_text(
        encoding="utf-8"
    )
    syntax_limitations = (
        root / "docs/SUPPORTED_SYNTAX_AND_LIMITATIONS.md"
    ).read_text(encoding="utf-8")
    findings = []
    if "private[quasiquotes] object DefinitionQuasiquotes" not in definition_source:
        findings.append("dqr owner is no longer package-private")
    if "def dqr" not in definition_source:
        findings.append("documented internal dqr implementation is absent")
    if "def dqr(using q: Quotes)(args: q.reflect.TypeRepr*): q.reflect.DefDef" not in construct_surface_source:
        findings.append("public dqr signature no longer matches documentation")
    if not source_contract_present(
        pattern_source,
        r"extension\s*\(\s*inline\s+[A-Za-z_]\w*\s*:\s*StringContext\s*\)\s*"
        r"transparent\s+inline\s+def\s+qq\s*"
        r"\(\s*using\s+[A-Za-z_]\w*\s*:\s*Quotes\s*\)\s*=",
    ):
        findings.append("public qq transparent-inline selector contract is absent")
    if not (
        source_contract_present(
            pattern_source,
            r"private\s*\[\s*matching\s*\]\s+def\s+scalarExtractor\s*"
            r"\(\s*[A-Za-z_]\w*\s*:\s*StringContext\s*\)\s*"
            r"\(\s*using\s+(?P<quotes>[A-Za-z_]\w*)\s*:\s*Quotes\s*\)\s*:\s*"
            r"TermPatternExtractor\s*\[\s*(?P=quotes)\.reflect\.Term\s*\]",
        )
        and "QuasiPattern.scalarExtractor" in pattern_macro_source
    ):
        findings.append("public qq scalar extractor route contract is absent")
    if not source_contract_present(
        pattern_source,
        r"@targetName\s*\(\s*\"qq\"\s*\)\s*"
        r"private\s*\[\s*matching\s*\]\s+def\s+qqLegacy\s*"
        r"\(\s*(?P<context>[A-Za-z_]\w*)\s*:\s*StringContext\s*\)\s*"
        r"\(\s*using\s+(?P<quotes>[A-Za-z_]\w*)\s*:\s*Quotes\s*\)\s*:\s*"
        r"TermPatternExtractor\s*\[\s*(?P=quotes)\.reflect\.Term\s*\]\s*=\s*"
        r"scalarExtractor\s*\(\s*(?P=context)\s*\)",
    ):
        findings.append("public qq legacy JVM bridge contract is absent")
    if not source_contract_present(
        extractor_source,
        r"def\s+unapplySeq\s*\(\s*value\s*:\s*T\s*\)\s*:\s*"
        r"Option\s*\[\s*Seq\s*\[\s*T\s*\]\s*\]",
    ):
        findings.append("public qq extractor protocol no longer matches documentation")
    if not (
        source_contract_present(
            ranked_extractor_source,
            r"final\s+class\s+RankedTermPatternExtractor\s*"
            r"\[\s*(?P<term>[A-Za-z_]\w*)\s*,\s*"
            r"(?P<captures>[A-Za-z_]\w*)\s*<:\s*Tuple\s*\].*?"
            r"def\s+unapply\s*\(\s*[A-Za-z_]\w*\s*:\s*(?P=term)\s*\)\s*:\s*"
            r"Option\s*\[\s*(?P=captures)\s*\]",
        )
        and "RankedTermPatternExtractorFactory" in pattern_macro_source
    ):
        findings.append("public qq ranked extractor contract is absent")
    if not (
        qq_matrix_contract_present(syntax_matrix)
        and qq_limitations_contract_present(syntax_limitations)
    ):
        findings.append("public qq scalar/ranked documentation contract is absent")
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


def durable_documentation_findings(root: Path) -> list[str]:
    readme = (root / "README.md").read_text(encoding="utf-8")
    architecture = (root / "docs/ARCHITECTURE.md").read_text(encoding="utf-8")
    roadmap = (root / "ROADMAP.md").read_text(encoding="utf-8")
    why = (root / "docs/WHY_QUASIQUOTES.md").read_text(encoding="utf-8")
    scalameta_experiment = (
        root / "docs/HYBRID_SCALAMETA_TERM_FRONTEND_EXPERIMENT.md"
    ).read_text(encoding="utf-8")
    north_star = (root / "docs/NORTH_STAR_QUASIQUOTE_EXAMPLES.md").read_text(
        encoding="utf-8"
    )
    findings = []
    for url in RELATED_PROJECT_URLS:
        if url not in readme:
            findings.append(f"README missing related-project link: {url}")
    if "(docs/ARCHITECTURE.md)" not in readme:
        findings.append("README missing canonical architecture link")
    for marker in ARCHITECTURE_FACT_MARKERS:
        if marker not in architecture:
            findings.append(f"canonical architecture missing fact: {marker}")
    if "(docs/NORTH_STAR_QUASIQUOTE_EXAMPLES.md)" not in roadmap:
        findings.append("roadmap missing north-star document link")
    if "(NORTH_STAR_QUASIQUOTE_EXAMPLES.md)" not in why:
        findings.append("why-quasiquotes missing north-star document link")
    if "(ARCHITECTURE.md)" not in scalameta_experiment:
        findings.append("Scalameta experiment missing canonical architecture link")
    for marker in NORTH_STAR_STATUS_MARKERS:
        if marker not in north_star:
            findings.append(f"north-star document missing status marker: {marker}")
    for checkpoint in range(1, 6):
        section_match = re.search(
            rf"(?ms)^## N{checkpoint}\b.*?(?=^## N(?:{checkpoint + 1})\b|\Z)",
            north_star,
        )
        if section_match is None:
            findings.append(f"north-star document missing checkpoint: N{checkpoint}")
        else:
            section = section_match.group(0)
            for required_section in (
                "### Manual/current baseline",
                "### Desired source-like shape",
                "### Required missing capabilities",
                "### Checkpoint criterion",
            ):
                if required_section not in section:
                    findings.append(
                        f"north-star checkpoint N{checkpoint} missing section: "
                        f"{required_section}"
                    )
        rows = [
            line
            for line in roadmap.splitlines()
            if re.match(rf"^\|\s*N{checkpoint}\b", line)
        ]
        if len(rows) != 1:
            findings.append(
                f"roadmap must contain exactly one checkpoint row for N{checkpoint}"
            )
        elif (
            "DESIGN_REQUIRED" not in rows[0]
            or "IMPLEMENTATION_REQUIRED" not in rows[0]
            or "CHECKPOINT_COMPLETE" in rows[0]
        ):
            findings.append(f"roadmap checkpoint N{checkpoint} has invalid status")
    return findings


def check(root: Path) -> list[str]:
    rows = api_rows(root)
    return sorted(
        relative_link_findings(root)
        + table_findings(root)
        + api_findings(rows)
        + api_count_findings(root, rows)
        + source_findings(root)
        + matrix_findings(root)
        + durable_documentation_findings(root)
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
