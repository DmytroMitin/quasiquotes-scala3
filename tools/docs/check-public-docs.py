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
    "typed Scalameta route is\nan explicit, released opt-in",
    "must agree wherever they\nboth advertise support",
    "only a Scalameta parse failure may fall back",
    "no public symbol-quasiquote family\nis currently planned",
    "typed backend's lowering plan.",
    "The compiler-free model is symbol-free",
    "pre-typer `untpd` backend must\nemit syntax without fabricating typed symbols",
)
NORTH_STAR_STATUS_MARKERS = (
    "Syntax explicitly marked conceptual remains future notation.",
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
    ranked_type_extractor_source = (
        root
        / "frontend/src/main/scala/quasiquotes/types/RankedTypePatternExtractor.scala"
    ).read_text(encoding="utf-8")
    construct_surface_source = (
        root / "frontend/src/main/scala/quasiquotes/construct/Quasiquotes.scala"
    ).read_text(encoding="utf-8")
    definition_pattern_source = (
        root / "frontend/src/main/scala/quasiquotes/matching/DefinitionPattern.scala"
    ).read_text(encoding="utf-8")
    definition_pattern_macro_source = (
        root / "frontend/src/main/scala/quasiquotes/matching/DefinitionPatternMacro.scala"
    ).read_text(encoding="utf-8")
    definition_pattern_extractor_source = (
        root
        / "frontend/src/main/scala/quasiquotes/matching/DefinitionPatternExtractor.scala"
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
    if not source_contract_present(
        type_surface_source,
        r"extension\s*\(\s*inline\s+[A-Za-z_]\w*\s*:\s*StringContext\s*\)\s*"
        r"transparent\s+inline\s+def\s+tqq\s*"
        r"\(\s*using\s+[A-Za-z_]\w*\s*:\s*Quotes\s*\)\s*=",
    ):
        findings.append("public tqq transparent-inline selector contract is absent")
    if not source_contract_present(
        type_surface_source,
        r"@targetName\s*\(\s*\"tqq\"\s*\)\s*"
        r"private\s*\[\s*types\s*\]\s+def\s+tqqLegacy\s*"
        r"\(\s*(?P<context>[A-Za-z_]\w*)\s*:\s*StringContext\s*\)\s*"
        r"\(\s*using\s+(?P<quotes>[A-Za-z_]\w*)\s*:\s*Quotes\s*\)\s*:\s*"
        r"TypePatternExtractor\s*\[\s*(?P=quotes)\.reflect\.TypeRepr\s*\]\s*=\s*"
        r"scalarExtractor\s*\(\s*(?P=context)\s*\)",
    ):
        findings.append("public tqq legacy JVM bridge contract is absent")
    if "def unapplySeq(value: T): Option[Seq[T]]" not in type_extractor_source:
        findings.append("public scalar tqq extractor protocol no longer matches documentation")
    if not source_contract_present(
        ranked_type_extractor_source,
        r"final\s+class\s+RankedTypePatternExtractor\s*"
        r"\[\s*(?P<target>[A-Za-z_]\w*)\s*,\s*"
        r"(?P<captures>[A-Za-z_]\w*)\s*<:\s*Tuple\s*\].*?"
        r"def\s+unapply\s*\(\s*[A-Za-z_]\w*\s*:\s*(?P=target)\s*\)\s*:\s*"
        r"Option\s*\[\s*(?P=captures)\s*\]",
    ):
        findings.append("public ranked tqq extractor contract is absent")
    if "def singleParameter(" not in definition_pattern_source:
        findings.append("public definition-pattern factory no longer matches documentation")
    if "def matchDefinition(using q: Quotes)(" not in definition_pattern_source:
        findings.append("public definition matcher no longer matches documentation")
    if not source_contract_present(
        definition_pattern_source,
        r"extension\s*\(\s*inline\s+[A-Za-z_]\w*\s*:\s*StringContext\s*\)\s*"
        r"transparent\s+inline\s+def\s+dqq\s*"
        r"\(\s*using\s+[A-Za-z_]\w*\s*:\s*Quotes\s*\)\s*=",
    ):
        findings.append("public dqq transparent-inline selector contract is absent")
    if not (
        "DefinitionPattern.singleParameterExtractor" in definition_pattern_macro_source
        and "DefinitionPattern.twoParameterExtractor" in definition_pattern_macro_source
    ):
        findings.append("public dqq structural specialization routes are absent")
    if not source_contract_present(
        definition_pattern_source,
        r"@targetName\s*\(\s*\"dqq\"\s*\)\s*"
        r"private\s*\[\s*matching\s*\]\s+def\s+dqqLegacy\s*"
        r"\(\s*(?P<context>[A-Za-z_]\w*)\s*:\s*StringContext\s*\)\s*"
        r"\(\s*using\s+(?P<quotes>[A-Za-z_]\w*)\s*:\s*Quotes\s*\)\s*:\s*"
        r"SingleParameterDefinitionPattern\s*=\s*"
        r"singleParameterExtractor\s*\(\s*(?P=context)\s*\)",
    ):
        findings.append("public dqq legacy JVM bridge contract is absent")
    if "def unapply(using q: Quotes)(" not in definition_pattern_source:
        findings.append("public definition extractor protocol no longer matches documentation")
    if "final class DefinitionPatternExtractor" not in definition_pattern_extractor_source or \
            "def unapply(using q: Quotes)(" not in definition_pattern_extractor_source:
        findings.append("public structural definition extractor protocol is absent")
    return findings


def markdown_table_rows(source: str) -> dict[str, list[list[str]]]:
    rows: dict[str, list[list[str]]] = {}
    for line in source.splitlines():
        if not line.lstrip().startswith("|"):
            continue
        cells = [cell.strip() for cell in line.strip().strip("|").split("|")]
        if not cells or cells[0] in {"Family", "---"}:
            continue
        rows.setdefault(cells[0].strip("`"), []).append(cells)
    return rows


def capability_status(cell: str) -> str | None:
    match = re.search(
        r"\b(SUPPORTED|BOUNDED|INTERNAL|NOT_YET|NOT_APPLICABLE)\b", cell
    )
    return match.group(1) if match else None


def cross_surface_rank_findings(root: Path) -> list[str]:
    matrix = (
        root / "docs/CROSS_SURFACE_CAPABILITY_MATRIX.md"
    ).read_text(encoding="utf-8")
    rows = markdown_table_rows(matrix)
    required = {
        "Rank-2 Term arguments in Apply / one-list New": (
            "rank-2 Term matrix",
            ("BOUNDED", "BOUNDED", "BOUNDED", "BOUNDED"),
        ),
        "Runtime-length Type application arguments": (
            "rank-2 Type matrix",
            ("BOUNDED", "BOUNDED", "BOUNDED", "BOUNDED"),
        ),
    }
    findings = []
    for family, (description, expected) in required.items():
        candidates = rows.get(family, [])
        if len(candidates) != 1:
            findings.append(
                f"{description} requires exactly one {family!r} row; "
                f"found {len(candidates)}"
            )
            continue
        row = candidates[0]
        if len(row) < 5:
            findings.append(f"{description} is missing capability columns")
            continue
        actual = tuple(capability_status(cell) for cell in row[1:5])
        if actual != expected:
            findings.append(
                f"{description} must report Q construct/match and typed-Scalameta "
                f"construct/match as {expected}; found {actual}"
            )
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
    syntax_limitations = (
        root / "docs/SUPPORTED_SYNTAX_AND_LIMITATIONS.md"
    ).read_text(encoding="utf-8")
    compatibility = (root / "docs/COMPATIBILITY.md").read_text(encoding="utf-8")
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
    q012rr_documentation_tokens = (
        "SingleParameterDefinitionPattern",
        "DefinitionPatternExtractor",
        "dynamic/non-static",
    )
    if any(token not in readme for token in q012rr_documentation_tokens) or any(
        token not in syntax_limitations for token in q012rr_documentation_tokens
    ) or any(token not in readme for token in ("dqq2", "dqq3", "dqq4")):
        findings.append("Q012RR definition-pattern documentation contract is absent")
    q012rr_compatibility_tokens = (
        "transparent-inline selector",
        "historical erased JVM descriptor",
        "source-hidden bridge",
        "experimental 0.x-minor-class change",
    )
    normalized_compatibility = " ".join(compatibility.split())
    if any(token not in normalized_compatibility for token in q012rr_compatibility_tokens):
        findings.append("Q012RR dqq compatibility accounting is absent")
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
            required_sections = (
                "### Manual/current baseline",
                "### Implemented bounded source-like shape" if checkpoint == 2
                else "### Desired source-like shape",
                "### Remaining capabilities" if checkpoint == 2
                else "### Required missing capabilities",
                "### Checkpoint criterion",
            )
            for required_section in required_sections:
                if required_section not in section:
                    findings.append(
                        f"north-star checkpoint N{checkpoint} missing section: "
                        f"{required_section}"
                    )
            if checkpoint == 2:
                normalized_section = " ".join(section.split())
                for marker in (
                    'tqr"$constructor[..$arguments]"',
                    "construction-only overload",
                    "caller-owned reflected class constructor",
                    "runtime-length ordered `Seq[TypeRepr]`",
                    "validates arity, kinds",
                    "directly in the caller's Quotes universe",
                    "exact constructor/argument identities",
                    "General TypeLambda authoring, aliases-as-aliases, instance-dependent prefixes",
                    "refinements, nontrivial constrained bounds and broader kind calculus remain outside the selected slice",
                    "Both `tqq` frontends also match one exact root constructor plus exactly one direct rank-2 argument slot",
                    "scalar captures remain exact `TypeRepr` values and the ranked capture is the original ordered `Seq[TypeRepr]`",
                    "Multiple rank-2 holes, rank 3, repeated or non-root constructor holes, non-direct tuple/function positions, and general selected source-constructor syntax remain unsupported",
                    "No neutral runtime-sequence model is implied by the direct reflection path",
                ):
                    if marker not in normalized_section:
                        findings.append(
                            f"north-star checkpoint N2 missing bounded contract: {marker}"
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
        else:
            row = rows[0]
            if checkpoint == 2:
                valid_status = (
                    "BOUNDED_STANDARD_AND_TYPED_SCALAMETA_CONSTRUCTION_IMPLEMENTED" in row
                    and "dynamic-constructor ranked `tqq` matching is implemented on both typed frontends" in row
                    and "construction and broader kinds remain narrower" in row
                    and "DESIGN_REQUIRED" not in row
                    and "IMPLEMENTATION_REQUIRED" not in row
                )
            else:
                valid_status = "DESIGN_REQUIRED" in row and "IMPLEMENTATION_REQUIRED" in row
            if not valid_status or "CHECKPOINT_COMPLETE" in row:
                findings.append(f"roadmap checkpoint N{checkpoint} has invalid status")
    return findings


def c065_truth_findings(root: Path) -> list[str]:
    limitations = (
        root / "docs/SUPPORTED_SYNTAX_AND_LIMITATIONS.md"
    ).read_text(encoding="utf-8")
    syntax_matrix = (root / "docs/SYNTAX_SUPPORT_MATRIX.md").read_text(
        encoding="utf-8"
    )
    cross_matrix = (root / "docs/CROSS_SURFACE_CAPABILITY_MATRIX.md").read_text(
        encoding="utf-8"
    )
    roadmap = " ".join((root / "ROADMAP.md").read_text(encoding="utf-8").split())
    normalized_limitations = " ".join(limitations.split())
    findings = []

    selected_forms = (
        "qq\"$receiver.$selectedName\"",
        "qq\"$receiver.$selectedName()\"",
        "qq\"$receiver.$selectedName($argument)\"",
        "qq\"$receiver.$selectedName(..$arguments)\"",
        "qq\"$receiver.$selectedName($head, ..$tail)\"",
        "qq\"$receiver.$selectedName(..$init, $last)\"",
        "qq\"$receiver.$selectedName($first, ..$middle, $last)\"",
    )
    type_forms = (
        "tqq\"$constructor[..$arguments]\"",
        "tqq\"$constructor[$head, ..$tail]\"",
        "tqq\"$constructor[..$init, $last]\"",
        "tqq\"$constructor[$first, ..$middle, $last]\"",
        "tqq\"$constructor[Int, ..$tail]\"",
    )
    for marker in selected_forms:
        if marker not in limitations:
            findings.append(f"C065 selected-member documentation missing layout: {marker}")
    for marker in type_forms:
        if marker not in limitations:
            findings.append(f"C065 dynamic tqq documentation missing layout: {marker}")
    for marker in (
        "SelectedMemberName",
        "already-resolved `Select.name`",
        "already-resolved overload",
        "Scalameta-primary",
        "compiler-presented `AppliedType`",
        "Matching is broader than the narrower construction contract",
    ):
        if marker not in limitations:
            findings.append(f"C065 detailed capability contract missing: {marker}")
    for marker in (
        "Dynamic selected-member name",
        "Runtime-sequence reflected Type application",
    ):
        if marker not in syntax_matrix:
            findings.append(f"C065 syntax matrix missing capability row: {marker}")
    for marker in (
        "Dynamic selected-member name",
        "compiler-presented `AppliedType`",
        "Scalameta-primary",
    ):
        if marker not in cross_matrix:
            findings.append(f"C065 cross-surface matrix missing contract: {marker}")

    for marker in (
        "`TermUntypedLowering` and `TermGeneratedOriginLowering`",
        "two or more successive ordinary argument clauses",
        "`ScalametaTermProjection` and `ScalametaTermShapeAuthoring` preserve completed nested clauses",
        "`CoreTermShapeUntypedLowerer` preserves completed nested clauses directly",
        "`ScalametaTermUntypedBridge` and `ScalametaTermGeneratedOriginBridge` preserve completed nested clauses without delegation",
    ):
        if marker not in cross_matrix:
            findings.append(f"C069 completed Apply coordinated surface missing: {marker}")

    for stale in (
        "`CoreTermShapeUntypedLowerer` remains one-list",
        "`ScalametaTermUntypedBridge` and `ScalametaTermGeneratedOriginBridge` remain one-list",
    ):
        if stale in cross_matrix:
            findings.append(f"C069 stale completed Apply one-list claim remains: {stale}")

    for marker in (
        "current 798-row / 779-group standard surface",
        "The direct Core source-free lowerer accepts direct identifiers, selections, and completed successive ordinary Apply clauses",
    ):
        if marker not in roadmap:
            findings.append(f"C069 roadmap truth missing: {marker}")
    for stale in (
        "current 794-row / 775-group standard surface",
        "The exact backend intentionally remains narrower: only the integer/infix core family lowers",
        "Identifier, Select, and Apply fail",
    ):
        if stale in roadmap:
            findings.append(f"C069 roadmap truth stale: {stale}")

    for marker in (
        "Both routes match one local identity method",
        "equal explicit `Int`, `String`, or `Boolean` parameter and result Types",
        "target method and parameter spellings remain alpha-equivalent by role",
    ):
        if marker.casefold() not in normalized_limitations.casefold():
            findings.append(f"C068 standard P3 matching contract missing: {marker}")

    by_name_doc = root / "docs/EXISTING_CLASS_UNTYPED_REWRITE.md"
    if by_name_doc.exists():
        calling_modes = " ".join(by_name_doc.read_text(encoding="utf-8").split())
        for marker in (
            "By-name parameter-Type replacement preserves a fresh `ByNameTypeTree` wrapper",
            "Repeated parameter-Type replacement preserves a fresh `PostfixOp` wrapper",
            "original `Ident(*)` marker by identity",
            "one ordinary clause of one or two parameters",
            "across two distinct admitted methods of the same captured class",
            "preserving independent method/parameter refs",
            "does not authorize arbitrary calling-mode rewriting",
        ):
            if marker not in calling_modes:
                findings.append(f"C068 bounded calling-mode safety contract missing: {marker}")
        for stale in (
            "Do not call `replaceParameterType` for a captured by-name parameter",
            "Do not call `replaceParameterType` for a captured repeated parameter",
        ):
            if stale in calling_modes:
                findings.append(f"C068 stale calling-mode safety warning: {stale}")
        for overclaim in (
            "arbitrary calling-mode rewriting is supported",
            "all method shapes support calling-mode parameter-Type replacement",
        ):
            if overclaim in calling_modes:
                findings.append(f"C068 calling-mode overclaim: {overclaim}")

    api_doc = root / "docs/API_COMPATIBILITY_REVIEW.md"
    if api_doc.exists():
        api = api_doc.read_text(encoding="utf-8")
        if "current standard inventory is 798 rows / 779 groups" not in api:
            findings.append("C065 active standard API inventory must be 798 rows / 779 groups")
        if "hybrid inventory remains 45 rows / 43 groups" not in api:
            findings.append("C065 active hybrid API inventory must be 45 rows / 43 groups")

    readme = (root / "README.md").read_text(encoding="utf-8")
    if "0.4.0-SNAPSHOT" in readme:
        for marker in (
            "0.4.0-SNAPSHOT standard inventory is 798 rows / 779 groups",
            "typed-Scalameta inventory is 45 rows / 43 groups",
            "across two distinct admitted methods of the same captured class",
        ):
            if marker not in " ".join(readme.split()):
                findings.append(f"C065 README truth missing: {marker}")
    return findings


def c070_truth_findings(root: Path) -> list[str]:
    limitations = (
        root / "docs/SUPPORTED_SYNTAX_AND_LIMITATIONS.md"
    ).read_text(encoding="utf-8")
    syntax_matrix = (root / "docs/SYNTAX_SUPPORT_MATRIX.md").read_text(
        encoding="utf-8"
    )
    cross_matrix = (root / "docs/CROSS_SURFACE_CAPABILITY_MATRIX.md").read_text(
        encoding="utf-8"
    )
    roadmap = (root / "ROADMAP.md").read_text(encoding="utf-8")
    rewrite_path = root / "docs/EXISTING_CLASS_UNTYPED_REWRITE.md"
    rewrite = rewrite_path.read_text(encoding="utf-8") if rewrite_path.exists() else ""
    pipeline_path = root / "docs/PROJECTION_LOWERING_BRIDGE_MATRIX.md"
    pipeline = pipeline_path.read_text(encoding="utf-8") if pipeline_path.exists() else ""
    findings = []

    normalized_limitations = " ".join(limitations.split())
    for marker in (
        "Standard and opt-in typed-Scalameta scalar `qq` matching share one bounded P3 local identity-method family",
        "Scalameta-primary compilation",
        "equal explicit `Int`, `String`, or `Boolean` parameter and result Types",
        "excluding symbolic and Unicode spellings",
        "target method and parameter spellings remain alpha-equivalent by role",
        "original caller-universe `quotes.reflect.Term` argument",
        "broader blocks, methods, clauses, captures, and ranked P3 remain unsupported",
    ):
        if marker.casefold() not in normalized_limitations.casefold():
            findings.append(f"C070 hybrid P3 bounded contract missing: {marker}")

    for marker in (
        "Scalameta-primary",
        "symbolic and Unicode source binders excluded",
        "original caller-universe `Term`",
    ):
        if marker not in cross_matrix:
            findings.append(f"C070 hybrid P3 matrix detail missing: {marker}")

    for marker in (
        "standard and opt-in typed-Scalameta `qq`",
        "symbolic/Unicode source binders excluded",
    ):
        if marker.casefold() not in syntax_matrix.casefold():
            findings.append(f"C070 syntax matrix P3 contract missing: {marker}")

    normalized_roadmap = " ".join(roadmap.split())
    for marker in (
        "Standard and opt-in typed-Scalameta scalar `qq` matching share the accepted bounded P3 local identity-method family",
    ):
        if marker.casefold() not in normalized_roadmap.casefold():
            findings.append(f"C070 roadmap P3 truth missing: {marker}")

    public_p3_text = "\n".join(
        (
            (root / "README.md").read_text(encoding="utf-8"),
            roadmap,
            cross_matrix,
            syntax_matrix,
            limitations,
            (root / "docs/GETTING_STARTED.md").read_text(encoding="utf-8")
            if (root / "docs/GETTING_STARTED.md").exists()
            else "",
            (root / "docs/NORTH_STAR_QUASIQUOTE_EXAMPLES.md").read_text(encoding="utf-8")
            if (root / "docs/NORTH_STAR_QUASIQUOTE_EXAMPLES.md").exists()
            else "",
        )
    ).casefold()
    for stale in (
        "not an accepted documented public capability",
        "has not yet completed its independent acceptance gate",
        "outside the accepted documented public boundary pending its independent acceptance gate",
    ):
        if stale in public_p3_text:
            findings.append(f"C070 stale hybrid P3 acceptance claim: {stale}")
    normalized_rewrite = " ".join(rewrite.split())
    for marker in (
        "bounded two-method transaction may combine those Type edits with capture-local omission of one direct member",
        "append of one ordinary accepted public `SemanticDefinition`",
        "Surviving original members retain exact order and identity",
        "generated member is last with deterministic generated-source provenance",
        "Both legal immutable plan-building orders are supported",
        "invalid combinations fail before a result is exposed",
        "does not authorize arbitrary multiple methods",
        "generated sibling-reference resolution",
        "does not authorize general transactional rollback across later compiler phases",
    ):
        if marker.casefold() not in normalized_rewrite.casefold():
            findings.append(f"C070 bounded owner transaction contract missing: {marker}")

    combined_owner_docs = (rewrite + "\n" + pipeline).casefold()
    for stale in (
        "does not authorize cross-member composition with omission or append",
        "does not claim cross-member composition with omission/append",
    ):
        if stale in combined_owner_docs:
            findings.append(f"C070 stale omit/append exclusion: {stale}")
    for overclaim in (
        "arbitrary multiple methods and general transactional rollback across later compiler phases are supported",
        "universal owner editing is supported",
    ):
        if overclaim in combined_owner_docs:
            findings.append(f"C070 existing-owner overclaim: {overclaim}")

    for marker in (
        "bounded two-method calling-mode edit plus capture-local omit and generated SemanticDefinition append",
        "not a universal owner editor",
    ):
        if marker.casefold() not in " ".join(pipeline.split()).casefold():
            findings.append(f"C070 pipeline matrix owner contract missing: {marker}")
    return findings


def c073_truth_findings(root: Path) -> list[str]:
    limitations = (
        root / "docs/SUPPORTED_SYNTAX_AND_LIMITATIONS.md"
    ).read_text(encoding="utf-8")
    syntax_matrix = (root / "docs/SYNTAX_SUPPORT_MATRIX.md").read_text(
        encoding="utf-8"
    )
    cross_matrix = (root / "docs/CROSS_SURFACE_CAPABILITY_MATRIX.md").read_text(
        encoding="utf-8"
    )
    roadmap = (root / "ROADMAP.md").read_text(encoding="utf-8")
    rewrite_path = root / "docs/EXISTING_CLASS_UNTYPED_REWRITE.md"
    rewrite = rewrite_path.read_text(encoding="utf-8") if rewrite_path.exists() else ""
    pipeline_path = root / "docs/PROJECTION_LOWERING_BRIDGE_MATRIX.md"
    pipeline = pipeline_path.read_text(encoding="utf-8") if pipeline_path.exists() else ""
    findings = []

    normalized_limitations = " ".join(limitations.split())
    for marker in (
        "Opt-in typed-Scalameta `qr` construction accepts that same fixed equal-Type identity shape only as the complete source root",
        "callback-owned `MethodType`, `DefDef`, `Quotes`, and source/Type sidecars",
        "Nested P3 in outer If, Apply, Tuple, P2 initializer, or Lambda body is rejected",
        "Immediate `qq` matching of a synthetically constructed P3 remains restricted because its TypeTrees lack explicit source provenance",
    ):
        if marker.casefold() not in normalized_limitations.casefold():
            findings.append(f"C073 hybrid P3 bounded contract missing: {marker}")

    rows = markdown_table_rows(cross_matrix)
    p3_rows = rows.get("Source-owned local identity method (P3)", [])
    if len(p3_rows) != 1:
        findings.append(
            "C073 hybrid P3 matrix requires exactly one source-owned local identity method row"
        )
    elif len(p3_rows[0]) < 5:
        findings.append("C073 hybrid P3 matrix is missing capability columns")
    else:
        actual = tuple(capability_status(cell) for cell in p3_rows[0][1:5])
        expected = ("BOUNDED", "BOUNDED", "BOUNDED", "BOUNDED")
        if actual != expected:
            findings.append(
                "C073 hybrid P3 matrix must record bounded standard and typed-Scalameta "
                f"construction/matching; expected {expected}, found {actual}"
            )
    for marker in (
        "complete-source-root local identity method only",
        "callback-owned `MethodType`, `DefDef`, `Quotes`, source and Type sidecars",
        "synthetic constructed P3 matching still requires explicit source TypeTree provenance",
    ):
        if marker not in cross_matrix:
            findings.append(f"C073 hybrid P3 matrix detail missing: {marker}")

    zero_rows = rows.get("Existing-class zero-parameter ordinary methods", [])
    if len(zero_rows) != 1:
        findings.append(
            "C073 zero-parameter matrix requires exactly one existing-class row"
        )
    elif capability_status(zero_rows[0][-1]) != "BOUNDED":
        findings.append("C073 zero-parameter matrix must be BOUNDED")
    for marker in (
        "`DefDef.paramss = Nil` projects as zero public term clauses",
        "`List(Nil)` projects as one empty ordinary clause",
        "both create zero parameter binders",
    ):
        if marker not in cross_matrix:
            findings.append(f"C073 zero-parameter matrix detail missing: {marker}")

    for marker in (
        "Opt-in typed-Scalameta `qr` construction is bounded to the complete source root",
        "nested P3 placement",
        "provenance-free matching of synthetic P3 trees remain unsupported",
    ):
        if marker.casefold() not in syntax_matrix.casefold():
            findings.append(f"C073 syntax matrix P3 contract missing: {marker}")

    normalized_roadmap = " ".join(roadmap.split())
    for marker in (
        "Opt-in typed-Scalameta `qr` construction admits the same one-method identity shape only as the complete source root",
        "synthetic provenance-free `qq` matching remain unavailable",
    ):
        if marker.casefold() not in normalized_roadmap.casefold():
            findings.append(f"C073 roadmap P3 truth missing: {marker}")

    normalized_rewrite = " ".join(rewrite.split())
    for marker in (
        "`DefDef.paramss = Nil`",
        "projects as zero public term parameter clauses",
        "`List(Nil)`",
        "projects as one empty ordinary clause",
        "both create zero `ParameterRef` binders",
    ):
        if marker.casefold() not in normalized_rewrite.casefold():
            findings.append(f"C073 zero-parameter topology contract missing: {marker}")

    normalized_pipeline = " ".join(pipeline.split())
    for marker in (
        "parameterless `Nil` as zero public term clauses",
        "explicit-empty `List(Nil)` as one empty ordinary clause",
        "zero parameter binders and preserved call syntax",
    ):
        if marker.casefold() not in normalized_pipeline.casefold():
            findings.append(f"C073 pipeline zero-parameter contract missing: {marker}")

    public_text = "\n".join(
        (
            (root / "README.md").read_text(encoding="utf-8"),
            roadmap,
            cross_matrix,
            syntax_matrix,
            limitations,
            rewrite,
            pipeline,
            (root / "docs/GETTING_STARTED.md").read_text(encoding="utf-8")
            if (root / "docs/GETTING_STARTED.md").exists()
            else "",
            (root / "docs/NORTH_STAR_QUASIQUOTE_EXAMPLES.md").read_text(encoding="utf-8")
            if (root / "docs/NORTH_STAR_QUASIQUOTE_EXAMPLES.md").exists()
            else "",
        )
    ).casefold()

    for stale in (
        "typed-scalameta p3 construction remains unavailable",
        "typed-scalameta construction | `not_yet`",
    ):
        if stale in public_text:
            findings.append(f"C073 stale hybrid P3 construction status: {stale}")

    for overclaim in (
        "nested typed-scalameta p3 construction is supported",
        "nested p3 construction is supported",
    ):
        if overclaim in public_text:
            findings.append(f"C073 hybrid P3 construction overclaim: {overclaim}")

    for overclaim in (
        "synthetic constructed p3 trees match `qq` without explicit source typetree provenance",
        "synthetic p3 qr trees are accepted by qq matching without source provenance",
    ):
        if overclaim in public_text:
            findings.append(f"C073 synthetic P3 matching overclaim: {overclaim}")

    conflation = "`defdef.paramss = nil` and `list(nil)` as the same zero-clause topology"
    if conflation in public_text:
        findings.append(f"C073 Nil/List(Nil) conflation: {conflation}")

    stale_facade = "the facade admits only one ordinary clause of one or two parameters"
    if stale_facade in public_text:
        findings.append(f"C073 stale existing-method facade claim: {stale_facade}")

    for overclaim in (
        "synthetic p3 qr trees are accepted by qq matching without source provenance",
        "repeated same-logical selected-name construction is a supported public capability",
    ):
        if overclaim in public_text:
            findings.append(f"C073 unaccepted shipping claim: {overclaim}")

    return findings


def c074_truth_findings(root: Path) -> list[str]:
    cross_matrix = (root / "docs/CROSS_SURFACE_CAPABILITY_MATRIX.md").read_text(
        encoding="utf-8"
    )
    rewrite = (root / "docs/EXISTING_CLASS_UNTYPED_REWRITE.md").read_text(
        encoding="utf-8"
    )
    pipeline = (root / "docs/PROJECTION_LOWERING_BRIDGE_MATRIX.md").read_text(
        encoding="utf-8"
    )
    findings = []

    rows = markdown_table_rows(cross_matrix)
    two_clause_rows = rows.get("Existing-class two ordinary unary clauses", [])
    if len(two_clause_rows) != 1:
        findings.append(
            "C074 two ordinary unary clause matrix requires exactly one existing-class row"
        )
    elif capability_status(two_clause_rows[0][-1]) != "BOUNDED":
        findings.append("C074 two ordinary unary clause matrix must be BOUNDED")
    for marker in (
        "exact `List(List(a), List(b))`",
        "two `Ordinary` clauses at ordinals 0 and 1",
        "distinct binders",
        "preserved `List(1, 1)` topology and curried call syntax",
    ):
        if marker not in cross_matrix:
            findings.append(f"C074 two ordinary unary clause matrix detail missing: {marker}")

    normalized_rewrite = " ".join(rewrite.split())
    for marker in (
        "admits exactly two ordinary unary clauses with distinct parameter names",
        "Those clauses remain separate at ordinals 0 and 1 with independent binders and `List(1, 1)` topology",
        "parameter-Type, result-Type, binder-aware body, and combined atomic edits preserve curried call syntax",
        "For the two-ordinary-clause family, duplicate names, generic methods, defaulted parameters, erased parameters, and by-name or repeated parameters remain unsupported",
    ):
        if marker.casefold() not in normalized_rewrite.casefold():
            findings.append(f"C074 two-clause topology contract missing: {marker}")

    normalized_pipeline = " ".join(pipeline.split())
    for marker in (
        "`List(List(a), List(b))` as two separate ordinary unary clauses",
        "distinct binders",
        "ordinals 0 and 1",
        "`List(1, 1)` topology",
        "curried call syntax",
    ):
        if marker.casefold() not in normalized_pipeline.casefold():
            findings.append(f"C074 pipeline two-clause contract missing: {marker}")

    public_text = "\n".join(
        (
            (root / "README.md").read_text(encoding="utf-8"),
            (root / "ROADMAP.md").read_text(encoding="utf-8"),
            cross_matrix,
            rewrite,
            pipeline,
            (root / "docs/DOTTY_INTERNAL_BACKEND.md").read_text(encoding="utf-8")
            if (root / "docs/DOTTY_INTERNAL_BACKEND.md").exists()
            else "",
            (root / "docs/GETTING_STARTED.md").read_text(encoding="utf-8")
            if (root / "docs/GETTING_STARTED.md").exists()
            else "",
            (root / "docs/SUPPORTED_SYNTAX_AND_LIMITATIONS.md").read_text(
                encoding="utf-8"
            ),
        )
    ).casefold()

    for stale in (
        "two ordinary unary clauses are not supported by existingclassuntypedrewrite",
        "two ordinary unary clauses remain unsupported by existingclassuntypedrewrite",
        "curried existing ordinary methods remain unsupported",
    ):
        if stale in public_text:
            findings.append(f"C074 stale two-clause rejection: {stale}")

    for overclaim in (
        "all contextual second clauses are supported by existingclassuntypedrewrite",
        "duplicate parameter names are supported in the two-clause family",
        "generic two-clause existing methods are supported",
        "by-name parameters are supported within the two-clause family",
        "repeated parameters are supported within the two-clause family",
    ):
        if overclaim in public_text:
            findings.append(f"C074 two-clause adjacent-family overclaim: {overclaim}")

    return findings


def c076_truth_findings(root: Path) -> list[str]:
    cross_matrix = (root / "docs/CROSS_SURFACE_CAPABILITY_MATRIX.md").read_text(
        encoding="utf-8"
    )
    rewrite = (root / "docs/EXISTING_CLASS_UNTYPED_REWRITE.md").read_text(
        encoding="utf-8"
    )
    pipeline = (root / "docs/PROJECTION_LOWERING_BRIDGE_MATRIX.md").read_text(
        encoding="utf-8"
    )
    findings = []

    rows = markdown_table_rows(cross_matrix)
    contextual_rows = rows.get("Existing-class contextual unary clauses", [])
    if len(contextual_rows) != 1:
        findings.append(
            "C076 contextual clause matrix requires exactly one existing-class row"
        )
    elif capability_status(contextual_rows[0][-1]) != "BOUNDED":
        findings.append("C076 contextual clause matrix must be BOUNDED")
    for marker in (
        "C1 exact `List(List(context))` exposes one `Contextual` clause at ordinal 0",
        "C2 exact `List(List(a), List(context))` exposes `Ordinary` at ordinal 0 then `Contextual` at ordinal 1",
        "distinct binders",
        "preserved `List(1)` or `List(1, 1)` topology and contextual call syntax",
    ):
        if marker not in cross_matrix:
            findings.append(f"C076 contextual clause matrix detail missing: {marker}")

    generic_g1_rows = rows.get("Existing-class generic G1 unary method", [])
    if len(generic_g1_rows) != 1:
        findings.append(
            "C076 accepted U068 G1 matrix requires exactly one existing-class row"
        )
    elif capability_status(generic_g1_rows[0][-1]) != "BOUNDED":
        findings.append("C076 accepted U068 G1 matrix must be BOUNDED")
    for marker in (
        "exact one unbounded private opaque `TypeDef` clause followed by one `Ordinary` strict unary term clause exposed at public term-clause ordinal 0",
        "Type-independent primitive parameter/result Types and binder-aware body edits preserve the original Type clause, `TypeDef`, and bounds by identity",
        "no public Type binder",
    ):
        if marker not in cross_matrix:
            findings.append(f"C076 accepted U068 G1 matrix detail missing: {marker}")

    generic_g2_rows = rows.get("Existing-class generic G2 unary method", [])
    if len(generic_g2_rows) != 1:
        findings.append(
            "C079 accepted U069 G2 matrix requires exactly one existing-class row"
        )
    elif capability_status(generic_g2_rows[0][-1]) != "BOUNDED":
        findings.append("C079 accepted U069 G2 matrix must be BOUNDED")
    for marker in (
        "exact one unbounded private opaque `TypeDef(A)` clause followed by one `Ordinary` strict unary term clause at public term-clause ordinal 0",
        "dependent parameter/result `Ident(A)` semantics and replacement remain `UNSUPPORTED_STRUCTURE`",
        "only the exact capture-local `x` reference is admitted as a replacement body",
        "no public Type binder",
    ):
        if marker not in cross_matrix:
            findings.append(f"C079 accepted U069 G2 matrix detail missing: {marker}")

    normalized_rewrite = " ".join(rewrite.split())
    for marker in (
        "C1 one contextual unary clause at ordinal 0",
        "C2 one ordinary unary clause at ordinal 0 followed by one contextual unary clause at ordinal 1",
        "Each clause exposes its true `Ordinary` or `Contextual` kind",
        '`ParameterClauseKind.Contextual.code == "CONTEXTUAL"`',
        "C1 preserves `List(List(context))` and `List(1)`",
        "C2 preserves `List(List(a), List(context))`, distinct binders, `List(1, 1)`, and contextual call syntax",
        "Contextual-before-ordinary, two contextual clauses, empty or wide contextual clauses, third clauses, duplicate names, Scala 2 implicit-style clauses, generic, defaulted, erased, by-name, and repeated contextual forms remain unsupported",
    ):
        if marker.casefold() not in normalized_rewrite.casefold():
            findings.append(f"C076 contextual rewrite contract missing: {marker}")

    for marker in (
        "The separate G1 family admits exact one unbounded private opaque `TypeDef` clause followed by one `Ordinary` strict unary term clause exposed at public term-clause ordinal 0",
        "Type-independent primitive parameter/result Types and binder-aware body edits preserve the original Type clause, `TypeDef`, and bounds by identity; no public Type binder is exposed",
    ):
        if marker.casefold() not in normalized_rewrite.casefold():
            findings.append(f"C076 accepted U068 G1 rewrite contract missing: {marker}")

    for marker in (
        "The separate G2 family admits exact one unbounded private opaque `TypeDef(A)` clause followed by one `Ordinary` strict unary term clause at public term-clause ordinal 0",
        "Dependent parameter/result `Ident(A)` semantics and replacement remain `UNSUPPORTED_STRUCTURE`",
        "only the exact capture-local `x` reference is admitted as a replacement body",
        "The Type clause, `TypeDef`, bounds, dependent Type nodes, source, span, method, and owner identities remain preserved",
        "no public Type binder is exposed",
        "Primitive-shadowing, bounded, wide, or multiple Type clauses, and contextual, defaulted, by-name, or repeated adjacent forms remain fail-closed",
    ):
        if marker.casefold() not in normalized_rewrite.casefold():
            findings.append(f"C079 accepted U069 G2 rewrite contract missing: {marker}")

    strict_val_warning = (
        "Generated strict `val` initializers must be appended in dependency order; ordinary "
        "Typer and TASTy success with no warnings does not make a forward eager reference "
        "safe, and the JVM may observe the default value"
    )
    if strict_val_warning.casefold() not in normalized_rewrite.casefold():
        findings.append("C079 accepted U070 strict-val warning missing")

    normalized_pipeline = " ".join(pipeline.split())
    for marker in (
        "C1 preserves one `Contextual` clause at ordinal 0",
        "C2 preserves `Ordinary` at ordinal 0 followed by `Contextual` at ordinal 1",
        "without flattening either clause topology",
        "contextual kind code is `CONTEXTUAL`",
    ):
        if marker.casefold() not in normalized_pipeline.casefold():
            findings.append(f"C076 pipeline contextual contract missing: {marker}")

    for marker in (
        "G1 preserves one private opaque unbounded `TypeDef` clause and exposes only one `Ordinary` term clause at public ordinal 0",
        "Type-independent primitive parameter/result/body edits preserve original `TypeDef` and bounds by identity",
    ):
        if marker.casefold() not in normalized_pipeline.casefold():
            findings.append(f"C076 accepted U068 G1 pipeline contract missing: {marker}")

    for marker in (
        "G2 preserves one private opaque unbounded `TypeDef(A)` clause and exposes only one `Ordinary` term clause at public ordinal 0",
        "dependent parameter/result `Ident(A)` semantics and replacement remain `UNSUPPORTED_STRUCTURE`",
        "only the exact capture-local `x` reference is admitted as a replacement body",
        "No public Type-binder carrier exists",
    ):
        if marker.casefold() not in normalized_pipeline.casefold():
            findings.append(f"C079 accepted U069 G2 pipeline contract missing: {marker}")

    public_text = "\n".join(
        (
            (root / "README.md").read_text(encoding="utf-8"),
            (root / "ROADMAP.md").read_text(encoding="utf-8"),
            cross_matrix,
            rewrite,
            pipeline,
            (root / "docs/ARCHITECTURE.md").read_text(encoding="utf-8"),
            (root / "docs/DOTTY_INTERNAL_BACKEND.md").read_text(encoding="utf-8")
            if (root / "docs/DOTTY_INTERNAL_BACKEND.md").exists()
            else "",
            (root / "docs/GETTING_STARTED.md").read_text(encoding="utf-8")
            if (root / "docs/GETTING_STARTED.md").exists()
            else "",
            (root / "docs/SYNTAX_SUPPORT_MATRIX.md").read_text(encoding="utf-8"),
            (root / "docs/SUPPORTED_SYNTAX_AND_LIMITATIONS.md").read_text(
                encoding="utf-8"
            ),
        )
    ).casefold()

    for stale in (
        "contextual existing-tree rewrite is `not_yet`",
        "no accepted contextual-method existing-tree rewrite",
    ):
        if stale in public_text:
            findings.append(f"C076 stale contextual availability claim: {stale}")

    for stale in (
        "all generic existing methods remain unsupported by existingclassuntypedrewrite",
        "generic existing-tree rewrite is `not_yet`",
    ):
        if stale in public_text:
            findings.append(f"C076 stale accepted U068 G1 rejection: {stale}")

    for stale in (
        "g2 locally type-dependent generic methods remain unsupported by existingclassuntypedrewrite",
        "g2 existing-tree rewrite is `not_yet`",
    ):
        if stale in public_text:
            findings.append(f"C079 stale accepted U069 G2 rejection: {stale}")

    for overclaim in (
        "all contextual method shapes are supported by existingclassuntypedrewrite",
        "all contextual second clauses are supported by existingclassuntypedrewrite",
        "scala 2 implicit-style clauses are supported by existingclassuntypedrewrite",
        "generic contextual methods are supported by existingclassuntypedrewrite",
        "defaulted contextual parameters are supported by existingclassuntypedrewrite",
        "erased contextual parameters are supported by existingclassuntypedrewrite",
        "by-name contextual parameters are supported by existingclassuntypedrewrite",
        "repeated contextual parameters are supported by existingclassuntypedrewrite",
    ):
        if overclaim in public_text:
            findings.append(f"C076 contextual adjacent-family overclaim: {overclaim}")

    for overclaim in (
        "g2 locally type-dependent generic methods are supported by existingclassuntypedrewrite",
        "g2 dependent parameter and result type replacement is supported by existingclassuntypedrewrite",
        "g2 arbitrary replacement bodies are supported by existingclassuntypedrewrite",
        "g2 option[string] method-type replacement is supported by existingclassuntypedrewrite",
        "existingclassuntypedrewrite exposes public type binders",
        "generic type substitution is supported by existingclassuntypedrewrite",
    ):
        if overclaim in public_text:
            findings.append(f"C079 G2 adjacent-family overclaim: {overclaim}")

    return findings


def c083_truth_findings(root: Path) -> list[str]:
    rewrite = (root / "docs/EXISTING_CLASS_UNTYPED_REWRITE.md").read_text(
        encoding="utf-8"
    )
    cross_matrix = (root / "docs/CROSS_SURFACE_CAPABILITY_MATRIX.md").read_text(
        encoding="utf-8"
    )
    findings = []

    rows = markdown_table_rows(cross_matrix)
    option_rows = rows.get(
        "Existing-class canonical resolved Option[String] method Type write", []
    )
    if len(option_rows) != 1:
        findings.append("C083 canonical Option cross-surface row missing")
    elif capability_status(option_rows[0][-1]) != "BOUNDED":
        findings.append("C083 canonical Option cross-surface row must be BOUNDED")

    normalized_rewrite = " ".join(rewrite.split())
    recipe_markers = (
        "`ResolvedTypeOwnerSegment(ResolvedTypeOwnerKind.Package, name)`",
        "`ResolvedTypeNameId`",
        "`TypeNormalForm.STypeResolved`",
        "`TypeNormalForm.STypeApply`",
    )
    if not all(marker in normalized_rewrite for marker in recipe_markers):
        findings.append("C083 canonical Option public construction recipe missing")

    for marker in (
        "one strict unary ordinary existing method",
        "`_root_.scala.Option[_root_.java.lang.String]`",
        "parameter-only, result-only, and combined capture-local body replacement",
        'Bare `STypeIdent("Option")` or `STypeIdent("String")`',
        "nested or other applied constructors",
        "G2-dependent Type replacement",
        "general `@addOption` expression wrapping",
        "`TypeSlotView.semantic` still returns `UNSUPPORTED_STRUCTURE`",
    ):
        if marker.casefold() not in normalized_rewrite.casefold():
            findings.append(f"C083 canonical Option contract missing: {marker}")

    public_paths = (
        "README.md",
        "ROADMAP.md",
        "docs/EXISTING_CLASS_UNTYPED_REWRITE.md",
        "docs/CROSS_SURFACE_CAPABILITY_MATRIX.md",
        "docs/SUPPORTED_SYNTAX_AND_LIMITATIONS.md",
        "docs/SYNTAX_SUPPORT_MATRIX.md",
        "docs/DOTTY_INTERNAL_BACKEND.md",
        "docs/PROJECTION_LOWERING_BRIDGE_MATRIX.md",
        "docs/GETTING_STARTED.md",
        "docs/ARCHITECTURE.md",
    )
    public_text = "\n".join(
        (root / relative).read_text(encoding="utf-8")
        for relative in public_paths
        if (root / relative).exists()
    ).casefold()

    for stale in (
        "all stypeapply existing method edits are unsupported",
        "option[string] method-type replacement remains unsupported",
        "nondependent applied method type such as option remains unsupported",
        "nondependent applied method type such as `option` remains unsupported",
    ):
        if stale in public_text:
            findings.append(f"C083 stale blanket applied method-Type rejection: {stale}")

    for overclaim in (
        "bare option and string aliases are canonical global identities for existing method edits",
        "nested option and other applied constructors are supported by existingclassuntypedrewrite",
        "g2 dependent option type replacement is supported by existingclassuntypedrewrite",
        "general @addoption expression wrapping is supported by existingclassuntypedrewrite",
    ):
        if overclaim in public_text:
            findings.append(
                f"C083 canonical Option adjacent-family overclaim: {overclaim}"
            )

    return findings


def c082_truth_findings(root: Path) -> list[str]:
    limitations = (
        root / "docs/SUPPORTED_SYNTAX_AND_LIMITATIONS.md"
    ).read_text(encoding="utf-8")
    syntax_matrix = (root / "docs/SYNTAX_SUPPORT_MATRIX.md").read_text(
        encoding="utf-8"
    )
    cross_matrix = (root / "docs/CROSS_SURFACE_CAPABILITY_MATRIX.md").read_text(
        encoding="utf-8"
    )
    normalized = " ".join(limitations.split())
    public_text = " ".join("\n".join((limitations, syntax_matrix, cross_matrix, (root / "README.md").read_text(encoding="utf-8"), (root / "docs/NORTH_STAR_QUASIQUOTE_EXAMPLES.md").read_text(encoding="utf-8"))).casefold().split())
    findings = []

    for marker in (
        'Standard `qq` also admits `qq"$receiver.$first.$second"` and `qq"$receiver.$first.$second($argument)"`.',
        "Different Scala source binder names produce two independent `SelectedMemberName` fields in source order",
        "target member names may be equal",
        "`if first == second` guard runs only after successful extraction",
        "Opt-in typed-Scalameta `qq` admits the same exact two-name direct/unary contract",
        "Native repeated same-binder patterns fail with E005",
        "neither `#repeat` nor `qq2` is public syntax",
    ):
        if marker not in normalized:
            findings.append(f"C082 standard two-selected-name contract missing: {marker}")

    source_boundary = (
        "Exact two-name and direct N=3..8 candidates require recoverable original source; "
        "legacy scalar, one-name, ranked, mixed, and local-method candidates bypass "
        "source-position inspection. A new-family candidate without source fails closed."
    )
    if source_boundary not in normalized:
        findings.append("C082 two-name source-position contract missing")

    if "multiple selected names remain unsupported on standard `qq`" in public_text:
        findings.append("C082 stale standard two-selected-name rejection")

    if (
        "typed-scalameta two-name formal acceptance is pending" in public_text
        or "typed-scalameta two-name parity is not yet part of the formally accepted support contract"
        in public_text
    ):
        findings.append("C082 stale typed-Scalameta pending claim")

    for overclaim in (
        'native `qq"$receiver.$same.$same"` is supported',
        "the `#repeat` marker is public syntax",
        "use the public `qq2` interpolator for two names",
    ):
        if overclaim in public_text:
            findings.append("C082 repeated-name or arity-family overclaim")

    if "two distinct direct/unary" not in syntax_matrix:
        findings.append("C082 syntax matrix missing standard two-name boundary")
    if "accepted two distinct direct/unary parity" not in cross_matrix:
        findings.append("C082 cross-surface matrix missing hybrid acceptance boundary")

    if "Both typed frontends support direct flat selected-member chains at depths N=1..8." not in normalized:
        findings.append("direct selected-chain boundary missing both-frontends N1..8 contract")
    for stale in (
        "more than two selected names remain unsupported",
        "at most two selected names are supported",
        "more than two selected names,",
        "more than two names,",
        "hybrid direct n3..8 is not yet supported",
    ):
        if stale in public_text:
            findings.append("direct selected-chain boundary contains stale depth limit")
    return findings


def check(root: Path) -> list[str]:
    rows = api_rows(root)
    return sorted(
        relative_link_findings(root)
        + table_findings(root)
        + api_findings(rows)
        + api_count_findings(root, rows)
        + source_findings(root)
        + cross_surface_rank_findings(root)
        + matrix_findings(root)
        + durable_documentation_findings(root)
        + c065_truth_findings(root)
        + c070_truth_findings(root)
        + c073_truth_findings(root)
        + c074_truth_findings(root)
        + c076_truth_findings(root)
        + c082_truth_findings(root)
        + c083_truth_findings(root)
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
