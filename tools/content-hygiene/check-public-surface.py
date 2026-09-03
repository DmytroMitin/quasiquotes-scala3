#!/usr/bin/env python3
"""Fail closed on private chronology and unsafe public-workflow contracts."""

from __future__ import annotations

import re
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path


TEXT_SUFFIXES = {".md", ".txt", ".yml", ".yaml", ".scala"}
PUBLIC_NARRATIVE_SUFFIXES = {".md", ".txt"}
CHRONOLOGY_PATTERNS = (
    re.compile(r"\bPrompt(?:\s+|-)\d+(?:\.\d+)?\b", re.IGNORECASE),
    re.compile(r"\bPhase(?:\s+|-)\d+[A-Za-z0-9]*\b", re.IGNORECASE),
    re.compile(
        r"\b(?:con" r"troller|control-repository)\s+(?:chronology|hand" r"off|schedule)",
        re.IGNORECASE,
    ),
)
PRIVATE_NARRATIVE_PATTERNS = (
    (
        "PRIVATE_CONTROLLER_TASK_ID",
        re.compile(r"\b[QNUC]\d{3}[A-Z]*\b", re.IGNORECASE),
    ),
    (
        "PRIVATE_LANE_MESSAGE_ID",
        re.compile(r"\b[QNUC]-[QNUC]-\d{4}\b", re.IGNORECASE),
    ),
    (
        "PRIVATE_PEER_MAILBOX_ID",
        re.compile(r"\b(?:inputs?|requests?)\s+\d{3}\b", re.IGNORECASE),
    ),
    (
        "PRIVATE_PEER_REQUEST_ID",
        re.compile(
            r"\b(?:AUXify|Macro-Paradise|Quasiquotes)-\d{3}\b",
            re.IGNORECASE,
        ),
    ),
    (
        "PRIVATE_CONTROL_REFERENCE",
        re.compile(
            r"\bquasiquotes-scala3-control\b|"
            r"\bcoordination/(?:state|messages)(?:/|\b)|"
            r"(?:^|[\s`])(?:prompts|reviews)/",
            re.IGNORECASE,
        ),
    ),
)
PRIVATE_IDENTIFIER_FILENAME_PATTERN = re.compile(
    r"(?:^|[/_.-])(?:"
    r"[QNUC]\d{3}[A-Z]*|"
    r"AUXIFY[-_]?\d{3}|"
    r"(?:PROMPT|PHASE)[-_]?\d+|"
    r"(?:INPUT|REQUEST)[-_]?\d{3}"
    r")(?=[/_.-]|$)",
    re.IGNORECASE,
)
STALE_SECURITY_PATTERN = re.compile(
    r"private\s+security(?:-reporting)?\s+channel.{0,300}?"
    r"(?:required\s+(?:gate|prerequisite)|pre-visibility|public-visibility\s+gate)",
    re.IGNORECASE | re.DOTALL,
)
ACTION_PATTERN = re.compile(r"^\s*(?:-\s*)?uses:\s*([^\s#]+)@([^\s#]+)")
IMMUTABLE_SHA_PATTERN = re.compile(r"^[0-9a-f]{40}$")
WRITE_PERMISSION_PATTERN = re.compile(r"^\s*[A-Za-z0-9_-]+:\s*write\s*(?:#.*)?$")
SECRET_REFERENCE_PATTERN = re.compile(r"\$\{\{\s*secrets\.", re.IGNORECASE)
PUBLICATION_COMMAND_PATTERN = re.compile(
    r"(?:\bsbt\b[^\n]*\bpublish(?:Signed)?\b|\bgh\s+release\b|"
    r"\bmvn\s+deploy\b|\bnpm\s+publish\b)",
    re.IGNORECASE,
)
READ_ONLY_PERMISSION_PATTERN = re.compile(
    r"(?m)^permissions:\s*(?:\n[ \t]+[^\n]*)*\n[ \t]+contents:\s*read\s*(?:#.*)?$"
)


@dataclass(frozen=True, order=True)
class Finding:
    path: str
    line: int
    code: str

    def render(self) -> str:
        return f"{self.code}\t{self.path}:{self.line}"


def tracked_text_files(root: Path) -> list[Path]:
    completed = subprocess.run(
        ["git", "-C", str(root), "ls-files", "-co", "--exclude-standard"],
        text=True,
        capture_output=True,
        check=False,
    )
    if completed.returncode == 0:
        candidates = [root / value for value in completed.stdout.splitlines()]
    else:
        candidates = list(root.rglob("*"))
    return sorted(
        path
        for path in candidates
        if path.is_file()
        and path.suffix.lower() in TEXT_SUFFIXES
        and not any(part in {".git", "target"} for part in path.parts)
    )


def audit(root: Path) -> list[Finding]:
    findings: list[Finding] = []
    for path in tracked_text_files(root):
        relative = path.relative_to(root).as_posix()
        text = path.read_text(errors="replace")
        if (
            path.suffix.lower() in PUBLIC_NARRATIVE_SUFFIXES
            and PRIVATE_IDENTIFIER_FILENAME_PATTERN.search(relative)
        ):
            findings.append(Finding(relative, 1, "PRIVATE_IDENTIFIER_FILENAME"))
        if relative.startswith(".github/workflows/") and not READ_ONLY_PERMISSION_PATTERN.search(text):
            findings.append(
                Finding(relative, 1, "WORKFLOW_READ_ONLY_PERMISSION_MISSING")
            )
        for match in STALE_SECURITY_PATTERN.finditer(text):
            line = text.count("\n", 0, match.start()) + 1
            findings.append(
                Finding(relative, line, "STALE_PRIVATE_CHANNEL_PREREQUISITE")
            )
        for number, line in enumerate(text.splitlines(), start=1):
            if any(pattern.search(line) for pattern in CHRONOLOGY_PATTERNS):
                findings.append(Finding(relative, number, "PRIVATE_DELIVERY_CHRONOLOGY"))
            if path.suffix.lower() in PUBLIC_NARRATIVE_SUFFIXES:
                for code, pattern in PRIVATE_NARRATIVE_PATTERNS:
                    if pattern.search(line):
                        findings.append(Finding(relative, number, code))
            if relative.startswith(".github/"):
                action = ACTION_PATTERN.search(line)
                if action and not IMMUTABLE_SHA_PATTERN.fullmatch(action.group(2)):
                    findings.append(
                        Finding(relative, number, "MUTABLE_THIRD_PARTY_ACTION")
                    )
                if WRITE_PERMISSION_PATTERN.search(line):
                    findings.append(Finding(relative, number, "WORKFLOW_WRITE_PERMISSION"))
                if SECRET_REFERENCE_PATTERN.search(line):
                    findings.append(Finding(relative, number, "WORKFLOW_SECRET_REFERENCE"))
                if PUBLICATION_COMMAND_PATTERN.search(line):
                    findings.append(Finding(relative, number, "WORKFLOW_PUBLICATION_COMMAND"))
    return sorted(set(findings))


def main() -> int:
    if len(sys.argv) != 2:
        print("usage: check-public-surface.py REPOSITORY", file=sys.stderr)
        return 64
    root = Path(sys.argv[1]).resolve()
    if not root.is_dir():
        print(f"repository is not a directory: {root}", file=sys.stderr)
        return 64
    findings = audit(root)
    if findings:
        for finding in findings:
            print(finding.render(), file=sys.stderr)
        return 1
    print("PUBLIC_CONTENT_AND_WORKFLOW_HYGIENE_PASS")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
