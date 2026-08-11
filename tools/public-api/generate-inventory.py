#!/usr/bin/env python3
"""Generate the phase-neutral deterministic core/frontend public API inventory."""

from __future__ import annotations

import json
import sys
import zipfile
from pathlib import Path


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


def load_pages(path: Path) -> list[dict[str, object]]:
    with zipfile.ZipFile(path) as archive:
        source = archive.read("scripts/searchData.js").decode("utf-8")
    prefix = "pages = "
    if not source.startswith(prefix):
        raise ValueError(f"unexpected Scaladoc search-data prefix in {path}")
    payload = source[len(prefix) :].strip()
    if payload.endswith(";"):
        payload = payload[:-1]
    return json.loads(payload)


def generate(core_jar: Path, frontend_jar: Path) -> list[tuple[str, ...]]:
    rows: set[tuple[str, ...]] = set()
    for module, jar in (("core", core_jar), ("frontend", frontend_jar)):
        for page in load_pages(jar):
            if page.get("e") is True:
                continue
            kind = str(page.get("k", ""))
            if kind not in ALLOWED_KINDS:
                continue
            rows.add(
                (
                    module,
                    str(page.get("d", "")),
                    kind,
                    str(page.get("n", "")),
                    str(page.get("t", "")).replace("\t", " ").replace("\n", " "),
                )
            )
    return sorted(rows)


def main() -> int:
    if len(sys.argv) != 4:
        print(
            "usage: generate-inventory.py CORE_DOCS_JAR FRONTEND_DOCS_JAR OUTPUT_TSV",
            file=sys.stderr,
        )
        return 64

    rows = generate(Path(sys.argv[1]), Path(sys.argv[2]))
    output = Path(sys.argv[3])
    output.parent.mkdir(parents=True, exist_ok=True)
    with output.open("w", encoding="utf-8", newline="\n") as stream:
        stream.write("module\towner\tkind\tname\tsignature\n")
        for row in rows:
            stream.write("\t".join(row) + "\n")
    print(f"wrote {len(rows)} public API rows to {output}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
