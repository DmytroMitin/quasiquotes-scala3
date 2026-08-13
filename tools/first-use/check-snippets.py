#!/usr/bin/env python3
"""Verify that documented first-use snippets match compiled fixtures exactly."""

import argparse
from pathlib import Path
import sys


SNIPPETS = {
    "core-first-use": Path(
        "public-core-examples/src/test/scala/external/consumer/CoreFirstUseSnippet.scala"
    ),
    "definition-first-use": Path(
        "public-core-examples/src/test/scala/external/consumer/DefinitionFirstUseSnippet.scala"
    ),
    "frontend-first-use": Path(
        "public-api-examples/src/test/scala/external/consumer/FrontendFirstUseSnippet.scala"
    ),
    "lambda1-first-use": Path(
        "public-api-examples/src/test/scala/external/consumer/Lambda1FirstUseSnippet.scala"
    ),
}


def between(text: str, start: str, end: str, path: Path) -> str:
    if text.count(start) != 1 or text.count(end) != 1:
        raise ValueError(f"{path}: expected exactly one {start!r} and {end!r}")
    return text.split(start, 1)[1].split(end, 1)[0].strip("\n")


def source_snippet(name: str, path: Path) -> str:
    return between(
        path.read_text(encoding="utf-8"),
        f"// snippet:{name}:start\n",
        f"// snippet:{name}:end",
        path,
    )


def documented_snippet(name: str, documentation: str, doc: Path) -> str:
    block = between(
        documentation,
        f"<!-- snippet:{name}:start -->\n",
        f"<!-- snippet:{name}:end -->",
        doc,
    )
    if not block.startswith("```scala\n") or not block.endswith("\n```"):
        raise ValueError(f"{doc}: snippet {name!r} must be one Scala fence")
    return block[len("```scala\n") : -len("\n```")]


def check(root: Path) -> list[str]:
    doc = root / "docs/GETTING_STARTED.md"
    documentation = doc.read_text(encoding="utf-8")
    return [
        name
        for name, relative_source in SNIPPETS.items()
        if source_snippet(name, root / relative_source)
        != documented_snippet(name, documentation, doc)
    ]


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--root",
        type=Path,
        default=Path(__file__).resolve().parents[2],
        help="implementation repository root",
    )
    args = parser.parse_args()

    try:
        mismatches = check(args.root.resolve())
    except (OSError, ValueError) as error:
        print(error, file=sys.stderr)
        return 2

    if mismatches:
        print("First-use snippet drift: " + ", ".join(mismatches), file=sys.stderr)
        return 1
    print("First-use snippets aligned: " + ", ".join(SNIPPETS))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
