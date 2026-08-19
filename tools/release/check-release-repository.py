#!/usr/bin/env python3
from __future__ import annotations

import argparse
import hashlib
import json
import subprocess
import sys
import xml.etree.ElementTree as ET
import zipfile
from pathlib import Path
from typing import Callable


GROUP = "com.github.dmytromitin"
GROUP_PATH = Path("com/github/dmytromitin")
LEGACY_GROUP_PATH = Path("io/github/dmytromitin")
VERSION = "0.2.0"
COORDINATES = {
    "quasiquotes-scala3-core_3": "3.3.8",
    "quasiquotes-scala3-frontend_3.3.8": "3.3.8",
    "quasiquotes-scala3-frontend_3.8.4": "3.8.4",
}
CLASSIFIERS = ("", "-sources", "-javadoc")
LICENSE_NAME = "Apache-2.0"
LICENSE_URL = "https://www.apache.org/licenses/LICENSE-2.0"
PROJECT_URL = "https://github.com/DmytroMitin/quasiquotes-scala3"
SCM_CONNECTION = "scm:git:git@github.com:DmytroMitin/quasiquotes-scala3.git"
PASS = "PHASE103N_LOCAL_SIGNATURE_AND_CHECKSUM_MANIFEST_PASS"


def digest(path: Path, algorithm: str) -> str:
    value = hashlib.new(algorithm)
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            value.update(chunk)
    return value.hexdigest()


def children(element: ET.Element, name: str) -> list[ET.Element]:
    return [child for child in element if child.tag.rsplit("}", 1)[-1] == name]


def one(element: ET.Element, name: str) -> ET.Element | None:
    values = children(element, name)
    return values[0] if len(values) == 1 else None


def text(element: ET.Element, name: str) -> str | None:
    value = one(element, name)
    return (value.text or "").strip() if value is not None else None


def verify_gpg(path: Path, signature: Path, fingerprint: str) -> bool:
    result = subprocess.run(
        ["gpg", "--batch", "--status-fd=1", "--verify", str(signature), str(path)],
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        check=False,
    )
    normalized = fingerprint.upper()
    return result.returncode == 0 and f"[GNUPG:] VALIDSIG {normalized} " in result.stdout


def pom_summary(path: Path, artifact: str, scala_line: str, errors: list[str]) -> list[dict[str, str]]:
    try:
        root = ET.parse(path).getroot()
    except (OSError, ET.ParseError) as error:
        errors.append(f"POM_INVALID:{artifact}:{error}")
        return []
    expected = {"groupId": GROUP, "artifactId": artifact, "version": VERSION}
    for field, wanted in expected.items():
        if text(root, field) != wanted:
            errors.append(f"POM_IDENTITY_INVALID:{artifact}:{field}")
    for field in ("name", "description", "url"):
        if not text(root, field):
            errors.append(f"POM_METADATA_MISSING:{artifact}:{field}")
    if text(root, "url") != PROJECT_URL:
        errors.append(f"POM_PROJECT_URL_INVALID:{artifact}")
    license_root = one(root, "licenses")
    licenses = children(license_root, "license") if license_root is not None else []
    if not (
        len(licenses) == 1
        and text(licenses[0], "name") == LICENSE_NAME
        and text(licenses[0], "url") == LICENSE_URL
        and text(licenses[0], "distribution") == "repo"
    ):
        errors.append(f"POM_LICENSE_INVALID:{artifact}")
    scm = one(root, "scm")
    if scm is None or text(scm, "url") != PROJECT_URL or text(scm, "connection") != SCM_CONNECTION:
        errors.append(f"POM_SCM_INVALID:{artifact}")
    developers_root = one(root, "developers")
    developers = children(developers_root, "developer") if developers_root is not None else []
    if len(developers) != 1 or any(not text(developers[0], field) for field in ("id", "name", "email", "url")):
        errors.append(f"POM_DEVELOPER_INVALID:{artifact}")
    if children(root, "repositories") or children(root, "distributionManagement"):
        errors.append(f"POM_FORBIDDEN_REPOSITORY_METADATA:{artifact}")
    rendered = path.read_text(encoding="utf-8", errors="replace")
    if any(token in rendered for token in ("0.2.0-SNAPSHOT", "/home/", "/tmp/")):
        errors.append(f"POM_PRIVATE_OR_PATH_LEAK:{artifact}")

    dependencies: list[dict[str, str]] = []
    dependencies_root = one(root, "dependencies")
    for dependency in children(dependencies_root, "dependency") if dependencies_root is not None else []:
        item = {
            "group": text(dependency, "groupId") or "",
            "artifact": text(dependency, "artifactId") or "",
            "version": text(dependency, "version") or "",
            "scope": text(dependency, "scope") or "compile",
        }
        dependencies.append(item)
    compile_dependencies = {(d["group"], d["artifact"], d["version"]) for d in dependencies if d["scope"] != "test"}
    scala_library = ("org.scala-lang", "scala3-library_3", scala_line)
    if scala_library not in compile_dependencies:
        errors.append(f"POM_SCALA_LIBRARY_INVALID:{artifact}:{scala_line}")
    if artifact.startswith("quasiquotes-scala3-frontend_"):
        required = {
            ("org.scala-lang", "scala3-compiler_3", scala_line),
            (GROUP, "quasiquotes-scala3-core_3", VERSION),
        }
        for dependency in sorted(required - compile_dependencies):
            errors.append(f"POM_FRONTEND_DEPENDENCY_INVALID:{artifact}:{':'.join(dependency)}")
    elif any(d[1] == "scala3-compiler_3" for d in compile_dependencies):
        errors.append(f"POM_CORE_COMPILER_LEAK:{artifact}")
    return sorted(dependencies, key=lambda d: (d["scope"], d["group"], d["artifact"]))


def check(
    project: Path,
    repository: Path,
    fingerprint: str,
    source_identity: str,
    sbt_version: str,
    verifier: Callable[[Path, Path, str], bool] = verify_gpg,
) -> tuple[dict[str, object], list[str]]:
    errors: list[str] = []
    group_root = repository / GROUP_PATH
    legacy_group_root = repository / LEGACY_GROUP_PATH
    if legacy_group_root.is_dir():
        for artifact in sorted(path.name for path in legacy_group_root.iterdir() if path.is_dir()):
            errors.append(f"LEGACY_NAMESPACE_PRESENT:{artifact}")
    actual = {path.name for path in group_root.iterdir() if path.is_dir()} if group_root.is_dir() else set()
    expected = set(COORDINATES)
    for artifact in sorted(expected - actual):
        errors.append(f"COORDINATE_MISSING:{artifact}")
    for artifact in sorted(actual - expected):
        errors.append(f"COORDINATE_UNEXPECTED:{artifact}")

    license_bytes = (project / "LICENSE").read_bytes()
    coordinates: list[dict[str, object]] = []
    for artifact, scala_line in COORDINATES.items():
        directory = group_root / artifact / VERSION
        base = f"{artifact}-{VERSION}"
        deployables = [directory / f"{base}.pom"] + [directory / f"{base}{classifier}.jar" for classifier in CLASSIFIERS]
        allowed: set[str] = set()
        files: list[dict[str, object]] = []
        for path in deployables:
            allowed.add(path.name)
            signature = path.with_name(path.name + ".asc")
            for suffix in (".md5", ".sha1", ".sha256", ".sha512"):
                allowed.add(path.name + suffix)
                allowed.add(signature.name + suffix)
            allowed.add(signature.name)
            if not path.is_file():
                errors.append(f"DEPLOYABLE_MISSING:{artifact}:{path.name}")
                continue
            for algorithm in ("md5", "sha1"):
                checksum = path.with_name(path.name + f".{algorithm}")
                if not checksum.is_file() or checksum.read_text().strip().lower() != digest(path, algorithm):
                    errors.append(f"CHECKSUM_INVALID:{artifact}:{path.name}:{algorithm}")
            verified = signature.is_file() and verifier(path, signature, fingerprint)
            if not verified:
                errors.append(f"SIGNATURE_INVALID:{artifact}:{path.name}")
            files.append({
                "filename": path.name,
                "size": path.stat().st_size,
                "sha256": digest(path, "sha256"),
                "sha512": digest(path, "sha512"),
                "signature": signature.name,
                "signature_verified": verified,
            })
        if directory.is_dir():
            for extra in sorted(path.name for path in directory.iterdir() if path.is_file() and path.name not in allowed):
                errors.append(f"FILE_UNEXPECTED:{artifact}:{extra}")
        pom = directory / f"{base}.pom"
        dependencies = pom_summary(pom, artifact, scala_line, errors) if pom.is_file() else []
        for jar in [directory / f"{base}{classifier}.jar" for classifier in CLASSIFIERS]:
            if jar.is_file():
                try:
                    with zipfile.ZipFile(jar) as archive:
                        if archive.read("META-INF/LICENSE") != license_bytes:
                            errors.append(f"JAR_LICENSE_INVALID:{artifact}:{jar.name}")
                except (KeyError, OSError, zipfile.BadZipFile):
                    errors.append(f"JAR_LICENSE_INVALID:{artifact}:{jar.name}")
        coordinates.append({
            "coordinate": f"{GROUP}:{artifact}:{VERSION}",
            "scala_compiler_line": scala_line,
            "files": files,
            "pom_dependencies": dependencies,
            "license": LICENSE_NAME,
        })
    manifest: dict[str, object] = {
        "schema": "quasiquotes-phase103n-release-manifest-v1",
        "source_identity": source_identity,
        "candidate_version": VERSION,
        "sbt_version": sbt_version,
        "synthetic_rehearsal_fingerprint": fingerprint.upper(),
        "coordinates": coordinates,
        "assertions": {
            "exact_coordinate_set": not any(e.startswith("COORDINATE_") for e in errors),
            "frontend_3.9.0-RC1_absent": "quasiquotes-scala3-frontend_3.9.0-RC1" not in actual,
            "root_internal_examples_absent": not any(
                token in name for name in actual for token in ("dotty-internal", "public-api-examples", "public-core-examples")
            ),
            "all_signatures_verified": not any(e.startswith("SIGNATURE_") for e in errors),
            "all_checksums_verified": not any(e.startswith("CHECKSUM_") for e in errors),
        },
    }
    return manifest, sorted(errors)


def markdown(manifest: dict[str, object]) -> str:
    lines = ["# Local signed release-candidate manifest", "", f"Source: `{manifest['source_identity']}`", f"Version: `{manifest['candidate_version']}`", f"sbt: `{manifest['sbt_version']}`", f"Synthetic fingerprint: `{manifest['synthetic_rehearsal_fingerprint']}`", ""]
    for coordinate in manifest["coordinates"]:  # type: ignore[index]
        lines += [f"## {coordinate['coordinate']}", "", f"Scala line: `{coordinate['scala_compiler_line']}`", f"License: `{coordinate['license']}`", "", "| File | Size | SHA-256 | SHA-512 | Signature |", "|---|---:|---|---|---|"]
        for item in coordinate["files"]:
            lines.append(f"| `{item['filename']}` | {item['size']} | `{item['sha256']}` | `{item['sha512']}` | {'PASS' if item['signature_verified'] else 'FAIL'} |")
        lines += ["", "POM dependencies:", ""]
        for dependency in coordinate["pom_dependencies"]:
            lines.append(f"- `{dependency['group']}:{dependency['artifact']}:{dependency['version']}` ({dependency['scope']})")
        lines.append("")
    return "\n".join(lines)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("project", type=Path)
    parser.add_argument("repository", type=Path)
    parser.add_argument("--fingerprint", required=True)
    parser.add_argument("--source-identity", required=True)
    parser.add_argument("--sbt-version", default="1.12.15")
    parser.add_argument("--json", type=Path, required=True)
    parser.add_argument("--markdown", type=Path, required=True)
    args = parser.parse_args()
    manifest, errors = check(args.project.resolve(), args.repository.resolve(), args.fingerprint, args.source_identity, args.sbt_version)
    if errors:
        for error in errors:
            print(error, file=sys.stderr)
        print("PHASE103N_RELEASE_MANIFEST_BLOCKED", file=sys.stderr)
        return 3
    args.json.write_text(json.dumps(manifest, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    args.markdown.write_text(markdown(manifest), encoding="utf-8")
    print(PASS)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
