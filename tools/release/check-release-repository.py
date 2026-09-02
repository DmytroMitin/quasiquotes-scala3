#!/usr/bin/env python3
from __future__ import annotations

import argparse
import hashlib
import json
import subprocess
import sys
import xml.etree.ElementTree as ET
import zipfile
from dataclasses import dataclass
from pathlib import Path
from typing import Callable


GROUP = "com.github.dmytromitin"
GROUP_PATH = Path("com/github/dmytromitin")
LEGACY_GROUP_PATH = Path("io/github/dmytromitin")
CLASSIFIERS = ("", "-sources", "-javadoc")
LICENSE_NAME = "Apache-2.0"
LICENSE_URL = "https://www.apache.org/licenses/LICENSE-2.0"
PROJECT_URL = "https://github.com/DmytroMitin/quasiquotes-scala3"
SCM_CONNECTION = "scm:git:git@github.com:DmytroMitin/quasiquotes-scala3.git"


@dataclass(frozen=True)
class CoordinateSpec:
    artifact: str
    scala_line: str
    role: str


@dataclass(frozen=True)
class ReleaseProfile:
    name: str
    version: str
    coordinates: tuple[CoordinateSpec, ...]
    pass_marker: str


RELEASE_PROFILES = {
    "0.2.0": ReleaseProfile(
        name="0.2.0",
        version="0.2.0",
        coordinates=(
            CoordinateSpec("quasiquotes-scala3-core_3", "3.3.8", "core"),
            CoordinateSpec("quasiquotes-scala3-frontend_3.3.8", "3.3.8", "frontend"),
            CoordinateSpec("quasiquotes-scala3-frontend_3.8.4", "3.8.4", "frontend"),
        ),
        pass_marker="QUASIQUOTES_RELEASE_REPOSITORY_0_2_0_PASS",
    ),
    "0.3.0-expanded": ReleaseProfile(
        name="0.3.0-expanded",
        version="0.3.0",
        coordinates=(
            CoordinateSpec("quasiquotes-scala3-core_3", "3.3.8", "core"),
            CoordinateSpec("quasiquotes-scala3-neutral-scalameta_3", "3.3.8", "neutral"),
            CoordinateSpec("quasiquotes-scala3-frontend_3.3.8", "3.3.8", "frontend"),
            CoordinateSpec("quasiquotes-scala3-scalameta-frontend_3.3.8", "3.3.8", "scalameta-frontend"),
            CoordinateSpec("quasiquotes-scala3-dotty-internal_3.3.8", "3.3.8", "dotty-internal"),
            CoordinateSpec("quasiquotes-scala3-frontend_3.8.4", "3.8.4", "frontend"),
            CoordinateSpec("quasiquotes-scala3-scalameta-frontend_3.8.4", "3.8.4", "scalameta-frontend"),
            CoordinateSpec("quasiquotes-scala3-dotty-internal_3.8.4", "3.8.4", "dotty-internal"),
            CoordinateSpec("quasiquotes-scala3-frontend_3.9.0", "3.9.0", "frontend"),
            CoordinateSpec("quasiquotes-scala3-scalameta-frontend_3.9.0", "3.9.0", "scalameta-frontend"),
            CoordinateSpec("quasiquotes-scala3-dotty-internal_3.9.0", "3.9.0", "dotty-internal"),
        ),
        pass_marker="QUASIQUOTES_RELEASE_REPOSITORY_0_3_0_EXPANDED_PASS",
    ),
}


def expected_compile_dependencies(
    coordinate: CoordinateSpec, profile: ReleaseProfile
) -> set[tuple[str, str, str]]:
    line = coordinate.scala_line
    version = profile.version
    dependencies = {("org.scala-lang", "scala3-library_3", line)}
    if coordinate.role == "neutral":
        dependencies |= {
            (GROUP, "quasiquotes-scala3-core_3", version),
            ("org.scalameta", "scalameta_3", "4.17.3"),
        }
    elif coordinate.role == "frontend":
        dependencies |= {
            (GROUP, "quasiquotes-scala3-core_3", version),
            ("org.scala-lang", "scala3-compiler_3", line),
        }
    elif coordinate.role == "scalameta-frontend":
        dependencies |= {
            (GROUP, f"quasiquotes-scala3-frontend_{line}", version),
            (GROUP, "quasiquotes-scala3-neutral-scalameta_3", version),
        }
    elif coordinate.role == "dotty-internal":
        dependencies |= {
            (GROUP, "quasiquotes-scala3-neutral-scalameta_3", version),
            ("org.scala-lang", "scala3-compiler_3", line),
        }
    elif coordinate.role != "core":
        raise ValueError(f"unknown coordinate role: {coordinate.role}")
    return dependencies


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


def pom_summary(
    path: Path,
    coordinate: CoordinateSpec,
    profile: ReleaseProfile,
    errors: list[str],
) -> list[dict[str, str]]:
    artifact = coordinate.artifact
    try:
        root = ET.parse(path).getroot()
    except (OSError, ET.ParseError) as error:
        errors.append(f"POM_INVALID:{artifact}:{error}")
        return []
    expected = {"groupId": GROUP, "artifactId": artifact, "version": profile.version}
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
    if any(token in rendered for token in ("-SNAPSHOT", "/home/", "/tmp/", "ProjectRef", "target/scala-")):
        errors.append(f"POM_PRIVATE_OR_PATH_LEAK:{artifact}")

    dependencies: list[dict[str, str]] = []
    dependencies_root = one(root, "dependencies")
    for dependency in children(dependencies_root, "dependency") if dependencies_root is not None else []:
        dependencies.append(
            {
                "group": text(dependency, "groupId") or "",
                "artifact": text(dependency, "artifactId") or "",
                "version": text(dependency, "version") or "",
                "scope": text(dependency, "scope") or "compile",
            }
        )
    actual_compile = {
        (dependency["group"], dependency["artifact"], dependency["version"])
        for dependency in dependencies
        if dependency["scope"] != "test"
    }
    expected_compile = expected_compile_dependencies(coordinate, profile)
    if actual_compile != expected_compile:
        missing = sorted(expected_compile - actual_compile)
        unexpected = sorted(actual_compile - expected_compile)
        errors.append(
            f"POM_DEPENDENCY_CONTRACT_INVALID:{artifact}:missing={missing}:unexpected={unexpected}"
        )
    return sorted(dependencies, key=lambda d: (d["scope"], d["group"], d["artifact"]))


def check_checksum(path: Path, algorithm: str, artifact: str, errors: list[str]) -> None:
    checksum = path.with_name(path.name + f".{algorithm}")
    if not checksum.is_file() or checksum.read_text().strip().lower() != digest(path, algorithm):
        errors.append(f"CHECKSUM_INVALID:{artifact}:{path.name}:{algorithm}")


def check(
    project: Path,
    repository: Path,
    profile: ReleaseProfile,
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
    expected = {coordinate.artifact for coordinate in profile.coordinates}
    for artifact in sorted(expected - actual):
        errors.append(f"COORDINATE_MISSING:{artifact}")
    for artifact in sorted(actual - expected):
        errors.append(f"COORDINATE_UNEXPECTED:{artifact}")

    license_bytes = (project / "LICENSE").read_bytes()
    coordinates: list[dict[str, object]] = []
    for coordinate in profile.coordinates:
        artifact = coordinate.artifact
        artifact_root = group_root / artifact
        if artifact_root.is_dir():
            versions = {path.name for path in artifact_root.iterdir() if path.is_dir()}
            for version in sorted(versions - {profile.version}):
                errors.append(f"VERSION_UNEXPECTED:{artifact}:{version}")
        directory = artifact_root / profile.version
        base = f"{artifact}-{profile.version}"
        deployables = [directory / f"{base}.pom"] + [
            directory / f"{base}{classifier}.jar" for classifier in CLASSIFIERS
        ]
        allowed: set[str] = set()
        files: list[dict[str, object]] = []
        for path in deployables:
            signature = path.with_name(path.name + ".asc")
            allowed.update((path.name, signature.name))
            for suffix in (".md5", ".sha1"):
                allowed.add(path.name + suffix)
                allowed.add(signature.name + suffix)
            if not path.is_file():
                errors.append(f"DEPLOYABLE_MISSING:{artifact}:{path.name}")
                continue
            for algorithm in ("md5", "sha1"):
                check_checksum(path, algorithm, artifact, errors)
            verified = signature.is_file() and verifier(path, signature, fingerprint)
            if not verified:
                errors.append(f"SIGNATURE_INVALID:{artifact}:{path.name}")
            if signature.is_file():
                for algorithm in ("md5", "sha1"):
                    check_checksum(signature, algorithm, artifact, errors)
            files.append(
                {
                    "filename": path.name,
                    "size": path.stat().st_size,
                    "sha256": digest(path, "sha256"),
                    "sha512": digest(path, "sha512"),
                    "signature": signature.name,
                    "signature_verified": verified,
                }
            )
        if directory.is_dir():
            for extra in sorted(path.name for path in directory.iterdir() if path.is_file() and path.name not in allowed):
                errors.append(f"FILE_UNEXPECTED:{artifact}:{extra}")
        pom = directory / f"{base}.pom"
        dependencies = pom_summary(pom, coordinate, profile, errors) if pom.is_file() else []
        for jar in [directory / f"{base}{classifier}.jar" for classifier in CLASSIFIERS]:
            if jar.is_file():
                try:
                    with zipfile.ZipFile(jar) as archive:
                        if archive.read("META-INF/LICENSE") != license_bytes:
                            errors.append(f"JAR_LICENSE_INVALID:{artifact}:{jar.name}")
                except (KeyError, OSError, zipfile.BadZipFile):
                    errors.append(f"JAR_LICENSE_INVALID:{artifact}:{jar.name}")
        coordinates.append(
            {
                "coordinate": f"{GROUP}:{artifact}:{profile.version}",
                "role": coordinate.role,
                "scala_compiler_line": coordinate.scala_line,
                "files": files,
                "pom_dependencies": dependencies,
                "license": LICENSE_NAME,
            }
        )
    manifest: dict[str, object] = {
        "schema": "quasiquotes-release-repository-manifest-v2",
        "release_set": profile.name,
        "source_identity": source_identity,
        "candidate_version": profile.version,
        "sbt_version": sbt_version,
        "synthetic_rehearsal_fingerprint": fingerprint.upper(),
        "coordinates": coordinates,
        "assertions": {
            "exact_coordinate_set": not any(e.startswith("COORDINATE_") for e in errors),
            "scala_3.9.0-RC1_coordinates_absent": not any("3.9.0-RC1" in name for name in actual),
            "root_and_examples_absent": not any(
                token in name for name in actual for token in ("public-api-examples", "public-core-examples")
            ),
            "all_signatures_verified": not any(e.startswith("SIGNATURE_") for e in errors),
            "all_checksums_verified": not any(e.startswith("CHECKSUM_") for e in errors),
            "pom_dependency_contracts_verified": not any(
                e.startswith("POM_DEPENDENCY_CONTRACT_") for e in errors
            ),
        },
    }
    return manifest, sorted(errors)


def markdown(manifest: dict[str, object]) -> str:
    lines = [
        "# Local signed release-candidate manifest",
        "",
        f"Release set: `{manifest['release_set']}`",
        f"Source: `{manifest['source_identity']}`",
        f"Version: `{manifest['candidate_version']}`",
        f"sbt: `{manifest['sbt_version']}`",
        f"Synthetic fingerprint: `{manifest['synthetic_rehearsal_fingerprint']}`",
        "",
    ]
    for coordinate in manifest["coordinates"]:  # type: ignore[index]
        lines += [
            f"## {coordinate['coordinate']}",
            "",
            f"Role: `{coordinate['role']}`",
            f"Scala line: `{coordinate['scala_compiler_line']}`",
            f"License: `{coordinate['license']}`",
            "",
            "| File | Size | SHA-256 | SHA-512 | Signature |",
            "|---|---:|---|---|---|",
        ]
        for item in coordinate["files"]:
            lines.append(
                f"| `{item['filename']}` | {item['size']} | `{item['sha256']}` | `{item['sha512']}` | "
                f"{'PASS' if item['signature_verified'] else 'FAIL'} |"
            )
        lines += ["", "POM dependencies:", ""]
        for dependency in coordinate["pom_dependencies"]:
            lines.append(
                f"- `{dependency['group']}:{dependency['artifact']}:{dependency['version']}` "
                f"({dependency['scope']})"
            )
        lines.append("")
    return "\n".join(lines)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("project", type=Path)
    parser.add_argument("repository", type=Path)
    parser.add_argument("--release-set", choices=sorted(RELEASE_PROFILES), default="0.2.0")
    parser.add_argument("--fingerprint", required=True)
    parser.add_argument("--source-identity", required=True)
    parser.add_argument("--sbt-version", default="1.12.15")
    parser.add_argument("--json", type=Path, required=True)
    parser.add_argument("--markdown", type=Path, required=True)
    args = parser.parse_args()
    profile = RELEASE_PROFILES[args.release_set]
    manifest, errors = check(
        args.project.resolve(),
        args.repository.resolve(),
        profile,
        args.fingerprint,
        args.source_identity,
        args.sbt_version,
    )
    if errors:
        for error in errors:
            print(error, file=sys.stderr)
        print("QUASIQUOTES_RELEASE_REPOSITORY_BLOCKED", file=sys.stderr)
        return 3
    args.json.write_text(json.dumps(manifest, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    args.markdown.write_text(markdown(manifest), encoding="utf-8")
    print(profile.pass_marker)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
