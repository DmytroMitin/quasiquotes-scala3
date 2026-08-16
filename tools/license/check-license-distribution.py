#!/usr/bin/env python3
from __future__ import annotations

import argparse
import hashlib
import sys
import xml.etree.ElementTree as ET
import zipfile
from pathlib import Path


PASS = "APACHE_2_0_DISTRIBUTION_PASS"
BLOCKED = "BLOCKING_APACHE_2_0_DISTRIBUTION_FAILURE"
GROUP_ID = "io.github.dmytromitin"
GROUP_PATH = Path("io/github/dmytromitin")
EXPECTED_ARTIFACTS = (
    "quasiquotes-scala3-core_3",
    "quasiquotes-scala3-frontend_3.3.8",
    "quasiquotes-scala3-frontend_3.8.4",
)
EXPECTED_LICENSE_NAME = "Apache-2.0"
EXPECTED_LICENSE_URL = "https://www.apache.org/licenses/LICENSE-2.0"
EXPECTED_PROJECT_URL = "https://github.com/DmytroMitin/quasiquotes-scala3"
EXPECTED_SCM_CONNECTION = (
    "scm:git:git@github.com:DmytroMitin/quasiquotes-scala3.git"
)


def add(
    findings: set[tuple[str, str, str]],
    code: str,
    subject: str,
    detail: str,
) -> None:
    findings.add((code, subject, detail))


def children(element: ET.Element, name: str) -> list[ET.Element]:
    return [child for child in element if child.tag.rsplit("}", 1)[-1] == name]


def one_text(element: ET.Element, name: str) -> str | None:
    matches = children(element, name)
    if len(matches) != 1:
        return None
    return (matches[0].text or "").strip()


def check_pom(
    pom: Path,
    artifact: str,
    version: str,
    findings: set[tuple[str, str, str]],
) -> None:
    try:
        root = ET.parse(pom).getroot()
    except (OSError, ET.ParseError) as error:
        add(findings, "POM_INVALID", artifact, str(error))
        return

    expected_identity = {
        "groupId": GROUP_ID,
        "artifactId": artifact,
        "version": version,
    }
    for name, expected in expected_identity.items():
        actual = one_text(root, name)
        if actual != expected:
            add(
                findings,
                "POM_IDENTITY_INVALID",
                artifact,
                f"{name}: expected={expected} actual={actual}",
            )

    license_containers = children(root, "licenses")
    licenses = (
        children(license_containers[0], "license")
        if len(license_containers) == 1
        else []
    )
    valid_license = (
        len(licenses) == 1
        and one_text(licenses[0], "name") == EXPECTED_LICENSE_NAME
        and one_text(licenses[0], "url") == EXPECTED_LICENSE_URL
        and one_text(licenses[0], "distribution") == "repo"
    )
    if not valid_license:
        add(
            findings,
            "POM_LICENSE_ENTRY_INVALID",
            artifact,
            "expected exactly Apache-2.0, canonical HTTPS URL, distribution=repo",
        )

    organizations = children(root, "organization")
    valid_organization = (
        len(organizations) == 1
        and one_text(organizations[0], "name") == GROUP_ID
        and one_text(organizations[0], "url") == EXPECTED_PROJECT_URL
    )
    if not valid_organization:
        add(
            findings,
            "POM_ORGANIZATION_INVALID",
            artifact,
            "expected existing coordinate/repository organization identity",
        )

    scm_entries = children(root, "scm")
    valid_scm = (
        len(scm_entries) == 1
        and one_text(scm_entries[0], "url") == EXPECTED_PROJECT_URL
        and one_text(scm_entries[0], "connection") == EXPECTED_SCM_CONNECTION
    )
    if not valid_scm:
        add(
            findings,
            "POM_SCM_INVALID",
            artifact,
            "expected existing project URL and Git connection",
        )

    developer_containers = children(root, "developers")
    developers = (
        children(developer_containers[0], "developer")
        if len(developer_containers) == 1
        else []
    )
    valid_developer = (
        len(developers) == 1
        and all(one_text(developers[0], field) for field in ("id", "name", "email", "url"))
    )
    if not valid_developer:
        add(findings, "POM_DEVELOPER_INVALID", artifact, "expected one complete developer entry")

    for forbidden in ("distributionManagement",):
        if children(root, forbidden):
            add(findings, "POM_UNAUTHORIZED_METADATA", artifact, forbidden)


def check_jar(
    jar: Path,
    artifact: str,
    expected_license: bytes,
    findings: set[tuple[str, str, str]],
) -> None:
    try:
        with zipfile.ZipFile(jar) as archive:
            names = set(archive.namelist())
            if "META-INF/LICENSE" not in names:
                add(findings, "JAR_LICENSE_MISSING", artifact, jar.name)
            elif archive.read("META-INF/LICENSE") != expected_license:
                add(findings, "JAR_LICENSE_MISMATCH", artifact, jar.name)
            notice_names = {
                name
                for name in names
                if name.upper() in {"META-INF/NOTICE", "META-INF/NOTICE.TXT"}
            }
            for notice in sorted(notice_names):
                add(findings, "UNEXPECTED_NOTICE", artifact, f"{jar.name}:{notice}")
    except (OSError, zipfile.BadZipFile) as error:
        add(findings, "JAR_INVALID", artifact, f"{jar.name}:{error}")


def check(project: Path, repository: Path, version: str, expected_hash: str) -> set[tuple[str, str, str]]:
    findings: set[tuple[str, str, str]] = set()
    license_path = project / "LICENSE"
    if not license_path.is_file():
        add(findings, "ROOT_LICENSE_MISSING", "project", str(license_path))
        return findings
    license_bytes = license_path.read_bytes()
    actual_hash = hashlib.sha256(license_bytes).hexdigest()
    if actual_hash != expected_hash:
        add(
            findings,
            "ROOT_LICENSE_HASH_MISMATCH",
            "project",
            f"expected={expected_hash} actual={actual_hash}",
        )
    if (project / "NOTICE").exists() or (project / "NOTICE.txt").exists():
        add(findings, "UNEXPECTED_NOTICE", "project", "root NOTICE exists")

    group = repository / GROUP_PATH
    actual_artifacts = (
        {path.name for path in group.iterdir() if path.is_dir()}
        if group.is_dir()
        else set()
    )
    expected_artifacts = set(EXPECTED_ARTIFACTS)
    for artifact in sorted(actual_artifacts - expected_artifacts):
        add(findings, "UNEXPECTED_COORDINATE", artifact, version)
    for artifact in sorted(expected_artifacts - actual_artifacts):
        add(findings, "EXPECTED_COORDINATE_MISSING", artifact, version)

    for artifact in EXPECTED_ARTIFACTS:
        directory = group / artifact / version
        base = f"{artifact}-{version}"
        required = (
            directory / f"{base}.pom",
            directory / f"{base}.jar",
            directory / f"{base}-sources.jar",
            directory / f"{base}-javadoc.jar",
        )
        for path in required:
            if not path.is_file():
                add(findings, "ARTIFACT_FILE_MISSING", artifact, path.name)
        pom = required[0]
        if pom.is_file():
            check_pom(pom, artifact, version, findings)
        for jar in required[1:]:
            if jar.is_file():
                check_jar(jar, artifact, license_bytes, findings)
    return findings


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("project", type=Path)
    parser.add_argument("repository", type=Path)
    parser.add_argument("version")
    parser.add_argument("--license-sha256", required=True)
    arguments = parser.parse_args()

    try:
        findings = check(
            arguments.project.resolve(),
            arguments.repository.resolve(),
            arguments.version,
            arguments.license_sha256.lower(),
        )
    except OSError as error:
        print(f"LICENSE_DISTRIBUTION_INPUT_ERROR\t{error}", file=sys.stderr)
        return 64

    if findings:
        for finding in sorted(findings):
            print("\t".join(finding))
        print(BLOCKED)
        return 3
    print(PASS)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
