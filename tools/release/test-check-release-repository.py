#!/usr/bin/env python3
from __future__ import annotations

import importlib.util
import sys
import tempfile
import unittest
import zipfile
from pathlib import Path


HERE = Path(__file__).resolve().parent
SPEC = importlib.util.spec_from_file_location("release_checker", HERE / "check-release-repository.py")
assert SPEC and SPEC.loader
CHECKER = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = CHECKER
SPEC.loader.exec_module(CHECKER)


class ReleaseRepositoryTest(unittest.TestCase):
    def setUp(self) -> None:
        self.temp = tempfile.TemporaryDirectory()
        self.root = Path(self.temp.name)
        self.project = self.root / "project"
        self.repository = self.root / "repository"
        self.project.mkdir()
        self.license = b"Apache fixture\n"
        (self.project / "LICENSE").write_bytes(self.license)
        self.profile = CHECKER.RELEASE_PROFILES["0.3.0-expanded"]
        for coordinate in self.profile.coordinates:
            self.write_coordinate(coordinate)

    def tearDown(self) -> None:
        self.temp.cleanup()

    def write_coordinate(self, coordinate) -> None:
        artifact = coordinate.artifact
        scala_line = coordinate.scala_line
        version = self.profile.version
        directory = self.repository / CHECKER.GROUP_PATH / artifact / version
        directory.mkdir(parents=True)
        base = f"{artifact}-{version}"
        dependencies = "".join(
            f"<dependency><groupId>{group}</groupId><artifactId>{name}</artifactId><version>{dependency_version}</version></dependency>"
            for group, name, dependency_version in CHECKER.expected_compile_dependencies(
                coordinate, self.profile
            )
        )
        pom = directory / f"{base}.pom"
        pom.write_text(f"""<project><groupId>{CHECKER.GROUP}</groupId><artifactId>{artifact}</artifactId><version>{version}</version><name>{artifact}</name><description>fixture</description><url>{CHECKER.PROJECT_URL}</url><licenses><license><name>{CHECKER.LICENSE_NAME}</name><url>{CHECKER.LICENSE_URL}</url><distribution>repo</distribution></license></licenses><scm><url>{CHECKER.PROJECT_URL}</url><connection>{CHECKER.SCM_CONNECTION}</connection></scm><developers><developer><id>rehearsal</id><name>Rehearsal</name><email>rehearsal@example.invalid</email><url>https://example.invalid</url></developer></developers><dependencies>{dependencies}</dependencies></project>""", encoding="utf-8")
        deployables = [pom]
        for classifier in CHECKER.CLASSIFIERS:
            jar = directory / f"{base}{classifier}.jar"
            with zipfile.ZipFile(jar, "w") as archive:
                archive.writestr("META-INF/LICENSE", self.license)
            deployables.append(jar)
        for path in deployables:
            signature = path.with_name(path.name + ".asc")
            signature.write_text("synthetic")
            for algorithm in ("md5", "sha1"):
                path.with_name(path.name + f".{algorithm}").write_text(CHECKER.digest(path, algorithm))
                signature.with_name(signature.name + f".{algorithm}").write_text(
                    CHECKER.digest(signature, algorithm)
                )

    def run_check(self) -> list[str]:
        _, errors = CHECKER.check(
            self.project,
            self.repository,
            self.profile,
            "A" * 40,
            "fixture",
            "1.12.15",
            verifier=lambda *_: True,
        )
        return errors

    def test_exact_repository_passes(self) -> None:
        self.assertEqual(self.run_check(), [])

    def test_forward_probe_coordinate_blocks(self) -> None:
        (self.repository / CHECKER.GROUP_PATH / "quasiquotes-scala3-frontend_3.9.0-RC1").mkdir()
        self.assertTrue(any(error.startswith("COORDINATE_UNEXPECTED") for error in self.run_check()))

    def test_legacy_namespace_coordinate_blocks(self) -> None:
        legacy = (
            self.repository
            / "io/github/dmytromitin"
            / "quasiquotes-scala3-core_3"
            / self.profile.version
        )
        legacy.mkdir(parents=True)
        self.assertTrue(
            any(error.startswith("LEGACY_NAMESPACE_PRESENT") for error in self.run_check())
        )

    def test_missing_checksum_blocks(self) -> None:
        artifact = self.profile.coordinates[0].artifact
        directory = self.repository / CHECKER.GROUP_PATH / artifact / self.profile.version
        (directory / f"{artifact}-{self.profile.version}.pom.sha1").unlink()
        self.assertTrue(any(error.startswith("CHECKSUM_INVALID") for error in self.run_check()))

    def test_missing_signature_checksum_blocks(self) -> None:
        artifact = self.profile.coordinates[0].artifact
        directory = self.repository / CHECKER.GROUP_PATH / artifact / self.profile.version
        (directory / f"{artifact}-{self.profile.version}.pom.asc.sha1").unlink()
        self.assertTrue(any(error.startswith("CHECKSUM_INVALID") for error in self.run_check()))

    def test_wrong_role_dependency_blocks(self) -> None:
        coordinate = next(c for c in self.profile.coordinates if c.role == "neutral")
        pom = (
            self.repository
            / CHECKER.GROUP_PATH
            / coordinate.artifact
            / self.profile.version
            / f"{coordinate.artifact}-{self.profile.version}.pom"
        )
        pom.write_text(
            pom.read_text().replace(
                "<artifactId>scalameta_3</artifactId><version>4.17.3</version>",
                "<artifactId>scala3-compiler_3</artifactId><version>3.3.8</version>",
            )
        )
        self.assertTrue(any(error.startswith("POM_DEPENDENCY_CONTRACT_INVALID") for error in self.run_check()))

    def test_historical_0_2_0_profile_remains_available(self) -> None:
        self.repository = self.root / "historical-repository"
        self.profile = CHECKER.RELEASE_PROFILES["0.2.0"]
        for coordinate in self.profile.coordinates:
            self.write_coordinate(coordinate)
        self.assertEqual(self.run_check(), [])


if __name__ == "__main__":
    unittest.main(verbosity=2)
