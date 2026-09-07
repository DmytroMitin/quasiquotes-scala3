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
        self.profile = CHECKER.RELEASE_PROFILES["0.3.0-candidate"]
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
        if self.profile.name == "0.3.0-candidate" and coordinate.role == "frontend":
            dependencies += self.annotation_dependency()
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

    @staticmethod
    def annotation_dependency(scope="provided", version="0.1.0") -> str:
        return ("<dependency><groupId>com.github.dmytromitin</groupId>"
                "<artifactId>allow-experimental-annotation_3</artifactId>"
                f"<version>{version}</version><scope>{scope}</scope></dependency>")

    def dependency_errors(self, coordinate, transform) -> list[str]:
        pom = (self.repository / CHECKER.GROUP_PATH / coordinate.artifact /
               self.profile.version / f"{coordinate.artifact}-{self.profile.version}.pom")
        original = pom.read_text()
        try:
            pom.write_text(transform(original))
            errors = []
            CHECKER.pom_summary(pom, coordinate, self.profile, errors)
            return errors
        finally:
            pom.write_text(original)

    def test_frontend_annotation_is_required_with_exact_scope_and_version(self) -> None:
        for coordinate in self.profile.coordinates:
            if coordinate.role != "frontend":
                continue
            for replacement in ("", self.annotation_dependency(version="0.2.0"),
                                *(self.annotation_dependency(scope=scope) for scope in
                                  ("compile", "runtime", "test", "system", "import"))):
                with self.subTest(artifact=coordinate.artifact, replacement=replacement):
                    errors = self.dependency_errors(coordinate, lambda text:
                        text.replace(self.annotation_dependency(), replacement))
                    self.assertTrue(any(e.startswith("POM_DEPENDENCY_CONTRACT_INVALID") for e in errors))

    def test_annotation_cannot_leak_to_other_modules(self) -> None:
        for coordinate in self.profile.coordinates:
            if coordinate.role != "frontend":
                with self.subTest(artifact=coordinate.artifact):
                    errors = self.dependency_errors(coordinate, lambda text:
                        text.replace("</dependencies>", self.annotation_dependency() + "</dependencies>"))
                    self.assertTrue(any(e.startswith("POM_DEPENDENCY_CONTRACT_INVALID") for e in errors))

    def test_permission_plugin_cannot_leak_in_any_scope(self) -> None:
        for coordinate in self.profile.coordinates:
            for scope in ("compile", "provided", "runtime", "test"):
                with self.subTest(artifact=coordinate.artifact, scope=scope):
                    plugin = self.annotation_dependency(scope).replace(
                        "allow-experimental-annotation_3", f"allow-experimental-plugin_{coordinate.scala_line}")
                    errors = self.dependency_errors(coordinate, lambda text:
                        text.replace("</dependencies>", plugin + "</dependencies>"))
                    self.assertTrue(any(e.startswith("POM_DEPENDENCY_CONTRACT_INVALID") for e in errors))

    def test_duplicate_provided_annotation_blocks(self) -> None:
        coordinate = next(c for c in self.profile.coordinates if c.role == "frontend")
        errors = self.dependency_errors(coordinate, lambda text:
            text.replace(self.annotation_dependency(), self.annotation_dependency() * 2))
        self.assertTrue(any(e.startswith("POM_DEPENDENCY_CONTRACT_INVALID") for e in errors))

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

    def test_candidate_profile_has_exact_final_lts_coordinate_set(self) -> None:
        self.assertNotIn("0.3.0-expanded", CHECKER.RELEASE_PROFILES)
        self.assertEqual(self.profile.name, "0.3.0-candidate")
        self.assertEqual(
            self.profile.pass_marker,
            "QUASIQUOTES_RELEASE_REPOSITORY_0_3_0_CANDIDATE_PASS",
        )
        self.assertEqual(
            {coordinate.artifact for coordinate in self.profile.coordinates},
            {
                "quasiquotes-scala3-core_3",
                "quasiquotes-scala3-neutral-scalameta_3",
                "quasiquotes-scala3-frontend_3.3.8",
                "quasiquotes-scala3-scalameta-frontend_3.3.8",
                "quasiquotes-scala3-dotty-internal_3.3.8",
                "quasiquotes-scala3-frontend_3.8.4",
                "quasiquotes-scala3-scalameta-frontend_3.8.4",
                "quasiquotes-scala3-dotty-internal_3.8.4",
                "quasiquotes-scala3-frontend_3.9.0",
                "quasiquotes-scala3-scalameta-frontend_3.9.0",
                "quasiquotes-scala3-dotty-internal_3.9.0",
            },
        )
        self.assertEqual(len(self.profile.coordinates), 11)

    def test_release_candidate_coordinate_blocks(self) -> None:
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

    def test_historical_0_2_0_profile_is_exactly_unchanged(self) -> None:
        profile = CHECKER.RELEASE_PROFILES["0.2.0"]
        self.assertEqual(profile.version, "0.2.0")
        self.assertEqual(profile.pass_marker, "QUASIQUOTES_RELEASE_REPOSITORY_0_2_0_PASS")
        self.assertEqual(
            [(coordinate.artifact, coordinate.scala_line, coordinate.role) for coordinate in profile.coordinates],
            [
                ("quasiquotes-scala3-core_3", "3.3.8", "core"),
                ("quasiquotes-scala3-frontend_3.3.8", "3.3.8", "frontend"),
                ("quasiquotes-scala3-frontend_3.8.4", "3.8.4", "frontend"),
            ],
        )


if __name__ == "__main__":
    unittest.main(verbosity=2)
