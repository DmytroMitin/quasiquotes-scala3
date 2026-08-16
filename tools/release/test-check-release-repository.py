#!/usr/bin/env python3
from __future__ import annotations

import importlib.util
import tempfile
import unittest
import zipfile
from pathlib import Path


HERE = Path(__file__).resolve().parent
SPEC = importlib.util.spec_from_file_location("release_checker", HERE / "check-release-repository.py")
assert SPEC and SPEC.loader
CHECKER = importlib.util.module_from_spec(SPEC)
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
        for artifact, scala_line in CHECKER.COORDINATES.items():
            self.write_coordinate(artifact, scala_line)

    def tearDown(self) -> None:
        self.temp.cleanup()

    def write_coordinate(self, artifact: str, scala_line: str) -> None:
        directory = self.repository / CHECKER.GROUP_PATH / artifact / CHECKER.VERSION
        directory.mkdir(parents=True)
        base = f"{artifact}-{CHECKER.VERSION}"
        dependencies = f"""<dependency><groupId>org.scala-lang</groupId><artifactId>scala3-library_3</artifactId><version>{scala_line}</version></dependency>"""
        if "frontend" in artifact:
            dependencies += f"""<dependency><groupId>org.scala-lang</groupId><artifactId>scala3-compiler_3</artifactId><version>{scala_line}</version></dependency><dependency><groupId>{CHECKER.GROUP}</groupId><artifactId>quasiquotes-scala3-core_3</artifactId><version>{CHECKER.VERSION}</version></dependency>"""
        pom = directory / f"{base}.pom"
        pom.write_text(f"""<project><groupId>{CHECKER.GROUP}</groupId><artifactId>{artifact}</artifactId><version>{CHECKER.VERSION}</version><name>{artifact}</name><description>fixture</description><url>{CHECKER.PROJECT_URL}</url><licenses><license><name>{CHECKER.LICENSE_NAME}</name><url>{CHECKER.LICENSE_URL}</url><distribution>repo</distribution></license></licenses><scm><url>{CHECKER.PROJECT_URL}</url><connection>{CHECKER.SCM_CONNECTION}</connection></scm><developers><developer><id>rehearsal</id><name>Rehearsal</name><email>rehearsal@example.invalid</email><url>https://example.invalid</url></developer></developers><dependencies>{dependencies}</dependencies></project>""", encoding="utf-8")
        deployables = [pom]
        for classifier in CHECKER.CLASSIFIERS:
            jar = directory / f"{base}{classifier}.jar"
            with zipfile.ZipFile(jar, "w") as archive:
                archive.writestr("META-INF/LICENSE", self.license)
            deployables.append(jar)
        for path in deployables:
            path.with_name(path.name + ".asc").write_text("synthetic")
            for algorithm in ("md5", "sha1"):
                path.with_name(path.name + f".{algorithm}").write_text(CHECKER.digest(path, algorithm))

    def run_check(self) -> list[str]:
        _, errors = CHECKER.check(self.project, self.repository, "A" * 40, "fixture", "1.12.15", verifier=lambda *_: True)
        return errors

    def test_exact_repository_passes(self) -> None:
        self.assertEqual(self.run_check(), [])

    def test_forward_probe_coordinate_blocks(self) -> None:
        (self.repository / CHECKER.GROUP_PATH / "quasiquotes-scala3-frontend_3.9.0-RC1").mkdir()
        self.assertTrue(any(error.startswith("COORDINATE_UNEXPECTED") for error in self.run_check()))

    def test_missing_checksum_blocks(self) -> None:
        artifact = next(iter(CHECKER.COORDINATES))
        directory = self.repository / CHECKER.GROUP_PATH / artifact / CHECKER.VERSION
        (directory / f"{artifact}-{CHECKER.VERSION}.pom.sha1").unlink()
        self.assertTrue(any(error.startswith("CHECKSUM_INVALID") for error in self.run_check()))


if __name__ == "__main__":
    unittest.main(verbosity=2)
