#!/usr/bin/env python3
from __future__ import annotations

import subprocess
import tempfile
import unittest
import zipfile
from pathlib import Path


ROOT = Path(__file__).resolve().parent
CHECKER = ROOT / "check-license-distribution.py"

LICENSE_BYTES = b"canonical Apache-2.0 fixture\n"
GROUP_PATH = Path("io/github/dmytromitin")
ARTIFACTS = (
    "quasiquotes-scala3-core_3",
    "quasiquotes-scala3-frontend_3.3.8",
    "quasiquotes-scala3-frontend_3.8.4",
)
VERSION = "0.1.0-RC1"


class LicenseDistributionTest(unittest.TestCase):
    def setUp(self) -> None:
        self.temp = tempfile.TemporaryDirectory()
        self.root = Path(self.temp.name)
        self.project = self.root / "project"
        self.repository = self.root / "maven"
        self.project.mkdir()
        (self.project / "LICENSE").write_bytes(LICENSE_BYTES)
        for artifact in ARTIFACTS:
            self._write_coordinate(artifact)

    def tearDown(self) -> None:
        self.temp.cleanup()

    def _write_coordinate(self, artifact: str) -> None:
        directory = self.repository / GROUP_PATH / artifact / VERSION
        directory.mkdir(parents=True)
        base = f"{artifact}-{VERSION}"
        (directory / f"{base}.pom").write_text(
            """<project xmlns="http://maven.apache.org/POM/4.0.0">
  <modelVersion>4.0.0</modelVersion>
  <groupId>io.github.dmytromitin</groupId>
  <artifactId>ARTIFACT</artifactId>
  <version>VERSION</version>
  <licenses>
    <license>
      <name>Apache-2.0</name>
      <url>https://www.apache.org/licenses/LICENSE-2.0</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <organization>
    <name>io.github.dmytromitin</name>
    <url>https://github.com/DmytroMitin/quasiquotes-scala3</url>
  </organization>
  <scm>
    <url>https://github.com/DmytroMitin/quasiquotes-scala3</url>
    <connection>scm:git:git@github.com:DmytroMitin/quasiquotes-scala3.git</connection>
  </scm>
  <developers>
    <developer>
      <id>rehearsal</id>
      <name>Quasiquotes Release Rehearsal</name>
      <email>rehearsal@example.invalid</email>
      <url>https://example.invalid/rehearsal</url>
    </developer>
  </developers>
</project>
""".replace("ARTIFACT", artifact).replace("VERSION", VERSION),
            encoding="utf-8",
        )
        for classifier in ("", "-sources", "-javadoc"):
            with zipfile.ZipFile(directory / f"{base}{classifier}.jar", "w") as archive:
                archive.writestr("META-INF/LICENSE", LICENSE_BYTES)

    def _run(self) -> subprocess.CompletedProcess[str]:
        if not CHECKER.exists():
            self.fail(f"checker is not implemented: {CHECKER}")
        return subprocess.run(
            [
                "python3",
                str(CHECKER),
                str(self.project),
                str(self.repository),
                VERSION,
                "--license-sha256",
                "ee97f0af0487d1110f67e2f934977b8cdf48ee914b8abe61ece8e53142ad4164",
            ],
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            check=False,
        )

    def assert_passes(self) -> None:
        result = self._run()
        self.assertEqual(result.returncode, 0, result.stdout)
        self.assertEqual(result.stdout.strip(), "APACHE_2_0_DISTRIBUTION_PASS")

    def assert_blocks(self, code: str) -> None:
        result = self._run()
        self.assertEqual(result.returncode, 3, result.stdout)
        self.assertIn(code, result.stdout)
        self.assertEqual(
            result.stdout.splitlines()[-1],
            "BLOCKING_APACHE_2_0_DISTRIBUTION_FAILURE",
        )

    def test_exact_selected_three_coordinate_distribution_passes(self) -> None:
        self.assert_passes()

    def test_missing_jar_license_blocks(self) -> None:
        artifact = ARTIFACTS[0]
        jar = (
            self.repository
            / GROUP_PATH
            / artifact
            / VERSION
            / f"{artifact}-{VERSION}.jar"
        )
        jar.unlink()
        with zipfile.ZipFile(jar, "w") as archive:
            archive.writestr("example.class", b"example")
        self.assert_blocks("JAR_LICENSE_MISSING")

    def test_mismatched_jar_license_blocks(self) -> None:
        artifact = ARTIFACTS[1]
        jar = (
            self.repository
            / GROUP_PATH
            / artifact
            / VERSION
            / f"{artifact}-{VERSION}-sources.jar"
        )
        jar.unlink()
        with zipfile.ZipFile(jar, "w") as archive:
            archive.writestr("META-INF/LICENSE", b"wrong license\n")
        self.assert_blocks("JAR_LICENSE_MISMATCH")

    def test_unexpected_notice_blocks(self) -> None:
        artifact = ARTIFACTS[2]
        jar = (
            self.repository
            / GROUP_PATH
            / artifact
            / VERSION
            / f"{artifact}-{VERSION}-javadoc.jar"
        )
        with zipfile.ZipFile(jar, "a") as archive:
            archive.writestr("META-INF/NOTICE", b"invented notice\n")
        self.assert_blocks("UNEXPECTED_NOTICE")

    def test_duplicate_pom_license_blocks(self) -> None:
        artifact = ARTIFACTS[2]
        pom = (
            self.repository
            / GROUP_PATH
            / artifact
            / VERSION
            / f"{artifact}-{VERSION}.pom"
        )
        text = pom.read_text(encoding="utf-8")
        duplicate = """    <license>
      <name>MIT</name>
      <url>https://opensource.org/license/mit</url>
    </license>
"""
        pom.write_text(text.replace("  </licenses>", duplicate + "  </licenses>"))
        self.assert_blocks("POM_LICENSE_ENTRY_INVALID")

    def test_false_organization_blocks(self) -> None:
        artifact = ARTIFACTS[0]
        pom = (
            self.repository
            / GROUP_PATH
            / artifact
            / VERSION
            / f"{artifact}-{VERSION}.pom"
        )
        text = pom.read_text(encoding="utf-8")
        pom.write_text(
            text.replace("<name>io.github.dmytromitin</name>", "<name>Apache Software Foundation</name>"),
            encoding="utf-8",
        )
        self.assert_blocks("POM_ORGANIZATION_INVALID")

    def test_false_scm_blocks(self) -> None:
        artifact = ARTIFACTS[1]
        pom = (
            self.repository
            / GROUP_PATH
            / artifact
            / VERSION
            / f"{artifact}-{VERSION}.pom"
        )
        text = pom.read_text(encoding="utf-8")
        pom.write_text(
            text.replace(
                "scm:git:git@github.com:DmytroMitin/quasiquotes-scala3.git",
                "scm:git:git@github.com:example/not-this-project.git",
            ),
            encoding="utf-8",
        )
        self.assert_blocks("POM_SCM_INVALID")

    def test_unexpected_coordinate_blocks(self) -> None:
        self._write_coordinate("quasiquotes-scala3-dotty-internal_3")
        self.assert_blocks("UNEXPECTED_COORDINATE")

    def test_missing_developer_metadata_blocks(self) -> None:
        artifact = ARTIFACTS[0]
        pom = self.repository / GROUP_PATH / artifact / VERSION / f"{artifact}-{VERSION}.pom"
        text = pom.read_text(encoding="utf-8")
        start = text.index("  <developers>")
        end = text.index("  </developers>") + len("  </developers>\n")
        pom.write_text(text[:start] + text[end:], encoding="utf-8")
        self.assert_blocks("POM_DEVELOPER_INVALID")


if __name__ == "__main__":
    unittest.main(verbosity=2)
