#!/usr/bin/env python3
from __future__ import annotations

import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]


class ReleaseConfigurationTest(unittest.TestCase):
    def test_selected_version_and_tool_versions_are_pinned(self) -> None:
        build = (ROOT / "build.sbt").read_text()
        self.assertIn('ThisBuild / version := "0.2.0"', build)
        self.assertIn('ThisBuild / organization := "com.github.dmytromitin"', build)
        self.assertIn('ThisBuild / organizationName := "com.github.dmytromitin"', build)
        self.assertNotIn("io.github.dmytromitin", build)
        self.assertEqual((ROOT / "project/build.properties").read_text().strip(), "sbt.version=1.12.15")
        self.assertIn(
            'addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.3.1")',
            (ROOT / "project/plugins.sbt").read_text(),
        )

    def test_release_path_is_local_and_owner_identity_is_fail_closed(self) -> None:
        build = (ROOT / "build.sbt").read_text()
        self.assertIn("localStaging.value", build)
        self.assertIn("verifyReleaseIdentity", build)
        for property_name in (
            "quasiquotes.release.developer.id",
            "quasiquotes.release.developer.name",
            "quasiquotes.release.developer.email",
            "quasiquotes.release.developer.url",
        ):
            self.assertIn(property_name, build)

    def test_only_selected_modules_are_publishable(self) -> None:
        build = (ROOT / "build.sbt").read_text()
        for module in (
            "neutralScalameta",
            "dottyInternal",
            "hybridScalametaFrontend",
            "publicApiExamples",
            "publicCoreExamples",
            "root",
        ):
            start = build.index(f"lazy val {module}")
            end = build.find("\nlazy val ", start + 1)
            section = build[start:] if end < 0 else build[start:end]
            self.assertIn("publish / skip := true", section, module)


if __name__ == "__main__":
    unittest.main(verbosity=2)
