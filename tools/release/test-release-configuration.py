#!/usr/bin/env python3
from __future__ import annotations

import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[2]


class ReleaseConfigurationTest(unittest.TestCase):
    def test_development_version_and_tool_versions_are_pinned(self) -> None:
        build = (ROOT / "build.sbt").read_text()
        self.assertIn('ThisBuild / version := "0.3.0-SNAPSHOT"', build)
        self.assertIn('ThisBuild / scalaVersion := "3.8.4"', build)
        self.assertIn(
            'lazy val supportedScalaVersions = Vector("3.3.8", "3.8.4", "3.9.0")',
            build,
        )
        self.assertIn('lazy val binaryArtifactBuildScalaVersion = "3.3.8"', build)
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

    def test_all_production_modules_are_normally_publishable(self) -> None:
        build = (ROOT / "build.sbt").read_text()
        self.assertNotIn("quasiquotes.expandedRelease", build)
        self.assertNotIn("expandedReleaseProperty", build)
        self.assertNotIn("expandedReleaseEnabled", build)
        for module in (
            "core",
            "frontend",
            "neutralScalameta",
            "dottyInternal",
            "hybridScalametaFrontend",
        ):
            start = build.index(f"lazy val {module}")
            end = build.find("\nlazy val ", start + 1)
            section = build[start:] if end < 0 else build[start:end]
            self.assertNotIn("publish / skip", section, module)

    def test_binary_cross_publication_requires_oldest_supported_line(self) -> None:
        build = (ROOT / "build.sbt").read_text()
        self.assertIn("lazy val verifyBinaryArtifactBuildBaseline = taskKey[Unit]", build)
        self.assertIn(
            "publish := publish.dependsOn(verifyBinaryArtifactBuildBaseline).value",
            build,
        )
        self.assertIn(
            "PgpKeys.publishSigned := PgpKeys.publishSigned.dependsOn(verifyBinaryArtifactBuildBaseline).value",
            build,
        )
        for module in ("core", "neutralScalameta"):
            start = build.index(f"lazy val {module}")
            end = build.find("\nlazy val ", start + 1)
            section = build[start:] if end < 0 else build[start:end]
            self.assertIn(".settings(binaryCrossPublicationSettings)", section, module)

    def test_root_and_examples_are_always_skipped(self) -> None:
        build = (ROOT / "build.sbt").read_text()
        for module in (
            "publicApiExamples",
            "publicCoreExamples",
            "root",
        ):
            start = build.index(f"lazy val {module}")
            end = build.find("\nlazy val ", start + 1)
            section = build[start:] if end < 0 else build[start:end]
            self.assertIn("publish / skip := true", section, module)

    def test_current_release_docs_do_not_restore_obsolete_publishability_gate(self) -> None:
        current_docs = (
            ROOT / "README.md",
            ROOT / "docs/ARCHITECTURE.md",
            ROOT / "docs/DOTTY_INTERNAL_BACKEND.md",
            ROOT / "docs/EXPERIMENTAL_CONTEXTUAL_METHOD_PEER_BRIDGE.md",
            ROOT / "docs/HYBRID_SCALAMETA_TERM_FRONTEND_EXPERIMENT.md",
            ROOT / "docs/RELEASE_PROCESS.md",
            ROOT / "docs/SCALAMETA_OPT_IN_ARTIFACT_TOPOLOGY.md",
            ROOT / "docs/VERSIONING_AND_STABILITY.md",
        )
        rendered = "\n".join(path.read_text() for path in current_docs)
        self.assertNotIn("quasiquotes.expandedRelease", rendered)
        self.assertNotIn("0.3.0-expanded", rendered)
        self.assertIn("normally publishable production", rendered)


if __name__ == "__main__":
    unittest.main(verbosity=2)
