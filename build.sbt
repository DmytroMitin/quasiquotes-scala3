import java.nio.charset.StandardCharsets
import java.util.jar.JarFile
import scala.collection.JavaConverters._

ThisBuild / version := "0.3.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.8.4"
ThisBuild / organization := "com.github.dmytromitin"
ThisBuild / organizationName := "com.github.dmytromitin"
ThisBuild / organizationHomepage := Some(url("https://github.com/DmytroMitin/quasiquotes-scala3"))
ThisBuild / versionScheme := Some("early-semver")
ThisBuild / homepage := Some(url("https://github.com/DmytroMitin/quasiquotes-scala3"))
ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/DmytroMitin/quasiquotes-scala3"),
    "scm:git:git@github.com:DmytroMitin/quasiquotes-scala3.git"
  )
)
ThisBuild / publishMavenStyle := true
ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishTo := localStaging.value
Global / concurrentRestrictions := Seq(Tags.limitAll(1))

lazy val munitVersion = "1.2.4"
lazy val scalametaVersion = "4.17.3"
lazy val supportedScalaVersions = Vector("3.3.8", "3.8.4", "3.9.0")
lazy val binaryArtifactBuildScalaVersion = "3.3.8"
lazy val expandedReleaseProperty = "quasiquotes.expandedRelease"
lazy val expandedReleaseEnabled = sys.props.get(expandedReleaseProperty) match {
  case None | Some("false") => false
  case Some("true") => true
  case Some(value) =>
    sys.error(
      s"-D$expandedReleaseProperty must be exactly true or false when supplied; found: $value"
    )
}

lazy val verifyCoreBoundary = taskKey[Unit](
  "Verify core source, classpath, artifact, and TASTy compiler freedom"
)
lazy val verifyNeutralScalametaBoundary = taskKey[Unit](
  "Verify neutral Scalameta source, dependency, artifact, and TASTy purity"
)
lazy val verifyModuleGraph = taskKey[Unit](
  "Verify the selected project dependency graph and aggregate-root packaging"
)
lazy val verifyScalametaArtifactTopology = taskKey[Unit](
  "Verify Scalameta coordinates, POM graph, packages, and explicit default/release publication policy"
)
lazy val verifyReleaseIdentity = taskKey[Unit](
  "Require explicitly supplied public developer metadata before signed staging"
)
lazy val verifyBinaryArtifactBuildBaseline = taskKey[Unit](
  "Require binary-cross release artifacts to use the oldest supported Scala line"
)

lazy val releaseDeveloperProperties = Vector(
  "quasiquotes.release.developer.id",
  "quasiquotes.release.developer.name",
  "quasiquotes.release.developer.email",
  "quasiquotes.release.developer.url"
)

ThisBuild / developers := {
  val values = releaseDeveloperProperties.map(name => sys.props.get(name).map(_.trim))
  if (values.forall(_.exists(_.nonEmpty))) {
    List(Developer(values(0).get, values(1).get, values(2).get, url(values(3).get)))
  } else Nil
}

ThisBuild / verifyReleaseIdentity := {
  val missing = releaseDeveloperProperties.filter(name =>
    sys.props.get(name).forall(_.trim.isEmpty)
  )
  if (missing.nonEmpty) {
    sys.error(
      "Signed release staging requires owner-approved public developer metadata: " +
        missing.mkString(", ")
    )
  }
}

lazy val commonSettings = Seq(
  libraryDependencies += "org.scalameta" %% "munit" % munitVersion % Test,
  Compile / exportJars := true,
  Test / exportJars := true,
  Test / classLoaderLayeringStrategy := ClassLoaderLayeringStrategy.Flat
)

lazy val publicationLicenseSettings = Seq(
  licenses := Seq(
    "Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0")
  ),
  Compile / packageBin / mappings +=
    baseDirectory.value.getParentFile / "LICENSE" -> "META-INF/LICENSE",
  Compile / packageSrc / mappings +=
    baseDirectory.value.getParentFile / "LICENSE" -> "META-INF/LICENSE",
  Compile / packageDoc / mappings +=
    baseDirectory.value.getParentFile / "LICENSE" -> "META-INF/LICENSE",
  PgpKeys.publishSigned := PgpKeys.publishSigned.dependsOn(verifyReleaseIdentity).value
)

lazy val binaryCrossPublicationSettings = Seq(
  verifyBinaryArtifactBuildBaseline := {
    val actual = scalaVersion.value
    if (actual != binaryArtifactBuildScalaVersion) {
      sys.error(
        s"Binary-cross artifacts must be built with Scala $binaryArtifactBuildScalaVersion; found $actual."
      )
    }
  },
  publish := publish.dependsOn(verifyBinaryArtifactBuildBaseline).value,
  PgpKeys.publishSigned := PgpKeys.publishSigned.dependsOn(verifyBinaryArtifactBuildBaseline).value
)

lazy val core = (project in file("core"))
  .settings(commonSettings)
  .settings(publicationLicenseSettings)
  .settings(binaryCrossPublicationSettings)
  .settings(
    name := "quasiquotes-scala3-core",
    description := "Compiler-free structural quasiquote values and algorithms for Scala 3",
    verifyCoreBoundary := {
      val log = streams.value.log
      val forbiddenSourceTokens =
        Vector("scala.quoted", "dotty.tools.dotc", "scala.meta.")
      val coreSources: Seq[File] = (Compile / sources).value
      val sourceViolations = for {
        source <- coreSources
        text = IO.read(source)
        token <- forbiddenSourceTokens
        if text.contains(token)
      } yield s"${source.getAbsolutePath}: $token"
      if (sourceViolations.nonEmpty) {
        sys.error(
          "Core source purity violation(s):\n" + sourceViolations.mkString("\n")
        )
      }

      val forbiddenClasspathTokens = Vector(
        "scala3-compiler",
        "scala3-interfaces",
        "tasty-core",
        "scala-asm",
        "scalameta_3",
        "semanticdb"
      )
      val compileClasspath = (Compile / fullClasspath).value.map(_.data)
      val runtimeClasspath = (Runtime / fullClasspath).value.map(_.data)
      val classpath = (compileClasspath ++ runtimeClasspath).distinct
      val classpathViolations = classpath.filter(path =>
        forbiddenClasspathTokens.exists(path.getName.contains)
      )
      if (classpathViolations.nonEmpty) {
        sys.error(
          "Core classpath purity violation(s):\n" +
            classpathViolations.map(_.getAbsolutePath).mkString("\n")
        )
      }

      val pom = (Compile / makePom).value
      val pomText = IO.read(pom)
      val forbiddenPomTokens = forbiddenClasspathTokens ++ Vector("parsers_3", "trees_3")
      val pomViolations = forbiddenPomTokens.filter(pomText.contains)
      if (pomViolations.nonEmpty) {
        sys.error(
          "Core generated POM purity violation(s): " + pomViolations.mkString(", ")
        )
      }

      val jar = (Compile / packageBin).value
      val archive = new JarFile(jar)
      try {
        val forbiddenEntryPrefixes =
          Vector("dotty/", "scala/quoted/", "quasiquotes/parser/Scala3Parser")
        val forbiddenEntries = archive.entries().asScala
          .map(_.getName)
          .filter(name => forbiddenEntryPrefixes.exists(name.startsWith))
          .toVector
        if (forbiddenEntries.nonEmpty) {
          sys.error(
            "Core artifact contains forbidden entries:\n" +
              forbiddenEntries.mkString("\n")
          )
        }
      } finally archive.close()

      val classAndTastyFiles =
        ((Compile / classDirectory).value ** ("*.class" | "*.tasty")).get
      val forbiddenBinaryTokens =
        Vector("dotty/tools/dotc", "scala/quoted", "scala3-compiler")
      val binaryViolations = for {
        file <- classAndTastyFiles
        bytes = IO.readBytes(file)
        text = new String(bytes, StandardCharsets.ISO_8859_1)
        token <- forbiddenBinaryTokens
        if text.contains(token)
      } yield s"${file.getAbsolutePath}: $token"
      if (binaryViolations.nonEmpty) {
        sys.error(
          "Core class/TASTy purity violation(s):\n" +
            binaryViolations.mkString("\n")
        )
      }

      log.info(
        s"Core boundary verified: ${coreSources.size} sources, " +
          s"${compileClasspath.size} compile and ${runtimeClasspath.size} runtime classpath entries, " +
          s"${classAndTastyFiles.size} class/TASTy files"
      )
    }
  )

lazy val frontend = (project in file("frontend"))
  .dependsOn(core % "compile->compile;test->test")
  .settings(commonSettings)
  .settings(publicationLicenseSettings)
  .settings(
    name := "quasiquotes-scala3-frontend",
    description := "Scala compiler-coupled parsing, reflection, and quasiquote frontend",
    crossVersion := CrossVersion.full,
    libraryDependencies ++= Seq(
      "org.scala-lang" %% "scala3-compiler" % scalaVersion.value,
      "org.scala-lang" %% "scala3-staging" % scalaVersion.value % Test
    )
  )

lazy val neutralScalameta = (project in file("neutral-scalameta"))
  .dependsOn(core % "compile->compile;test->test")
  .settings(commonSettings)
  .settings(publicationLicenseSettings)
  .settings(binaryCrossPublicationSettings)
  .settings(
    name := "quasiquotes-scala3-neutral-scalameta",
    description := "Experimental compiler-free Scalameta-backed neutral quasiquotes",
    libraryDependencies +=
      "org.scalameta" %% "scalameta" % scalametaVersion,
    publish / skip := !expandedReleaseEnabled,
    verifyNeutralScalametaBoundary := {
      val log = streams.value.log
      val forbiddenSourceTokens = Vector("scala.quoted", "dotty.tools.dotc")
      val productionSources = (Compile / sources).value
      val sourceViolations = for {
        source <- productionSources
        text = IO.read(source)
        token <- forbiddenSourceTokens
        if text.contains(token)
      } yield s"${source.getAbsolutePath}: $token"
      if (sourceViolations.nonEmpty) {
        sys.error(
          "Neutral Scalameta source purity violation(s):\n" +
            sourceViolations.mkString("\n")
        )
      }

      val forbiddenClasspathTokens = Vector(
        "scala3-compiler",
        "scala3-interfaces",
        "tasty-core",
        "scala-asm",
        "scala3-staging",
        "semanticdb"
      )
      val compileClasspath = (Compile / fullClasspath).value.map(_.data)
      val runtimeClasspath = (Runtime / fullClasspath).value.map(_.data)
      val classpath = (compileClasspath ++ runtimeClasspath).distinct
      val classpathViolations = classpath.filter(path =>
        forbiddenClasspathTokens.exists(path.getName.contains)
      )
      if (classpathViolations.nonEmpty) {
        sys.error(
          "Neutral Scalameta classpath purity violation(s):\n" +
            classpathViolations.map(_.getAbsolutePath).mkString("\n")
        )
      }
      if (!classpath.exists(_.getName.startsWith("scalameta_3-4.17.3"))) {
        sys.error("Neutral Scalameta classpath is missing scalameta_3 4.17.3.")
      }

      val pom = (Compile / makePom).value
      val pomText = IO.read(pom)
      val pomViolations = forbiddenClasspathTokens.filter(pomText.contains)
      if (pomViolations.nonEmpty) {
        sys.error(
          "Neutral Scalameta generated POM purity violation(s): " +
            pomViolations.mkString(", ")
        )
      }
      if (!pomText.contains("<artifactId>scalameta_3</artifactId>") ||
          !pomText.contains("<version>4.17.3</version>")) {
        sys.error("Neutral Scalameta generated POM lacks the exact Scalameta dependency.")
      }

      val jar = (Compile / packageBin).value
      val archive = new JarFile(jar)
      try {
        val forbiddenEntryPrefixes = Vector("dotty/", "scala/quoted/")
        val forbiddenEntries = archive.entries().asScala
          .map(_.getName)
          .filter(name => forbiddenEntryPrefixes.exists(name.startsWith))
          .toVector
        if (forbiddenEntries.nonEmpty) {
          sys.error(
            "Neutral Scalameta artifact contains forbidden entries:\n" +
              forbiddenEntries.mkString("\n")
          )
        }
      } finally archive.close()

      val classAndTastyFiles =
        ((Compile / classDirectory).value ** ("*.class" | "*.tasty")).get
      val forbiddenBinaryTokens =
        Vector("dotty/tools/dotc", "scala/quoted", "semanticdb")
      val binaryViolations = for {
        file <- classAndTastyFiles
        bytes = IO.readBytes(file)
        text = new String(bytes, StandardCharsets.ISO_8859_1)
        token <- forbiddenBinaryTokens
        if text.contains(token)
      } yield s"${file.getAbsolutePath}: $token"
      if (binaryViolations.nonEmpty) {
        sys.error(
          "Neutral Scalameta class/TASTy purity violation(s):\n" +
            binaryViolations.mkString("\n")
        )
      }

      log.info(
        s"Neutral Scalameta boundary verified: ${productionSources.size} sources, " +
          s"${compileClasspath.size} compile and ${runtimeClasspath.size} runtime classpath entries, " +
          s"${classAndTastyFiles.size} class/TASTy files; Scalameta 4.17.3 present; " +
          "no compiler implementation, scala3-staging, or SemanticDB"
      )
    }
  )

lazy val dottyInternal = (project in file("dotty-internal"))
  .dependsOn(neutralScalameta % "compile->compile;test->test")
  .settings(commonSettings)
  .settings(publicationLicenseSettings)
  .settings(
    name := "quasiquotes-scala3-dotty-internal",
    description := "Exact-Scala-version experimental backend for tightly coupled peer integration",
    crossVersion := CrossVersion.full,
    libraryDependencies +=
      "org.scala-lang" %% "scala3-compiler" % scalaVersion.value,
    publish / skip := !expandedReleaseEnabled
  )

lazy val hybridScalametaFrontend = (project in file("hybrid-scalameta-frontend"))
  .dependsOn(
    frontend % "compile->compile;test->test",
    neutralScalameta % "compile->compile;test->test"
  )
  .settings(commonSettings)
  .settings(publicationLicenseSettings)
  .settings(
    name := "quasiquotes-scala3-scalameta-frontend",
    description := "Experimental Scalameta-primary Term and Type opt-in frontend",
    crossVersion := CrossVersion.full,
    libraryDependencies +=
      "org.scala-lang" %% "scala3-staging" % scalaVersion.value % Test,
    publish / skip := !expandedReleaseEnabled
  )

lazy val publicApiExamples = (project in file("public-api-examples"))
  .dependsOn(frontend)
  .settings(commonSettings)
  .settings(
    name := "quasiquotes-scala3-public-api-examples",
    libraryDependencies +=
      "org.scala-lang" %% "scala3-staging" % scalaVersion.value % Test,
    publish / skip := true,
    Compile / sources := Seq(
      (Compile / scalaSource).value / "external" / "consumer" / "DqrNegativeMacros.scala",
      (Compile / scalaSource).value / "external" / "consumer" / "DefinitionPatternFirstUseMacros.scala"
    )
  )

lazy val publicCoreExamples = (project in file("public-core-examples"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    name := "quasiquotes-scala3-public-core-examples",
    publish / skip := true,
    Compile / sources := Nil
  )

lazy val root = (project in file("."))
  .aggregate(core, neutralScalameta, frontend, dottyInternal, hybridScalametaFrontend)
  .settings(
    name := "quasiquotes-scala3",
    publish / skip := true,
    Compile / sources := Nil,
    Test / sources := Nil,
    Compile / packageBin / mappings := Nil,
    verifyModuleGraph := {
      val log = streams.value.log
      val frontendCompile = (frontend / Compile / fullClasspath).value
        .map(_.data.getAbsolutePath.replace('\\', '/')).mkString("\n")
      val frontendTest = (frontend / Test / fullClasspath).value
        .map(_.data.getAbsolutePath.replace('\\', '/')).mkString("\n")
      val backendCompile = (dottyInternal / Compile / fullClasspath).value
        .map(_.data.getAbsolutePath.replace('\\', '/')).mkString("\n")
      val backendTest = (dottyInternal / Test / fullClasspath).value
        .map(_.data.getAbsolutePath.replace('\\', '/')).mkString("\n")
      val neutralCompile = (neutralScalameta / Compile / fullClasspath).value
        .map(_.data.getAbsolutePath.replace('\\', '/')).mkString("\n")
      val neutralTest = (neutralScalameta / Test / fullClasspath).value
        .map(_.data.getAbsolutePath.replace('\\', '/')).mkString("\n")
      val hybridCompile = (hybridScalametaFrontend / Compile / fullClasspath).value
        .map(_.data.getAbsolutePath.replace('\\', '/')).mkString("\n")
      val hybridTest = (hybridScalametaFrontend / Test / fullClasspath).value
        .map(_.data.getAbsolutePath.replace('\\', '/')).mkString("\n")
      val coreCompile = (core / Compile / fullClasspath).value
        .map(_.data.getAbsolutePath.replace('\\', '/')).mkString("\n")
      val coreRuntime = (core / Runtime / fullClasspath).value
        .map(_.data.getAbsolutePath.replace('\\', '/')).mkString("\n")
      val publicCoreClasspath =
        (publicCoreExamples / Test / fullClasspath).value
          .map(_.data.getAbsolutePath.replace('\\', '/')).mkString("\n")
      var hiddenCycleViolations = Vector.empty[String]
      if (frontendCompile.contains("/dotty-internal/target/"))
        hiddenCycleViolations :+= "frontend compile -> dottyInternal"
      if (frontendTest.contains("/dotty-internal/target/"))
        hiddenCycleViolations :+= "frontend test -> dottyInternal"
      if (frontendCompile.contains("/neutral-scalameta/target/"))
        hiddenCycleViolations :+= "frontend compile -> neutralScalameta"
      if (frontendTest.contains("/neutral-scalameta/target/"))
        hiddenCycleViolations :+= "frontend test -> neutralScalameta"
      if (backendCompile.contains("/frontend/target/"))
        hiddenCycleViolations :+= "dottyInternal compile -> frontend"
      if (backendTest.contains("/frontend/target/"))
        hiddenCycleViolations :+= "dottyInternal test -> frontend"
      if (!backendCompile.contains("/neutral-scalameta/target/"))
        hiddenCycleViolations :+= "dottyInternal compile missing neutralScalameta"
      if (!neutralCompile.contains("/core/target/"))
        hiddenCycleViolations :+= "neutralScalameta compile missing core"
      if (neutralCompile.contains("/frontend/target/") ||
          neutralCompile.contains("/dotty-internal/target/"))
        hiddenCycleViolations :+= "neutralScalameta compile -> compiler-coupled sibling"
      if (neutralTest.contains("/frontend/target/") ||
          neutralTest.contains("/dotty-internal/target/"))
        hiddenCycleViolations :+= "neutralScalameta test -> compiler-coupled sibling"
      if (Vector("scala3-compiler", "scala3-interfaces", "tasty-core", "scala-asm", "scala3-staging", "semanticdb")
          .exists(token => neutralCompile.contains(token) || neutralTest.contains(token)))
        hiddenCycleViolations :+= "neutralScalameta -> compiler implementation, staging, or SemanticDB"
      if (Vector("neutral-scalameta", "scalameta_3", "parsers_3", "trees_3", "semanticdb")
          .exists(token => coreCompile.contains(token) || coreRuntime.contains(token)))
        hiddenCycleViolations :+= "core -> neutralScalameta or Scalameta"
      if (!hybridCompile.contains("/frontend/target/") ||
          !hybridCompile.contains("/neutral-scalameta/target/"))
        hiddenCycleViolations :+= "hybridScalametaFrontend compile missing frontend or neutralScalameta"
      if (!hybridTest.contains("/frontend/target/") ||
          !hybridTest.contains("/neutral-scalameta/target/"))
        hiddenCycleViolations :+= "hybridScalametaFrontend test missing frontend or neutralScalameta"
      if (frontendCompile.contains("/hybrid-scalameta-frontend/target/") ||
          neutralCompile.contains("/hybrid-scalameta-frontend/target/"))
        hiddenCycleViolations :+= "published or neutral module -> hybridScalametaFrontend"
      if (publicCoreClasspath.contains("/frontend/target/"))
        hiddenCycleViolations :+= "publicCoreExamples test -> frontend"
      if (publicCoreClasspath.contains("/dotty-internal/target/"))
        hiddenCycleViolations :+= "publicCoreExamples test -> dottyInternal"
      if (publicCoreClasspath.contains("/neutral-scalameta/target/") ||
          Vector("scalameta_3", "parsers_3", "trees_3", "semanticdb")
            .exists(publicCoreClasspath.contains))
        hiddenCycleViolations :+= "publicCoreExamples test -> neutralScalameta or Scalameta"
      if (Vector("scala3-compiler", "scala3-interfaces", "tasty-core", "scala-asm")
          .exists(publicCoreClasspath.contains))
        hiddenCycleViolations :+= "publicCoreExamples test -> compiler implementation"
      if (hiddenCycleViolations.nonEmpty) {
        sys.error(
          "Forbidden project/classpath edge(s):\n" + hiddenCycleViolations.mkString("\n")
        )
      }

      val coreProduction = (core / Compile / sources).value
      val neutralProduction = (neutralScalameta / Compile / sources).value
      val frontendProduction = (frontend / Compile / sources).value
      val backendProduction = (dottyInternal / Compile / sources).value
      val hybridProduction = (hybridScalametaFrontend / Compile / sources).value
      val misplacedSources =
        coreProduction.filterNot(_.getAbsolutePath.contains("/core/src/main/")) ++
          neutralProduction.filterNot(_.getAbsolutePath.contains("/neutral-scalameta/src/main/")) ++
          frontendProduction.filterNot(_.getAbsolutePath.contains("/frontend/src/main/")) ++
          backendProduction.filterNot(_.getAbsolutePath.contains("/dotty-internal/src/main/")) ++
          hybridProduction.filterNot(_.getAbsolutePath.contains("/hybrid-scalameta-frontend/src/main/"))
      if (misplacedSources.nonEmpty) {
        sys.error(
          "Hidden production source reuse detected:\n" +
            misplacedSources.map(_.getAbsolutePath).mkString("\n")
        )
      }

      val rootJar = (Compile / packageBin).value
      val archive = new JarFile(rootJar)
      try {
        val packagedClasses = archive.entries().asScala
          .map(_.getName)
          .filter(name => name.endsWith(".class") || name.endsWith(".tasty"))
          .toVector
        if (packagedClasses.nonEmpty) {
          sys.error(
            "Aggregate root packages production classes:\n" +
              packagedClasses.mkString("\n")
          )
        }
      } finally archive.close()
      log.info(
        "Module graph verified: neutralScalameta -> core, dottyInternal -> neutralScalameta -> core, frontend -> core, " +
          "hybridScalametaFrontend -> frontend + neutralScalameta, " +
          "publicCoreExamples -> core without compiler implementation, " +
          "core/neutralScalameta/frontend/dottyInternal/hybridScalametaFrontend production source roots are owned, " +
          "no sibling compile/test classpath edges or hidden production source reuse, " +
          "aggregate root packages no classes"
      )
    },
    verifyScalametaArtifactTopology := {
      val log = streams.value.log
      val line = scalaVersion.value
      if (!supportedScalaVersions.contains(line)) {
        sys.error(
          s"Unsupported candidate Scala line $line; expected one of ${supportedScalaVersions.mkString(", ")}"
        )
      }
      if (binaryArtifactBuildScalaVersion != supportedScalaVersions.head) {
        sys.error(
          "Binary-cross candidate artifacts must be built with the oldest supported Scala line."
        )
      }
      val neutralPom = IO.read((neutralScalameta / Compile / makePom).value)
      val frontendPom = IO.read((hybridScalametaFrontend / Compile / makePom).value)
      val backendPom = IO.read((dottyInternal / Compile / makePom).value)

      def requirePom(pom: String, label: String, tokens: Seq[String]): Unit = {
        val missing = tokens.filterNot(pom.contains)
        if (missing.nonEmpty) {
          sys.error(s"$label POM missing: ${missing.mkString(", ")}")
        }
      }

      requirePom(
        neutralPom,
        "neutral Scalameta",
        Seq(
          "<artifactId>quasiquotes-scala3-neutral-scalameta_3</artifactId>",
          "<artifactId>quasiquotes-scala3-core_3</artifactId>",
          "<artifactId>scalameta_3</artifactId>",
          "<version>4.17.3</version>",
          "<name>Apache-2.0</name>"
        )
      )
      requirePom(
        frontendPom,
        "Scalameta frontend",
        Seq(
          s"<artifactId>quasiquotes-scala3-scalameta-frontend_$line</artifactId>",
          s"<artifactId>quasiquotes-scala3-frontend_$line</artifactId>",
          "<artifactId>quasiquotes-scala3-neutral-scalameta_3</artifactId>",
          "<name>Apache-2.0</name>"
        )
      )
      requirePom(
        backendPom,
        "exact backend",
        Seq(
          s"<artifactId>quasiquotes-scala3-dotty-internal_$line</artifactId>",
          "<artifactId>quasiquotes-scala3-neutral-scalameta_3</artifactId>",
          "<artifactId>scala3-compiler_3</artifactId>",
          s"<version>$line</version>",
          "<name>Apache-2.0</name>"
        )
      )

      val forbiddenPomTokens = Seq("ProjectRef", "target/scala-", "quasiquotes-scala3-control")
      val contaminated = Seq(
        "neutral" -> neutralPom,
        "frontend" -> frontendPom,
        "backend" -> backendPom
      ).flatMap { case (label, pom) =>
        forbiddenPomTokens.filter(pom.contains).map(token => s"$label:$token")
      }
      if (contaminated.nonEmpty) {
        sys.error("Candidate POM contamination: " + contaminated.mkString(", "))
      }

      val neutralRemotelySkipped = (neutralScalameta / publish / skip).value
      val frontendRemotelySkipped = (hybridScalametaFrontend / publish / skip).value
      val backendRemotelySkipped = (dottyInternal / publish / skip).value
      val expandedSkipValues = Vector(
        "neutralScalameta" -> neutralRemotelySkipped,
        "hybridScalametaFrontend" -> frontendRemotelySkipped,
        "dottyInternal" -> backendRemotelySkipped
      )
      val coreSkipped = (core / publish / skip).value
      val currentFrontendSkipped = (frontend / publish / skip).value
      val rootAndExampleSkipValues = Vector(
        "root" -> (publish / skip).value,
        "publicApiExamples" -> (publicApiExamples / publish / skip).value,
        "publicCoreExamples" -> (publicCoreExamples / publish / skip).value
      )
      if (coreSkipped || currentFrontendSkipped) {
        sys.error("Core and current-Dotty frontend must remain publish-enabled real modules.")
      }
      val wrongExpandedPolicy = expandedSkipValues.filter { case (_, skipped) =>
        if (expandedReleaseEnabled) skipped else !skipped
      }
      if (wrongExpandedPolicy.nonEmpty) {
        val expected = if (expandedReleaseEnabled) "publish-enabled" else "skipped"
        sys.error(
          s"Expanded modules must be $expected in the current mode: " +
            wrongExpandedPolicy.map(_._1).mkString(", ")
        )
      }
      val accidentallyPublishable = rootAndExampleSkipValues
        .filter(entry => !entry._2)
        .map(_._1)
      if (accidentallyPublishable.nonEmpty) {
        sys.error(
          "Root/example modules must remain skipped in every mode: " +
            accidentallyPublishable.mkString(", ")
        )
      }

      val packages = Seq(
        (neutralScalameta / Compile / packageBin).value,
        (neutralScalameta / Compile / packageSrc).value,
        (neutralScalameta / Compile / packageDoc).value,
        (hybridScalametaFrontend / Compile / packageBin).value,
        (hybridScalametaFrontend / Compile / packageSrc).value,
        (hybridScalametaFrontend / Compile / packageDoc).value,
        (dottyInternal / Compile / packageBin).value,
        (dottyInternal / Compile / packageSrc).value,
        (dottyInternal / Compile / packageDoc).value
      )
      val empty = packages.filter(file => !file.isFile || file.length == 0L)
      if (empty.nonEmpty) {
        sys.error("Missing or empty candidate package(s): " + empty.mkString(", "))
      }

      def jarEntries(file: File): Set[String] = {
        val archive = new JarFile(file)
        try archive.entries().asScala.map(_.getName).toSet
        finally archive.close()
      }

      val typedEntries = jarEntries((hybridScalametaFrontend / Compile / packageBin).value)
      val requiredTypedApi = Set(
        "quasiquotes/scalameta/TypeFrontend$.class",
        "quasiquotes/scalameta/TypeFrontend$Failure.class",
        "quasiquotes/scalameta/TypeFrontend$BuildResult.class",
        "quasiquotes/scalameta/TypeFrontend$CompileResult.class",
        "quasiquotes/scalameta/TypeFrontend$MatchResult.class",
        "quasiquotes/scalameta/ScalametaTypePatternExtractor.class"
      )
      val missingTypedApi = requiredTypedApi.diff(typedEntries)
      if (missingTypedApi.nonEmpty) {
        sys.error(
          "Scalameta typed artifact is missing public Type opt-in API entries: " +
            missingTypedApi.toVector.sorted.mkString(", ")
        )
      }

      val forbiddenTypeApiPrefixes = Vector(
        "quasiquotes/scalameta/TypeFrontend",
        "quasiquotes/scalameta/ScalametaTypePatternExtractor"
      )
      val nonTypedEntries = Seq(
        "core" -> jarEntries((core / Compile / packageBin).value),
        "frontend" -> jarEntries((frontend / Compile / packageBin).value),
        "neutral" -> jarEntries((neutralScalameta / Compile / packageBin).value)
      )
      val leakedTypeApi = nonTypedEntries.flatMap { case (label, entries) =>
        entries
          .filter(entry => forbiddenTypeApiPrefixes.exists(entry.startsWith))
          .toVector
          .sorted
          .map(entry => s"$label:$entry")
      }
      if (leakedTypeApi.nonEmpty) {
        sys.error("Type opt-in API leaked outside the typed artifact: " + leakedTypeApi.mkString(", "))
      }

      log.info(
        s"Scalameta artifact topology verified for Scala $line in " +
          s"${if (expandedReleaseEnabled) "explicit expanded-release" else "ordinary default"} mode: " +
          "neutral binary-cross, frontend/backend full-cross, " +
          "public Type opt-in API confined to the typed coordinate, truthful POM closure, binary/source/doc packages, " +
          "Apache-2.0 metadata, and fail-closed publication policy"
      )
    }
  )
