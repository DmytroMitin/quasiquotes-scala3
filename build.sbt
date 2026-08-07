import java.nio.charset.StandardCharsets
import java.util.jar.JarFile
import scala.collection.JavaConverters._

ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.8.4"

lazy val munitVersion = "1.2.4"

lazy val verifyCoreBoundary = taskKey[Unit](
  "Verify core source, classpath, artifact, and TASTy compiler freedom"
)
lazy val verifyModuleGraph = taskKey[Unit](
  "Verify the selected project dependency graph and aggregate-root packaging"
)

lazy val commonSettings = Seq(
  libraryDependencies += "org.scalameta" %% "munit" % munitVersion % Test
)

lazy val core = (project in file("core"))
  .settings(commonSettings)
  .settings(
    name := "quasiquotes-scala3-core",
    verifyCoreBoundary := {
      val log = streams.value.log
      val forbiddenSourceTokens = Vector("scala.quoted", "dotty.tools.dotc")
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

      val forbiddenClasspathTokens =
        Vector("scala3-compiler", "scala3-interfaces", "tasty-core", "scala-asm")
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
      val pomViolations = forbiddenClasspathTokens.filter(pomText.contains)
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
  .settings(
    name := "quasiquotes-scala3-frontend",
    libraryDependencies +=
      "org.scala-lang" %% "scala3-compiler" % scalaVersion.value
  )

lazy val dottyInternal = (project in file("dotty-internal"))
  .dependsOn(core % "compile->compile;test->test")
  .settings(commonSettings)
  .settings(
    name := "quasiquotes-scala3-dotty-internal",
    libraryDependencies +=
      "org.scala-lang" %% "scala3-compiler" % scalaVersion.value,
    publish / skip := true
  )

lazy val publicApiExamples = (project in file("public-api-examples"))
  .dependsOn(frontend)
  .settings(commonSettings)
  .settings(
    name := "quasiquotes-scala3-public-api-examples",
    publish / skip := true,
    Compile / sources := Nil
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
  .aggregate(core, frontend, dottyInternal)
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
      val publicCoreClasspath =
        (publicCoreExamples / Test / fullClasspath).value
          .map(_.data.getAbsolutePath.replace('\\', '/')).mkString("\n")
      var hiddenCycleViolations = Vector.empty[String]
      if (frontendCompile.contains("/dotty-internal/target/"))
        hiddenCycleViolations :+= "frontend compile -> dottyInternal"
      if (frontendTest.contains("/dotty-internal/target/"))
        hiddenCycleViolations :+= "frontend test -> dottyInternal"
      if (backendCompile.contains("/frontend/target/"))
        hiddenCycleViolations :+= "dottyInternal compile -> frontend"
      if (backendTest.contains("/frontend/target/"))
        hiddenCycleViolations :+= "dottyInternal test -> frontend"
      if (publicCoreClasspath.contains("/frontend/target/"))
        hiddenCycleViolations :+= "publicCoreExamples test -> frontend"
      if (publicCoreClasspath.contains("/dotty-internal/target/"))
        hiddenCycleViolations :+= "publicCoreExamples test -> dottyInternal"
      if (Vector("scala3-compiler", "scala3-interfaces", "tasty-core", "scala-asm")
          .exists(publicCoreClasspath.contains))
        hiddenCycleViolations :+= "publicCoreExamples test -> compiler implementation"
      if (hiddenCycleViolations.nonEmpty) {
        sys.error(
          "Forbidden project/classpath edge(s):\n" + hiddenCycleViolations.mkString("\n")
        )
      }

      val coreProduction = (core / Compile / sources).value
      val frontendProduction = (frontend / Compile / sources).value
      val backendProduction = (dottyInternal / Compile / sources).value
      val misplacedSources =
        coreProduction.filterNot(_.getAbsolutePath.contains("/core/src/main/")) ++
          frontendProduction.filterNot(_.getAbsolutePath.contains("/frontend/src/main/")) ++
          backendProduction.filterNot(_.getAbsolutePath.contains("/dotty-internal/src/main/"))
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
        "Module graph verified: frontend -> core, dottyInternal -> core, " +
          "publicCoreExamples -> core without compiler implementation, " +
          "core/frontend/dottyInternal production source roots are owned, " +
          "no sibling compile/test classpath edges or hidden production source reuse, " +
          "aggregate root packages no classes"
      )
    }
  )
