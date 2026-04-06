ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.3.3"

lazy val root = (project in file("."))
  .settings(
    name := "quasiquotes-scala3",
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.2" % Test
  )
