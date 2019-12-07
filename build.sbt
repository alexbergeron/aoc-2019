import Dependencies._

ThisBuild / scalaVersion     := "2.13.1"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "io.github.alexbergeron"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "saoc-2019",
    libraryDependencies += cats,
    libraryDependencies += fs2Io,
    libraryDependencies += scalaTest % Test
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
