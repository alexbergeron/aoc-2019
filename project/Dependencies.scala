import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.8"
  lazy val cats = "org.typelevel" %% "cats-core" % "2.0.0"
  lazy val catsEffect = "org.typelevel" %% "cats-effect" % "2.0.0"
  lazy val fs2 = "co.fs2" %% "fs2-core" % "2.1.0"
  lazy val fs2Io = "co.fs2" %% "fs2-io" % "2.1.0"
}
