organization := "io.malcolmgreaves"

name := "mlbigbook"

version := "0.0.1"

scalaVersion := "2.11.6"

crossScalaVersions := Seq("2.10.5", "2.11.6")

resolvers ++= Seq(
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
)

libraryDependencies ++= Seq(
  "org.spire-math" %% "spire" % "0.9.1",
  "org.scalanlp" %% "breeze" % "0.11.2",
  "org.apache.spark" %% "spark-core" % "1.4.0",
  "org.scalatest" %% "scalatest" % "2.2.4" % Test
)

testOptions += Tests.Argument(TestFrameworks.JUnit, "-v")

instrumentSettings

CoverallsPlugin.coverallsSettings

