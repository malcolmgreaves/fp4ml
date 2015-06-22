name := "bigmlbook"

version := "0.1.0"

organization := "io.malcolmgreaves"

scalaVersion := "2.11.6"

val jvm = "1.7"

// code coverage plugins

resolvers ++= Seq(
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "Akka Repository" at "http://repo.akka.io/releases/",
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
)

libraryDependencies ++= Seq(
  "org.apache.spark" %% "spark-core" % "1.3.0",
  "org.scalatest" %% "scalatest" % "2.2.1" % "test"
)

testOptions += Tests.Argument(TestFrameworks.JUnit, "-v")

scalacOptions ++= Seq(
  //"-optimize",
  s"-target:jvm-$jvm",
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:experimental.macros",
  "-unchecked",
  "-Xfatal-warnings",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-value-discard",
  "-Xfuture",
  "-Yinline-warnings"
)

instrumentSettings

CoverallsPlugin.coverallsSettings

fork in Test := true

parallelExecution in Test := false

