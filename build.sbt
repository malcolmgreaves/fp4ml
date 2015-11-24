organization := "io.malcolmgreaves"

name := "bigmlbook"

version := "0.0.0"

////////////////////////////////////////////
//    dependencies and their resolvers    //
////////////////////////////////////////////

resolvers ++= Seq(
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
)

libraryDependencies ++= Seq(
  "org.apache.spark"  %% "spark-core"    % "1.4.0",
  "org.scalanlp"      %% "breeze"        % "0.11.2",
  "com.quantifind"    %% "wisp"          % "0.0.4",
  "io.malcolmgreaves" %% "abstract_data" % "0.0.2",
  "org.scalatest"     %% "scalatest"     % "2.2.4" % Test
)

///////////////////////////////////////////////////////////////////////////////

///////////////////////////////////////////////////////
//    scala compilation and java runtime settings    //
///////////////////////////////////////////////////////

// as of 1.4.0, spark still requires jvm 1.7
lazy val jvm = "1.7"

scalaVersion := "2.11.7"

crossScalaVersions := Seq("2.10.5", "2.11.7")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)
scalacOptions ++= Seq(
  "-optimise",
  "-Xfatal-warnings",
  "-Xlint",
  "-Xfuture",
  s"-target:jvm-$jvm",
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-unchecked",
  "-language:postfixOps",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-language:experimental.macros",
  "-Ybackend:GenBCode",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-value-discard",
  "-Ywarn-infer-any",
  "-Yinline",
  "-Yinline-handlers",
  //"-Yopt-warnings",
  "-Yopt:_"
)

///////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////
//    testing and code coverage settings    //
//////////////////////////////////////////////

fork in Test := true

parallelExecution in Test := false

testOptions += Tests.Argument(TestFrameworks.JUnit, "-v")

///////////////////////////////////////////////////////////////////////////////

//////////////////////////////////////////////////
//   Code formatting settings for scalariform   //
//////////////////////////////////////////////////

defaultScalariformSettings

ScalariformKeys.preferences := {
  import scalariform.formatter.preferences._
  FormattingPreferences()
    .setPreference(AlignParameters, true)
    .setPreference(AlignSingleLineCaseStatements, true)
    .setPreference(CompactControlReadability, false)
    .setPreference(CompactStringConcatenation, true)
    .setPreference(DoubleIndentClassDeclaration, true)
    .setPreference(FormatXml, true)
    .setPreference(IndentLocalDefs, true)
    .setPreference(IndentPackageBlocks, true)
    .setPreference(IndentSpaces, 2)
    .setPreference(MultilineScaladocCommentsStartOnFirstLine, false)
    .setPreference(PreserveDanglingCloseParenthesis, true)
    .setPreference(PreserveSpaceBeforeArguments, false)
    .setPreference(RewriteArrowSymbols, false)
    .setPreference(SpaceBeforeColon, false)
    .setPreference(SpaceInsideBrackets, false)
    .setPreference(SpacesWithinPatternBinders, true)
}

///////////////////////////////////////////////////////////////////////////////
