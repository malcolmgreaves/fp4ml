name := "fp4ml-spark"

import SharedBuild._

com.typesafe.sbt.SbtScalariform.defaultScalariformSettings
ScalariformKeys.preferences := sharedCodeFmt

addCompilerPlugin(scalaMacros)

libraryDependencies := 
  fp4mlSparkDeps ++
  testDeps

fork in run := false
