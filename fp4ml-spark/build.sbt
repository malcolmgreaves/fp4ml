name := "fp4ml-spark"

import SharedBuild._

addCompilerPlugin(scalaMacros)

libraryDependencies ++=
  fp4mlSparkDeps ++
    testDeps

fork in run := false

pomExtra := pomExtraInfo
