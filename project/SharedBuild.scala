import sbt._
import Keys._

object SharedBuild {

  // // // // // // // //
  // //   Versions  // //
  // // // // // // // //

  lazy val breezeV = "0.12"
  lazy val dataV   = "1.0.0"

  // // // // // // // // // //
  // //    Dependencies   // //
  // // // // // // // // // //

  lazy val scalaMacros = "org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full

  lazy val avroCodegen = "com.gonitro" %% "avro-codegen-runtime" % "0.3.4"
  lazy val shapeless   = "com.chuusai" %% "shapeless"            % "2.2.5"

  lazy val miscDeps = Seq(
    "io.malcolmgreaves"    %% "abstract_data" % dataV,
    "io.argonaut"          %% "argonaut"      % "6.1",
    "org.scalaj"           %% "scalaj-http"   % "2.2.1"
  ) :+ shapeless

  lazy val mathMlDeps = Seq(
    "org.scalanlp"   %% "breeze"         % breezeV,
    "org.scalanlp"   %% "breeze-natives" % breezeV,
    "org.scalanlp"   %% "nak"            % "1.3",
    "com.quantifind" %% "wisp"           % "0.0.4"
  )

  lazy val testDeps = Seq(
    "org.scalatest" %% "scalatest" % "2.2.6" % Test
  )

  //////////////////////////////////////////////////
  //   Code formatting settings for scalariform   //
  //////////////////////////////////////////////////

  lazy val sharedCodeFmt = {
    import scalariform.formatter.preferences._
    FormattingPreferences()
      .setPreference(AlignParameters,                            true  )
      .setPreference(AlignSingleLineCaseStatements,              true  )
      .setPreference(CompactControlReadability,                  false )
      .setPreference(CompactStringConcatenation,                 true  )
      .setPreference(DoubleIndentClassDeclaration,               true  )
      .setPreference(FormatXml,                                  true  )
      .setPreference(IndentLocalDefs,                            true  )
      .setPreference(IndentPackageBlocks,                        true  )
      .setPreference(IndentSpaces,                               2     )
      .setPreference(MultilineScaladocCommentsStartOnFirstLine,  false )
      .setPreference(PreserveDanglingCloseParenthesis,           true  )
      .setPreference(PreserveSpaceBeforeArguments,               false )
      .setPreference(RewriteArrowSymbols,                        false )
      .setPreference(SpaceBeforeColon,                           false )
      .setPreference(SpaceInsideBrackets,                        false )
      .setPreference(SpacesWithinPatternBinders,                 true  )
  }

}