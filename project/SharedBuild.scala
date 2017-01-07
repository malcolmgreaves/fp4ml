import sbt._
import Keys._

object SharedBuild {

  // // // // // // // //
  // //   Versions  // //
  // // // // // // // //

  lazy val breezeV      = "0.12"
  lazy val nakV         = "1.3"
  lazy val dataTcV      = "0.0.0"
  lazy val scalaMacrosV = "2.1.0"
  lazy val avroCgV      = "0.3.4"
  lazy val shapelessV   = "2.2.5"
  lazy val wispV        = "0.0.4"
  lazy val argonautV    = "6.1"
  lazy val scalajV      = "2.2.1"

  // // // // // // // // // //
  // //    Dependencies   // //
  // // // // // // // // // //

  lazy val scalaMacros = 
    "org.scalamacros" % "paradise" % scalaMacrosV cross CrossVersion.full

  lazy val fp4mlMainDeps = Seq(
    "org.scalanlp"      %% "breeze"               % breezeV,
    "org.scalanlp"      %% "breeze-natives"       % breezeV,
    "org.scalanlp"      %% "nak"                  % nakV,
    "com.quantifind"    %% "wisp"                 % wispV,
    // [B] necessary?
    "io.argonaut"       %% "argonaut"             % argonautV,
    "org.scalaj"        %% "scalaj-http"          % scalajV,
    // [E] necessary?
    "com.chuusai"       %% "shapeless"            % shapelessV,
    "com.gonitro"       %% "avro-codegen-runtime" % avroCgV,
    "io.malcolmgreaves" %% "data-tc-extra"        % dataTcV,
    "io.malcolmgreaves" %% "data-tc-scala"        % dataTcV
  )

  lazy val fp4mlSparkDeps = Seq(
    "io.malcolmgreaves" %% "data-tc-spark" % dataTcV
  )

  lazy val testDeps = Seq(
    "org.scalatest" %% "scalatest" % "2.2.6" % Test
  )

}
