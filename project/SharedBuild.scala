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

  lazy val pomExtraInfo = {
    <url>https://github.com/malcolmgreaves/fp4ml</url>
    <licenses>
      <license>
        <name>Apache 2.0</name>
        <url>https://www.apache.org/licenses/LICENSE-2.0.txt</url>
        <distribution>repo</distribution>
      </license>
    </licenses>
    <scm>
      <url>git@github.com:malcolmgreaves/fp4ml.git</url>
      <connection>scm:git@github.com:malcolmgreaves/fp4ml.git</connection>
    </scm>
    <developers>
      <developer>
        <id>malcolmgreaves</id>
        <name>Malcolm Greaves</name>
        <email>greaves.malcolm@gmail.com</email>
        <url>https://malcolmgreaves.io/</url>
      </developer>
    </developers>
 }

}
