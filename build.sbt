import sbtcrossproject.CrossType

lazy val attoVersion                 = "0.9.3"
lazy val catsVersion                 = "2.5.0"
lazy val kindProjectorVersion        = "0.11.3"
lazy val monocleVersion              = "2.1.0"
lazy val catsTestkitScalaTestVersion = "2.1.3"
lazy val scalaJavaTimeVersion        = "2.2.0"
lazy val geminiLocalesVersion        = "0.5.1"
lazy val jtsVersion                  = "0.1.2"
lazy val svgdotjsVersion             = "0.0.4"
lazy val coulombVersion              = "0.5.6"
lazy val spireVersion                = "0.17.0"
lazy val singletonOpsVersion         = "0.5.2"
lazy val refinedVersion              = "0.9.23"
lazy val catsTimeVersion             = "0.3.4"
lazy val circeVersion                = "0.13.0"
lazy val catsScalacheckVersion       = "0.3.0"

inThisBuild(
  Seq(
    homepage := Some(url("https://github.com/gemini-hlsw/lucuma-core")),
    addCompilerPlugin(
      ("org.typelevel" % "kind-projector" % kindProjectorVersion).cross(CrossVersion.full)
    ),
    Global / onChangedBuildSource := ReloadOnSourceChanges,
    scalacOptions += "-Ymacro-annotations"
  ) ++ lucumaPublishSettings
)

skip in publish := true

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("modules/core"))
  .settings(
    name := "lucuma-core",
    libraryDependencies ++= Seq(
      "org.tpolecat"               %%% "atto-core"                  % attoVersion,
      "org.tpolecat"               %%% "atto-refined"               % attoVersion,
      "org.typelevel"              %%% "cats-core"                  % catsVersion,
      "com.github.julien-truffaut" %%% "monocle-core"               % monocleVersion,
      "com.github.julien-truffaut" %%% "monocle-macro"              % monocleVersion,
      "com.github.julien-truffaut" %%% "monocle-state"              % monocleVersion,
      "edu.gemini"                 %%% "lucuma-jts"                 % jtsVersion,
      "com.manyangled"             %%% "coulomb"                    % coulombVersion,
      "com.manyangled"             %%% "coulomb-si-units"           % coulombVersion,
      "com.manyangled"             %%% "coulomb-accepted-units"     % coulombVersion,
      "com.manyangled"             %%% "coulomb-time-units"         % coulombVersion,
      "com.manyangled"             %%% "coulomb-cats"               % coulombVersion,
      "com.manyangled"             %%% "coulomb-refined"            % coulombVersion,
      "com.manyangled"             %%% "coulomb-physical-constants" % coulombVersion,
      "org.typelevel"              %%% "spire"                      % spireVersion,
      "eu.timepit"                 %%% "singleton-ops"              % singletonOpsVersion,
      "eu.timepit"                 %%% "refined"                    % refinedVersion,
      "eu.timepit"                 %%% "refined-cats"               % refinedVersion,
      "io.chrisdavenport"          %%% "cats-time"                  % catsTimeVersion,
      "io.circe"                   %%% "circe-core"                 % circeVersion,
      "io.circe"                   %%% "circe-refined"              % circeVersion
    )
  )
  .jvmConfigure(_.enablePlugins(AutomateHeaderPlugin))
  .jvmSettings(
    libraryDependencies ++= Seq(
      "edu.gemini" %%% "lucuma-jts-awt" % jtsVersion
    )
  )
  .jsSettings(lucumaScalaJsSettings: _*)
  .jsSettings(
    libraryDependencies ++= Seq(
      "io.github.cquiroz" %%% "scala-java-time" % scalaJavaTimeVersion,
      "edu.gemini"        %%% "gemini-locales"  % geminiLocalesVersion,
      "edu.gemini"        %%% "lucuma-svgdotjs" % svgdotjsVersion
    )
  )

lazy val testkit = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/testkit"))
  .dependsOn(core)
  .settings(
    name := "lucuma-core-testkit",
    libraryDependencies ++= Seq(
      "org.typelevel"              %%% "cats-testkit"           % catsVersion,
      "org.typelevel"              %%% "cats-testkit-scalatest" % catsTestkitScalaTestVersion,
      "com.manyangled"             %%% "coulomb-scalacheck"     % coulombVersion,
      "com.github.julien-truffaut" %%% "monocle-law"            % monocleVersion,
      "org.typelevel"              %%% "spire-laws"             % spireVersion,
      "eu.timepit"                 %%% "refined-scalacheck"     % refinedVersion,
      "io.circe"                   %%% "circe-testing"          % circeVersion,
      "io.chrisdavenport"          %%% "cats-scalacheck"        % catsScalacheckVersion
    )
  )
  .jvmConfigure(_.enablePlugins(AutomateHeaderPlugin))
  .jsSettings(lucumaScalaJsSettings: _*)

lazy val tests = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("modules/tests"))
  .dependsOn(testkit)
  .settings(
    name := "lucuma-core-tests",
    skip in publish := true,
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit"            % "0.7.23" % Test,
      "org.typelevel" %%% "discipline-munit" % "1.0.7"  % Test
    ),
    testFrameworks += new TestFramework("munit.Framework"),
    scalaJSUseMainModuleInitializer := true
  )
  .jvmConfigure(_.enablePlugins(AutomateHeaderPlugin))
  .jvmSettings(
    resolvers += "Gemini Repository".at(
      "https://github.com/gemini-hlsw/maven-repo/raw/master/releases"
    ),
    libraryDependencies ++= Seq(
      "edu.gemini.ocs" %% "edu-gemini-util-skycalc"     % "2020001.1.7" % Test,
      "com.47deg"      %% "scalacheck-toolbox-datetime" % "0.5.0"       % Test
    )
  )
  .jsSettings(lucumaScalaJsSettings: _*)
  .jsSettings(scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule)))
