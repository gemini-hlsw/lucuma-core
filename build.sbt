import sbtcrossproject.CrossType

lazy val attoVersion                 = "0.8.0"
lazy val catsVersion                 = "2.1.1"
lazy val catsEffectVersion           = "2.1.4"
lazy val collCompatVersion           = "2.1.6"
lazy val kindProjectorVersion        = "0.11.0"
lazy val monocleVersion              = "2.1.0"
lazy val catsTestkitScalaTestVersion = "2.0.0"
lazy val scalaJavaTimeVersion        = "2.0.0"
lazy val geminiLocalesVersion        = "0.5.0"
lazy val jtsVersion                  = "0.0.9"
lazy val svgdotjsVersion             = "0.0.1"
lazy val newType                     = "0.4.4"
lazy val coulombVersion              = "0.5.0"
lazy val spireVersion                = "0.17.0-RC1"
lazy val singletonOpsVersion         = "0.5.1"
lazy val refinedVersion              = "0.9.15"
lazy val catsTimeVersion             = "0.3.4"

inThisBuild(
  Seq(
    homepage := Some(url("https://github.com/gemini-hlsw/gsp-math")),
    addCompilerPlugin(
      ("org.typelevel" % "kind-projector" % kindProjectorVersion).cross(CrossVersion.full)
    ),
    Global / onChangedBuildSource := ReloadOnSourceChanges,
    scalacOptions += "-Ymacro-annotations"
  ) ++ gspPublishSettings
)

skip in publish := true

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("modules/core"))
  .settings(
    name := "lucuma-core",
    libraryDependencies ++= Seq(
      "org.tpolecat"               %%% "atto-core"               % attoVersion,
      "org.typelevel"              %%% "cats-core"               % catsVersion,
      "org.typelevel"              %%% "cats-effect"             % catsEffectVersion,
      "com.github.julien-truffaut" %%% "monocle-core"            % monocleVersion,
      "com.github.julien-truffaut" %%% "monocle-macro"           % monocleVersion,
      "org.scala-lang.modules"     %%% "scala-collection-compat" % collCompatVersion,
      "edu.gemini"                 %%% "gpp-jts"                 % jtsVersion,
      "com.manyangled"             %%% "coulomb"                 % coulombVersion,
      "com.manyangled"             %%% "coulomb-si-units"        % coulombVersion,
      "com.manyangled"             %%% "coulomb-accepted-units"  % coulombVersion,
      "com.manyangled"             %%% "coulomb-time-units"      % coulombVersion,
      "com.manyangled"             %%% "coulomb-cats"            % coulombVersion,
      "com.manyangled"             %%% "coulomb-refined"         % coulombVersion,
      "org.typelevel"              %%% "spire"                   % spireVersion,
      "eu.timepit"                 %%% "singleton-ops"           % singletonOpsVersion,
      "eu.timepit"                 %%% "refined"                 % refinedVersion,
      "eu.timepit"                 %%% "refined-cats"            % refinedVersion,
      "io.chrisdavenport"          %%% "cats-time"               % catsTimeVersion
    )
  )
  .jvmConfigure(_.enablePlugins(AutomateHeaderPlugin))
  .jvmSettings(
    libraryDependencies ++= Seq(
      "edu.gemini" %%% "gpp-jts-awt" % jtsVersion
    )
  )
  .jsSettings(gspScalaJsSettings: _*)
  .jsSettings(
    libraryDependencies ++= Seq(
      "io.github.cquiroz" %%% "scala-java-time" % scalaJavaTimeVersion,
      "edu.gemini"        %%% "gemini-locales"  % geminiLocalesVersion,
      "edu.gemini"        %%% "gpp-svgdotjs"    % svgdotjsVersion
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
      "eu.timepit"                 %%% "refined-scalacheck"     % refinedVersion
    )
  )
  .jvmConfigure(_.enablePlugins(AutomateHeaderPlugin))
  .jsSettings(gspScalaJsSettings: _*)

lazy val tests = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("modules/tests"))
  .dependsOn(testkit)
  .settings(
    name := "lucuma-core-tests",
    skip in publish := true,
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit"            % "0.7.11" % Test,
      "org.typelevel" %%% "discipline-munit" % "0.2.3"  % Test
    ),
    testFrameworks += new TestFramework("munit.Framework"),
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule))
  )
  .jvmConfigure(_.enablePlugins(AutomateHeaderPlugin))
  .jvmSettings(
    resolvers += "Gemini Repository".at(
      "https://github.com/gemini-hlsw/maven-repo/raw/master/releases"
    ),
    libraryDependencies ++= Seq(
      "edu.gemini.ocs" %% "edu-gemini-util-skycalc"     % "2020001.1.7" % Test,
      "com.47deg"      %% "scalacheck-toolbox-datetime" % "0.3.5"       % Test
    )
  )
  .jsSettings(gspScalaJsSettings: _*)
