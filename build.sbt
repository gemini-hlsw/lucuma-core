ThisBuild / tlBaseVersion                         := "0.45"
ThisBuild / tlCiReleaseBranches                   := Seq("master")
ThisBuild / githubWorkflowEnv += "MUNIT_FLAKY_OK" -> "true"
ThisBuild / scalacOptions += "-Xsource:3"

Global / concurrentRestrictions += Tags.limit(Tags.Compile, 1)

lazy val attoVersion           = "0.9.5"
lazy val catsVersion           = "2.8.0"
lazy val kindProjectorVersion  = "0.13.2"
lazy val monocleVersion        = "3.1.0"
lazy val scalaJavaTimeVersion  = "2.4.0"
lazy val geminiLocalesVersion  = "0.7.0"
lazy val jtsVersion            = "0.3.0"
lazy val coulombVersion        = "0.5.8"
lazy val spireVersion          = "0.18.0"
lazy val singletonOpsVersion   = "0.5.2"
lazy val refinedVersion        = "0.10.1"
lazy val catsTimeVersion       = "0.5.0"
lazy val circeVersion          = "0.14.2"
lazy val catsScalacheckVersion = "0.3.1"
lazy val shapelessVersion      = "2.3.9"

Global / onChangedBuildSource := ReloadOnSourceChanges

val root = tlCrossRootProject.aggregate(core, testkit, tests)

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("modules/core"))
  .settings(
    name := "lucuma-core",
    libraryDependencies ++= Seq(
      "org.tpolecat"   %%% "atto-core"                  % attoVersion,
      "org.tpolecat"   %%% "atto-refined"               % attoVersion,
      "org.typelevel"  %%% "cats-core"                  % catsVersion,
      "dev.optics"     %%% "monocle-core"               % monocleVersion,
      "dev.optics"     %%% "monocle-macro"              % monocleVersion,
      "dev.optics"     %%% "monocle-state"              % monocleVersion,
      "edu.gemini"     %%% "lucuma-jts"                 % jtsVersion,
      "com.manyangled" %%% "coulomb"                    % coulombVersion,
      "com.manyangled" %%% "coulomb-si-units"           % coulombVersion,
      "com.manyangled" %%% "coulomb-accepted-units"     % coulombVersion,
      "com.manyangled" %%% "coulomb-time-units"         % coulombVersion,
      "com.manyangled" %%% "coulomb-cats"               % coulombVersion,
      "com.manyangled" %%% "coulomb-refined"            % coulombVersion,
      "com.manyangled" %%% "coulomb-physical-constants" % coulombVersion,
      "org.typelevel"  %%% "spire"                      % spireVersion,
      "org.typelevel"  %%% "spire-extras"               % spireVersion,
      "eu.timepit"     %%% "singleton-ops"              % singletonOpsVersion,
      "eu.timepit"     %%% "refined"                    % refinedVersion,
      "eu.timepit"     %%% "refined-cats"               % refinedVersion,
      "org.typelevel"  %%% "cats-time"                  % catsTimeVersion,
      "io.circe"       %%% "circe-core"                 % circeVersion,
      "io.circe"       %%% "circe-generic"              % circeVersion,
      "io.circe"       %%% "circe-refined"              % circeVersion,
      "com.chuusai"    %%% "shapeless"                  % shapelessVersion
    )
  )
  .jvmConfigure(_.enablePlugins(AutomateHeaderPlugin))
  .jvmSettings(
    libraryDependencies ++= Seq(
      "edu.gemini" %%% "lucuma-jts-awt" % jtsVersion
    )
  )
  .jsSettings(
    libraryDependencies ++= Seq(
      "io.github.cquiroz" %%% "scala-java-time" % scalaJavaTimeVersion,
      "edu.gemini"        %%% "gemini-locales"  % geminiLocalesVersion
    )
  )

lazy val testkit = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/testkit"))
  .dependsOn(core)
  .settings(
    name := "lucuma-core-testkit",
    libraryDependencies ++= Seq(
      "org.typelevel"     %%% "cats-testkit"       % catsVersion,
      "com.manyangled"    %%% "coulomb-scalacheck" % coulombVersion,
      "dev.optics"        %%% "monocle-law"        % monocleVersion,
      "org.typelevel"     %%% "spire-laws"         % spireVersion,
      "eu.timepit"        %%% "refined-scalacheck" % refinedVersion,
      "io.circe"          %%% "circe-testing"      % circeVersion,
      "io.chrisdavenport" %%% "cats-scalacheck"    % catsScalacheckVersion
    )
  )
  .jvmConfigure(_.enablePlugins(AutomateHeaderPlugin))

val MUnitFramework = new TestFramework("munit.Framework")
val MUnitFlakyOK   = sys.env.get("MUNIT_FLAKY_OK") match {
  case Some("true") => Tests.Argument(MUnitFramework, "--exclude-tags=ScalaCheckFlaky")
  case _            => Tests.Argument()
}

lazy val tests = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("modules/tests"))
  .enablePlugins(NoPublishPlugin)
  .dependsOn(testkit)
  .settings(
    name := "lucuma-core-tests",
    libraryDependencies ++= Seq(
      "org.scalameta" %%% "munit"            % "0.7.29" % Test,
      "org.typelevel" %%% "discipline-munit" % "1.0.9"  % Test
    ),
    testFrameworks += MUnitFramework,
    testOptions += MUnitFlakyOK
  )
  .jvmConfigure(_.enablePlugins(AutomateHeaderPlugin))
  .jvmSettings(
    resolvers += "Gemini Repository".at(
      "https://github.com/gemini-hlsw/maven-repo/raw/master/releases"
    ),
    libraryDependencies ++= Seq(
      "edu.gemini.ocs" %% "edu-gemini-util-skycalc"     % "2020001.1.7" % Test,
      "com.47deg"      %% "scalacheck-toolbox-datetime" % "0.6.0"       % Test
    )
  )

lazy val benchmarks = project
  .in(file("modules/benchmarks"))
  .dependsOn(core.jvm)
  .settings(name := "lucuma-core-benchmarks")
  .enablePlugins(NoPublishPlugin, JmhPlugin)
