import org.scalajs.linker.interface.ESVersion

ThisBuild / tlBaseVersion                         := "0.118"
ThisBuild / tlCiReleaseBranches                   := Seq("master")
ThisBuild / githubWorkflowEnv += "MUNIT_FLAKY_OK" -> "true"

ThisBuild / tlCiReleaseBranches += "topic/scala3"

Global / concurrentRestrictions += Tags.limit(Tags.Compile, 1)

ThisBuild / crossScalaVersions := Seq("3.3.5")
ThisBuild / scalacOptions += "-language:implicitConversions" // TODO

lazy val catsVersion           = "2.13.0"
lazy val kindProjectorVersion  = "0.13.2"
lazy val monocleVersion        = "3.3.0"
lazy val scalaJavaTimeVersion  = "2.6.0"
lazy val geminiLocalesVersion  = "0.7.0"
lazy val jtsVersion            = "0.4.0"
lazy val coulombVersion        = "0.8.0"
lazy val spireVersion          = "0.18.0"
lazy val refinedVersion        = "0.11.3"
lazy val lucumaRefinedVersion  = "0.1.3"
lazy val catsTimeVersion       = "0.5.1"
lazy val circeVersion          = "0.14.10"
lazy val circeRefinedVersion   = "0.15.1"
lazy val catsScalacheckVersion = "0.3.2"
lazy val catsParseVersion      = "1.1.0"
lazy val kittensVersion        = "3.5.0"
lazy val scalajsStubVersion    = "1.1.0"

Global / onChangedBuildSource := ReloadOnSourceChanges

val root = tlCrossRootProject.aggregate(core, testkit, tests)

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("modules/core"))
  .settings(
    name := "lucuma-core",
    libraryDependencies ++= Seq(
      "org.typelevel"  %%% "cats-parse"     % catsParseVersion,
      "org.typelevel"  %%% "cats-core"      % catsVersion,
      "dev.optics"     %%% "monocle-core"   % monocleVersion,
      "dev.optics"     %%% "monocle-macro"  % monocleVersion,
      "dev.optics"     %%% "monocle-state"  % monocleVersion,
      "edu.gemini"     %%% "lucuma-jts"     % jtsVersion,
      "com.manyangled" %%% "coulomb-core"   % coulombVersion,
      "com.manyangled" %%% "coulomb-spire"  % coulombVersion,
      "com.manyangled" %%% "coulomb-units"  % coulombVersion,
      "org.typelevel"  %%% "spire"          % spireVersion,
      "org.typelevel"  %%% "spire-extras"   % spireVersion,
      "eu.timepit"     %%% "refined"        % refinedVersion,
      "eu.timepit"     %%% "refined-cats"   % refinedVersion,
      "edu.gemini"     %%% "lucuma-refined" % lucumaRefinedVersion,
      "org.typelevel"  %%% "cats-time"      % catsTimeVersion,
      "org.typelevel"  %%% "kittens"        % kittensVersion,
      "io.circe"       %%% "circe-core"     % circeVersion,
      "io.circe"       %%% "circe-refined"  % circeRefinedVersion
    )
  )
  .jvmConfigure(_.enablePlugins(AutomateHeaderPlugin))
  .jvmSettings(
    libraryDependencies ++= Seq(
      "edu.gemini"   %% "lucuma-jts-awt" % jtsVersion,
      "org.scala-js" %% "scalajs-stubs"  % scalajsStubVersion
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
      "dev.optics"        %%% "monocle-law"        % monocleVersion,
      "org.typelevel"     %%% "spire-laws"         % spireVersion,
      "eu.timepit"        %%% "refined-scalacheck" % refinedVersion,
      "io.circe"          %%% "circe-testing"      % circeVersion,
      "com.manyangled"    %%% "coulomb-testkit"    % coulombVersion,
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
      "org.scalameta" %%% "munit"            % "1.1.0" % Test,
      "org.typelevel" %%% "discipline-munit" % "2.0.0" % Test
    ),
    testFrameworks += MUnitFramework,
    testOptions += MUnitFlakyOK
  )
  .jvmConfigure(_.enablePlugins(AutomateHeaderPlugin))
  .jvmSettings(
    resolvers += "Gemini Repository".at(
      "https://github.com/gemini-hlsw/maven-repo/raw/master/releases"
    ),
    run / fork := true,
    libraryDependencies ++= Seq(
      ("edu.gemini.ocs" %% "edu-gemini-util-skycalc"     % "2020001.1.7" % Test)
        .cross(CrossVersion.for3Use2_13)
        .exclude("org.scala-lang.modules", "scala-xml_2.13"),
      "com.47deg"       %% "scalacheck-toolbox-datetime" % "0.7.0"       % Test
    )
  )

lazy val benchmarks = project
  .in(file("modules/benchmarks"))
  .dependsOn(core.jvm)
  .settings(name := "lucuma-core-benchmarks")
  .enablePlugins(NoPublishPlugin, JmhPlugin)

// for publishing to npm
lazy val npmPackage = taskKey[File]("Prepare the npm package")
lazy val npmPublish = taskKey[Unit]("Run npm publish")
lazy val npm        = project
  .in(file("modules/npm"))
  .dependsOn(core.js)
  .enablePlugins(ScalaJSPlugin, NoPublishPlugin)
  .settings(
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.ESModule)
      .withESFeatures(_.withESVersion(ESVersion.ES2021).withAvoidLetsAndConsts(false))),
    npmPackage := {
      val _      = (Compile / fullLinkJS).value
      val outDir = (Compile / fullLinkJS / scalaJSLinkerOutputDirectory).value
      IO.write(
        outDir / "package.json",
        s"""|{
            |  "name": "lucuma-core",
            |  "version": "${version.value}",
            |  "license": "${licenses.value.head._1}",
            |  "main": "main.js",
            |  "types": "main.d.ts",
            |  "type": "module"
            |}
            |""".stripMargin
      )
      IO.copyDirectory((Compile / sourceDirectory).value / "typescript", outDir)
      outDir
    },
    npmPublish := {
      import scala.sys.process._
      val outDir = npmPackage.value
      Process(List("npm", "publish"), outDir).!!
    }
  )

ThisBuild / githubWorkflowPublishPreamble +=
  WorkflowStep.Use(
    UseRef.Public("actions", "setup-node", "v4"),
    Map(
      "node-version" -> "22",
      "registry-url" -> "https://registry.npmjs.org"
    )
  )

ThisBuild / githubWorkflowPublish ++= Seq(
  WorkflowStep.Sbt(
    List("npm/npmPublish"),
    name = Some("NPM Publish"),
    env = Map("NODE_AUTH_TOKEN" -> s"$${{ secrets.NPM_REPO_TOKEN }}"),
    cond = Some("startsWith(github.ref, 'refs/tags/v')")
  )
)
