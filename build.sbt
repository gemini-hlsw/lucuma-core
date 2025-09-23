import org.scalajs.linker.interface.ESVersion

ThisBuild / tlBaseVersion                         := "0.146"
ThisBuild / tlCiReleaseBranches                   := Seq("master")
ThisBuild / githubWorkflowEnv += "MUNIT_FLAKY_OK" -> "true"

ThisBuild / tlCiReleaseBranches += "topic/scala3"

Global / concurrentRestrictions += Tags.limit(Tags.Compile, 1)

ThisBuild / crossScalaVersions := Seq("3.7.3")
ThisBuild / scalacOptions += "-language:implicitConversions" // TODO

lazy val catsVersion                = "2.13.0"
lazy val catsCollctionsVersion      = "0.9.10"
lazy val catsEffectVersion          = "3.6.3"
lazy val catsParseVersion           = "1.1.0"
lazy val catsScalacheckVersion      = "0.3.2"
lazy val catsTimeVersion            = "0.6.0"
lazy val circeVersion               = "0.14.14"
lazy val circeRefinedVersion        = "0.15.1"
lazy val coulombVersion             = "0.9.1"
lazy val fs2Version                 = "3.12.2"
lazy val fs2DataVersion             = "1.12.0"
lazy val geminiLocalesVersion       = "0.12.1"
lazy val http4sVersion              = "0.23.31"
lazy val http4sDomVersion           = "0.2.12"
lazy val http4sJdkHttpClientVersion = "0.10.0"
lazy val jtsVersion                 = "0.4.1"
lazy val kindProjectorVersion       = "0.13.2"
lazy val kittensVersion             = "3.5.0"
lazy val lucumaRefinedVersion       = "0.1.4"
lazy val monocleVersion             = "3.3.0"
lazy val munitVersion               = "1.2.0"
lazy val munitDisciplineVersion     = "2.0.0"
lazy val munitCatsEffectVersion     = "2.1.0"
lazy val pprintVersion              = "0.9.3"
lazy val refinedVersion             = "0.11.3"
lazy val scalaJavaTimeVersion       = "2.6.0"
lazy val scalajsStubVersion         = "1.1.0"
lazy val scalaXmlVersion            = "2.4.0"
lazy val spireVersion               = "0.18.0"

Global / onChangedBuildSource := ReloadOnSourceChanges

val root = tlCrossRootProject.aggregate(core, testkit, tests, catalog, ags, catalogTestkit, catalogTests)

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("modules/core"))
  .settings(
    name := "lucuma-core",
    libraryDependencies ++= Seq(
      "org.typelevel"  %%% "cats-parse"            % catsParseVersion,
      "org.typelevel"  %%% "cats-core"             % catsVersion,
      "org.typelevel"  %%% "cats-collections-core" % catsCollctionsVersion,
      "org.typelevel"  %%% "cats-effect"           % catsEffectVersion,
      "dev.optics"     %%% "monocle-core"          % monocleVersion,
      "dev.optics"     %%% "monocle-macro"         % monocleVersion,
      "dev.optics"     %%% "monocle-state"         % monocleVersion,
      "edu.gemini"     %%% "lucuma-jts"            % jtsVersion,
      "com.manyangled" %%% "coulomb-core"          % coulombVersion,
      "com.manyangled" %%% "coulomb-units"         % coulombVersion,
      "com.manyangled" %%% "coulomb-refined"       % coulombVersion,
      "org.typelevel"  %%% "spire"                 % spireVersion,
      "org.typelevel"  %%% "spire-extras"          % spireVersion,
      "eu.timepit"     %%% "refined"               % refinedVersion,
      "eu.timepit"     %%% "refined-cats"          % refinedVersion,
      "org.typelevel"  %%% "cats-time"             % catsTimeVersion,
      "org.typelevel"  %%% "kittens"               % kittensVersion,
      "io.circe"       %%% "circe-core"            % circeVersion,
      "io.circe"       %%% "circe-refined"         % circeRefinedVersion
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
      "org.scalameta" %%% "munit"             % munitVersion % Test,
      "org.typelevel" %%% "discipline-munit"  % munitDisciplineVersion % Test,
      "org.typelevel" %%% "munit-cats-effect" % munitCatsEffectVersion % Test,
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

lazy val catalog = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/catalog"))
  .dependsOn(core)
  .settings(
    name := "lucuma-catalog",
    libraryDependencies ++= Seq(
      "co.fs2"        %%% "fs2-core"             % fs2Version,
      "org.gnieh"     %%% "fs2-data-xml"         % fs2DataVersion,
      "org.gnieh"     %%% "fs2-data-csv"         % fs2DataVersion,
      "org.gnieh"     %%% "fs2-data-csv-generic" % fs2DataVersion,
      "edu.gemini"    %%% "lucuma-refined"       % lucumaRefinedVersion,
      "org.typelevel" %%% "cats-core"            % catsVersion,
      "dev.optics"    %%% "monocle-core"         % monocleVersion,
      "dev.optics"    %%% "monocle-state"        % monocleVersion,
      "eu.timepit"    %%% "refined"              % refinedVersion,
      "eu.timepit"    %%% "refined-cats"         % refinedVersion,
      "org.http4s"    %%% "http4s-core"          % http4sVersion,
      "org.http4s"    %%% "http4s-client"        % http4sVersion,
      "org.typelevel" %%% "cats-parse"           % catsParseVersion,
      "org.typelevel" %%% "kittens"              % kittensVersion
    )
  )
  .jsConfigure(_.enablePlugins(BundleMonPlugin))

lazy val ags = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/ags"))
  .settings(
    name := "lucuma-ags",
    libraryDependencies ++= Seq(
      "edu.gemini"    %%% "lucuma-refined" % lucumaRefinedVersion,
      "org.typelevel" %%% "cats-core"      % catsVersion
    )
  )
  .jsConfigure(_.enablePlugins(BundleMonPlugin))
  .dependsOn(catalog)

lazy val catalogTestkit = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("modules/catalog-testkit"))
  .dependsOn(catalog, testkit, ags)
  .settings(
    name := "lucuma-catalog-testkit",
    libraryDependencies ++= Seq(
      "org.typelevel"          %%% "cats-testkit"        % catsVersion,
      "eu.timepit"             %%% "refined-scalacheck"  % refinedVersion,
      "org.scala-lang.modules" %%% "scala-xml"           % scalaXmlVersion,
      "io.chrisdavenport"      %%% "cats-scalacheck"     % catsScalacheckVersion
    )
  )

lazy val catalogTests = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("modules/catalog-tests"))
  .enablePlugins(NoPublishPlugin)
  .dependsOn(catalog, catalogTestkit, ags)
  .settings(
    name := "lucuma-catalog-tests",
    libraryDependencies ++= Seq(
      "org.typelevel"          %%% "cats-effect"         % catsEffectVersion      % Test,
      "org.scalameta"          %%% "munit"               % munitVersion           % Test,
      "org.typelevel"          %%% "discipline-munit"    % munitDisciplineVersion % Test,
      "org.typelevel"          %%% "munit-cats-effect"   % munitCatsEffectVersion % Test,
      "org.scala-lang.modules" %%% "scala-xml"           % scalaXmlVersion        % Test,
      "org.http4s"             %%% "http4s-core"         % http4sVersion,
      "com.lihaoyi"            %%% "pprint"              % pprintVersion,
      "org.typelevel"          %%% "cats-time"           % catsTimeVersion
    )
  )
  .jsSettings(
    scalaJSUseMainModuleInitializer := true,
    scalacOptions ~= (_.filterNot(Set("-Wdead-code"))),
    libraryDependencies ++= Seq(
      "org.http4s" %%% "http4s-dom" % http4sDomVersion
    ),
    jsEnv                           := {
      import org.scalajs.jsenv.nodejs.NodeJSEnv
      new NodeJSEnv(NodeJSEnv.Config().withArgs(List("--experimental-fetch")))
    }
  )
  .jvmSettings(
    libraryDependencies ++= Seq(
      "co.fs2"     %% "fs2-io"                 % fs2Version,
      "org.http4s" %% "http4s-jdk-http-client" % http4sJdkHttpClientVersion
    )
  )


// for publishing to npm
lazy val npmPackage = taskKey[File]("Prepare the npm package")
lazy val npmPublish = taskKey[Unit]("Run npm publish")
lazy val npm        = project
  .in(file("modules/npm"))
  .dependsOn(core.js)
  .enablePlugins(ScalaJSPlugin, NoPublishPlugin, BundleMonPlugin)
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
