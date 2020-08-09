import sbtcrossproject.CrossType

lazy val attoVersion                 = "0.8.0"
lazy val catsVersion                 = "2.1.1"
lazy val collCompatVersion           = "2.1.6"
lazy val kindProjectorVersion        = "0.10.3"
lazy val monocleVersion              = "2.0.5"
lazy val catsTestkitScalaTestVersion = "2.0.0"
lazy val scalaJavaTimeVersion        = "2.0.0"
lazy val geminiLocalesVersion        = "0.5.0"
lazy val jtsVersion                  = "0.0.9"
lazy val svgdotjsVersion             = "0.0.1"
lazy val newType                     = "0.4.4"

inThisBuild(
  Seq(
    homepage := Some(url("https://github.com/gemini-hlsw/gsp-math")),
    addCompilerPlugin("org.typelevel" %% "kind-projector" % kindProjectorVersion),
    Global / onChangedBuildSource := ReloadOnSourceChanges
  ) ++ gspPublishSettings
)

skip in publish := true

lazy val math = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("modules/math"))
  .settings(
    name := "gsp-math",
    libraryDependencies ++= Seq(
      "org.tpolecat"               %%% "atto-core"               % attoVersion,
      "org.typelevel"              %%% "cats-core"               % catsVersion,
      "com.github.julien-truffaut" %%% "monocle-core"            % monocleVersion,
      "com.github.julien-truffaut" %%% "monocle-macro"           % monocleVersion,
      "org.scala-lang.modules"     %%% "scala-collection-compat" % collCompatVersion,
      "edu.gemini"                 %%% "gpp-jts"                 % jtsVersion
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
  .crossType(CrossType.Full)
  .in(file("modules/testkit"))
  .dependsOn(math)
  .settings(
    name := "gsp-math-testkit",
    libraryDependencies ++= Seq(
      "org.typelevel"              %%% "cats-testkit"           % catsVersion,
      "org.typelevel"              %%% "cats-testkit-scalatest" % catsTestkitScalaTestVersion,
      "com.github.julien-truffaut" %%% "monocle-law"            % monocleVersion
    )
  )
  .jvmConfigure(_.enablePlugins(AutomateHeaderPlugin))
  .jsSettings(gspScalaJsSettings: _*)

lazy val tests = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("modules/tests"))
  .dependsOn(math, testkit)
  .settings(
    name := "gsp-math-tests",
    skip in publish := true,
    libraryDependencies ++= Seq(
      "com.disneystreaming" %%% "weaver-framework" % "0.4.2"
    ),
    testFrameworks += new TestFramework("weaver.framework.TestFramework"),
    scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule))
  )
  .jvmConfigure(_.enablePlugins(AutomateHeaderPlugin))
  .jvmSettings(
    resolvers += "Gemini Repository".at(
      "https://github.com/gemini-hlsw/maven-repo/raw/master/releases"
    ),
    libraryDependencies ++= Seq(
      "edu.gemini.ocs"      %% "edu-gemini-util-skycalc"     % "2020001.1.7",
      "com.disneystreaming" %% "weaver-scalacheck"           % "0.4.2",
      "com.47deg"           %% "scalacheck-toolbox-datetime" % "0.3.5"
    )
  )
  .jsSettings(gspScalaJsSettings: _*)
