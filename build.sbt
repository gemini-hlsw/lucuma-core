import sbtcrossproject.crossProject
import sbtcrossproject.CrossType

lazy val attoVersion          = "0.6.5"
lazy val catsVersion          = "1.6.0"
lazy val catsEffectVersion    = "1.3.0"
lazy val kindProjectorVersion = "0.9.10"
lazy val monocleVersion       = "1.5.1-cats"
lazy val scala12Version       = "2.12.8"

inThisBuild(Seq(
  homepage := Some(url("https://github.com/gemini-hlsw/gsp-math")),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % kindProjectorVersion),
) ++ gspPublishSettings)

lazy val math = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("modules/math"))
  .settings(
    name := "gsp-math",
    libraryDependencies ++= Seq(
      "org.tpolecat"               %% "atto-core"     % attoVersion,
      "org.typelevel"              %% "cats-core"     % catsVersion,
      "org.typelevel"              %% "cats-effect"   % catsEffectVersion,
      "com.github.julien-truffaut" %% "monocle-core"  % monocleVersion,
      "com.github.julien-truffaut" %% "monocle-macro" % monocleVersion,
    )
  )
  .jvmConfigure(_.enablePlugins(AutomateHeaderPlugin))
  .jsSettings(gspScalaJsSettings: _*)


lazy val testkit = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("modules/testkit"))
  .dependsOn(math)
  .settings(
    name := "gsp-math-testkit",
    libraryDependencies ++= Seq(
      "org.typelevel"              %% "cats-testkit"  % catsEffectVersion,
      "com.github.julien-truffaut" %% "monocle-law"   % monocleVersion,
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
    skip in publish := true
  )
  .jvmConfigure(_.enablePlugins(AutomateHeaderPlugin))
  .jsSettings(gspScalaJsSettings: _*)

