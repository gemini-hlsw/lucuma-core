resolvers += "gemini-hlsw" at
  "https://raw.githubusercontent.com/gemini-hlsw/maven-repo/master/releases"

addSbtPlugin("edu.gemini"         % "sbt-lucuma-lib" % "0.17-ffab195-SNAPSHOT")
addSbtPlugin("pl.project13.scala" % "sbt-jmh"        % "0.4.8")
