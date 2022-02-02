resolvers += Resolver.sonatypeRepo("public")
resolvers += Resolver.sonatypeRepo("snapshots")

addSbtPlugin("edu.gemini"         % "sbt-lucuma-lib"           % "0.6-3ce8fc2-SNAPSHOT")
addSbtPlugin("org.scala-js"       % "sbt-scalajs"              % "1.8.0")
addSbtPlugin("com.timushev.sbt"   % "sbt-updates"              % "0.6.1")
