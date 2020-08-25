resolvers += Resolver.sonatypeRepo("public")

addSbtPlugin("edu.gemini"            % "sbt-gsp"                  % "0.2.5")
addSbtPlugin("com.geirsson"          % "sbt-ci-release"           % "1.5.3")
addSbtPlugin("org.scala-js"          % "sbt-scalajs"              % "1.1.1")
addSbtPlugin("org.portable-scala"    % "sbt-scalajs-crossproject" % "1.0.0")
addSbtPlugin("com.timushev.sbt"      % "sbt-updates"              % "0.5.1")
addSbtPlugin("ch.epfl.scala"         % "sbt-scalajs-bundler"      % "0.18.0")
addSbtPlugin("io.github.davidmweber" % "flyway-sbt"               % "6.5.0")

libraryDependencies ++= Seq(
  "org.postgresql" % "postgresql" % "42.2.15" // needed by flyway
)
