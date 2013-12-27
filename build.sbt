name := "knittery-ui"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  "com.typesafe.akka" %% "akka-actor" % "2.2.3",
  "ch.inventsoft.akka" %% "rxtx-akka-io" % "1.0.2-SNAPSHOT",
  "org.webjars" %% "webjars-play" % "2.2.1",
  "org.webjars" % "jquery" % "2.0.3-1",
  "org.webjars" % "bootstrap" % "3.0.3"
)

resolvers +=
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

initialize ~= { _ =>
  System.setProperty("java.awt.headless", "true")
}

play.Project.playScalaSettings
