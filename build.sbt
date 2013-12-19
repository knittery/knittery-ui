name := "knittery-ui"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  "com.typesafe.akka" %% "akka-actor" % "2.2.3",
  "ch.inventsoft.akka" %% "rxtx-akka-io" % "1.0.0-SNAPSHOT"
)     

play.Project.playScalaSettings
