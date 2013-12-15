name := "knittery"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  "com.typesafe.akka" %% "akka-actor" % "2.2.3",
  "rxtx-akka-io" %% "rxtx-akka-io" % "1.0.0"
)     

play.Project.playScalaSettings
