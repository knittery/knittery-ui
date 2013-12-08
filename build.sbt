name := "knittery"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  "org.rxtx" % "rxtx" % "2.1.7"
)     

play.Project.playScalaSettings
