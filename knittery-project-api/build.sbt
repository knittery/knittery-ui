name := "knittery-project-api"
version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.7"
scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.1.3",
  "com.squants" %% "squants" % "0.4.2",
  "org.webjars" % "swagger-ui" % "2.1.8-M1"
)

// for 3d knitting
libraryDependencies ++= Seq(
  "ch.inventsoft.graph" %% "graph-layout" % "1.0.2",
  "com.assembla.scala-incubator" %% "graph-core" % "1.9.0",
  "com.assembla.scala-incubator" %% "graph-dot" % "1.9.0",
  "com.assembla.scala-incubator" %% "graph-json" % "1.9.1"
)

// Test dependencies
libraryDependencies ++= Seq(
  specs2 % Test
)
javaOptions in Test := Seq("-Djava.awt.headless=true")

// Play provides two styles of routers, one expects its actions to be injected, the
// other, legacy style, accesses its actions statically.
routesGenerator := InjectedRoutesGenerator
