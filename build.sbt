name := "knittery-ui"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.2"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.0.6",
  "com.squants" %% "squants" % "0.4.2",
  "com.assembla.scala-incubator" %% "graph-core" % "1.9.0",
  "com.assembla.scala-incubator" %% "graph-dot" % "1.9.0",
  "com.assembla.scala-incubator" %% "graph-json" % "1.9.1",
  "ch.inventsoft.akka" %% "rxtx-akka-io" % "1.0.4",
  "ch.inventsoft.graph" %% "graph-layout" % "1.0.2",
  "org.webjars" % "jquery" % "2.1.1",
  "org.webjars" % "bootstrap" % "3.2.0",
  "org.webjars" % "three.js" % "r66",
  "org.webjars" % "leapjs" % "0.4.1")

libraryDependencies ++= Seq(
  "com.storm-enroute" %% "scalameter" % "0.6" % "test")

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")



mappings in Universal ++= Seq(
  file("librxtxSerial.jnilib") -> "librxtxSerial.jnilib",
  file("librxtxSerial32.jnilib") -> "librxtxSerial32.jnilib",
  file("librxtxSerial.so") -> "librxtxSerial.so")



NativePackagerKeys.bashScriptExtraDefines += """addJava "-Djava.awt.headless=true""""

initialize ~= { _ =>
  System.setProperty("java.awt.headless", "true")
}

javaOptions in Test := Seq("-Djava.awt.headless=true")