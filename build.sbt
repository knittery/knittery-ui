name := "knittery-ui"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  "org.scalaz" %% "scalaz-core" % "7.0.6",
  "com.squants"  %% "squants"  % "0.4.2",
  "ch.inventsoft.akka" %% "rxtx-akka-io" % "1.0.2-SNAPSHOT",
  "ch.inventsoft.graph" %% "graph-layout" % "1.0.0-SNAPSHOT",
  "org.webjars" %% "webjars-play" % "2.2.1",
  "org.webjars" % "jquery" % "2.1.0-2",
  "org.webjars" % "bootstrap" % "3.1.1",
  "org.webjars" % "three.js" % "r66",
  "org.webjars" % "leapjs" % "0.4.1",
  "com.assembla.scala-incubator" %% "graph-core" % "1.7.2",
  "com.assembla.scala-incubator" %% "graph-dot" % "1.7.0",
  "com.assembla.scala-incubator" %% "graph-json" % "1.7.0",
  "com.github.axel22" %% "scalameter" % "0.4" % "test"
)

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

resolvers +=
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

initialize ~= { _ =>
  System.setProperty("java.awt.headless", "true")
}

javaOptions in Test := Seq("-Djava.awt.headless=true")

play.Project.playScalaSettings
