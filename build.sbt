name := "stripe-scala"

scalaVersion := "2.11.7"

crossScalaVersions := Seq("2.11.7")

organization := "org.mdedetrich"

version := "1.0.0-SNAPSHOT"

resolvers ++= Seq(
  Resolver.jcenterRepo,
  Resolver.sonatypeRepo("snapshots"),
  "Typesafe Releases" at "https://repo.typesafe.com/typesafe/releases/"
)

scalacOptions ++= Seq(
  "-target:jvm-1.8",
  "-encoding", "UTF-8",
  "-deprecation", // warning and location for usages of deprecated APIs
  "-feature", // warning and location for usages of features that should be imported explicitly
  "-unchecked", // additional warnings where generated code depends on assumptions
  "-Xlint", // recommended additional warnings
  "-Xcheckinit", // runtime error when a val is not initialized due to trait hierarchies (instead of NPE somewhere else)
  "-Ywarn-adapted-args", // Warn if an argument list is modified to match the receiver
  "-Ywarn-value-discard", // Warn when non-Unit expression results are unused
  "-Ywarn-inaccessible",
  "-Ywarn-dead-code"
)

val jawnVersion = "0.8.4"

libraryDependencies ++= Seq(
  "net.databinder.dispatch" %% "dispatch-core" % "0.11.3",
  "com.github.nscala-time" %% "nscala-time" % "2.8.0",
  "org.spire-math" %% "jawn-parser" % jawnVersion,
  "org.spire-math" %% "jawn-play" % jawnVersion,
  "com.iheart" %% "ficus" % "1.2.0",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
  "org.mdedetrich" %% "play-json-utils" % "1.0.0-SNAPSHOT",
  "org.mdedetrich" %% "utforsca" % "2.2.0"
)
