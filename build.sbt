name := "Stripe Scala"

val currentScalaVersion = "2.11.8"

scalaVersion := currentScalaVersion

crossScalaVersions := Seq(currentScalaVersion)

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
val enumeratumVersion = "1.3.7"

libraryDependencies ++= Seq(
  "net.databinder.dispatch" %% "dispatch-core" % "0.11.3",
  "joda-time" % "joda-time" % "2.9.2",
  "com.beachape" %% "enumeratum" % enumeratumVersion,
  "com.beachape" %% "enumeratum-play-json" % enumeratumVersion,
  "org.spire-math" %% "jawn-parser" % jawnVersion,
  "org.spire-math" %% "jawn-play" % jawnVersion,
  "com.iheart" %% "ficus" % "1.2.2",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.1.0",
  "com.netaporter" %% "scala-uri" % "0.4.13",
  "org.mdedetrich" %% "play-json-utils" % "1.0.0-SNAPSHOT"
)
