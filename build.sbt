name := "stripe-scala"

val currentScalaVersion = "2.12.8"
val scala211Version     = "2.11.11"
val circeVersion        = "0.11.1"

scalaVersion := currentScalaVersion

crossScalaVersions := Seq(currentScalaVersion, scala211Version)

organization := "org.mdedetrich"

scalacOptions ++= Seq(
  "-Xfatal-warnings",
  "-target:jvm-1.8",
  "-encoding",
  "UTF-8",
  "-deprecation", // warning and location for usages of deprecated APIs
  "-feature", // warning and location for usages of features that should be imported explicitly
  "-unchecked", // additional warnings where generated code depends on assumptions
  "-Xlint", // recommended additional warnings
  "-Xcheckinit", // runtime error when a val is not initialized due to trait hierarchies (instead of NPE somewhere else)
  "-Ywarn-adapted-args", // Warn if an argument list is modified to match the receiver
  "-Ywarn-value-discard", // Warn when non-Unit expression results are unused
  "-Ywarn-inaccessible",
  "-Ywarn-dead-code",
  "-language:postfixOps"
)

Defaults.itSettings

configs(IntegrationTest)

val enumeratumVersion      = "1.5.13"
val enumeratumCirceVersion = "1.5.21"
val akkaStreamJson         = "3.4.0"

libraryDependencies ++= Seq(
  "com.typesafe.akka"          %% "akka-http"         % "10.1.8",
  "com.typesafe.akka"          %% "akka-stream"       % "2.5.22",
  "de.knutwalker"              %% "akka-stream-circe" % akkaStreamJson,
  "de.knutwalker"              %% "akka-http-circe"   % akkaStreamJson,
  "io.circe"                   %% "circe-core"        % circeVersion,
  "io.circe"                   %% "circe-generic"     % circeVersion,
  "io.circe"                   %% "circe-parser"      % circeVersion,
  "com.beachape"               %% "enumeratum"        % enumeratumVersion,
  "com.beachape"               %% "enumeratum-circe"  % enumeratumCirceVersion,
  "com.iheart"                 %% "ficus"             % "1.4.6",
  "com.typesafe.scala-logging" %% "scala-logging"     % "3.9.0",
  "org.scalatest"              %% "scalatest"         % "3.0.7" % "test, it",
  "ch.qos.logback"             % "logback-classic"    % "1.2.3" % "test, it"
)

homepage := Some(url("https://github.com/mdedetrich/stripe-scala"))

scmInfo := Some(
  ScmInfo(url("https://github.com/mdedetrich/stripe-scala"), "git@github.com:mdedetrich/stripe-scala.git"))

developers := List(
  Developer("mdedetrich", "Matthew de Detrich", "mdedetrich@gmail.com", url("https://github.com/mdedetrich")),
  Developer("leonardehrenfried", "Leonard Ehrenfried", "leonard.ehrenfried@gmail.com", url("https://leonard.io"))
)

licenses += ("BSD 3 Clause", url("https://opensource.org/licenses/BSD-3-Clause"))

publishMavenStyle := true

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false

pomIncludeRepository := (_ => false)

import ReleaseTransformations._
releaseCrossBuild := true
releasePublishArtifactsAction := PgpKeys.publishSigned.value // Use publishSigned in publishArtifacts step
releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  publishArtifacts,
  setNextVersion,
  commitNextVersion,
  releaseStepCommand("sonatypeReleaseAll"),
  pushChanges
)

val flagsFor11 = Seq(
  "-Yconst-opt",
  "-Ywarn-infer-any",
  "-Yclosure-elim",
  "-Ydead-code"
)

val flagsFor12 = Seq(
  "-Xlint:-unused", //because 2.11 needs unused import: cats.syntax.either._
  "-Ywarn-infer-any",
  "-opt-inline-from:<sources>"
)

scalacOptions ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, n)) if n >= 12 =>
      flagsFor12
    case Some((2, n)) if n == 11 =>
      flagsFor11
  }
}

parallelExecution in IntegrationTest := false
