name := "stripe-scala"

val currentScalaVersion = "2.11.8"

scalaVersion := currentScalaVersion

crossScalaVersions := Seq(currentScalaVersion)

organization := "org.mdedetrich"

version := "0.1.1"

scalacOptions ++= Seq(
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

val enumeratumVersion = "1.4.16"
val akkaStreamJson    = "3.1.0"

libraryDependencies ++= Seq(
  "com.typesafe.akka"          %% "akka-http"            % "10.0.3",
  "de.knutwalker"              %% "akka-stream-json"     % akkaStreamJson,
  "de.knutwalker"              %% "akka-http-json"       % akkaStreamJson,
  "com.beachape"               %% "enumeratum"           % enumeratumVersion,
  "com.beachape"               %% "enumeratum-play-json" % enumeratumVersion,
  "com.iheart"                 %% "ficus"                % "1.3.4",
  "com.typesafe.scala-logging" %% "scala-logging"        % "3.4.0",
  "com.netaporter"             %% "scala-uri"            % "0.4.13" exclude ("io.spray", "spray-json_2.11"),
  "com.typesafe.play"          %% "play-json"            % "2.5.8",
  "org.spire-math"             %% "jawn-play"            % "0.10.1",
  "org.scalatest"              %% "scalatest"            % "3.0.0" % "test, it",
  "ch.qos.logback"             % "logback-classic"       % "1.1.7" % "test, it"
)

homepage := Some(url("https://github.com/mdedetrich/stripe-scala"))

scmInfo := Some(
  ScmInfo(url("https://github.com/mdedetrich/stripe-scala"), "git@github.com:mdedetrich/stripe-scala.git"))

developers := List(
  Developer("mdedetrich", "Matthew de Detrich", "mdedetrich@gmail.com", url("https://github.com/mdedetrich")),
  Developer("leonardehrenfried", "Leonard Ehrenfried", "leonard.ehrenfried@gmail.com", url("https://leonard.io"))
)

licenses += ("BSD 3 Clause", url("https://opensource.org/licenses/BSD-3-Clause"))

pomIncludeRepository := (_ => false)
