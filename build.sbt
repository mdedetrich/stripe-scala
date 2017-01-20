name := "stripe-scala"

val currentScalaVersion = "2.11.8"

scalaVersion := currentScalaVersion

crossScalaVersions := Seq(currentScalaVersion)

organization := "org.mdedetrich"

version := "0.2.0-SNAPSHOT"

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

libraryDependencies ++= Seq(
  "net.databinder.dispatch"    %% "dispatch-core"        % "0.11.3",
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

pomExtra := <url>https://github.com/mdedetrich/stripe-scala</url>
  <licenses>
    <license>
      <name>BSD 3-Clause</name>
      <url>https://opensource.org/licenses/BSD-3-Clause</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:mdedetrich/stripe-scala.git</url>
    <connection>scm:git:git@github.com:mdedetrich/stripe-scala.git</connection>
  </scm>
  <developers>
    <developer>
      <id>mdedetrich</id>
      <name>Matthew de Detrich</name>
      <email>mdedetrich@gmail.com</email>
    </developer>
    <developer>
      <id>leonardehrenfried</id>
      <name>Leonard Ehrenfried</name>
      <email>leonard.ehrenfried@gmail.com</email>
    </developer>
  </developers>
