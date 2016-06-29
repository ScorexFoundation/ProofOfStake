scalaVersion := "2.11.8"

name := "scorex-consensus"
organization := "org.scorexfoundation"
version := "2.0.0-SNAPSHOT"

resolvers += "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/"

libraryDependencies ++= Seq(
  "org.scorexfoundation" %% "scorex-basics" % "2.0.0-M1"
)

scalacOptions ++= Seq("-feature", "-deprecation")

javaOptions ++= Seq("-server")

testOptions in Test += Tests.Argument("-oD", "-u", "target/test-reports")

//publishing settings

publishMavenStyle in ThisBuild := true

publishArtifact in Test := false

publishTo in ThisBuild := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

fork in ThisBuild := true

pomIncludeRepository in ThisBuild := { _ => false }

licenses in ThisBuild := Seq("CC0" -> url("https://creativecommons.org/publicdomain/zero/1.0/legalcode"))

homepage in ThisBuild := Some(url("https://github.com/ScorexFoundation/Scorex"))

pomExtra in ThisBuild :=
  <scm>
    <url>git@github.com:ScorexFoundation/Scorex.git</url>
    <connection>scm:git:git@github.com:ScorexFoundation/Scorex.git</connection>
  </scm>
    <developers>
      <developer>
        <id>kushti</id>
        <name>Alexander Chepurnoy</name>
        <url>http://chepurnoy.org/</url>
      </developer>
      <developer>
        <id>catena</id>
        <name>catena</name>
        <url>https://github.com/catena2w</url>
      </developer>
    </developers>

credentials in ThisBuild += Credentials(Path.userHome / ".ivy2" / ".credentials")
