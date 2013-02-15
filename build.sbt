name := "nak"

version := "1.1.0-SNAPSHOT"

organization := "org.scalanlp"

scalaVersion := "2.10.0"

crossPaths := false

libraryDependencies ++= Seq(
  "com.novocode" % "junit-interface" % "0.8" % "test->default",
  "commons-logging" % "commons-logging" % "1.1.1",
  "log4j" % "log4j" % "1.2.16"
)

publishTo <<= version { v: String =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT"))
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

publishMavenStyle := true

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra := (
  <url>http://scalanlp.org/</url>
  <licenses>
    <license>
      <name>Apache License 2.0</name>
      <url>http://www.apache.org/licenses/LICENSE-2.0.html</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:scalanlp/nak.git</url>
    <connection>scm:git:git@github.com:scalanlp/nak.git</connection>
  </scm>
  <developers>
    <developer>
      <id>jasonbaldridge</id>
      <name>Jason Baldridge</name>
      <url>http://www.jasonbaldridge.com</url>
    </developer>
  </developers>
)