name := "nak"

version := "1.2.0-SNAPSHOT"

organization := "org.scalanlp"

scalaVersion := "2.10.1"

retrieveManaged := true

crossPaths := false

resolvers ++= Seq(
  "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
)


libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test",
  "com.novocode" % "junit-interface" % "0.8" % "test->default",
  "org.rogach" %% "scallop" % "0.8.1",
  "org.scalanlp" % "breeze-core_2.10" % "0.4-SNAPSHOT",
  "org.scalanlp" % "breeze-math_2.10" % "0.4-SNAPSHOT",
  "commons-logging" % "commons-logging" % "1.1.1",
  "log4j" % "log4j" % "1.2.16",
  "gov.nist.math" % "jama" % "1.0.2"
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
