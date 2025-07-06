ThisBuild / scalaVersion     := "3.6.4"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "compiler-sasl-scala",
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.0.0-M10" % Test,
      "com.monovore" %% "decline-effect" % "2.5.0"
    )
  )

javacOptions ++= Seq("-encoding", "UTF-8")
scalacOptions ++= Seq("-encoding", "UTF-8")

// For running and forking JVMs
javaOptions ++= Seq("-Dfile.encoding=UTF-8")
// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
