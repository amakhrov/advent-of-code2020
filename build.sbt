import Dependencies._

ThisBuild / scalaVersion := "2.13.3"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.amakhrov"
ThisBuild / organizationName := "amakhrov"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.3.2"

lazy val root = (project in file("."))
  .settings(
    name := "scala-advent-of-code",
    libraryDependencies += scalaTest % Test
  )

scalacOptions ++= Seq(
  "-Xfatal-warnings",
  "-deprecation"
)

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
