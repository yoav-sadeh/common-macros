import sbt.Keys.scalaVersion

scalaVersion := "2.11.8"

name:="common-macros"

organization := "com.hamlazot"

version := "0.1.0-SNAPSHOT"

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _)

libraryDependencies ++= Seq(
  //"org.json4s" %% "json4s-jackson" % "3.3.0",
  //"org.json4s" %% "json4s-core" % "3.3.0",
  //"org.json4s" %% "json4s-ext" % "3.3.0",
  //"org.json4s" %% "json4s-scalaz" % "3.3.0",
  "com.hamlazot" %% "common" % "0.1.0-SNAPSHOT",
  "org.specs2" %% "specs2-core" % "3.7" % Test
)