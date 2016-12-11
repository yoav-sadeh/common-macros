import sbt.Keys.scalaVersion

scalaVersion := "2.11.8"

name:="common-macros"

organization := "com.hamlazot"

version := "0.1.0-SNAPSHOT"

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _)

libraryDependencies ++= Seq(
  "com.hamlazot" %% "common" % "0.1.0-SNAPSHOT",
  "com.typesafe" % "config" % "1.2.1",
  "org.specs2" %% "specs2-core" % "3.7" % Test
)