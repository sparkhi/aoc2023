ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    name := "aoc2023"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test
