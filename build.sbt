ThisBuild / organization := "svend"
ThisBuild / scalaVersion := "3.2.0"

lazy val root = (project in file(".")).settings(
  name := "sudoku-solver",
  libraryDependencies ++= Seq(
    // "core" module - IO, IOApp, schedulers
    "org.typelevel" %% "cats-effect" % "3.3.12",
    "org.typelevel" %% "cats-effect-std" % "3.3.12",
    "co.fs2" %% "fs2-core" % "3.2.5",
    "org.scalatest" %% "scalatest" % "3.2.14" % "test",
    "org.scalatestplus" %% "scalacheck-1-15" % "3.2.9.0" % "test"
  )
)
