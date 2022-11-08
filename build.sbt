ThisBuild / organization := "svend"
ThisBuild / scalaVersion := "3.2.0"

lazy val root = (project in file(".")).settings(
  name := "sudoku-solver",
  libraryDependencies ++= Seq(
    // "core" module - IO, IOApp, schedulers
    // This pulls in the kernel and std modules automatically.
    "org.typelevel" %% "cats-effect" % "3.3.12",
    // concurrency abstractions and primitives (Concurrent, Sync, Async etc.)
    "org.typelevel" %% "cats-effect-kernel" % "3.3.12",
    // standard "effect" library (Queues, Console, Random etc.)
    "org.typelevel" %% "cats-effect-std" % "3.3.12",
    "org.scalatest" %% "scalatest" % "3.2.14" % "test",
    "org.scalatestplus" %% "scalacheck-1-15" % "3.2.9.0" % "test"
  )
)
