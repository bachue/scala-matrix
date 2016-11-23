name := "Matrix Test"

scalaVersion := "2.11.7"

scalacOptions ++= Seq(
  "-encoding", "UTF-8", "-optimise",
  "-deprecation", "-unchecked", "-feature", "-Xlint",
  "-Ywarn-infer-any", // Nice, but hard to eliminate these warnings: "-Ywarn-value-discard")
  "-language:experimental.macros")

javacOptions  ++= Seq("-Xlint:unchecked", "-Xlint:deprecation", "-Xdiags:verbose")

incOptions := incOptions.value.withNameHashing(true)
