ThisBuild / scalaVersion     := "2.13.1"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "me.shadaj"

lazy val root = project.in(file(".")).settings(
  name := "fluid-quotes"
).aggregate(
  core,
  pipelines,
  quillPlus
)

ThisBuild / scalacOptions += "-feature"
ThisBuild / scalacOptions += "-deprecation"
ThisBuild / scalacOptions += "-language:existentials"

lazy val core = project.in(file("core"))
  .dependsOn(testUtils % Test)

lazy val coreBench = project.in(file("coreBench"))
  .dependsOn(core)

lazy val pipelines = project.in(file("pipelines"))
  .dependsOn(core)

lazy val pipelinesBench = project.in(file("pipelinesBench"))
  .dependsOn(pipelines)

lazy val quillPlus = project.in(file("quillPlus"))
  .dependsOn(core)

lazy val testUtils = project.in(file("testUtils"))
