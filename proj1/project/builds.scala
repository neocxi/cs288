import sbt._
import Keys._
import sbtassembly.Plugin._
import AssemblyKeys._

object Builds extends Build {
  lazy val buildSettings = Defaults.defaultSettings ++ Seq(
    name := "proj1",
    version := "1.0",
    scalaVersion := "2.11.2"
  )
  lazy val librarySettings = buildSettings ++ 
    sbtassembly.Plugin.assemblySettings ++ Seq(
      jarName in assembly := "assign1-submit.jar"
    )
  lazy val library = Project("proj1", file("."), settings = librarySettings)
}
