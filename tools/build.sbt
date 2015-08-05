organization := "Universita' Ca' Foscari"
version := "0.0.0"
scalaVersion := "2.11.7"
name := "yaasa"

scalaSource in Compile := baseDirectory.value / "src"
resourceDirectory in Compile := baseDirectory.value / "resources"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"


