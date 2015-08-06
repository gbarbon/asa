organization := "Universita' Ca' Foscari"
version := "0.0.0"
scalaVersion := "2.11.7"
name := "yaasa"

scalaSource in Compile := baseDirectory.value / "src"

includeFilter in (Compile, unmanagedSources) := "*.scala" //|| "*.java"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"


