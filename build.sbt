name := "algonquin.transcal"

version := "0.1"

scalaVersion := "2.11.12"

libraryDependencies += "org.scala-lang" % "scala-library" %  "2.11.12"
libraryDependencies += "ch.qos.logback" % "logback-classic" %  "1.2.3"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0"
libraryDependencies += "org.xerial" % "sqlite-jdbc" %  "3.25.2"

//libraryDependencies += "com.microsoft.z3"
//libraryDependencies += "earley"
//libraryDependencies += "org.rogach" %  "scallop_2.11" %  "3.1.5"
//libraryDependencies += "org.algonquin" %  "bellmaniac"

libraryDependencies += "org.scalatest" %% "scalatest" %  "3.0.5" % Test
libraryDependencies += "com.typesafe.play" %% "play-json" %  "2.6.10" % Test
libraryDependencies += "org.scalacheck" %% "scalacheck" %  "1.14.0" % Test

lazy val core = RootProject(file("../bellmaniac"))
val main = Project(id = "transcal", base = file(".")).dependsOn(core)