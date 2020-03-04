name := "algonquin.transcal"

version := "0.1"

scalaVersion := "2.12.10"

libraryDependencies += "org.scala-lang" % "scala-library" %  "2.12.10"
libraryDependencies += "ch.qos.logback" % "logback-classic" %  "1.2.3"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"
libraryDependencies += "org.xerial" % "sqlite-jdbc" %  "3.30.1"

libraryDependencies += "org.rogach" %% "scallop" % "3.4.0"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"

libraryDependencies += "org.scalatest" %% "scalatest" %  "3.1.1" % Test
libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.1" % Test
libraryDependencies += "com.typesafe.play" %% "play-json" %  "2.8.1" % Test
libraryDependencies += "org.scalacheck" %% "scalacheck" %  "1.14.3" % Test

scalacOptions ++= Seq(
  "-deprecation",
  "-explaintypes",
  "-feature",
)