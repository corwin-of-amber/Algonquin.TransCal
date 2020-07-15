name := "algonquin.transcal"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies += "org.scala-lang" % "scala-library" %  "2.12.8"
libraryDependencies += "ch.qos.logback" % "logback-classic" %  "1.2.3"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"
libraryDependencies += "org.xerial" % "sqlite-jdbc" %  "3.25.2"

libraryDependencies += "org.rogach" %% "scallop" % "3.3.1"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
libraryDependencies += "com.typesafe.play" %% "play-json" %  "2.7.4"

libraryDependencies += "com.regblanc" %% "scala-smtlib" % "0.2.2"

libraryDependencies += "org.scalatest" %% "scalatest" %  "3.0.8" % Test
libraryDependencies += "org.scalacheck" %% "scalacheck" %  "1.14.0" % Test

scalacOptions ++= Seq(
  "-deprecation",
  "-explaintypes",
  "-feature",
)

// Required for deserialization to work when running through sbt
// (ObjectInputStream + ClassLoader problem)
fork in run := true