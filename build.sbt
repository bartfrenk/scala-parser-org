name := "parser-org"

scalaVersion := "2.11.7"

mainClass in Compile := Some("run.Main")

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.0"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.5" % "test"

scalacOptions ++= Seq("-feature", "-deprecation")

initialCommands in console := """
import parsers._
import OrgParsers._
//val source = scala.io.Source.fromFile("test.org")
//val lines = try source.mkString finally source.close()
"""
