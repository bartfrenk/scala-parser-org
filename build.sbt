name := "parser-org"

scalaVersion := "2.11.7"

mainClass in Compile := Some("run.Main")

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.0"

scalacOptions ++= Seq("-feature")

initialCommands in console := """
import parser.Org._
import Combinators._
import ParseState._
val org = "* 1\n** 11\n** 12\n*** 121\n* 2\n"
"""
