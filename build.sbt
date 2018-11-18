name := "st8"

version := "0.1"

scalaVersion := "2.12.6"

lazy val log4jVersion = "11.0"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"
libraryDependencies += "org.slf4j" % "slf4j-api" % "latest.release"
libraryDependencies += "org.testng" % "testng" % "latest.release" % "test"
libraryDependencies += "org.apache.logging.log4j" % "log4j-api" % "2.11.0"
libraryDependencies += "org.apache.logging.log4j" % "log4j-core" % "2.11.0"
libraryDependencies += "org.apache.logging.log4j" %% "log4j-api-scala" % log4jVersion
libraryDependencies += "com.google.guava" % "guava" % "latest.release"

libraryDependencies += "com.typesafe.akka" %% "akka-actor" % "2.5.13"
libraryDependencies += "com.typesafe.akka" %% "akka-testkit" % "2.5.13" % Test
