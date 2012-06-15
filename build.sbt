name := "scala-macro-experiments"

scalaVersion := "2.10.0-M4"

libraryDependencies += "org.scala-lang" % "scala-reflect" % "2.10.0-M4"

resolvers += "snaps" at "https://oss.sonatype.org/content/repositories/snapshots"

scalacOptions in Compile ++= Seq("-deprecation", "-unchecked", "-feature")
