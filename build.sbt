name := "scala-macro-experiments"

scalaVersion := "2.10.0-SNAPSHOT"

resolvers += "snaps" at "https://oss.sonatype.org/content/repositories/snapshots"

scalacOptions in Compile ++= Seq("-Xmacros", "-Yreify-copypaste")
