name := "scala-macro-experiments"

scalaVersion := "2.10.0-M3"

resolvers += "snaps" at "https://oss.sonatype.org/content/repositories/snapshots"

scalacOptions in Compile ++= Seq("-deprecation", "-unchecked", "-Xmacros", "-Yreify-copypaste")
