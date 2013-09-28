name := "course"

scalaVersion := "2.10.2"

resolvers ++= Seq(
  "Sonatype Snapshots"  at "http://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype Releases"   at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"
)
