name := "BCGTrans"

version := "0.1"

scalaVersion := "2.13.2"
libraryDependencies ++= Seq(
  // https://mvnrepository.com/artifact/com.google.cloud/google-cloud-translate
  "com.google.cloud" % "google-cloud-translate" % "1.84.0",
  // https://mvnrepository.com/artifact/org.scala-lang.modules/scala-parallel-collections
  "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0"
)
mainClass in assembly := Some("org.hackaton.bcg.Trans")
assemblyMergeStrategy in assembly := {
  case PathList("META-INF", xs@_*) => MergeStrategy.discard
  case x => MergeStrategy.first
}