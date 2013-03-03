name := "scala-lisp"

scalaVersion := "2.10.0"

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

libraryDependencies ++= Seq(
  "org.scala-lang" % "jline" % "2.10.0"
)
