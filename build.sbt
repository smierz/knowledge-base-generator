name := "KnowledgeBaseGenerator"
version := "1.0"
scalaVersion := "2.12.3"

libraryDependencies ++= Seq(
 	 "org.scalatest" %% "scalatest" % "3.0.1" % "test",
 	 "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6",
 	 "org.rogach" %% "scallop" % "3.1.5"
)