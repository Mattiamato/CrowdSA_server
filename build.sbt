name := "crowdsaserver"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.4"

fork := true

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache
)

libraryDependencies += "mysql" % "mysql-connector-java" % "5.1.21"

libraryDependencies += "org.apache.pdfbox" % "pdfbox" % "1.8.7"


play.PlayImport.PlayKeys.playDefaultPort := 9000

lazy val crowdsaserver = (project in file(".")).enablePlugins(PlayScala)

