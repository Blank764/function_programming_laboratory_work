scalaVersion := "3.9.0"

lazy val root = rootProject
  .settings(
    name := "untitled",
    libraryDependencies ++= Seq(
      //You can add library dependencies here, for example,
      //"org.scalatest" %% "scalatest" % "3.2.19" % Test,
      //"org.scalameta" %% "munit" % "1.2.3" % Test
      "com.github.pathikrit" %% "better-files" % "3.9.2",
      "org.ini4j" % "ini4j" % "0.5.4",
      "org.slf4j" % "slf4j-api" % "2.0.16",
      "ch.qos.logback" % "logback-classic" % "1.5.15",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5",
    )
  )
