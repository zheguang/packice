lazy val commonSettings = Seq(
  version := "1.0",
  scalaVersion := "2.11.7"
)

lazy val packice = (project in file("."))
    .settings(
      name := "packice",
      commonSettings,
      libraryDependencies ++= Seq(
        "edu.stanford.nlp" % "stanford-corenlp" % "3.8.0" % Compile,
        "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.6" % Compile,
        "org.scalatest" %% "scalatest" % "3.0.1" % Test
      )
    )
