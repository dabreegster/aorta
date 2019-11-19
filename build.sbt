ThisBuild / scalaVersion := "2.10.3"

lazy val root = (project in file("."))
  .settings(
    name := "AORTA",

    scalaSource in Compile := baseDirectory.value / "utexas",

    libraryDependencies += "org.scala-lang" % "scala-swing" % scalaVersion.value,

    libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value,

    libraryDependencies += "org.jfree" % "jfreechart" % "1.0.15",

    libraryDependencies += "jline" % "jline" % "2.11",
)

enablePlugins(PackPlugin)
