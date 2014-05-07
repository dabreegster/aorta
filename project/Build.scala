import sbt._
import Keys._

object AortaBuild extends Build {
  val buildSettings = Defaults.defaultSettings ++ xerial.sbt.Pack.packSettings ++ Seq(
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.10.3",
    scalacOptions ++= Seq() // Seq("-unchecked", "-deprecation", "-Xlint")
  )

  lazy val aorta = Project(
    id = "aorta",
    base = file("."),
    settings = buildSettings ++ Seq(
      scalaSource in Compile := baseDirectory.value / "utexas",
      libraryDependencies ++= Seq(
        "org.scala-lang" % "scala-swing" % scalaVersion.value,
        "org.scala-lang" % "scala-compiler" % scalaVersion.value,
        "org.jfree" % "jfreechart" % "1.0.15",
        "jline" % "jline" % "2.11"
      )
    )
  )
}
