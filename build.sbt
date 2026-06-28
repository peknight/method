import com.peknight.build.gav.*
import com.peknight.build.sbt.*

commonSettings

ThisBuild / scalacOptions --= Seq("-Werror", "-Xfatal-warnings")

lazy val method = (project in file("."))
  .settings(name := "method")
  .aggregate(methodCore.projectRefs *)

lazy val methodCore = (projectMatrix in file("method-core"))
  .settings(name := "method-core")
  .settings(libraryDependencies ++= dependencies(
    peknight.error,
    peknight.random,
    peknight.spire,
  ))
  .jvmPlatform(scalaVersions = Seq(scala.scala3.version))
  .jsPlatform(scalaVersions = Seq(scala.scala3.version))
