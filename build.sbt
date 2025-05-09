ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.0"

ThisBuild / organization := "com.peknight"

lazy val commonSettings = Seq(
  scalacOptions ++= Seq(
    "-feature",
    "-deprecation",
    "-unchecked",
    "-Xfatal-warnings",
    "-language:strictEquality",
    "-Xmax-inlines:64"
  ),
)

lazy val method = (project in file("."))
  .aggregate(
    methodCore.jvm,
    methodCore.js,
  )
  .settings(commonSettings)
  .settings(
    name := "method",
  )

lazy val methodCore = (crossProject(JSPlatform, JVMPlatform) in file("method-core"))
  .settings(commonSettings)
  .settings(
    name := "method-core",
    libraryDependencies ++= Seq(
      "com.peknight" %%% "error-core" % pekErrorVersion,
      "com.peknight" %%% "random-core" % pekRandomVersion,
      "com.peknight" %%% "spire-ext" % pekExtVersion,
    ),
  )

val pekVersion = "0.1.0-SNAPSHOT"
val pekRandomVersion = pekVersion
val pekErrorVersion = pekVersion
val pekExtVersion = pekVersion
