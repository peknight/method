ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.6.2"

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
  )
  .settings(commonSettings)
  .settings(
    name := "method",
  )

lazy val methodError = (crossProject(JSPlatform, JVMPlatform) in file("method-error"))
  .settings(commonSettings)
  .settings(
    name := "method-error",
    libraryDependencies ++= Seq(
      "com.peknight" %%% "error-core" % pekErrorVersion,
      "org.typelevel" %%% "cats-effect" % catsEffectVersion,
    ),
  )

val catsEffectVersion = "3.5.7"
val pekVersion = "0.1.0-SNAPSHOT"
val pekErrorVersion = pekVersion
