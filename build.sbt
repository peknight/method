import com.peknight.build.gav.*
import com.peknight.build.sbt.*

commonSettings

lazy val method = (project in file("."))
  .aggregate(
    methodCore.jvm,
    methodCore.js,
  )
  .settings(
    name := "method",
  )

lazy val methodCore = (crossProject(JVMPlatform, JSPlatform) in file("method-core"))
  .settings(crossDependencies(
    peknight.error,
    peknight.random,
    peknight.ext.spire,
  ))
  .settings(
    name := "method-core",
  )
