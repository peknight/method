import com.peknight.build.gav.*
import com.peknight.build.sbt.*

commonSettings

lazy val method = (project in file("."))
  .settings(name := "method")
  .aggregate(
    methodCore.jvm,
    methodCore.js,
  )

lazy val methodCore = (crossProject(JVMPlatform, JSPlatform) in file("method-core"))
  .settings(name := "method-core")
  .settings(crossDependencies(
    peknight.error,
    peknight.random,
    peknight.ext.spire,
  ))
