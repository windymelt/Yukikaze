import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "Yukikaze"
  val appVersion      = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    // Add your project dependencies here,
    jdbc,
    anorm,
    "com.github.scala-incubator.io" %% "scala-io-core" % "0.4.0",
    "com.github.scala-incubator.io" %% "scala-io-file" % "0.4.0"

  )


  val main = play.Project(appName, appVersion, appDependencies).settings(
    // Add your own project settings here      
  )

}
