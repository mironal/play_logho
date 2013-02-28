import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "play_logho"
  val appVersion      = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    // Add your project dependencies here,
    jdbc,
    anorm,
    "com.github.nscala-time" %% "nscala-time" % "0.2.0",
    "mysql" % "mysql-connector-java" % "5.1.21"
  )


  val main = play.Project(appName, appVersion, appDependencies).settings(
    // Add your own project settings here
  )

}
