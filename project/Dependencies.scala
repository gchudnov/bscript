import sbt._

object Dependencies {

  object versions {
    val scopt              = "4.0.1"
    val scalatest          = "3.2.11"
    val json4sNative       = "4.0.4"
  }

  private val scalatest = "org.scalatest" %% "scalatest" % versions.scalatest

  private val json4sNative = "org.json4s" %% "json4s-native" % versions.json4sNative

  private val scopt = "com.github.scopt" %% "scopt" % versions.scopt

  val Lang: Seq[ModuleID] = {
    val compile = Seq(
    )
    val test = Seq(
      scalatest
    ) map (_ % "test")
    compile ++ test
  }

  val Serde: Seq[ModuleID] = {
    val compile = Seq(
      json4sNative
    )
    val test = Seq(
      scalatest
    ) map (_ % "test")
    compile ++ test
  }

  val B1: Seq[ModuleID] = {
    val compile = Seq(
    )
    val test = Seq(
      scalatest
    ) map (_ % "test")
    compile ++ test
  }

  val Cli: Seq[ModuleID] = {
    val compile = Seq(
      scopt
    )
    val test = Seq(
      scalatest
    ) map (_ % "test")
    compile ++ test
  }
}
