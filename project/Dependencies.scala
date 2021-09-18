import sbt._

object Dependencies {

  object versions {
    val circe              = "0.14.1"
    val circeGenericExtras = "0.14.1"
    val kindProjector      = "0.10.3"
    val scalaLogging = "3.9.4"
    val logbackClassic     = "1.2.5"
    val scopt              = "4.0.1"
    val scalatest    = "3.2.9"
  }

  // compiler plugins
  private val kindProjector = compilerPlugin("org.typelevel" %% "kind-projector" % versions.kindProjector)

  private val compiler = Seq(
    kindProjector
  )

  private val logbackClassic = "ch.qos.logback" % "logback-classic" % versions.logbackClassic
  private val scalaLogging = "com.typesafe.scala-logging" %% "scala-logging"   % versions.scalaLogging

  private val scalatest         = "org.scalatest"     %% "scalatest"           % versions.scalatest

  private val circeCore          = "io.circe" %% "circe-core"           % versions.circe
  private val circeGeneric       = "io.circe" %% "circe-generic"        % versions.circe
  private val circeGenericEntras = "io.circe" %% "circe-generic-extras" % versions.circeGenericExtras
  private val circeParser        = "io.circe" %% "circe-parser"         % versions.circe

  private val scopt = "com.github.scopt" %% "scopt" % versions.scopt

  val Lib: Seq[ModuleID] = {
    val compile = Seq(
      logbackClassic,
      scalaLogging,
      scalaLogging,
      circeCore,
      circeGeneric,
      circeGenericEntras,
      circeParser,
    )
    val test = Seq(
      scalatest
    ) map (_ % "test")
    compile ++ test ++ compiler
  }

  val Cli: Seq[ModuleID] = {
    val compile = Seq(
      logbackClassic,
      scalaLogging,
      scopt,
    )
    val test = Seq(
      scalatest
    ) map (_ % "test")
    compile ++ test ++ compiler
  }
}