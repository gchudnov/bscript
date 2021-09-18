import sbt.Keys._
import sbt._
import sbtassembly.AssemblyPlugin.defaultUniversalScript

Global / cancelable   := true
Global / scalaVersion := Settings.globalScalaVersion

def testFilter(name: String): Boolean = name endsWith "Spec"

lazy val testSettings = Seq(
  Test / testOptions ++= Seq(Tests.Filter(testFilter))
)

lazy val allSettings = Settings.shared ++ testSettings

lazy val lib = (project in file("lib"))
  .settings(allSettings: _*)
  .settings(
    name := "lib",
    libraryDependencies ++= Dependencies.Lib
  )

lazy val cli = (project in file("cli"))
  .enablePlugins(BuildInfoPlugin)
  .dependsOn(lib)
  .settings(allSettings: _*)
  .settings(Settings.assemblySettings)
  .settings(
    name := "bscript",
    libraryDependencies ++= Dependencies.Cli,
    buildInfoKeys                 := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage              := "com.github.gchudnov.bscript",
    assembly / mainClass          := Some("com.github.gchudnov.bscript.Main"),
    assembly / assemblyOption     := (assembly / assemblyOption).value.withPrependShellScript(Some(defaultUniversalScript(shebang = false))),
    assembly / assemblyOutputPath := new File(s"./target/${name.value}.jar"),
    assembly / assemblyJarName    := s"${name.value}"
  )

lazy val root = (project in file("."))
  .aggregate(lib, cli)
  .settings(allSettings: _*)
  .settings(
    name := "bscript-root"
  )

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("chk", "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck")
addCommandAlias("plg", "; reload plugins ; libraryDependencies ; reload return")
// NOTE: to use version check for plugins, add to the meta-project (/project/proect) sbt-updates.sbt with "sbt-updates" plugin as well.
addCommandAlias("upd", ";dependencyUpdates; reload plugins; dependencyUpdates; reload return")
