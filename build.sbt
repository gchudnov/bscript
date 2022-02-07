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

lazy val lang = (project in file("lang"))
  .settings(allSettings: _*)
  .settings(
    name := "lang",
    libraryDependencies ++= Dependencies.Lang
  )

lazy val serde = (project in file("serde"))
  .dependsOn(lang)
  .settings(allSettings: _*)
  .settings(
    name := "serde",
    libraryDependencies ++= Dependencies.Serde
  )

lazy val builder = (project in file("builder"))
  .dependsOn(lang)
  .settings(allSettings: _*)
  .settings(
    name := "builder",
    libraryDependencies ++= Dependencies.Builder
  )

lazy val interpreter = (project in file("interpreter"))
  .dependsOn(lang, builder)
  .settings(allSettings: _*)
  .settings(
    name := "interpreter",
    libraryDependencies ++= Dependencies.Interpreter
  )

lazy val b1 = (project in file("b1"))
  .dependsOn(lang)
  .settings(allSettings: _*)
  .settings(
    name := "b1",
    libraryDependencies ++= Dependencies.B1
  )

lazy val cli = (project in file("cli"))
  .enablePlugins(BuildInfoPlugin)
  .dependsOn(lang, b1)
  .settings(allSettings: _*)
  .settings(Settings.assemblySettings)
  .settings(
    name := "cli",
    libraryDependencies ++= Dependencies.Cli,
    buildInfoKeys                 := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage              := "com.github.gchudnov.bscript",
    assembly / mainClass          := Some("com.github.gchudnov.bscript.Main"),
    assembly / assemblyOption     := (assembly / assemblyOption).value.withPrependShellScript(Some(defaultUniversalScript(shebang = false))),
    assembly / assemblyOutputPath := new File(s"./target/${name.value}.jar"),
    assembly / assemblyJarName    := s"${name.value}"
  )

lazy val root = (project in file("."))
  .aggregate(lang, serde, builder, interpreter, b1, cli)
  .settings(allSettings: _*)
  .settings(
    name := "bscript"
  )

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("chk", "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck")
addCommandAlias("plg", "; reload plugins ; libraryDependencies ; reload return")
// NOTE: to use version check for plugins, add to the meta-project (/project/proect) sbt-updates.sbt with "sbt-updates" plugin as well.
addCommandAlias("upd", ";dependencyUpdates; reload plugins; dependencyUpdates; reload return")
