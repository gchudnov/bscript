import sbt.Keys._
import sbt._
import sbtassembly.AssemblyPlugin.defaultUniversalScript

Global / cancelable   := true
Global / scalaVersion := Settings.globalScalaVersion
Global / semanticdbEnabled := true

def testFilter(name: String): Boolean = name endsWith "Spec"

lazy val testSettings = Seq(
  Test / testOptions ++= Seq(Tests.Filter(testFilter))
)

lazy val allSettings = Settings.shared ++ testSettings

lazy val lang = (project in file("lang"))
  .disablePlugins(AssemblyPlugin)
  .settings(allSettings: _*)
  .settings(
    name := "bscript-lang",
    libraryDependencies ++= Dependencies.Lang
  )

lazy val rewriter = (project in file("rewriter"))
  .dependsOn(lang)
  .disablePlugins(AssemblyPlugin)
  .settings(allSettings: _*)
  .settings(
    name := "bscript-rewriter",
    libraryDependencies ++= Dependencies.Rewriter
  )

lazy val serde = (project in file("serde"))
  .dependsOn(lang, rewriter)
  .disablePlugins(AssemblyPlugin)
  .settings(allSettings: _*)
  .settings(
    name := "bscript-serde",
    libraryDependencies ++= Dependencies.Serde
  )

lazy val builder = (project in file("builder"))
  .dependsOn(lang)
  .disablePlugins(AssemblyPlugin)
  .settings(allSettings: _*)
  .settings(
    name := "bscript-builder",
    libraryDependencies ++= Dependencies.Builder
  )

lazy val interpreter = (project in file("interpreter"))
  .dependsOn(lang, builder)
  .disablePlugins(AssemblyPlugin)
  .settings(allSettings: _*)
  .settings(
    name := "bscript-interpreter",
    libraryDependencies ++= Dependencies.Interpreter
  )

lazy val translator = (project in file("translator"))
  .dependsOn(lang, builder)
  .disablePlugins(AssemblyPlugin)
  .settings(allSettings: _*)
  .settings(
    name := "bscript-translator",
    libraryDependencies ++= Dependencies.Translator
  )

lazy val inspector = (project in file("inspector"))
  .dependsOn(lang, builder, interpreter, translator, rewriter)
  .disablePlugins(AssemblyPlugin)
  .settings(allSettings: _*)
  .settings(
    name := "bscript-inspector",
    libraryDependencies ++= Dependencies.Inspector
  )

lazy val b1 = (project in file("b1"))
  .dependsOn(lang, serde, builder, interpreter, translator, inspector)
  .disablePlugins(AssemblyPlugin)
  .settings(allSettings: _*)
  .settings(
    name := "bscript-b1",
    libraryDependencies ++= Dependencies.B1
  )

lazy val b1Cli = (project in file("b1-cli"))
  .enablePlugins(BuildInfoPlugin)
  .dependsOn(b1, interpreter, inspector)
  .settings(allSettings: _*)
  .settings(Settings.assemblySettings)
  .settings(Settings.noPublish)
  .settings(
    name := "b1-cli",
    libraryDependencies ++= Dependencies.B1Cli,
    buildInfoKeys                 := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
    buildInfoPackage              := "com.github.gchudnov.bscript.b1.cli",
    assembly / mainClass          := Some("com.github.gchudnov.bscript.b1.cli.Main"),
    assembly / assemblyOption     := (assembly / assemblyOption).value.withPrependShellScript(Some(defaultUniversalScript(shebang = true))),
    assembly / assemblyOutputPath := new File(s"./target/${name.value}"),
    assembly / assemblyJarName    := s"${name.value}"
  )

lazy val root = (project in file("."))
  .aggregate(lang, rewriter, serde, builder, interpreter, translator, inspector, b1, b1Cli)
  .disablePlugins(AssemblyPlugin)
  .settings(allSettings: _*)
  .settings(
    name := "bscript"
  )

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias("chk", "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck")
addCommandAlias("plg", "; reload plugins ; libraryDependencies ; reload return")
// NOTE: to use version check for plugins, add to the meta-project (/project/project) sbt-updates.sbt with "sbt-updates" plugin as well.
addCommandAlias("upd", ";dependencyUpdates; reload plugins; dependencyUpdates; reload return")
