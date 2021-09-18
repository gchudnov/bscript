import sbt.Keys._
import sbt._
import sbtassembly.AssemblyKeys._
import sbtassembly.MergeStrategy

object Settings {
  private val scalaV = "3.1.1"

  private val sharedScalacOptions = Seq(
    "-deprecation",                  // emit warning and location for usages of deprecated APIs
    "-explain",                      // explain errors in more detail
    "-explain-types",                // explain type errors in more detail
    "-feature",                      // emit warning and location for usages of features that should be imported explicitly
    "-indent",                       // allow significant indentation.
    "-new-syntax",                   // require scala 3.0 new syntax.
    "-print-lines",                  // show source code line numbers.
    "-unchecked",                    // enable additional warnings where generated code depends on assumptions
    "-Ykind-projector",              // allow `*` as wildcard to be compatible with kind projector
    "-Xfatal-warnings",              // fail the compilation if there are any warnings
    "-Xmigration",                   // warn about constructs whose behavior may have changed since version
    "-language:existentials",        // Existential types (besides wildcard types) can be written and inferred
    "-language:experimental.macros", // Allow macro definition (besides implementation and application)
    "-language:higherKinds",         // Allow higher-kinded types
    "-language:implicitConversions", // Allow definition of implicit functions called views
    "-language:postfixOps"           // Enable postfixOps
  )

  type MergeStrategySelector = String => MergeStrategy

  def defaultMergeStrategy(fallbackStrategy: MergeStrategySelector): MergeStrategySelector = {
    case x if x.contains("module-info.class")            => MergeStrategy.discard
    case x if x.contains("io.netty.versions.properties") => MergeStrategy.filterDistinctLines
    case x                                               => fallbackStrategy(x)
  }

  val globalScalaVersion: String = scalaV

  val assemblySettings: Seq[Setting[_]] = Seq(
    assembly / test                  := {},
    assembly / assemblyOutputPath    := new File("./target") / (assembly / assemblyJarName).value,
    assembly / assemblyMergeStrategy := defaultMergeStrategy((assembly / assemblyMergeStrategy).value)
  )

  val sharedResolvers: Vector[MavenRepository] = Seq(
    Resolver.mavenLocal,
    Resolver.sonatypeRepo("snapshots")
  ).toVector

  val shared: Seq[Setting[_]] = Seq(
    scalacOptions ++= sharedScalacOptions,
    scalaVersion      := scalaV,
    ThisBuild / turbo := true,
    resolvers         := Resolver.combineDefaultResolvers(sharedResolvers),
    compileOrder      := CompileOrder.JavaThenScala,
    organization      := "com.github.gchudnov"
  )

}
