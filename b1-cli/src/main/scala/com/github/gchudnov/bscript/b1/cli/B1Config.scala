package com.github.gchudnov.bscript.b1.cli

import com.github.gchudnov.bscript.b1.cli.BuildInfo as AppBuildInfo
import com.github.gchudnov.bscript.b1.cli.eopt.SuccessExitException
import com.github.gchudnov.bscript.b1.cli.eopt.oeeffectsetup.OEEffectSetup
import com.github.gchudnov.bscript.interpreter.memory.CellPath
import scopt.OEffect.ReportError
import scopt.{ OEffect, OParser, OParserSetup, Read }

import java.io.File

enum Lang:
  case Scala3
  case Scala3J

sealed trait Command:
  import Command.Run
  import Command.Debug
  import Command.Export

  def withDebugCell(cellPath: CellPath): Command =
    this match
      case x: Debug =>
        x.copy(cellPath = cellPath)
      case _ =>
        Debug.empty.copy(cellPath = cellPath)

  def withExportOut(outFile: File): Command =
    this match
      case x: Export =>
        x.copy(outFile = outFile)
      case _ =>
        Export.empty.copy(outFile = outFile)

  def withExportLang(lang: Lang): Command =
    this match
      case x: Export =>
        x.copy(lang = lang)
      case _ =>
        Export.empty.copy(lang = lang)

  def withExportPrelude(prelude: Boolean): Command =
    this match
      case x: Export =>
        x.copy(prelude = prelude)
      case _ =>
        Export.empty.copy(prelude = prelude)

object Command:
  // Run
  final case class Run() extends Command

  object Run:
    val empty: Run =
      Run()

  // Debug
  final case class Debug(cellPath: CellPath) extends Command

  object Debug:
    val empty: Debug =
      Debug(cellPath = CellPath.empty)

  // Export
  final case class Export(lang: Lang, outFile: File, prelude: Boolean) extends Command

  object Export:
    val empty: Export =
      Export(lang = Lang.Scala3, outFile = new File("."), prelude = true)

/**
 * A config built from the command line arguments.
 */
final case class B1Config(
  file: File,
  command: Command
)

object B1Config:
  import Command.*

  val empty: B1Config =
    new B1Config(
      file = new File("."),
      command = Command.Run()
    )

  private val ArgHelpShort   = 'h'
  private val ArgHelpLong    = "help"
  private val ArgVersionLong = "version"
  private val ArgRefShort    = 'r'
  private val ArgRefLong     = "ref"
  private val ArgOutShort    = 'o'
  private val ArgOutLong     = "out"
  private val ArgLangShort   = 'l'
  private val ArgLangLong    = "lang"
  private val ArgPreludeLong = "prelude"

  private val CmdRun    = "run"
  private val CmdDebug  = "debug"
  private val CmdExport = "export"

  private val OEffectPrefix     = "OEFFECT"
  private val OEffectHelpKey    = s"$OEffectPrefix:HELP"
  private val OEffectVersionKey = s"$OEffectPrefix:VERSION"

  private val scala3Key                      = Lang.Scala3.toString.toLowerCase
  private val scala3JKey                     = Lang.Scala3J.toString.toLowerCase
  private val allowedLanguages: List[String] = List(scala3Key, scala3JKey)
  private val allowedLanguagesStr: String    = allowedLanguages.mkString(",")

  private implicit val langRead: Read[Lang] =
    Read.stringRead.map {
      case `scala3Key` =>
        Lang.Scala3
      case `scala3JKey` =>
        Lang.Scala3J
      case _ =>
        throw new IllegalArgumentException(s"lang must be one of [${allowedLanguagesStr}]")
    }

  private val argsBuilder = OParser.builder[B1Config]
  private val argsParser =
    import argsBuilder.*
    OParser.sequence(
      programName(BuildInfo.name),
      head(BuildInfo.name, BuildInfo.version),
      opt[Unit](ArgHelpShort, ArgHelpLong)
        .optional()
        .text("prints this usage text")
        .validate(_ => Left(OEffectHelpKey)),
      opt[Unit](ArgVersionLong)
        .optional()
        .text("prints the version")
        .validate(_ => Left(OEffectVersionKey)),
      arg[File]("<file>")
        .required()
        .action((x, c) => c.copy(file = x))
        .text("AST file to be processed"),
      note(""),
      cmd(CmdRun)
        .optional()
        .action((_, c) => c.copy(command = Run.empty))
        .text("run AST-file")
        .children(
        ),
      note(""),
      cmd(CmdDebug)
        .optional()
        .action((_, c) => c.copy(command = Debug.empty))
        .text("debug AST-file")
        .children(
          opt[String](ArgRefShort, ArgRefLong)
            .required()
            .action((x, c) => c.copy(command = c.command.withDebugCell(CellPath(x))))
            .text("reference to the variable to watch: a.b.c")
        ),
      note(""),
      cmd(CmdExport)
        .optional()
        .action((_, c) => c.copy(command = Export.empty))
        .text("export AST-file")
        .children(
          opt[Lang](ArgLangShort, ArgLangLong)
            .required()
            .action((x, c) => c.copy(command = c.command.withExportLang(x)))
            .text(s"language to export: [${allowedLanguagesStr}]"),
          opt[File](ArgOutShort, ArgOutLong)
            .required()
            .action((x, c) => c.copy(command = c.command.withExportOut(x)))
            .text("Path to output file"),
          opt[Boolean](ArgPreludeLong)
            .optional()
            .action((x, c) => c.copy(command = c.command.withExportPrelude(x)))
            .text(s"include standard library functions in the generated code (default: ${Export.empty.prelude})")
        ),
      note("""
             |Examples:
             |
             |  - Run AST
             |    b1-cli run /path/to/ast.json
             |
             |  - Debug AST
             |    b1-cli debug --ref="a.b.c" /path/to/ast.json
             |
             |  - Export AST
             |    b1-cli export --lang=scala3 --out=/path/to/out.scala /path/to/ast.json
             |""".stripMargin),
      checkConfig(c =>
        // NOTE: if an error, return Left("String with a description")
        Right(())
      )
    )

  def fromArgs(args: List[String])(argParserSetup: OParserSetup, oEEffectSetup: OEEffectSetup): Either[Throwable, B1Config] =
    OParser.runParser(argsParser, args, B1Config.empty, argParserSetup) match
      case (result, effects) =>
        for
          pEffects <- preprocessOEffects(effects)(oEEffectSetup)
          _        <- oEEffectSetup.runOEffects(pEffects)
          config   <- result.toRight(new IllegalArgumentException(s"Use --$ArgHelpLong for more information."))
        yield config

  private def preprocessOEffects(effects: List[OEffect])(oEEffectSetup: OEEffectSetup): Either[Throwable, List[OEffect]] =
    val hasHelp    = hasKey(OEffectHelpKey)(effects)
    val hasVersion = hasKey(OEffectVersionKey)(effects)

    if hasHelp || hasVersion then
      val value = (hasHelp, hasVersion) match
        case (true, _) =>
          usage()
        case (false, true) =>
          version()
        case (_, _) =>
          ""
      oEEffectSetup.displayToOut(value).flatMap(_ => Left(new SuccessExitException()))
    else Right(effects.filterNot(it => it.isInstanceOf[ReportError] && it.asInstanceOf[ReportError].msg.startsWith(OEffectPrefix)))

  private def hasKey(key: String)(effects: List[OEffect]): Boolean =
    effects.exists {
      case ReportError(msg) if (msg == key) => true
      case _                                => false
    }

  def usage(): String =
    OParser.usage(argsParser)

  def version(): String =
    s"${AppBuildInfo.name} ${AppBuildInfo.version}"
