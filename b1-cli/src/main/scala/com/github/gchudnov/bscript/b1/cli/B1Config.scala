package com.github.gchudnov.bscript.b1.cli

import com.github.gchudnov.bscript.b1.cli.BuildInfo as AppBuildInfo
import com.github.gchudnov.bscript.b1.cli.eopt.SuccessExitException
import com.github.gchudnov.bscript.b1.cli.eopt.oeeffectsetup.OEEffectSetup
import com.github.gchudnov.bscript.interpreter.memory.CellPath
import scopt.OEffect.ReportError
import scopt.{ OEffect, OParser, OParserSetup, Read }

import java.io.File

enum Lang:
  case Scala2
  case Scala2J

sealed trait Command

object Command:
  final case class Run() extends Command

  final case class Debug(cellPath: CellPath) extends Command

  final case class Export(lang: Lang, outFile: File) extends Command

  val run: Run =
    Run()

  val debug: Debug =
    Debug(cellPath = CellPath.empty)

  val exprt: Export =
    Export(lang = Lang.Scala2, outFile = new File("."))

  def withDebugCell(c: Command, cellPath: CellPath): Command =
    c match
      case x: Debug =>
        x.copy(cellPath = cellPath)
      case _ =>
        Command.debug.copy(cellPath = cellPath)

  def withExportOut(c: Command, outFile: File): Command =
    c match
      case x: Export =>
        x.copy(outFile = outFile)
      case _ =>
        Command.exprt.copy(outFile = outFile)

  def withExportLang(c: Command, lang: Lang): Command =
    c match
      case x: Export =>
        x.copy(lang = lang)
      case _ =>
        Command.exprt.copy(lang = lang)

/**
 * A config built from the command line arguments.
 */
final case class B1Config(
  file: File,
  command: Command
)

object B1Config:

  val empty: B1Config =
    new B1Config(
      file = new File("."),
      command = Command.Run()
    )

  private val ArgHelpShort   = 'h'
  private val ArgHelpLong    = "help"
  private val ArgVersionLong = "version"
  private val ArgCellShort   = 'c'
  private val ArgCellLong    = "cell"
  private val ArgOutShort    = 'o'
  private val ArgOutLong     = "out"
  private val ArgLangShort   = 'l'
  private val ArgLangLong    = "lang"

  private val CmdRun    = "run"
  private val CmdDebug  = "debug"
  private val CmdExport = "export"

  private val OEffectPrefix     = "OEFFECT"
  private val OEffectHelpKey    = s"$OEffectPrefix:HELP"
  private val OEffectVersionKey = s"$OEffectPrefix:VERSION"

  private val scala2Key                      = "scala2"
  private val scala2JKey                     = "scala2j"
  private val allowedLanguages: List[String] = List(scala2Key, scala2JKey)
  private val allowedLanguagesStr: String    = allowedLanguages.mkString(",")

  private implicit val langRead: Read[Lang] =
    Read.stringRead.map {
      case `scala2Key` =>
        Lang.Scala2
      case `scala2JKey` =>
        Lang.Scala2J
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
        .text("AST file to be interpreted"),
      cmd(CmdRun)
        .optional()
        .action((_, c) => c.copy(command = Command.run))
        .text("run AST")
        .children(
        ),
      cmd(CmdDebug)
        .optional()
        .action((_, c) => c.copy(command = Command.debug))
        .text("debug AST")
        .children(
          opt[String](ArgCellShort, ArgCellLong)
            .required()
            .action((x, c) => c.copy(command = Command.withDebugCell(c.command, CellPath(x))))
            .text("Path to the variable to debug: a.b.c")
        ),
      cmd(CmdExport)
        .optional()
        .action((_, c) => c.copy(command = Command.exprt))
        .text("export AST")
        .children(
          opt[Lang](ArgLangShort, ArgLangLong)
            .required()
            .action((x, c) => c.copy(command = Command.withExportLang(c.command, x)))
            .text(s"language to export: [${allowedLanguagesStr}]"),
          opt[File](ArgOutShort, ArgOutLong)
            .required()
            .action((x, c) => c.copy(command = Command.withExportOut(c.command, x)))
            .text("Path to output file")
        ),
      note("""
             |Examples:
             |
             |  - Run AST
             |    b1-cli --run /path/to/ast.json
             |    b1-cli /path/to/ast.json
             |
             |  - Debug AST
             |    b1-cli --debug --cell="a.b.c" /path/to/ast.json
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
