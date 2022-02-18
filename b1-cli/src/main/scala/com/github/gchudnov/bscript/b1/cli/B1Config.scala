package com.github.gchudnov.bscript.b1.cli

import com.github.gchudnov.bscript.b1.cli.eopt.SuccessExitException
import com.github.gchudnov.bscript.b1.cli.eopt.oeeffectsetup.OEEffectSetup
import com.github.gchudnov.bscript.b1.cli.BuildInfo as AppBuildInfo
import com.github.gchudnov.bscript.interpreter.memory.CellPath
import scopt.OEffect
import scopt.OEffect.ReportError
import scopt.OParser
import scopt.OParserSetup

import java.io.File

sealed trait Action

case object RunAction   extends Action
case object DebugAction extends Action

/**
 * A config built from the command line arguments.
 */
final case class B1Config(
  file: File,
  cellPath: CellPath,
  action: Action
)

object B1Config:

  val empty: B1Config =
    new B1Config(
      file = new File("."),
      cellPath = CellPath.empty,
      action = RunAction
    )

  private val ArgHelpShort   = 'h'
  private val ArgHelpLong    = "help"
  private val ArgVersionLong = "version"
  private val ArgRunShort    = 'r'
  private val ArgRunLong     = "run"
  private val ArgDebugShort  = 'd'
  private val ArgDebugLong   = "debug"
  private val ArgFileShort   = 'f'
  private val ArgFileLong    = "file"
  private val ArgCellShort   = 'c'
  private val ArgCellLong    = "cell"

  private val OEffectPrefix     = "OEFFECT"
  private val OEffectHelpKey    = s"$OEffectPrefix:HELP"
  private val OEffectVersionKey = s"$OEffectPrefix:VERSION"

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
      opt[Unit](ArgRunShort, ArgRunLong)
        .action((x, c) => c.copy(action = RunAction))
        .text("Run AST"),
      opt[File](ArgDebugShort, ArgDebugLong)
        .action((x, c) => c.copy(action = DebugAction))
        .text("Debug AST"),
      opt[String](ArgCellShort, ArgCellLong)
        .action((x, c) => c.copy(cellPath = CellPath(x)))
        .text("Path to the variable to trace its value"),
      arg[File]("<file>")
        .required()
        .action((x, c) => c.copy(file = x))
        .text("AST file to be interpreted"),
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
        if c.action == DebugAction && c.cellPath.isEmpty
        then Left("Cell path is required for debug mode")
        else Right(())
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
