package com.github.gchudnov.bscript.b1.cli

import com.github.gchudnov.bscript.b1.B1
import com.github.gchudnov.bscript.b1.cli.display.{ CellDisplay, MemWatchDisplay }
import com.github.gchudnov.bscript.b1.cli.eopt.ExitException
import com.github.gchudnov.bscript.b1.cli.eopt.oeeffectsetup.{ OEEffectSetup, StdioEffectSetup }
import com.github.gchudnov.bscript.b1.internal.util.FileOps
import scopt.{ DefaultOParserSetup, OParserSetup }

object Main:
  private val osetup: OEEffectSetup = makeOEEffectSetup()
  private val psetup: OParserSetup  = makePEffectSetup()

  def main(args: Array[String]): Unit =
    run(args.toList).fold(
      {
        case x: ExitException =>
          sys.exit(x.code)
        case t =>
          Console.err.println(t)
          sys.exit(1)
      },
      _ => ()
    )

  private def run(args: List[String]): Either[Throwable, Unit] =
    for
      cfg <- B1Config.fromArgs(args)(psetup, osetup)
      _   <- runAction(cfg)
    yield ()

  private def makeOEEffectSetup(): OEEffectSetup =
    StdioEffectSetup.make()

  private def makePEffectSetup(): OParserSetup =
    new DefaultOParserSetup with OParserSetup:
      override def errorOnUnknownArgument: Boolean   = false
      override def showUsageOnError: Option[Boolean] = Some(false)

  private def runAction(cfg: B1Config): Either[Throwable, Unit] =
    for
      data <- FileOps.stringFrom(cfg.file.toPath)
      ast0 <- B1.load(data)
      _ <- cfg.command match
             case Command.Run() =>
               for
                 retValue <- B1.run(ast0)
                 _         = CellDisplay.print(retValue)
               yield ()
             case Command.Debug(cellPath) =>
               for
                 res            <- B1.debug(cellPath.value, ast0)
                 (retValue, log) = res
                 _               = CellDisplay.print(retValue)
                 _               = MemWatchDisplay.print(log)
               yield ()
             case Command.Export(lang, outFile) =>
               ???
    yield ()
