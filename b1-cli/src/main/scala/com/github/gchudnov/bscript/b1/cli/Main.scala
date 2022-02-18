package com.github.gchudnov.bscript.b1.cli

import com.github.gchudnov.bscript.b1.cli.eopt.ExitException
import com.github.gchudnov.bscript.b1.cli.eopt.oeeffectsetup.OEEffectSetup
import com.github.gchudnov.bscript.b1.cli.eopt.oeeffectsetup.StdioEffectSetup
import com.github.gchudnov.bscript.b1.internal.util.FileOps
import scopt.DefaultOParserSetup
import scopt.OParserSetup
import com.github.gchudnov.bscript.b1.B1

object Main:
  private val osetup: OEEffectSetup = makeOZEffectSetup()
  private val psetup: OParserSetup  = makePEffectSetup()

  def main(args: Array[String]): Unit =
    run(args.toList).fold(
      {
        case x: ExitException =>
          sys.exit(x.code)
        case t =>
          Console.err.println(t.getMessage)
          sys.exit(1)
      },
      _ => ()
    )

  private def run(args: List[String]): Either[Throwable, Unit] =
    for
      cfg <- B1Config.fromArgs(args)(psetup, osetup)
      _   <- runAction(cfg)
    yield ()

  private def makeOZEffectSetup(): OEEffectSetup =
    StdioEffectSetup.make()

  private def makePEffectSetup(): OParserSetup =
    new DefaultOParserSetup with OParserSetup:
      override def errorOnUnknownArgument: Boolean   = false
      override def showUsageOnError: Option[Boolean] = Some(false)

  private def runAction(cfg: B1Config): Either[Throwable, Unit] =
    for
      data <- FileOps.stringFrom(cfg.file.toPath)
      ast0 <- B1.load(data)
      _ <- cfg.action match
             case RunAction =>
               for
                 retValue <- B1.run(ast0)
                 _         = Console.out.println(retValue)
               yield ()
             case DebugAction =>
               for
                 res            <- B1.debug(cfg.cellPath.value, ast0)
                 (retValue, log) = res
                 _               = Console.out.println(retValue)
                 _               = Console.out.println(log)
               yield ()
    yield ()
