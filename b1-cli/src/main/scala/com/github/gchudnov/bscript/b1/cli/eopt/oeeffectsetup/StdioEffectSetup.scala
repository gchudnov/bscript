package com.github.gchudnov.bscript.b1.cli.eopt.oeeffectsetup

import com.github.gchudnov.bscript.b1.cli.eopt.{ FailureExitException, SuccessExitException }
import scala.util.control.Exception.allCatch
import scopt.OEffect
import scopt.OEffect.*

final class StdioEffectSetup() extends OEEffectSetup:

  override def displayToOut(msg: String): Either[Throwable, Unit] =
    allCatch.either(Console.out.println(msg))

  override def displayToErr(msg: String): Either[Throwable, Unit] =
    allCatch.either(Console.err.println(msg))

  override def reportError(msg: String): Either[Throwable, Unit] =
    displayToErr("Error: " + msg)

  override def reportWarning(msg: String): Either[Throwable, Unit] =
    displayToErr("Warning: " + msg)

  override def terminate(exitState: Either[String, Unit]): Either[Throwable, Unit] =
    exitState match
      case Left(_)  => Left(new FailureExitException())
      case Right(_) => Left(new SuccessExitException())

  override def runOEffects(effects: List[OEffect]): Either[Throwable, Unit] =
    effects.foldLeft(Right(()): Either[Throwable, Unit]) { case (acc, effect) =>
      acc.flatMap(_ =>
        effect match
          case DisplayToOut(msg)    => displayToOut(msg)
          case DisplayToErr(msg)    => displayToErr(msg)
          case ReportError(msg)     => reportError(msg)
          case ReportWarning(msg)   => reportWarning(msg)
          case Terminate(exitState) => terminate(exitState)
      )
    }

object StdioEffectSetup:

  def make(): OEEffectSetup =
    new StdioEffectSetup()
