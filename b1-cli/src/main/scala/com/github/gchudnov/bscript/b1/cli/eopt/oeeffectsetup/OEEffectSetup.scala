package com.github.gchudnov.bscript.b1.cli.eopt.oeeffectsetup

import scopt.OEffect
import scopt.OEffect.*

trait OEEffectSetup:
  def displayToOut(msg: String): Either[Throwable, Unit]
  def displayToErr(msg: String): Either[Throwable, Unit]
  def reportError(msg: String): Either[Throwable, Unit]
  def reportWarning(msg: String): Either[Throwable, Unit]
  def terminate(exitState: Either[String, Unit]): Either[Throwable, Unit]

  def runOEffects(effects: List[OEffect]): Either[Throwable, Unit]
