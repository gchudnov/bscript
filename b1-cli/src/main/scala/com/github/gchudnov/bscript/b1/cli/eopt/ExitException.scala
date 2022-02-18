package com.github.gchudnov.bscript.b1.cli.eopt

sealed abstract class ExitException(val code: Int) extends RuntimeException(s"Exit Code: $code")

final class SuccessExitException() extends ExitException(0)
final class FailureExitException() extends ExitException(1)

object ExitException:
  def apply(t: Throwable): Boolean = t match
    case _: ExitException => true
    case _                => false

  def unapply(t: Throwable): Option[Throwable] = Some(t).filter(apply)
