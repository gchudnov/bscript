package com.github.gchudnov.bscript.interpreter.internal

trait StashEntry

final case class Stash(m: Map[String, StashEntry])

object Stash:
  val empty: Stash =
    Stash(Map.empty[String, StashEntry])
