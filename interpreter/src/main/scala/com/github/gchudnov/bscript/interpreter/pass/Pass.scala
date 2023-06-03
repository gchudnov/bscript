package com.github.gchudnov.bscript.interpreter.pass

/**
 * Pass
 *
 * Represents a step of the builder that takes an `In` state and produces an `Out` state.
 */
trait Pass:
  type In
  type Out

  def run(in: In): Out
