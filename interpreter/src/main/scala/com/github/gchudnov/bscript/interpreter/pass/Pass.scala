package com.github.gchudnov.bscript.interpreter.pass

/**
 * Pass
 *
 * Represents a step of the interpreter that takes an `I` state and produces an `O` state.
 */
private[interpreter] trait Pass[I, O]:
  def run(in: I): O
