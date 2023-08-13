package com.github.gchudnov.bscript.builder.pass

/**
 * Pass
 *
 * Represents a step of the builder that takes an `I` state and produces an `O` state.
 */
private[builder] trait Pass[I, O]:
  def run(in: I): O
