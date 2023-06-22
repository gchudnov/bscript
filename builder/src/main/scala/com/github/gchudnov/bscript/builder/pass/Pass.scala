package com.github.gchudnov.bscript.builder.pass

/**
 * Pass
 *
 * Represents a step of the builder that takes an `In` state and produces an `Out` state.
 */
trait Pass[I, O]:
  def run(in: I): O
