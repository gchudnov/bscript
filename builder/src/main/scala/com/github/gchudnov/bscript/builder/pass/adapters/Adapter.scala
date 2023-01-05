package com.github.gchudnov.bscript.builder.pass.adapters

/**
  * Adapter is an interface to transform states between passes
  */
trait Adapter[I, O]:
  def map(in: I): O
