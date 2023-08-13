package com.github.gchudnov.bscript.lang.util

/**
  * A trait to convert a value to a string
  */
trait Show[A]:
  def show(a: A): String
