package com.github.gchudnov.bscript.lang.util

trait Show[A]:
  extension (a: A) def show: String
