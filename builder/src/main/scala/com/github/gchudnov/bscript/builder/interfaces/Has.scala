package com.github.gchudnov.bscript.builder.interfaces

/**
  * Has
  * 
  * Represents a state that has a value of type `A`.
  */
trait Has[A]:
  def get: A
