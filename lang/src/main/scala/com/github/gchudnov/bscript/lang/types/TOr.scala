package com.github.gchudnov.bscript.lang.types

/**
  * OR type
  *
  * @param left 
  *   left type
  * @param right
  *   right type
  */
final case class TOr(left: TType, right: TType) extends TAndOr
