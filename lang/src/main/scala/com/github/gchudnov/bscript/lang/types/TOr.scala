package com.github.gchudnov.bscript.lang.types

/**
  * OR type
  * 
  * {{{
  *   int | string
  * }}}
  *
  * @param left 
  *   left type
  * @param right
  *   right type
  */
final case class TOr(left: TType, right: TType) extends TAndOr
