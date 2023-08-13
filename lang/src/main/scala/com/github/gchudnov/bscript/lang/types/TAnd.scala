package com.github.gchudnov.bscript.lang.types

/**
  * And type
  * 
  * {{{
  *   int & string
  * }}}
  *
  * @param left
  *   left type
  * @param right
  *   right type
  */
final case class TAnd(left: TType, right: TType) extends TAndOr
