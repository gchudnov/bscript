package com.github.gchudnov.bscript.lang.types

/**
  * A vector type
  * 
  * {{{
  *   vec<int>
  * }}}
  *
  * @param elemType
  *   element type
  */
final case class TVec(
  elemType: TType,
) extends TType
