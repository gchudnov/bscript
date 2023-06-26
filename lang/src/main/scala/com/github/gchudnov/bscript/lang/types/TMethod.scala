package com.github.gchudnov.bscript.lang.types

/**
  * A method type
  * 
  * {{{
  *   (int, string) -> bool
  * }}}
  *
  * @param params
  *   list of parameters
  * @param retType
  *   return type
  */
final case class TMethod(
  params: List[TType],
  retType: TType
) extends TType
