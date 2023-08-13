package com.github.gchudnov.bscript.lang.types

/**
  * A struct type
  * 
  * {{{
  *   struct {
  *     a: int
  *     b: string
  *   }
  * }}}
  * 
  * @param fields
  *   list of fields
  */
final case class TStruct(
  fields: List[TType],
)
