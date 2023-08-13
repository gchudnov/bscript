package com.github.gchudnov.bscript.lang.types

/**
  * A map type
  * 
  * {{{
  *   map<keyType, valueType>
  * }}}
  * 
  * @param keyType
  *   key type
  * @param valueType
  *   value type
  */
final case class TMap(
  keyType: TType,
  valueType: TType
) extends TType
