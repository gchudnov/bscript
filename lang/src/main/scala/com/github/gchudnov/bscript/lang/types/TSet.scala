package com.github.gchudnov.bscript.lang.types

/**
 * A set type
 * 
 * {{{
 *   set<int>
 * }}}
 *
 * @param keyType
 *   key type
 */
final case class TSet(
  keyType: TType,
) extends TType
