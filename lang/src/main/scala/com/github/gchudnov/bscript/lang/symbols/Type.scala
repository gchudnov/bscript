package com.github.gchudnov.bscript.lang.symbols

/**
 * Type is used to distinguish between user-defined types and other program symbols.
 *
 * We’re using it only as a tag.
 * 
 * Usage:
 * {{{
 * Type -+
 *       |
 *       +- NamedType -+- TypeRef
 *       |
 *       +- AppliedType
 *       |
 *       +- AndOrType -+- AndType
 *                     +- OrType
 * 
 * }}}
 */
trait Type
