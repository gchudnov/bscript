package com.github.gchudnov.bscript.lang.types

/**
 * Type of a symbol.
 *
 * Usage:
 * {{{
 * TType -+
 *        |
 *        +- TRef
 *        |
 *        +- TApplied
 *        |
 *        +- TAndOr -+- TAnd
 *                   +- TOr
 *
 * }}}
 */
trait TType
