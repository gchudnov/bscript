package com.github.gchudnov.bscript.lang.ast

/**
 * Aggregates both evalType and promoteToType to enforce static typing.
 */
trait HasStaticTypeSafety extends HasEvalType with HasPromoteToType {}
