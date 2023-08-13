package com.github.gchudnov.bscript.lang.ast.lit

import com.github.gchudnov.bscript.lang.ast.types.TypeAST
import com.github.gchudnov.bscript.lang.ast.Expr

/**
  * Collection Literal, e.g. Vector, Array or Map.
  *
  * @param cType
  * @param elems
  */
final case class CollectionLit(cType: TypeAST, elems: List[Expr]) extends Lit
