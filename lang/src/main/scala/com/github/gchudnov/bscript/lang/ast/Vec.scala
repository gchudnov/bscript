package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.symbols.Type
import com.github.gchudnov.bscript.lang.ast.types.TypeAST
// import com.github.gchudnov.bscript.lang.symbols.TypeRef

/**
 * Collection (Vector)
 *
 * {{{
 *   [1, 2, 3, 4, 5]
 * }}}
 *
 * @param elems
 *   Elements of the collection
 * @param elemType
 *   Elements of the collection
 */
final case class Vec(elems: List[Expr], elemType: TypeAST) extends Col

// object Vec:
//   def apply(): Vec =
//     new Vec(elements = List.empty[Expr], elementType = TypeRef.auto)

//   def apply(elements: List[Expr]): Vec =
//     new Vec(elements = elements, elementType = TypeRef.auto)
