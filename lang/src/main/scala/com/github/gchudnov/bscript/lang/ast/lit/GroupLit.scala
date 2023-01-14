package com.github.gchudnov.bscript.lang.ast.lit

import com.github.gchudnov.bscript.lang.ast.types.TypeAST
import com.github.gchudnov.bscript.lang.ast.Expr

final case class GroupLit(cType: TypeAST, elems: List[Expr]) extends Lit
