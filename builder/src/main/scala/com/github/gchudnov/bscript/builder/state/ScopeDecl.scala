package com.github.gchudnov.bscript.builder.state

import com.github.gchudnov.bscript.lang.ast.decls.Decl

/**
 * A Declaration with a Scope
 *
 * @param scope
 *   scope
 * @param decl
 *   declaration
 */
final case class ScopeDecl(scope: Scope, decl: Decl)
