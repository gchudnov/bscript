package com.github.gchudnov.bscript.lang.ast.decls

import com.github.gchudnov.bscript.lang.ast.Expr

// TODO: do we want to add EnumDecl ?

/**
 * An abstract declaration
 */
abstract class Decl extends Expr:
  /**
   * Name of the declaration
   *
   * @return
   *   name
   */
  def name: String
