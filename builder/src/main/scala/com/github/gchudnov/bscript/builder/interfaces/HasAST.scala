package com.github.gchudnov.bscript.builder.interfaces

import com.github.gchudnov.bscript.lang.ast.AST

trait HasAST:
  def ast: AST

object HasAST:
  def apply(a: AST): HasAST = new HasAST:
    override def ast: AST = a
