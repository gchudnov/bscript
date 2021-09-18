package com.github.gchudnov.bscript.lang.ast

import com.github.gchudnov.bscript.lang.ast.visitors.TreeVisitor

abstract class AST:
  def visit[S, R](s: S, v: TreeVisitor[S, R]): Either[Throwable, R]
