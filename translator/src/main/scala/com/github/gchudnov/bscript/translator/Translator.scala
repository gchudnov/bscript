package com.github.gchudnov.bscript.translator

import com.github.gchudnov.bscript.lang.ast.AST

trait Translator:
  def fromAST(ast1: AST): Either[Throwable, String]

  inline def toAST[T](inline x: T): AST