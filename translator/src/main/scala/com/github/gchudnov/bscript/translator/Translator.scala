package com.github.gchudnov.bscript.translator

import com.github.gchudnov.bscript.lang.ast.AST

trait Translator:
  def translate(ast1: AST): Either[Throwable, String]
