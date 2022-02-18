package com.github.gchudnov.bscript.inspector

import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.interpreter.memory.CellPath
import com.github.gchudnov.bscript.lang.types.TypeNames
import com.github.gchudnov.bscript.inspector.internal.dbglib.MemWatch

object Inspector:

  /**
   * Adds memory tracing capabilities to the given AST.
   */
  def memWatch(path: CellPath, ast0: AST, typeNames: TypeNames): Either[Throwable, AST] =
    MemWatch.make(path, ast0, typeNames)
