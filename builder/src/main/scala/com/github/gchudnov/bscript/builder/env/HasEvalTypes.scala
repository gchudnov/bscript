package com.github.gchudnov.bscript.builder.env

import com.github.gchudnov.bscript.builder.state.EvalTypes
import com.github.gchudnov.bscript.builder.state.ReadEvalTypes
import com.github.gchudnov.bscript.builder.state.WriteEvalTypes

trait HasReadEvalTypes:
  def evalTypes: ReadEvalTypes

trait HasWriteEvalTypes:
  def evalTypes: WriteEvalTypes

trait HasEvalTypes:
  def evalTypes: EvalTypes

object HasEvalTypes:
  def apply(et: EvalTypes): HasEvalTypes = new HasEvalTypes:
    override val evalTypes: EvalTypes = et
