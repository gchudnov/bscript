package com.github.gchudnov.bscript.builder.env

import com.github.gchudnov.bscript.builder.state.EvalTypes

trait HasEvalTypes:
  def evalTypes: EvalTypes

object HasEvalTypes:
  def apply(et: EvalTypes): HasEvalTypes = new HasEvalTypes:
    override val evalTypes: EvalTypes = et
