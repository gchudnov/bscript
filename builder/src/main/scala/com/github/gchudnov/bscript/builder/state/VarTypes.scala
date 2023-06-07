package com.github.gchudnov.bscript.builder.state

import com.github.gchudnov.bscript.lang.symbols.SVar
import com.github.gchudnov.bscript.lang.symbols.Type
import com.github.gchudnov.bscript.builder.util.Ptr

/**
 * A dictionary of SVar -> Type mappings
 */
final case class VarTypes(dict: Map[Ptr[SVar], Type]):

  def decl(s: SVar, t: Type): VarTypes =
    VarTypes(dict + (Ptr(s) -> t))

object VarTypes:
  lazy val empty: VarTypes =
    VarTypes(Map.empty[Ptr[SVar], Type])
