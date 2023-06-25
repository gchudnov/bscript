package com.github.gchudnov.bscript.builder.state

import com.github.gchudnov.bscript.lang.symbols.SVar
import com.github.gchudnov.bscript.lang.types.Type
import com.github.gchudnov.bscript.builder.util.Ptr
import com.github.gchudnov.bscript.builder.state.{VarTypes, BasicVarTypes}

trait VarTypes:
  def decl(s: SVar, t: Type): VarTypes

  def get(s: SVar): Option[Type]

  def getOrElse(s: SVar, default: => Type): Type

object VarTypes:
  lazy val empty: VarTypes =
    BasicVarTypes(Map.empty[Ptr[SVar], Type])

/**
 * A dictionary of SVar -> Type mappings
 */
final case class BasicVarTypes(dict: Map[Ptr[SVar], Type]) extends VarTypes:

  override def decl(s: SVar, t: Type): VarTypes =
    BasicVarTypes(dict + (Ptr(s) -> t))

  override def get(s: SVar): Option[Type] =
    dict.get(Ptr(s))

  override def getOrElse(s: SVar, default: => Type): Type =
    dict.getOrElse(Ptr(s), default)
