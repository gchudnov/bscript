package com.github.gchudnov.bscript.builder.state

import com.github.gchudnov.bscript.lang.ast.types.TypeAST
import com.github.gchudnov.bscript.builder.util.Ptr
import com.github.gchudnov.bscript.lang.ast.AST

/**
 * Read Eval Types
 */
sealed trait ReadEvalTypes:
  def isEmpty: Boolean
  def size: Int

  def typeOf(a: AST): Option[TypeAST]

  def apply(a: AST): TypeAST

/**
 * Write Eval Types
 */
sealed trait WriteEvalTypes:
  def assignType(a: AST, t: TypeAST): EvalTypes

/**
 * A Dictionary of AST -> Type mappings
 *
 * Used to propagate types in the AST so that we can do type checking
 */
sealed trait EvalTypes extends ReadEvalTypes with WriteEvalTypes

object EvalTypes:
  lazy val empty: EvalTypes =
    BasicEvalTypes(Map.empty[Ptr[AST], TypeAST])

/**
 * A dictionary of AST -> Type mappings implementation
 */
private final case class BasicEvalTypes(dict: Map[Ptr[AST], TypeAST]) extends EvalTypes:

  override def isEmpty: Boolean =
    dict.isEmpty

  override def size: Int =
    dict.size

  override def assignType(a: AST, t: TypeAST): EvalTypes =
    BasicEvalTypes(dict + (Ptr(a) -> t))

  override def typeOf(a: AST): Option[TypeAST] =
    dict.get(Ptr(a))

  override def apply(a: AST): TypeAST =
    dict(Ptr(a))
