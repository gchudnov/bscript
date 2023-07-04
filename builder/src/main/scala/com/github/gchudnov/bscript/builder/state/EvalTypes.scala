package com.github.gchudnov.bscript.builder.state

import com.github.gchudnov.bscript.lang.ast.types.TypeAST
import com.github.gchudnov.bscript.builder.util.Ptr
import com.github.gchudnov.bscript.lang.ast.AST

/**
 * A Dictionary of AST -> Type mappings
 *
 * Used to propagate types in the AST so that we can do type checking
 */
sealed trait EvalTypes:
  def isEmpty: Boolean
  def size: Int

  def assignType(a: AST, t: TypeAST): EvalTypes

  def typeOf(a: AST): Option[TypeAST]

  def apply(a: AST): TypeAST

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
