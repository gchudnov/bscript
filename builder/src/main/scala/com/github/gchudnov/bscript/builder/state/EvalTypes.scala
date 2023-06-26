package com.github.gchudnov.bscript.builder.state

import com.github.gchudnov.bscript.lang.types.TType
import com.github.gchudnov.bscript.builder.util.Ptr
import com.github.gchudnov.bscript.lang.ast.AST

trait EvalTypes {}

// trait EvalTypes:
//   def assign(a: AST, t: Type): EvalTypes

//   def get(a: AST): Option[Type]

//   def apply(a: AST): Type

//   def getOrElse(a: AST, default: => Type): Type

// object EvalTypes:
//   lazy val empty: EvalTypes =
//     BasicEvalTypes(Map.empty[Ptr[AST], Type])

// /**
//  * A dictionary of AST -> Type mappings
//  */
// final case class BasicEvalTypes(dict: Map[Ptr[AST], Type]) extends EvalTypes:

//   override def assign(a: AST, t: Type): EvalTypes =
//     BasicEvalTypes(dict + (Ptr(a) -> t))

//   override def get(a: AST): Option[Type] =
//     dict.get(Ptr(a))

//   override def apply(a: AST): Type =
//     dict(Ptr(a))

//   override def getOrElse(a: AST, default: => Type): Type =
//     dict.getOrElse(Ptr(a), default)
