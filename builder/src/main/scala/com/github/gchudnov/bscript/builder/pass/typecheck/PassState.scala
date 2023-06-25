package com.github.gchudnov.bscript.builder.pass.typecheck

import com.github.gchudnov.bscript.builder.state.EvalTypes
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.decls.*
import com.github.gchudnov.bscript.lang.types.Type
import com.github.gchudnov.bscript.lang.ast.types.*
import com.github.gchudnov.bscript.builder.state.ScopeAsts

private[typecheck] final case class PassState(
  scopeAsts: ScopeAsts,
  evalTypes: EvalTypes,
):

  /**
    * Find type declaration: MethodDecl, StructDecl, or TypeDecl, starting from the current scopt
    *
    * @return
    */
  def findTypeDecl(typeId: TypeId): Option[Decl] =
    // evalTypes.get(ast)
    ???

  /**
   * Set the type of the AST
   *
   * @param ast
   *   AST to set the type for
   * @param evalType
   *   type to set
   * @return
   *   state with the type set
   */
  def setEvalType(ast: AST, evalType: Type): PassState =
    this.copy(evalTypes = evalTypes.assign(ast, evalType))

  def assertCanAssign(fromAst: AST, toAst: AST): PassState =
    val fromType = evalTypes(fromAst)
    val toType   = evalTypes(toAst)

    if !canAssign(fromType, toType) then throw new IllegalStateException(s"Cannot assign type '${fromType}' to '${toType}'.")

    this

  /**
   * Check if the `from` type can be assigned to the `to` type.
   *
   * @param fromType
   *   the type to assign
   * @param toType
   *   the type to assign to
   * @return
   *   `true` if the `from` type can be assigned to the `to` type, `false` otherwise
   */
  private def canAssign(fromType: Type, toType: Type): Boolean =
    fromType == toType

private[typecheck] object PassState:

  def from(s: InState): PassState =
    PassState(
      scopeAsts = s.scopeAsts,
      evalTypes = EvalTypes.empty,
    )

  def into(s: PassState, ast: AST): OutState =
    OutState(
      ast = ast,
    )
