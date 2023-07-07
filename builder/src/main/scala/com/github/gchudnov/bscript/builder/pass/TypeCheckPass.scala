package com.github.gchudnov.bscript.builder.pass

import com.github.gchudnov.bscript.lang.func.ASTFolder
import com.github.gchudnov.bscript.builder.state.*
import com.github.gchudnov.bscript.builder.env.*
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.decls.*
import com.github.gchudnov.bscript.lang.ast.lit.*
import com.github.gchudnov.bscript.lang.ast.types.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.builder.BuilderException
import com.github.gchudnov.bscript.lang.ast.refs.Access
import com.github.gchudnov.bscript.lang.ast.refs.Id
import com.github.gchudnov.bscript.lang.const.Const
import com.github.gchudnov.bscript.lang.types.TypeName

/**
 * #4 - Type Check Pass
 *
 *   - Checks the types of the AST nodes to see if they are matching.
 */
final class TypeCheckPass extends Pass[HasAST & HasReadEvalTypes, Unit]:

  override def run(in: HasAST & HasReadEvalTypes): Unit =
    val state0 = TypeCheckState.from(in.evalTypes)
    val ast0   = in.ast

    val folder = new TypeCheckFolder()

    val state1 = folder.foldAST(state0, ast0)

    val out = ()

    out

/**
 * Type Check Folder
 */
private final class TypeCheckFolder() extends ASTFolder[TypeCheckState]:

  override def foldAST(s: TypeCheckState, ast: AST): TypeCheckState =
    ast match
      case x: VarDecl =>
        foldOverAST(s, x).checkVarDecl(x)

      case x: Assign =>
        foldOverAST(s, x).checkAssign(x)

      case other =>
        foldOverAST(s, other)

      // case other =>
      //   throw new MatchError(s"Unsupported AST type in TypeCheckFolder: ${other}")

private final case class TypeCheckState(evalTypes: ReadEvalTypes):

  /**
   * Get type of the AST node.
   *
   * @param ast
   *   AST node
   * @return
   *   type
   */
  def evalTypeOf(ast: AST): TypeAST =
    val ot = evalTypes.typeOf(ast)
    ot.getOrElse(throw BuilderException(s"Type of the AST node is not defined: ${ast}, this is a bug in BScript."))

  /**
   * Check the types of the Assign operator
   *
   * NOTE: it is always allowed to assign Nothing to a variable.
   */
  def checkAssign(a: Assign): TypeCheckState =
    val lhsType = evalTypeOf(a.lhs)
    val rhsType = evalTypeOf(a.rhs)

    if (lhsType != rhsType) && !TypeAST.isNothing(rhsType) then throw BuilderException(s"Type mismatch: ${lhsType} != ${rhsType} in the assignment")

    if lhsType == Auto then throw BuilderException(s"Cannot assign to Auto type")

    this

  /**
   * Check the types of the VarDecl operator
   *
   * NOTE: it is always allowed to assign Nothing to a variable.
   */
  def checkVarDecl(v: VarDecl): TypeCheckState =
    val aType    = evalTypeOf(v.aType)
    val exprType = evalTypeOf(v.expr)

    if (aType != exprType) && !TypeAST.isNothing(exprType) then throw BuilderException(s"Type mismatch: ${aType} != ${exprType} in the variable declaration")

    if exprType == Auto then throw BuilderException(s"Cannot assign to Auto type")

    this

private object TypeCheckState:
  def from(evalTypes: ReadEvalTypes): TypeCheckState =
    TypeCheckState(
      evalTypes = evalTypes,
    )
