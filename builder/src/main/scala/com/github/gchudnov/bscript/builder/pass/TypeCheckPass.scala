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
  def typeOf(ast: AST): TypeAST =
    val ot = evalTypes.typeOf(ast)
    ot.getOrElse(throw BuilderException(s"Type of the AST node is not defined: ${ast}, this is a bug."))

  /**
   * Check the types of the Assign operator
   */
  def checkAssign(a: Assign): TypeCheckState =
    val lhsType = typeOf(a.lhs)
    val rhsType = typeOf(a.rhs)

    if lhsType != rhsType then throw BuilderException(s"Type mismatch: ${lhsType} != ${rhsType} in the assignment")

    if lhsType == Auto then throw BuilderException(s"Cannot assign to Auto type")

    this

  def checkVarDecl(v: VarDecl): TypeCheckState =
    val aType    = typeOf(v.aType)
    val exprType = typeOf(v.expr)

    if aType != exprType then throw BuilderException(s"Type mismatch: ${aType} != ${exprType} in the variable declaration")

    if exprType == Auto then throw BuilderException(s"Cannot assign to Auto type")

    this

private object TypeCheckState:
  def from(evalTypes: ReadEvalTypes): TypeCheckState =
    TypeCheckState(
      evalTypes = evalTypes,
    )
