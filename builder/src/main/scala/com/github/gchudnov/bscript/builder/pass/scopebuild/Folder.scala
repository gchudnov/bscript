package com.github.gchudnov.bscript.builder.pass.scopebuild

import com.github.gchudnov.bscript.builder.pass.scopebuild.PassState
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.decls.*
import com.github.gchudnov.bscript.lang.ast.lit.*
import com.github.gchudnov.bscript.lang.ast.types.*
import com.github.gchudnov.bscript.lang.func.AstFolder
import com.github.gchudnov.bscript.lang.symbols.*

/**
 * Fold the AST to build it
 */
private[scopebuild] final class Folder() extends AstFolder[PassState]:

  override def foldAST(s: PassState, ast: AST): PassState =
    ast match
      case x: Access =>
        foldOverAST(s, x)
      case x @ Id(name) =>
        foldOverAST(s, x)

      case x @ MethodDecl(name, mType, body) =>
        foldOverAST(s.define(SMethod(name)).push(), x).pop()
      case x @ StructDecl(name, sType) =>
        foldOverAST(s.define(SStruct(name)).push(), x).pop()
      case x @ VarDecl(name, vType, expr) =>
        foldOverAST(s.define(SVar(name)).bind(x), x)
      case x @ TypeDecl(name) =>
        foldOverAST(s.define(SType(name)).bind(x), x)

      case x: Annotated =>
        foldOverAST(s, x)
      case x: Assign =>
        foldOverAST(s, x)
      case x: Block =>
        foldOverAST(s.push(), x).pop()
      case x @ Call(id, args) =>
        foldOverAST(s.bind(x), x)
      case x @ Compiled(callback, retType) =>
        foldOverAST(s, x)
      case x: If =>
        foldOverAST(s, x)
      case x @ Init() =>
        foldOverAST(s, x)

      case x @ ConstLit(const) =>
        foldOverAST(s, x)
      case x @ GroupLit(cType, elems) =>
        foldOverAST(s, x)
      case x @ MethodLit(mType, body) =>
        foldOverAST(s.define(SMethod("anon")).push(), x).pop()

      case x @ Auto() =>
        foldOverAST(s, x)
      case x @ TypeId(name) =>
        foldOverAST(s, x)
      case x @ VecType(elemType) =>
        foldOverAST(s, x)
      case x @ MapType(keyType, valType) =>
        foldOverAST(s, x)
      case x @ StructType(tfields, fields) =>
        foldOverAST(s, x)
      case x @ MethodType(tparams, params, retType) =>
        foldOverAST(s, x)

      case other =>
        throw new MatchError(s"Unsupported AST type in Folder: ${other}")

private[scopebuild] object Folder:

  def make(): Folder =
    new Folder()
