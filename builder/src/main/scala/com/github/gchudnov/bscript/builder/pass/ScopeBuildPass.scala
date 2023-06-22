package com.github.gchudnov.bscript.builder.pass

import com.github.gchudnov.bscript.lang.func.AstFolder
import com.github.gchudnov.bscript.builder.interfaces.*
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.decls.*
import com.github.gchudnov.bscript.lang.ast.lit.*
import com.github.gchudnov.bscript.lang.ast.types.*
import com.github.gchudnov.bscript.lang.symbols.*

/*
output:
    ast: AST,
  scopeTree: Tree[Scope],
  scopeSymbols: ScopeSymbols,
  scopeAsts: ScopeAsts

*/

/**
 * #1 - Scope Build Pass
  */
final class ScopeBuildPass extends Pass[HasAST, HasScopeTree & HasAST] {

  override def run(in: HasAST): HasScopeTree & HasAST = 
    val state0 = ScopeBuildState.empty
    val ast0 = in.ast

    val folder = new ScopeBuildFolder()

    val state1 = folder.foldAST(state0, ast0)

    // TODO: produce the output state

    ???


}

/**
  * Scope Build Folder
  */
private final class ScopeBuildFolder() extends AstFolder[ScopeBuildState] {

  override def foldAST(s: ScopeBuildState, ast: AST): ScopeBuildState =
    ast match
      case x: Access =>
        foldOverAST(s, x)
      case x @ Id(name) =>
        foldOverAST(s, x)

      // TODO: a declaration should be unified, see cpp2 tests
      case x @ MethodDecl(name, mType, body) =>
        foldOverAST(s.define(SMethod(name)).bind(x).push(), x).pop()
      case x @ StructDecl(name, sType) =>
        foldOverAST(s.define(SStruct(name)).bind(x).push(), x).pop()
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
      // TODO: define for KeyValue

      case x @ ConstLit(const) =>
        foldOverAST(s, x)
      case x @ CollectionLit(cType, elems) =>
        foldOverAST(s, x)
      case x @ MethodLit(mType, body) =>
        foldOverAST(s.define(SMethod("anon")).push(), x).pop() // TODO: predefined name is incorrect, instead we should generate a new one ?

      case x @ Auto() =>
        foldOverAST(s, x)
      case x @ TypeId(name) =>
        foldOverAST(s, x).bind(x)
      case x @ VecType(elemType) =>
        foldOverAST(s, x)
      case x @ MapType(keyType, valType) =>
        foldOverAST(s, x)
      case x @ StructType(tfields, fields) =>
        foldOverAST(s, x)
      case x @ MethodType(tparams, params, retType) =>
        foldOverAST(s, x)

      case other =>
        throw new MatchError(s"Unsupported AST type in Build-Folder: ${other}")

}

/**
  * Scope Build State
  *
  */
private final case class ScopeBuildState() {

  def pushState(): ScopeBuildState =
    ???

  def popState(): ScopeBuildState =
    ???

  def defineSymbol(symbol: Symbol): ScopeBuildState =
    ???

  def bindAstToScope(ast: AST): ScopeBuildState =
    ???
}

/**
  * Scope Build State Companion
  */
private object ScopeBuildState {
  lazy val empty: ScopeBuildState = 
    ScopeBuildState()
}
