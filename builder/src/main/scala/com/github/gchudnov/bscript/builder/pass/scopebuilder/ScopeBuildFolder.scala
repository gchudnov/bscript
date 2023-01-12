
import com.github.gchudnov.bscript.lang.ast.decls.TypeDecl
import com.github.gchudnov.bscript.lang.ast.decls.VarDeclpackage com.github.gchudnov.bscript.builder.pass.scopebuilder

import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.types.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.func.AstFolder

import com.github.gchudnov.bscript.lang.ast.decls.MethodDecl

import com.github.gchudnov.bscript.lang.ast.decls.StructDeclprivate[scopebuilder] final class ScopeBuildFolder() extends AstFolder[ScopeBuildState]:

  override def foldAST(s: ScopeBuildState, ast: AST): ScopeBuildState =
    ast match
      case x: Access =>
        foldOverAST(s, x)
      case x @ Id(name) =>
        foldOverAST(s, x)
      case x: Assign =>
        foldOverAST(s, x)
      case x: Block =>
        foldOverAST(s.push(), x).pop()
      case x @ Literal(const) =>
        foldOverAST(s, x)
      case x @ Call(id, args) =>
        foldOverAST(s.bind(x), x)
      case x @ Compiled(callback, retType) =>
        foldOverAST(s, x)
      case x: If =>
        foldOverAST(s, x)
      case x @ Init() =>
        foldOverAST(s, x)
      case x @ MethodDecl(name, tparams, params, retType, body) =>
        foldOverAST(s.define(SMethod(name)).push(), x).pop()
      case x @ StructDecl(name, tfields, fields) =>
        foldOverAST(s.define(SStruct(name)).push(), x).pop()
      case x @ VarDecl(name, vType, expr) =>
        foldOverAST(s.define(SVar(name)).bind(x), x)
      case x @ TypeDecl(name) =>
        foldOverAST(s.define(SType(name)).bind(x), x)
      case x @ Auto() =>
        foldOverAST(s, x)
      case x @ TypeId(_) =>
        foldOverAST(s, x)
      case x @ Applied(aType, args) =>
        foldOverAST(s, x)
      case x @ Vec(elems, elemType) =>
        foldOverAST(s, x)
      case x @ Dict(m, keyType, valType) =>
        foldOverAST(s, x)
      case other =>
        throw new MatchError(s"Unsupported AST type in ScopeBuildFolder: ${other}")

private[scopebuilder] object ScopeBuildFolder:

  def make(): ScopeBuildFolder =
    new ScopeBuildFolder()
