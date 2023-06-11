package com.github.gchudnov.bscript.builder.pass.scoperesolve

import com.github.gchudnov.bscript.builder.BuilderException
import com.github.gchudnov.bscript.builder.pass.scoperesolve.InState
import com.github.gchudnov.bscript.builder.pass.scoperesolve.OutState
import com.github.gchudnov.bscript.builder.state.*
import com.github.gchudnov.bscript.lang.ast.*
import com.github.gchudnov.bscript.lang.ast.types.*
import com.github.gchudnov.bscript.lang.symbols.*
import com.github.gchudnov.bscript.lang.util.Casting
import com.github.gchudnov.bscript.lang.ast.decls.*

/**
  * ScopeResolve pass state
  */
private[scoperesolve] final case class PassState(
  scopeTree: Tree[Scope],
  scopeSymbols: ScopeSymbols,
  scopeAsts: ScopeAsts,
  varTypes: VarTypes,
):
  import Casting.*

  // /**
  //  * Resolve variable declaration:
  //  *   - resolve the name
  //  *   - resolve the type
  //  *
  //  * @param name
  //  *   variable name to resolve IN the scope
  //  * @param vType
  //  *   variable type to resolve IN & UP the scope
  //  * @param ast
  //  *   AST to resolve in
  //  * @return
  //  *   state with resolved variable and type
  //  */
  // def resolveVarDecl(name: String, vType: TypeAST, ast: AST): PassState =
  //   val errOrState = for
  //     scope        <- scopeAsts.scope(ast).toRight(new BuilderException(s"Scope for AST '${ast}' cannot be found"))
  //     resolvedName <- scopeSymbols.resolveIn(name, scope).toRight(new BuilderException(s"Symbol '${name}' cannot be resolved in scope '${scope}'"))
  //     resolvedType <- scopeSymbols.resolveUp(vType, scope)
  //     nameSVar     <- resolvedName.asSVar
  //     s1            = this.copy(varTypes = varTypes.decl(nameSVar, resolvedType))
  //   yield s1
  //   errOrState.toTry.get

  // // /**
  // //   * Resolve type declaration
  // //   *
  // //   * @param name
  // //   * @param from
  // //   * @return
  // //   */
  // // def resolveTypeDecl(name: String, from: AST): PassState =
  // //   val errOrState = for {
  // //     scope        <- scopeAsts.scope(from).toRight(new BuilderException(s"Scope for AST '${from}' cannot be found"))
  // //     resolvedName <- scopeSymbols.resolveIn(name, scope).toRight(new BuilderException(s"Symbol '${name}' cannot be resolved in scope '${scope}'"))
  // //   } yield this
  // //   errOrState.toTry.get

  // def findVarDecl(varDecl: VarDecl): Either[Throwable, SVar] =
  //   val errOrState = for
  //     scope        <- scopeAsts.scope(varDecl).toRight(new BuilderException(s"Scope for AST '${varDecl}' cannot be found"))
  //     resolvedName <- scopeSymbols.resolveIn(varDecl.name, scope).toRight(new BuilderException(s"Symbol '${varDecl.name}' cannot be resolved in scope '${scope}'"))
  //     typeId       <- varDecl.vType.asTypeId
  //     resolvedType <- scopeSymbols.resolveUp(typeId.name, scope, scopeTree).toRight(new BuilderException(s"Symbol '${typeId.name}' cannot be resolved up in scope '${scope}'"))
  //     nameSVar     <- resolvedName.asSVar
  //     s1            = this.copy(varTypes = varTypes.decl(nameSVar, resolvedType))
  //   yield s1
  //   errOrState.toTry.get

  /**
    * Find type declaration: MethodDecl, StructDecl, or TypeDecl, starting from the current scopt
    *
    * @return
    */
  def resolveTypeAST(typeId: TypeAST): Either[Throwable, Symbol & Type] =
    for {
      scope        <- scopeAsts.scope(typeId).toRight(new BuilderException(s"Scope for AST '${typeId}' cannot be found"))
      resolvedName <- scopeSymbols.resolveUp(typeId.name, scope, scopeTree).toRight(new BuilderException(s"Symbol '${typeId.name}' cannot be resolved up in scope '${scope}'"))
    } yield ()
    ???

    // TODO: finish implementation

    // TODO: we need to store observed types?

    // TODO: check how AST in Scala 3 + Go is implemented

    // TODO: in Go 


  // /**
  //  * Resolve type
  //  *
  //  * @param astType
  //  *   type to resolve UP the scope
  //  * @param ast
  //  *   AST to resolve in
  //  * @return
  //  *   state with resolved type
  //  */
  // def resolveType(ta: TypeAST, ast: AST): PassState =
  //   val errOrState = for
  //     scope        <- scopeAsts.scope(ast).toRight(new BuilderException(s"Scope for AST '${ast}' cannot be found"))
  //     resolvedType <- tryResolveUp(ta, scope)
  //     s1            = this
  //   yield s1
  //   errOrState.toTry.get

  // /**
  //  * Resolve the reference to a type, going up the scope hierarchy.
  //  *
  //  * @param vType
  //  *   type reference
  //  * @param start
  //  *   scope to start from
  //  * @return
  //  *   resolved type
  //  */
  // private[scoperesolve] def resolveUp(vType: TypeAST, start: Scope): Option[Type] =
  //   vType match
  //     case TypeId(name) =>
  //       Type.parse(name)
  //     case _ =>
  //       None

  // private[scoperesolve] def tryResolveUp(vType: TypeAST, start: Scope): Either[Throwable, Type] =
  //   resolveUp(vType, start).toRight(new BuilderException(s"TypeAST '${vType}' cannot be resolved up in scope '${start}'"))

object PassState:

  def from(s: InState): PassState =
    PassState(
      scopeTree = s.scopeTree,
      scopeSymbols = s.scopeSymbols,
      scopeAsts = s.scopeAsts,
      varTypes = VarTypes.empty,
    )

  def into(s: PassState, ast: AST, scopeAsts: ScopeAsts): OutState =
    OutState(
      ast = ast,
      scopeAsts = scopeAsts,
    )


// def resolveVarDecl(name: String, vType: TypeAST, ast: AST): PassState =
//   // val (sVar, sType) = (for
//   //   scope        <- scopeFor(ast).toRight(new BuilderException(s"Scope for AST '${ast}' cannot be found"))
//   //   resolvedName <- resolveIn(name, scope).toRight(new BuilderException(s"Variable '${name}' cannot be resolved in scope '${scope}'"))
//   //   resolvedType <- resolveUp(vType.name, scope).toRight(throw new BuilderException(s"Cannot resolve the variable type '${vType}' in scope '${scope}'"))
//   //   sVar         <- resolvedName.asSVar
//   //   sType        <- resolvedType.asType
//   // yield (sVar, sType)).toTry.get

//   // new BasicScopeResolver(forest, scopeSymbols, scopeAsts, varTypes.decl(sVar, sType))
//   ???
