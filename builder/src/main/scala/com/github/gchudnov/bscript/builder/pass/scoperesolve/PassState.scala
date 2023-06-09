package com.github.gchudnov.bscript.builder.pass.scoperesolve

import com.github.gchudnov.bscript.builder.state.Scope
import com.github.gchudnov.bscript.builder.state.ScopeRef
import com.github.gchudnov.bscript.builder.state.Tree
import com.github.gchudnov.bscript.builder.state.ScopeAsts
import com.github.gchudnov.bscript.builder.state.ScopeSymbols
import com.github.gchudnov.bscript.builder.state.VarTypes
import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.lang.symbols.Symbol
import com.github.gchudnov.bscript.lang.symbols.Type
import com.github.gchudnov.bscript.lang.util.Casting
import com.github.gchudnov.bscript.lang.ast.types.TypeAST
import com.github.gchudnov.bscript.builder.pass.scoperesolve.InState
import com.github.gchudnov.bscript.builder.pass.scoperesolve.OutState
import com.github.gchudnov.bscript.builder.BuilderException
import com.github.gchudnov.bscript.lang.ast.types.TypeId
import com.github.gchudnov.bscript.lang.symbols.SType

private[scoperesolve] final case class PassState(
  scopeTree: Tree[Scope],
  scopeSymbols: ScopeSymbols,
  scopeAsts: ScopeAsts,
  varTypes: VarTypes,
):
  import Casting.*

  /**
   * Resolve variable declaration:
   *   - resolve the name
   *   - resolve the type
   *
   * @param name
   *   variable name to resolve IN the scope
   * @param vType
   *   variable type to resolve IN & UP the scope
   * @param ast
   *   AST to resolve in
   * @return
   *   state with resolved variable and type
   */
  def resolveVarDecl(name: String, vType: TypeAST, ast: AST): PassState =
    val errOrState = for
      scope        <- tryScopeFor(ast)
      resolvedName <- tryResolveIn(name, scope)
      resolvedType <- tryResolveUp(vType, scope)
      nameSVar     <- resolvedName.asSVar
      s1            = this.copy(varTypes = varTypes.decl(nameSVar, resolvedType))
    yield s1
    errOrState.toTry.get

  /**
   * Resolve type
   *
   * @param astType
   *   type to resolve UP the scope
   * @param ast
   *   AST to resolve in
   * @return
   *   state with resolved type
   */
  def resolveType(astType: TypeAST, ast: AST): PassState =
    val errOrState = for
      scope        <- tryScopeFor(astType)
      resolvedType <- tryResolveUp(astType, scope)
      s1            = this
    yield s1
    errOrState.toTry.get

    // TODO: finish the implementation ^^^

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

  /**
   * Get scope for the given AST
   *
   * @param ast
   * @return
   */
  private[scoperesolve] def scopeFor(ast: AST): Option[Scope] =
    scopeAsts.scope(ast)

  /**
   * Get scope for the given AST
   *
   * @param ast
   *   AST to resolve in
   * @return
   *   resolved scope
   */
  private[scoperesolve] def tryScopeFor(ast: AST): Either[Throwable, Scope] =
    scopeFor(ast).toRight(new BuilderException(s"Scope for AST '${ast}' cannot be found"))

  /**
   * Resolve the reference to the symbol in the given scope only.
   *
   * @param sym
   *   symbol reference
   * @return
   *   resolved symbol
   */
  private[scoperesolve] def resolveIn(name: String, in: Scope): Option[Symbol] =
    scopeSymbols
      .symbols(in)
      .find(_.name == name)

  /**
   * Resolve the reference to the symbol in the given scope only.
   *
   * @param name
   *   symbol reference
   * @param in
   *   scope to resolve in
   * @return
   *   resolved symbol
   */
  private[scoperesolve] def tryResolveIn(name: String, in: Scope): Either[Throwable, Symbol] =
    resolveIn(name, in).toRight(new BuilderException(s"Symbol '${name}' cannot be resolved in scope '${in}'"))

  /**
   * Resolve the reference to a symbol, going up the scope hierarchy.
   *
   * @param sym
   *   symbol reference
   * @return
   *   resolved symbol
   */
  private[scoperesolve] def resolveUp(name: String, start: Scope): Option[Symbol] =
    scopeSymbols
      .symbols(start)
      .find(_.name == name)
      .orElse(scopeTree.parentOf(start).flatMap(parent => resolveUp(name, parent)))

  /**
   * Resolve the reference to a symbol, going up the scope hierarchy.
   *
   * @param name
   *   symbol reference
   * @param start
   *   scope to start from
   * @return
   *   resolved symbol
   */
  private[scoperesolve] def tryResolveUp(name: String, start: Scope): Either[Throwable, Symbol] =
    resolveUp(name, start).toRight(new BuilderException(s"Symbol '${name}' cannot be resolved up in scope '${start}'"))

  /**
   * Resolve the reference to a type, going up the scope hierarchy.
   *
   * @param vType
   *   type reference
   * @param start
   *   scope to start from
   * @return
   *   resolved type
   */
  private[scoperesolve] def resolveUp(vType: TypeAST, start: Scope): Option[Type] =
    vType match
      case TypeId(name) =>
        SType.parse(name)
      case _ =>
        None

  private[scoperesolve] def tryResolveUp(vType: TypeAST, start: Scope): Either[Throwable, Type] =
    resolveUp(vType, start).toRight(new BuilderException(s"TypeAST '${vType}' cannot be resolved up in scope '${start}'"))

  // TODO: resolve types, implement it; add tests

  // TODO: check `VarTypes(dict: Map[Ptr[SVar], Type])` and how is it being used

  // TODO: add more interfacs to use instead of case clases that open the implementation

  // TODO: state should be interface-based as well?

object PassState:

  def from(s: InState): PassState =
    PassState(
      scopeTree = s.scopeTree,
      scopeSymbols = s.scopeSymbols,
      scopeAsts = s.scopeAsts,
      varTypes = VarTypes.empty,
    )

  def into(s: PassState, ast: AST): OutState =
    OutState(
      ast = ast,
    )
