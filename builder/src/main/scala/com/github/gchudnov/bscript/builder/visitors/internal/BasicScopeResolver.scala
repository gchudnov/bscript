package com.github.gchudnov.bscript.builder.visitors.internal

import com.github.gchudnov.bscript.builder.visitors.ScopeResolver
import com.github.gchudnov.bscript.builder.state.ScopeAsts
import com.github.gchudnov.bscript.builder.state.ScopeSymbols
import com.github.gchudnov.bscript.builder.Meta
import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.builder.Scope
import com.github.gchudnov.bscript.lang.symbols.Type
import com.github.gchudnov.bscript.builder.ScopeRef
import com.github.gchudnov.bscript.lang.symbols.TypeRef
import com.github.gchudnov.bscript.lang.symbols.SymbolRef
import com.github.gchudnov.bscript.builder.state.Forest
import com.github.gchudnov.bscript.lang.symbols.Symbol

final class BasicScopeResolver(forest: Forest[Scope], scopeSymbols: ScopeSymbols, scopeAsts: ScopeAsts) extends ScopeResolver {

  override def defineVar(scope: ScopeRef, name: String, vType: TypeRef): ScopeResolver =
    ???

  /**
    * Resolve the reference to a symbol.
    *
    * @param sym
    * @return
    */
  private[visitors] def resolve(sym: SymbolRef, start: Scope): Option[Symbol] =
    scopeSymbols
      .symbols(start)
      .find(_.name == sym.name)
      .orElse(forest.parentOf(start).flatMap(parent => resolve(sym, parent)))
    

  private def resolveMember(sym: SymbolRef): Option[Symbol] =
    ???

//    * Resolve a symbol in the scope recursively up to the root
//    */
//   def resolve(name: String, in: Scope): Either[ScopeStateException, Symbol] =
//     maybeResolve(name, in)
//       .toRight(new ScopeStateException(s"Cannot find a Symbol '${name}' starting from Scope '${in.name}'"))

//   private def maybeResolve(name: String, in: Scope): Option[Symbol] =
//     scopeSymbols
//       .get(Ptr(in))
//       .flatMap(_.find(_.name == name))
//       .orElse(scopeTree.parentOf(in).flatMap(parentScope => maybeResolve(name, parentScope)))

//   /**
//    * Resolves a member of a scope by Name
//    */
//   def resolveMember(name: String, in: Scope): Either[ScopeStateException, Symbol] =
//     maybeResolveMember(name, in)
//       .toRight(new ScopeStateException(s"Cannot find a Symbol '${name}' in Scope '${in.name}'"))

//   private def maybeResolveMember(name: String, in: Scope): Option[Symbol] =
//     scopeSymbols
//       .get(Ptr(in))
//       .flatMap(_.find(_.name == name))



  // override def scope(ast: AST): Option[Scope] =
  //   scopeAsts.scope(ast)



  override def result: Meta =
    Meta(
      forest = forest,
      scopeSymbols = scopeSymbols,
      scopeAsts = scopeAsts
    )

}
