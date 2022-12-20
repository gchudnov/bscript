package com.github.gchudnov.bscript.builder.visitors.internal

import com.github.gchudnov.bscript.builder.visitors.ScopeResolver
import com.github.gchudnov.bscript.builder.state.ScopeAsts
import com.github.gchudnov.bscript.builder.state.ScopeSymbols
import com.github.gchudnov.bscript.builder.Meta
import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.builder.Scope
import com.github.gchudnov.bscript.lang.symbols.Type

final class BasicScopeResolver(scopeAsts: ScopeAsts) extends ScopeResolver {

  override def resolve(symbol: Symbol): Option[Symbol] =
    ???

  override def resolveMember(symbol: Symbol): Option[Symbol] =
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



  override def scope(ast: AST): Option[Scope] =
    scopeAsts.scope(ast)

  override def defineVar(scope: Scope, name: String, vType: Type): ScopeResolver =
    ???


  override def result: Meta =
    Meta(
      forest = ???,
      scopeSymbols = ???,
      scopeAsts = scopeAsts
    )

}
