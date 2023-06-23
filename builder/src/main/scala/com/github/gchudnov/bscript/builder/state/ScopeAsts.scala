package com.github.gchudnov.bscript.builder.state

import com.github.gchudnov.bscript.builder.util.Dict
import com.github.gchudnov.bscript.builder.state.Scope
import com.github.gchudnov.bscript.lang.ast.AST
import com.github.gchudnov.bscript.builder.util.Ptr
import com.github.gchudnov.bscript.builder.util.Show
import ScopeAsts.given

/**
 * A Dictionary of Scope -> AST Interface
 *
 *   - a scope can have multiple ASTs
 *   - an AST can belong to only one scope
 */
sealed trait ScopeAsts:
  def link(scope: Scope, ast: AST): ScopeAsts

  def scope(ast: AST): Option[Scope]
  def asts(scope: Scope): List[AST]

  def print: String

object ScopeAsts:
  lazy val empty: ScopeAsts =
    BasicScopeAsts(keyValues = Map.empty[Scope, Set[Ptr[AST]]], valueKey = Map.empty[Ptr[AST], Scope])

  given showAst: Show[AST] = new Show[AST]:
    override def show(a: AST): String =
      s"ast(${a.toString})"

  given showPtrAst: Show[Ptr[AST]] = new Show[Ptr[AST]]:
    override def show(a: Ptr[AST]): String =
      s"ptr(${showAst.show(a.value)})"

/**
 * A Dictionary of Scope -> AST Implementation
 */
private[state] final case class BasicScopeAsts(keyValues: Map[Scope, Set[Ptr[AST]]], valueKey: Map[Ptr[AST], Scope]) extends Dict[Scope, Ptr[AST], BasicScopeAsts] with ScopeAsts:
  override protected def clone(keyValues: Map[Scope, Set[Ptr[AST]]], valueKey: Map[Ptr[AST], Scope]): BasicScopeAsts =
    BasicScopeAsts(keyValues = keyValues, valueKey = valueKey)

  override def link(scope: Scope, ast: AST): ScopeAsts =
    set(scope, Ptr(ast))

  override def scope(ast: AST): Option[Scope] =
    key(Ptr(ast))

  override def asts(scope: Scope): List[AST] =
    values(scope).map(_.value)

  override def print: String =
    val sb = new StringBuilder
    sb.append("scopeAsts ").append(this.show)
    sb.toString
